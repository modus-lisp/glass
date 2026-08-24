;;;; backend.lisp — a McCLIM backend that renders into a glass framebuffer and
;;;; serves it over VNC (RFB), with keyboard/pointer coming back from the client.
;;;;
;;;; McCLIM's software renderer (mcclim-render) already rasterizes every CLIM
;;;; drawing op into an (unsigned-byte 32) 0xAARRGGBB image.  The stock CLX-fb
;;;; backend blits that image to an X window; we instead copy the dirty region
;;;; into a glass framebuffer and let glass ship it over RFB.  So this file is
;;;; almost entirely glue: reuse RENDER-PORT-MIXIN / RENDER-MEDIUM-MIXIN for the
;;;; drawing, add a mirror that owns a glass fb, and translate RFB input events
;;;; into CLIM events.  No X server, no FFI — the whole path is Lisp.

(in-package #:clim-glass)

;;; ---- port ------------------------------------------------------------------

;;; A port is a SESSION: what runs.  A SEAT is who is watching (see seat.lisp) — a
;;; screen, a pointer, a keyboard, and an arrangement of the session's windows.  Every
;;; slot that was really one watcher's has moved to the seat; what is left here is what
;;; a second watcher must not get a second copy of.
;;;
;;; The old GLASS-PORT-FB / -PX / -MENU / ... accessors are kept, DELEGATING to the
;;; port's default seat, so every call site — in this tree, in loom, in warren, in the
;;; inspect harnesses, and in whatever is typed at a running desktop's control socket —
;;; goes on meaning what it meant.  They are deprecated in the sense that new code
;;; inside the WM should take a SEAT and say (SEAT-FB SEAT); they are not going away,
;;; because "the default seat" is exactly the right answer for a session that has one.
(defclass glass-port (mcclim-render::render-port-mixin)
  ((seats    :initform '() :accessor glass-port-seats)        ; every seat watching, newest-first
   (default-seat :initform nil :accessor glass-port-default-seat)  ; the one the old accessors mean
   ;; WHICH SEAT IS DRIVING McCLIM, and how that changes hands.  McCLIM has ONE pointer,
   ;; ONE keyboard focus and ONE sheet transformation per port (CLIMI::PORT-POINTER,
   ;; PORT-KEYBOARD-INPUT-FOCUS, SHEET-TRANSFORMATION), so McCLIM windows are one
   ;; consolidated seat however many are watching.  This is that token — held by the last
   ;; seat to press inside a McCLIM window, free again when that seat's pointer leaves,
   ;; goes idle or goes away, and never handed over in the middle of a gesture.  Its whole
   ;; policy, and the reason the token's "primary" is NOT the home seat's, is
   ;; clim-token.lisp.  Native glass surfaces — terminals, the browser, warren, a nested
   ;; remote desktop — carry no such assumption and are genuinely per-seat.  This is a
   ;; documented seam, not a bug: the seat abstraction lives at the surface level and
   ;; McCLIM is one surface-producing client that does not implement it.
   ;; (No :accessor — GLASS-PORT-CLIM-TOKEN is an ordinary function in clim-token.lisp,
   ;; which is loaded first and must be able to name it.)
   (clim-token :initform (make-instance 'clim-token))
   (mailbox  :initform (sb-concurrency:make-mailbox) :reader glass-port-mailbox)
   (top      :initform nil :accessor glass-port-top)          ; the MAIN top-level sheet
   (mirrors  :initform '() :accessor glass-port-mirrors)      ; all top-level mirrors, newest-first
   (clock    :initform 0   :accessor glass-port-clock)        ; monotonic timestamps
   ;; --- window-manager mode (OPEN LOOK) ---
   (wm-p     :initform nil :accessor glass-port-wm-p)         ; decorate + manage windows?
   ;; THE SESSION'S TASTE IN WALLPAPER: the picture, not any seat's pixels of it.
   ;; SEAT-BG holds a rasterisation at ONE seat's screen size -- that is why the two are
   ;; separate, and why this has to exist at all.  Without it the picture is consumed at
   ;; set time and discarded, so a seat whose screen CHANGES SIZE is left holding a
   ;; wallpaper rendered for a size it no longer is, with nothing to re-render from.
   (bg-path  :initform nil :accessor glass-port-bg-path)
   (bg-mode  :initform :cover :accessor glass-port-bg-mode)
   ;; Next window placement offset.  SESSION-wide, not per-seat, because what it places
   ;; is a window's DEFAULT position — the one a seat sees until it moves the window
   ;; itself — and there is only one default to place.
   (cascade  :initform 0   :accessor glass-port-cascade)
   (surfaces :initform '() :accessor glass-port-surfaces)     ; non-McCLIM windows (e.g. terminals)
   ;; Hands out the next z.  MIRRORS and SURFACES record MEMBERSHIP; neither records
   ;; stacking any more, because where a window sits in the stack is not a property of
   ;; which kind of window it is.  See WM-STACKING-ORDER.  One counter for the whole
   ;; session even though stacking is per-seat: a z is only a ticket, and tickets drawn
   ;; from one monotonic counter stay comparable between a window this seat restacked
   ;; and one it has never touched.
   (zclock   :initform 0   :accessor glass-port-zclock)
   (menu-items :initform '() :accessor glass-port-menu-items)  ; (label . thunk) list for the root menu
   ;; A medium kept only to be ASKED things — see the text-measurement section below.
   (ruler    :initform nil :accessor glass-port-ruler))
  (:default-initargs :pointer (make-instance 'climi::standard-pointer)))

(defun parse-glass-server-path (path) path)     ; plist tail becomes initargs

(defmethod find-port-type ((type (eql :glass)))
  (values 'glass-port 'parse-glass-server-path))

;;; ---- the delegating accessors ----------------------------------------------
;;; Each of these was a GLASS-PORT slot and is now the default seat's.  Written out
;;; through a macro rather than by hand so that the list IS the documentation of what
;;; turned out to be per-seat.

(defmacro define-seat-delegate (port-accessor seat-accessor)
  "Define PORT-ACCESSOR (and its SETF) as SEAT-ACCESSOR of PORT's default seat."
  `(progn
     (defun ,port-accessor (port) (,seat-accessor (glass-port-default-seat port)))
     (defun (setf ,port-accessor) (value port)
       (setf (,seat-accessor (glass-port-default-seat port)) value))))

(define-seat-delegate glass-port-fb            seat-fb)
(define-seat-delegate glass-port-screen-w      seat-screen-w)
(define-seat-delegate glass-port-screen-h      seat-screen-h)
(define-seat-delegate glass-port-bg            seat-bg)
(define-seat-delegate glass-port-wake          seat-wake)
(define-seat-delegate glass-port-server        seat-server)
(define-seat-delegate glass-port-num           seat-port-num)
(define-seat-delegate glass-port-mods          seat-mods)
(define-seat-delegate glass-port-buttons       seat-buttons)
(define-seat-delegate glass-port-px            seat-px)
(define-seat-delegate glass-port-py            seat-py)
(define-seat-delegate glass-port-focus-surface seat-focus-surface)
(define-seat-delegate glass-port-grab-sheet    seat-grab-sheet)
(define-seat-delegate glass-port-drag          seat-drag)
(define-seat-delegate glass-port-drag-wire     seat-drag-wire)
(define-seat-delegate glass-port-drag-wire-box seat-drag-wire-box)
(define-seat-delegate glass-port-menu          seat-menu)
(define-seat-delegate glass-port-frame-copy    seat-frame-copy)
(define-seat-delegate glass-port-pending       seat-pending)
(define-seat-delegate glass-port-pending-lock  seat-pending-lock)
(define-seat-delegate glass-port-clipboard     seat-clipboard)

(defun port-seat (port &optional seat)
  "SEAT if given, else PORT's default seat — the argument-defaulting every WM entry
   point does, in one place, so `(wm-hit port x y)' still means what it always did."
  (or seat (glass-port-default-seat port)))

(defun add-seat (port &key name (port-num 5900) (width 1000) (height 720) primary fb)
  "Attach a new SEAT to PORT: a screen of its own at WIDTH x HEIGHT, its own hands, and
   an empty set of views (so it sees the session's windows exactly where they stand).
   The FIRST seat is the HOME seat: it inherits the things a session has exactly one of
   (the session clipboard, the session mix, the key injector, the desktop name), it is
   PORT-SEAT's default, and its window moves write the session's own arrangement.  That
   is a fixed role and NOT the same as driving McCLIM, which is a token that changes
   hands — see clim-token.lisp.

   The seat also gets an IDENTITY of its own if this image can mint one (:glass/nostr),
   keyed by its NAME so the same seat is the same npub across a restart.  Without that
   system the identity is NIL and the desktop is exactly the desktop it was.

   PORT-NUM is the port this seat serves on WHEN IT SERVES.  Nothing listens because a
   seat exists: OPEN-SEAT-TRANSPORT is what exposes one, and it is a separate call."
  (let* ((first-p (null (glass-port-seats port)))
         (primary (or primary first-p))
         (seat (make-instance 'seat :name (or name (format nil "seat-~d" port-num))
                                    :port port :primary primary :port-num port-num
                                    :screen-w width :screen-h height
                                    :fb fb
                                    ;; The first seat IS the session's selection, so a
                                    ;; one-seat desktop keeps having exactly one and
                                    ;; everything that reaches the clipboard without a seat
                                    ;; to ask (dictation, a paste chord, an app calling
                                    ;; SESSION-CLIPBOARD) lands where the only person here
                                    ;; is looking.  A further seat gets its own.
                                    :clipboard (if primary
                                                   (glass:session-clipboard)
                                                   (glass:make-clipboard)))))
    (setf (glass-port-seats port) (append (glass-port-seats port) (list seat)))
    ;; Who this seat IS — asked for once, when the place is made.  A no-op without
    ;; :glass/nostr, and never fatal: see ENSURE-SEAT-IDENTITY.
    (ensure-seat-identity seat)
    ;; THIS SEAT'S KEYBOARD, made with the seat and not with its listener.  It was made by
    ;; START-SEAT-SERVER, which was fine while a seat and a listener arrived together and
    ;; is not now: a seat that serves nothing still has hands (dictation types on this, and
    ;; so does a paste), and a seat that opens two transports must not grow two keyboards.
    ;; The listener still installs it as GLASS:*KEY-INJECTOR* — see OPEN-SEAT-TRANSPORT.
    (setf (seat-injector seat) (lambda (down k) (glass-on-key port down k seat)))
    (when primary
      (setf (glass-port-default-seat port) seat)
      ;; The first seat starts out DRIVING McCLIM — not because it is the home seat, but
      ;; because it is the only pair of hands there is and a desktop with one person must
      ;; be driving from its first instruction, before anybody has moved a mouse.  Seeded,
      ;; not contested: nothing was taken from anyone.
      (clim-token-grant port seat :initial))
    seat))

(defun port-forget-window (port window)
  "Drop every seat's view of WINDOW — it has closed."
  (dolist (seat (glass-port-seats port)) (seat-forget-window seat window)))

(defmethod initialize-instance :after ((port glass-port) &rest initargs
                                       &key server-path &allow-other-keys)
  (add-seat port :primary t :name "seat-0")
  ;; (MAKE-INSTANCE 'GLASS-PORT :PORT N) and (:glass :port N) both name the RFB port
  ;; the first seat listens on.  It is an initarg of the port because that is how it
  ;; has always been written, and a slot of the seat because that is whose it is.
  (let ((n (or (getf initargs :port) (and server-path (getf (rest server-path) :port)))))
    (when n (setf (glass-port-num port) n)))
  (push (make-instance 'glass-frame-manager :port port)
        (slot-value port 'climi::frame-managers)))

(defun next-timestamp (port) (incf (glass-port-clock port)))

;;; ---- graft + frame manager -------------------------------------------------

(defclass glass-graft (graft) ())

(defparameter +glass-dpi+ 96 "Assumed pixel density; makes point/pixel sizing (fonts!) come out right.")

(defun graft-extent-in (px units)
  "Convert a device pixel count PX into UNITS (:device/:inches/:millimeters)."
  (ecase units
    ((:device :screen-sized) px)
    (:inches (/ px +glass-dpi+))
    (:millimeters (/ px (/ +glass-dpi+ 25.4)))))

(defmethod graft-width ((graft glass-graft) &key (units :device))
  (let ((fb (glass-port-fb (port graft))))
    (graft-extent-in (if fb (glass:fb-width fb) 1280) units)))

(defmethod graft-height ((graft glass-graft) &key (units :device))
  (let ((fb (glass-port-fb (port graft))))
    (graft-extent-in (if fb (glass:fb-height fb) 1024) units)))

(defmethod make-graft ((port glass-port) &key (orientation :default) (units :device))
  (make-instance 'glass-graft :port port :mirror t :orientation orientation :units units))

(defclass glass-frame-manager (climi::standard-frame-manager) ())

;;; ---- mirror ----------------------------------------------------------------
;;; One mirror per top-level sheet.  The MAIN one (the application frame) owns the
;;; framebuffer and drives its size; secondary ones (menus, dialogs, tooltips) are
;;; composited on top of it at their screen position — glass serves one screen, so
;;; the backend is a tiny compositor over all the top-level mirrors.

(defclass glass-mirror (mcclim-render::image-mirror-mixin)
  ((x    :initform 0   :accessor glass-mirror-x)             ; content screen position
   (y    :initform 0   :accessor glass-mirror-y)
   ;; Where McCLIM'S SHEET TRANSFORMATION HAS BEEN TOLD to put this window — the seat
   ;; DRIVING McCLIM's view of it, and not necessarily the window's own position above.
   ;; They were one pair of slots, and that is precisely why a second seat could not take
   ;; over the geometry: aiming the sheet at its arrangement also rewrote the window's own
   ;; position and made the first seat's screen jump.  Compared against on every token
   ;; acquisition, which is what makes an unchanged layout cost nothing to resync.
   ;;
   ;; TOLD, not observed: the value is the target of the last move ENQUEUED for this
   ;; mirror, so while the event queue is behind it is a few milliseconds AHEAD of the
   ;; sheet — which is the answer every reader here wants.  Written by CLIM-SHEET-GOTO
   ;; (driver-follow moves, at request time) and by SET-MIRROR-GEOMETRY (everything
   ;; McCLIM does on its own account), never by both for the same move.  The full
   ;; statement, and the race that came of a third writer, are in clim-token.lisp.
   (clim-x :initform 0 :accessor glass-mirror-clim-x)
   (clim-y :initform 0 :accessor glass-mirror-clim-y)
   (main :initform nil :accessor glass-mirror-main)          ; owns the fb + the RFB server?
   ;; --- window-manager mode ---
   (managed :initform nil :accessor glass-mirror-managed)    ; gets a title bar + border?
   (title   :initform "" :accessor glass-mirror-title)
   (sheet   :initform nil :accessor glass-mirror-sheet)      ; backref (WM pointer routing)
   (deco    :initform nil :accessor glass-mirror-deco)       ; cached (image . width) title bar
   ;; The same bar tinted for a seat that is NOT driving McCLIM (the holder indicator,
   ;; *CLIM-TOKEN-INDICATOR*, off by default).  A second cache and not a per-seat one:
   ;; the bar has exactly two appearances however many people are watching, so a window
   ;; costs one extra title bar when the indicator is on and nothing when it is off.
   (deco-other :initform nil :accessor glass-mirror-deco-other)
   (deco-w  :initform -1 :accessor glass-mirror-deco-w)
   ;; Place in the ONE stacking order shared with surface windows.  The accessor is
   ;; named for a window and not for a mirror on purpose: WM-SURFACE carries the same
   ;; slot under the same name, which is what lets the compositor order both kinds
   ;; together without asking either one what it is.
   (z       :initform 0   :accessor wm-window-z)))

;;; A mirror answers the seat's geometry questions with the position McCLIM believes
;;; in — which is the DEFAULT position every seat sees until it moves the window for
;;; itself, and which the primary seat keeps writing (see seat.lisp).
(defmethod window-own-x ((m glass-mirror)) (glass-mirror-x m))
(defmethod (setf window-own-x) (v (m glass-mirror)) (setf (glass-mirror-x m) v))
(defmethod window-own-y ((m glass-mirror)) (glass-mirror-y m))
(defmethod (setf window-own-y) (v (m glass-mirror)) (setf (glass-mirror-y m) v))
(defmethod window-own-z ((m glass-mirror)) (wm-window-z m))
(defmethod (setf window-own-z) (v (m glass-mirror)) (setf (wm-window-z m) v))

(defconstant +wm-titleh+ 22 "OPEN LOOK title-bar height (px).")
(defconstant +wm-border+ 1  "Window border thickness (px).")

(defmethod realize-mirror ((port glass-port) (sheet climi::mirrored-sheet-mixin))
  (let ((mirror (make-instance 'glass-mirror)))
    (setf (sheet-direct-mirror sheet) mirror
          (glass-mirror-sheet mirror) sheet)
    (when (typep sheet 'climi::top-level-sheet-mixin)
      (when (null (glass-port-top port))                     ; first top-level = the main frame
        (setf (glass-port-top port) sheet (glass-mirror-main mirror) t))
      (push mirror (glass-port-mirrors port))
      ;; A new window opens on top, which is where the shared z-order gets its first
      ;; value.  It matters that this is here and not in the WM clause below: an
      ;; UNMANAGED mirror (a pull-down, a tooltip) never reaches that clause, and a
      ;; window with no z would sort as though it opened before everything.
      (setf (wm-window-z mirror) (incf (glass-port-zclock port)))
      ;; window-manager mode: decorate managed frames + give them a cascaded slot
      (when (and (glass-port-wm-p port)
                 (not (typep sheet 'climi::unmanaged-sheet-mixin)))
        (setf (glass-mirror-managed mirror) t
              (glass-mirror-title mirror) (wm-sheet-title sheet))
        (let ((c (glass-port-cascade port)))
          ;; Place the window by MOVING its McCLIM sheet, not by poking glass-mirror-x/y
          ;; directly: this keeps McCLIM's coordinate math in sync, so pull-down menus,
          ;; dialogs and tooltips land relative to the window's REAL screen spot (a
          ;; window at a non-zero origin otherwise gets its menus at the native origin).
          ;; native-transformation stays identity (content still renders at the image's
          ;; 0,0); only the on-screen mirror position shifts.  glass-mirror-x/y is then
          ;; read back from McCLIM's region in set-mirror-geometry (below).
          (setf (sheet-transformation sheet)
                (make-translation-transformation (+ 40 c) (+ 40 c +wm-titleh+))
                (glass-port-cascade port) (mod (+ c 28) 200)))))
    (climi::update-mirror-geometry sheet)          ; creates the render image (via set-mirror-geometry)
    (dispatch-repaint sheet climi::+everywhere+)
    mirror))

(defmethod destroy-mirror ((port glass-port) (sheet climi::mirrored-sheet-mixin))
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (setf (glass-port-mirrors port) (remove mirror (glass-port-mirrors port))
          (mcclim-render::image-mirror-image mirror) nil)
    (port-forget-window port mirror)                ; no seat keeps a view of a gone window
    (composite-all port)))                          ; erase a closed menu/dialog

(defmethod enable-mirror ((port glass-port) (sheet climi::mirrored-sheet-mixin)) nil)
(defmethod disable-mirror ((port glass-port) (sheet climi::mirrored-sheet-mixin)) nil)

;;; ---- push rendered pixels into the framebuffer -----------------------------

(defun image-wh (image)
  (let ((a (climi::pattern-array image)))
    (values (array-dimension a 1) (array-dimension a 0))))

;;; ---- exposing a seat on a wire ----------------------------------------------
;;;
;;; SERVING IS A SEAT'S DECISION, NOT THE SESSION'S.  Running a session and opening a
;;; socket a stranger can reach used to be one call (RUN-WM), which is why there was no
;;; way to have a session that serves nothing, and why :5903 listens on 0.0.0.0 — not by
;;; decision, but because the session WAS the listener.  These two functions are the
;;; decision, and RUN-WM is now a session plus one call to the first of them.
;;;
;;; See docs/seats-and-transports.md for the argument, and backend/seat.lisp's
;;; SEAT-TRANSPORT for the state.

(defparameter *seat-transport-kind* :rfb
  "The kind of wire OPEN-SEAT-TRANSPORT opens when nobody says which.

   :RFB — a TCP port, which is what glass has always opened and what everything already
   pointed at a desktop expects.  IT IS STILL THE DEFAULT, and changing it is a deployment
   decision: a launcher that says :RFB-UNIX gets a socket file only its owner can open, and
   anything connecting over TCP stops finding it.  That is the whole of the migration and
   it is one word in one launcher, deliberately left to whoever knows what is connected.")

(defparameter *seat-bind-address* "127.0.0.1"
  "The interface a seat's RFB listener binds when nobody says otherwise.

   LOOPBACK.  This was 0.0.0.0, on the reasoning that it is what glass had always bound
   and that changing it might cut off something already pointed at a desktop here — with
   the note that it was the operator's call.  The operator made it: nothing should be
   reachable off this machine because a default said so.  Exposure is now something a
   caller ASKS for, per call via OPEN-SEAT-TRANSPORT's :ADDRESS or by rebinding this.

   Nothing was cut off, and the check is worth recording: at the time of the change no
   glass desktop on this box was listening on TCP at all — the live session ran entirely
   on UNIX sockets at 0600 (~/.glass/run/seat-0.rfb and friends), which is the stronger
   form of the same posture and the one to prefer when a socket file will do.")

(defun seat-socket-path (seat)
  "Where SEAT's UNIX-domain RFB socket lives when nobody names a path.

   Under the seat's NAME rather than its port, because a socket file is not a port and the
   whole reason to have one is that the name is ours to choose: `seat-home.rfb' says which
   seat, which is what somebody looking at the directory wants to know, and it survives the
   seat being re-exposed on a different port."
  (glass:socket-path (format nil "~a.rfb" (or (seat-name seat) "seat"))))

(defun open-seat-transport (seat &key (kind *seat-transport-kind*)
                                      (port-num (seat-port-num seat))
                                      (address *seat-bind-address*)
                                      path (password :inherit))
  "Expose SEAT on a wire: an RFB listener on PORT-NUM at ADDRESS, with SEAT's own screen
   framebuffer, its own wake, its own selection and input callbacks closed over SEAT —
   which is the whole of what a seat is on the wire.  Returns the SEAT-TRANSPORT.

   Nothing here is shared with another seat's transport, so a second seat is a second
   call: same session, second screen, second pair of hands.

   IDEMPOTENT PER (ADDRESS, PORT): asking twice for the same wire returns the one that is
   already open rather than fighting itself for the port.  A seat may hold several.

   THE SOCKET IS BOUND BEFORE THIS RETURNS, in the caller's thread, and handed to the
   accept loop.  Two things follow, both wanted: a port already in use signals HERE
   instead of in a thread nobody is looking at, and the transport can be CLOSED, because
   the socket is an object somebody holds rather than a local of a parked accept.

   PORT-NUM defaults to SEAT-PORT-NUM and, when given, becomes it — the seat's port is the
   port it serves on, and the audio ports beside it (START-SEAT-AUDIO) are derived from it.

   KIND :RFB-UNIX opens the same RFB listener on a UNIX-DOMAIN SOCKET instead — at PATH, or
   at SEAT-SOCKET-PATH.  A SIBLING of :RFB and not a mode of it: RFB is a stream protocol,
   so everything below this line is identical and the only difference is who the kernel lets
   near the wire.  A port on 127.0.0.1 is reachable by every process of every uid on the
   box; a socket file at mode 0600 is reachable by its owner, checked on connect(), and the
   server can additionally ask WHO connected (GLASS:PEER-CREDENTIALS) — which is real peer
   authentication with no key material anywhere, and the thing a TCP loopback cannot have.
   A seat may hold both at once: same screen, same hands, two doors.

   PASSING :PATH implies :RFB-UNIX, because a path is not something a TCP transport could
   have meant.

   PASSWORD IS THIS WIRE'S CREDENTIAL AND NOT THE SESSION'S.  :INHERIT (the default, and
   what every existing caller gets) is GLASS:*VNC-PASSWORD* read at each handshake — the
   behaviour glass has always had, live-settable, unchanged.  A string is a credential
   only this wire's clients are asked for.

   AND ON :RFB-UNIX IT IS FORCED TO NIL, which is a refusal and not an oversight.  A
   socket file's access control is the filesystem and the peer's uid, checked by the
   kernel on connect() — stronger than a DES challenge over a plaintext stream, and not
   fooled by an attacker who has watched one handshake.  Asking for a password there
   would buy nothing and would break the thing this whole exercise is about: the VP8
   capture is an RFB client that speaks security type None, it comes in over this socket,
   and a credential on it locks the video out of the desktop it is filming.  A caller who
   passes :PASSWORD with :RFB-UNIX is told so and ignored, loudly."
  (when path (setf kind :rfb-unix))
  (ecase kind
    (:rfb (check-type port-num (integer 1 65535)))
    (:rfb-unix
     (setf path (or path (seat-socket-path seat)))
     (unless (or (eq password :inherit) (null password))
       (warn "open-seat-transport: :PASSWORD is ignored on a UNIX transport (~a) — the ~
              filesystem and SO_PEERCRED are its access control, and a VNC password here ~
              would only lock out the capture client, which speaks security type None."
             path))
     (setf password nil)))
  (let ((already (find-if (lambda (tr) (and (transport-open-p tr)
                                            (eq (transport-kind tr) kind)
                                            (if (eq kind :rfb-unix)
                                                (equal (transport-path tr) path)
                                                (and (eql (transport-port-num tr) port-num)
                                                     (equal (transport-address tr) address)))))
                          (seat-transports seat))))
    ;; Idempotent per WIRE — but a credential is not part of the wire's name, so a second
    ;; call for the same address and port with a DIFFERENT password would otherwise hand
    ;; back a listener that does not ask for it, and the caller would go on to tell
    ;; somebody a password that opens nothing.  Say so instead.
    (when (and already (not (equal (transport-password already) password)))
      (error "seat ~a is already serving ~a with a different credential — close it first"
             (seat-name seat) (transport-endpoint already)))
    (or already
      (let* ((fb (seat-fb seat)) (port (seat-port seat))
             ;; The seat's OWN keyboard (made by ADD-SEAT), not a new one per listener:
             ;; dictation for this seat types on it, and two transports of one seat are two
             ;; ways to reach the same pair of hands.  Only the primary seat's also becomes
             ;; the session's GLASS:*KEY-INJECTOR*, or the newest listener would own the
             ;; typing that nobody addressed to a seat.
             (on-key (or (seat-injector seat)
                         (setf (seat-injector seat)
                               (lambda (down k) (glass-on-key port down k seat)))))
             (socket (if (eq kind :rfb-unix)
                         (glass:open-listener :unix :path path)
                         (glass:open-listener :tcp :port port-num :address address)))
             (tr (make-instance 'seat-transport :seat seat :kind kind
                                                :address address :port-num port-num
                                                :path (and (eq kind :rfb-unix) path)
                                                :password password
                                                :socket socket)))
        (when (eq kind :rfb) (setf (seat-port-num seat) port-num))
        (setf (transport-thread tr)
              (sb-thread:make-thread
               (lambda ()
                 ;; A CLOSE tears the listening socket out from under this accept loop on
                 ;; purpose, and the error that comes of it is the answer, not a fault.
                 ;; Anything else is reported and ends this transport rather than the
                 ;; process: an unhandled error in a thread takes a --disable-debugger
                 ;; image down with it, and one seat's wire must not be able to do that.
                 (handler-case
                     (glass:serve fb port-num
                                  :listen     socket
                                  :address    address
                                  ;; THIS wire's credential — the one thing about a seat's
                                  ;; transports that is deliberately not shared between them.
                                  :password   password
                                  :on-key     on-key
                                  :install-injector (seat-primary-p seat)
                                  :on-pointer (lambda (b x y) (glass-on-pointer port b x y seat))
                                  :on-resize  (lambda (w h) (glass-on-resize port w h seat))
                                  :wake       (seat-wake seat)
                                  ;; Nobody is watching this screen any more: this seat has
                                  ;; no hands, so it must not go on holding the McCLIM token
                                  ;; that the people still here are waiting for.
                                  :on-clients (lambda (n)
                                                (when (zerop n) (clim-token-seat-gone port seat)))
                                  ;; every transport of THIS seat shares THIS selection
                                  :clipboard  (seat-clipboard seat)
                                  ;; The RFB desktop name is what a viewer puts in its title
                                  ;; bar.  The primary seat keeps the name it has always
                                  ;; advertised — a one-seat desktop must look identical from
                                  ;; the outside, and a nested desktop shows this string in
                                  ;; the hosting window's title — and only a further seat says
                                  ;; which one it is, where the question can actually arise.
                                  :name (if (seat-primary-p seat)
                                            "glass-mcclim"
                                            (format nil "glass-mcclim (~a)" (seat-name seat))))
                   (error (e)
                     (unless (transport-closing-p tr)
                       (format *error-output* "~&glass: seat ~a transport ~a ended: ~a~%"
                               (seat-name seat) (transport-endpoint tr) e)
                       (force-output *error-output*))))
                 (setf (transport-socket tr) nil))
               :name (format nil "glass-server-~a" (seat-name seat))))
        (push tr (seat-transports seat))
        ;; GLASS-PORT-SERVER has always meant "the thread serving this screen", and it
        ;; goes on meaning it.
        (setf (seat-server seat) (transport-thread tr))
        tr))))

(defun close-seat-transport (transport)
  "Stop TRANSPORT: the port stops listening and its accept loop ends.  T if it was open.

   The clients already connected are NOT hunted down — their sockets are their own and
   they end when they end.  What this closes is the way IN, which is the thing a seat is
   deciding about."
  (when (and transport (transport-open-p transport))
    (let ((seat (transport-seat transport))
          (sock (transport-socket transport))
          (thread (transport-thread transport)))
      (setf (transport-closing-p transport) t)
      ;; Both, in this order, and for the reason CLOSE-LISTENER spells out: a bare
      ;; SOCKET-CLOSE leaves the port listening while a thread is parked on it.
      (glass:close-listener sock)
      (setf (transport-socket transport) nil)
      (when (and thread (not (eq thread sb-thread:*current-thread*)))
        ;; It has one syscall to notice.  If it somehow does not, it is a thread parked on
        ;; a dead descriptor and it must not be left there — a later listener could be
        ;; handed the same fd number.
        (unless (ignore-errors (sb-thread:join-thread thread :timeout 2 :default :timeout))
          (ignore-errors (sb-thread:terminate-thread thread))))
      (when seat
        (setf (seat-transports seat) (remove transport (seat-transports seat)))
        (when (eq (seat-server seat) thread)
          (setf (seat-server seat)
                (let ((next (find-if #'transport-open-p (seat-transports seat))))
                  (and next (transport-thread next))))))
      t)))

(defun close-seat-transports (seat)
  "Close every transport of SEAT — it is watched by nobody and reachable from nowhere.
   Returns how many were closed.  The seat itself, its screen and its arrangement stay:
   a seat with no wire is still a place at the session, which is the whole point."
  (let ((n 0))
    (dolist (tr (copy-list (seat-transports seat)) n)
      (when (close-seat-transport tr) (incf n)))))

(defun seat-serving-p (seat)
  "Is SEAT reachable — does it hold any open transport?"
  (and seat (some #'transport-open-p (seat-transports seat)) t))

;;; ---- plain VNC, on demand, with a credential ---------------------------------
;;;
;;; The desktop below this line may be serving nothing at all — a socket file for the
;;; gateway and no port anywhere — which is the posture the design note argues for and
;;; the one that leaves a person with a VNC viewer and no way in.  This is the way in:
;;; a seat opens a TCP wire when somebody asks for one, with a credential minted for
;;; that wire alone, and closes it again.
;;;
;;; It is a separate function from OPEN-SEAT-TRANSPORT because it makes DECISIONS —
;;; which credential, which interface — that OPEN-SEAT-TRANSPORT deliberately does not:
;;; that one binds what it is told, as every launcher and gate already relies on.  This
;;; one is the thing a menu item calls, and a menu item has nobody to ask.

(defparameter *seat-vnc-address* "0.0.0.0"
  "The interface SERVE-SEAT-VNC binds when nobody says otherwise.

   0.0.0.0 — every interface — and unlike *SEAT-BIND-ADDRESS* that is not inherited
   caution but a choice, because a loopback-only VNC listener is nearly pointless on
   this box: a UNIX socket is already there for anything local, and it is better at
   being local (mode 0600 and SO_PEERCRED beat `any process of any uid'), so a port on
   127.0.0.1 would be a worse copy of a door that already exists.  What makes THIS door
   worth opening is that a person with a VNC viewer on another machine can walk through
   it — which is also why it is opened only when asked for, only with a credential, and
   closed from the same menu item that opened it.

   THE CREDENTIAL IS THE CONDITION, and it is enforced rather than recommended: with no
   usable credential SERVE-SEAT-VNC binds loopback whatever this says.  Somebody who
   turns the password off gets a listener that stops being reachable off-box, not a
   listener that quietly still is.")

(defparameter *vnc-password-file*
  (merge-pathnames ".glass-vnc-pass" (user-homedir-pathname))
  "An operator's own VNC password, one line, if they keep one.  When it exists it WINS
   over a generated credential, for a reason worth stating: a generated password is new
   every time the menu item is picked, and a viewer that saved the last one (macOS puts
   it in the Keychain) would be refused by a desktop that thinks it is being helpful.
   A person who wants a stable password already has the file — serve-desktop.lisp has
   read it for as long as it has existed — and this reads the same one.")

(defun vnc-password-file-credential (&optional (file *vnc-password-file*))
  "The credential in *VNC-PASSWORD-FILE*, or NIL if there is no usable one there."
  (ignore-errors
   (when (probe-file file)
     (let ((pw (string-trim '(#\Space #\Tab #\Newline #\Return)
                            (with-open-file (in file) (or (read-line in nil "") "")))))
       (and (plusp (length pw)) pw)))))

(defun loopback-address-p (address)
  "Is ADDRESS an interface only this box can reach?"
  (and (stringp address)
       (or (string= address "localhost") (string= address "::1")
           (and (>= (length address) 4) (string= "127." (subseq address 0 4))))))

(defun seat-vnc-transport (seat)
  "SEAT's open TCP (:RFB) transport, or NIL.  The one a person means by `is this
   desktop on a VNC port right now' — a socket file is not what they are asking about."
  (and seat (find-if (lambda (tr) (and (transport-open-p tr) (eq (transport-kind tr) :rfb)))
                     (seat-transports seat))))

(defun local-address (&optional (fallback "this host"))
  "The address of the interface that would carry a packet off this box — what somebody
   should point a VNC viewer at, as opposed to 0.0.0.0, which is what we BOUND and is
   not an address anybody can connect to.  Asked of the routing table by opening a UDP
   socket towards a public address and reading back the local end: no packet is sent
   (UDP connect() is a routing decision, not a handshake) and nothing is contacted."
  (or (ignore-errors
       (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :datagram :protocol :udp)))
         (unwind-protect
              (progn (sb-bsd-sockets:socket-connect
                      sock (sb-bsd-sockets:make-inet-address "1.1.1.1") 53)
                     (multiple-value-bind (addr port) (sb-bsd-sockets:socket-name sock)
                       (declare (ignore port))
                       (format nil "~{~d~^.~}" (coerce addr 'list))))
           (ignore-errors (sb-bsd-sockets:socket-close sock)))))
      fallback))

(defun attach-seat-local (seat)
  "Everything a viewer IN THIS IMAGE needs to show SEAT and put hands on it, with no wire
   between them.  Returns (values FB ON-KEY ON-POINTER ON-RESIZE WAKE) — the same five
   things OPEN-SEAT-TRANSPORT closes over before it hands them to GLASS:SERVE, and the
   whole of what a seat is to a viewer.

   The point is what is NOT here.  A viewer in this image already has the pixels: the
   framebuffer is an object, not a stream of rectangles, and asking a socket to hand it
   back means encoding a screen the caller is holding, writing it to the kernel, reading
   it out again and decoding it — a process talking to itself in a protocol designed for
   a network it does not have to cross.  On the hardware this is eventually for there is
   no socket to have; `glass/fb' IS the screen, and this is what that looks like from
   here.

   IT FILLS SEAT-INJECTOR, exactly as opening a transport does.  A seat made with
   :SERVE NIL has none, and the injector is the keyboard this seat's dictation types on
   — so a locally-attached seat that skipped it would be a desktop you can type into by
   hand and not by voice, which is a difference nobody would think to look for."
  (let* ((port (seat-port seat))
         (on-key (or (seat-injector seat)
                     (setf (seat-injector seat)
                           (lambda (down k) (glass-on-key port down k seat))))))
    (values (seat-fb seat)
            on-key
            (lambda (b x y) (glass-on-pointer port b x y seat))
            ;; A LOCAL VIEWER IS THIS SEAT'S SCREEN, so "resize" means resize the seat --
            ;; not GLASS-ON-RESIZE, which is the SetDesktopSize path and resizes the CLIM
            ;; main top-level sheet (one application's window, and nothing at all when no
            ;; application is open).  A transport hands its clients the other one because
            ;; an RFB client asking for a size is asking on behalf of a window; a viewer
            ;; in this image holding the whole screen is not.
            (lambda (w h) (resize-seat-screen seat w h))
            (seat-wake seat))))

(defun serve-seat-vnc (seat &key (port-num (seat-port-num seat))
                                 (address *seat-vnc-address*)
                                 (password :generate))
  "Expose SEAT on a TCP VNC port with a credential of its own.  Returns
   (values TRANSPORT CREDENTIAL NOTE) — NOTE is a string when something was decided
   differently from what was asked, and NIL when it was not.

   THE CREDENTIAL, in order: PASSWORD if a string; else *VNC-PASSWORD-FILE* if it holds
   one; else a fresh GLASS:MAKE-VNC-CREDENTIAL.  PASSWORD NIL means the caller has
   deliberately asked for an open wire and gets one — on loopback.

   IT ASKS WHETHER THE IMAGE CAN VERIFY A PASSWORD AT ALL, which is not paranoia: the
   DES verifier is the optional :glass/vncauth system, and a desktop without it that
   sets a password does not become secure, it becomes UNREACHABLE (the handshake fails
   closed and rejects every client).  A listener nobody can use is not the answer to
   `let me in', so with no verifier this serves WITHOUT a credential and — because of
   that — on loopback, and says so in NOTE rather than either lying or refusing.

   AND WITH NO CREDENTIAL IT WILL NOT BIND OFF-BOX.  ADDRESS is honoured when there is
   a password to go with it and overridden to 127.0.0.1 when there is not.  That is the
   one place glass overrules an explicit argument, and it is this narrow on purpose:
   this function is the one that chooses an address on a person's behalf (the menu item
   has nobody to ask), and choosing on their behalf is exactly what obliges it not to
   choose an exposure they did not agree to.  OPEN-SEAT-TRANSPORT below it still binds
   precisely what it is told."
  (let* ((verifier (glass:vnc-auth-available-p))
         (asked address)
         (credential (cond ((stringp password) password)
                           ((null password) nil)
                           ((not verifier) nil)
                           (t (or (vnc-password-file-credential) (glass:make-vnc-credential)))))
         ;; FORMAT NIL and not a literal: a `~' continuation is a FORMAT directive, and
         ;; this string is printed with ~A by its caller, where the tildes would survive.
         (note (cond ((and (not verifier) (not (null password)))
                      (format nil "no DES verifier in this image (:glass/vncauth is not ~
                                   loaded), so a password could only reject every client: ~
                                   serving WITHOUT one, on loopback only"))
                     ((null credential)
                      "no credential was asked for: serving on loopback only")))
         (address (if credential address "127.0.0.1")))
    (when (and note (not (equal asked address)))
      (setf note (format nil "~a (asked for ~a)" note asked)))
    (values (open-seat-transport seat :kind :rfb :port-num port-num
                                      :address address :password credential)
            credential
            note)))

(defun stop-seat-vnc (seat)
  "Close SEAT's TCP VNC transport.  T if there was one.  Any UNIX transport it holds —
   the gateway's, the capture's — is left exactly alone: they are different doors, and
   this item is about the one that faces the network."
  (let ((tr (seat-vnc-transport seat)))
    (and tr (close-seat-transport tr))))

(defun start-seat-server (seat)
  "Start the RFB listener for SEAT on SEAT-PORT-NUM.  Idempotent.

   The name RUN-WM and every launcher and gate already says, kept meaning what it meant:
   OPEN-SEAT-TRANSPORT with everything defaulted.  New code inside the WM should say the
   longer name, because the shorter one cannot express `on this address' or `close it'."
  (unless (seat-serving-p seat) (open-seat-transport seat)))

(defun start-glass-server (port &optional seat)
  "Start the RFB server thread for PORT's default seat (or SEAT).  Idempotent."
  (start-seat-server (port-seat port seat)))

;;; ---- this seat's sound ------------------------------------------------------
;;; The screen is served by the seat's RFB listener; the sound is not — RFB carries no
;;; audio, and the mix reaches a listener over sockets of its own.  So a seat's audio is
;;; a GLASS:HEADSET (src/headset.lisp) and this is the two lines of seam: derive its
;;; ports from the seat's screen port, and hand it the seat's keyboard so that what the
;;; seat DICTATES lands in the window the seat has focused.
;;;
;;; Looked up by name because :glass/headset is optional, exactly as the WM resolves
;;; SPEAK: a desktop built without audio is a working desktop with silent seats.

(defun start-seat-audio (port &key seat (address "127.0.0.1") audio-port mic-port (mic t))
  "Give SEAT (or PORT's default seat) sound of its own: its mix out and its microphone in,
   on the ports beside its screen's (5903 -> 5913 / 5914).  Idempotent — returns the
   seat's existing headset if it has one.

   The PRIMARY seat's headset is the SESSION's: its mix is GLASS:SESSION-MIXER's own and
   its microphone port is the session's, so a one-seat desktop is the same objects, the
   same ports and the same code that ran before seats had audio.  Every further seat gets
   a private composite of the SAME sources — one podcast, played once, heard by both, at
   whatever gain each of them chose.

   Returns the HEADSET, or NIL if this image has no :glass/headset."
  (let* ((seat (port-seat port seat))
         (make (let ((s (find-symbol "MAKE-HEADSET" '#:glass))) (and s (fboundp s) s))))
    (or (seat-headset seat)
        (when make
          (setf (seat-headset seat)
                (funcall make :name (seat-name seat)
                              :rfb-port (seat-port-num seat)
                              :audio-port audio-port :mic-port mic-port
                              :address address :mic mic
                              :primary (seat-primary-p seat)
                              ;; the seat's own keyboard, so dictation reaches ITS focus
                              :injector (seat-injector seat)))))))

(defun stop-seat-audio (port &optional seat)
  "Close this seat's audio ports and stop its ear.  The session's sources go on playing
   to everybody else."
  (let* ((seat (port-seat port seat))
         (stop (let ((s (find-symbol "STOP-HEADSET" '#:glass))) (and s (fboundp s) s))))
    (when (and stop (seat-headset seat))
      (funcall stop (seat-headset seat))
      (setf (seat-headset seat) nil)
      t)))

(defun seat-mix (seat)
  "The composite SEAT hears, or NIL if it has no sound.  What to address a sound AT when
   it is for that person only — reading their selection aloud, say."
  (let* ((h (and seat (seat-headset seat)))
         (mix (let ((s (find-symbol "HEADSET-MIX" '#:glass))) (and s (fboundp s) s))))
    (and h mix (funcall mix h))))

(defun ensure-fb-and-server (port mirror)
  "The MAIN mirror allocates the framebuffer (sized to its image) and starts the
   RFB server, once its image exists.  In WM mode RUN-WM owns the screen fb."
  (when (and (not (glass-port-wm-p port)) (not (glass-port-fb port)))
    (when-let ((image (mcclim-render::image-mirror-image mirror)))
      (multiple-value-bind (w h) (image-wh image)
        (setf (glass-port-fb port) (glass:make-framebuffer w h))
        (start-glass-server port)))))

;;; ---- a pop-up belongs to a window, and every seat holds that window somewhere ----
;;;
;;; Pull-downs, submenus, dialogs and tooltips are UNMANAGED mirrors: session-wide
;;; windows with ONE position, placed by McCLIM from the owning frame's sheet
;;; transformation.  That transformation follows whoever is DRIVING McCLIM
;;; (clim-token.lisp), so a pop-up's own position is expressed in the DRIVER's
;;; arrangement — B presses "File" on its copy of the window at 430,300 and the menu is
;;; correctly placed at 430,325.  On a seat holding that same window at 120,140 the menu
;;; then floats over bare workspace with no window under it.
;;;
;;; The cure is a per-seat DRAW OFFSET: how far this seat's copy of the OWNING window is
;;; from where McCLIM believes that window to be.  It is zero for the driver BY
;;; CONSTRUCTION — CLIM-RESYNC-GEOMETRY aims GLASS-MIRROR-CLIM-X/Y at the driver's view
;;; on every acquisition, and WM-SYNC-SHEET keeps it there — and zero for everybody when
;;; nothing has diverged, so a one-seat desktop and an undiverged two-seat one draw
;;; exactly the pixels they drew before.
;;;
;;; THIS IS A DRAWING QUESTION AND ONLY A DRAWING QUESTION.  The pointer paths that a
;;; posted pop-up runs through — GRAB-FRAME-LEAF-AT and ROUTE-TO-GRABBED-SHEET — are
;;; reachable only by the seat CLIM has GRABBED the pointer for, which is the driver
;;; (PORT-GRAB-POINTER records the grab on CLIM-TOKEN-SEAT, and GLASS-ON-POINTER's grab
;;; branch additionally requires CLIM-TOKEN-CLAIM to succeed).  The driver's offset is
;;; zero, so those two would compute the same numbers whether they applied this or not,
;;; and they are left alone: the delicate menu-dismiss routing is not touched.

(defun mirror-frame (mirror)
  "The CLIM application frame MIRROR's sheet belongs to, or NIL."
  (when-let ((sheet (glass-mirror-sheet mirror)))
    (ignore-errors (pane-frame sheet))))

(defun clim-popup-owner (mirror &optional port)
  "The MANAGED window a pop-up MIRROR hangs off, or NIL if it has none.

   CLIM pop-ups carry no owner link — a pull-down is simply another top-level sheet —
   so it is derived the way GRAB-FRAME-LEAF-AT already derives it: BY FRAME.  A menu
   bar and every pull-down and submenu it opens are separate mirrors of the SAME
   application frame, and that frame's managed window is the one the person pressed.
   Where a frame somehow has more than one managed window the frontmost wins (greatest
   own z), which is the one the pop-up was most plausibly opened from; and a pop-up
   whose frame has no managed window at all answers NIL, which is what makes such a
   pop-up draw exactly where it always did.

   Note that a SUBMENU answers with the managed window too, not with the pull-down that
   opened it — so a chain of pop-ups is offset once each, never once per link."
  (when (and (typep mirror 'glass-mirror) (not (glass-mirror-managed mirror)))
    (when-let* ((sheet (glass-mirror-sheet mirror))
                (frame (ignore-errors (pane-frame sheet)))
                (port  (or port (ignore-errors (port sheet)))))
      (let ((best nil))
        (dolist (m (glass-port-mirrors port) best)
          (when (and (typep m 'glass-mirror) (glass-mirror-managed m) (not (eq m mirror))
                     (eq (mirror-frame m) frame)
                     (or (null best) (> (window-own-z m) (window-own-z best))))
            (setf best m)))))))

(defun seat-popup-offset (seat window)
  "(values dx dy): how far WINDOW's pixels must be shifted on SEAT's screen so that a
   pop-up lands on SEAT's copy of the window that opened it.  Zero for everything that
   is not a pop-up with a managed owner, and zero for the seat driving McCLIM."
  (let ((owner (clim-popup-owner window (and seat (seat-port seat)))))
    (if owner
        (values (- (seat-window-x seat owner) (glass-mirror-clim-x owner))
                (- (seat-window-y seat owner) (glass-mirror-clim-y owner)))
        (values 0 0))))

(defun seat-draw-x (seat window)
  "Where WINDOW's content is DRAWN on SEAT's screen.  SEAT-WINDOW-X for everything a
   seat can arrange for itself; SEAT-WINDOW-X plus the pop-up offset for the transient
   windows it cannot, which have one position and must follow their owner per seat."
  (multiple-value-bind (dx dy) (seat-popup-offset seat window)
    (declare (ignore dy))
    (+ (seat-window-x seat window) dx)))

(defun seat-draw-y (seat window)
  (multiple-value-bind (dx dy) (seat-popup-offset seat window)
    (declare (ignore dx))
    (+ (seat-window-y seat window) dy)))

(defun blit-mirror (mirror fb &optional seat)
  "Composite one mirror's image into FB at the mirror's screen position FOR SEAT
   (NIL = the window's own position), honoring FB's clip box — so a damage-limited
   recomposite (McCLIM repaint) only touches the changed region, like blit-fb does for
   surfaces.

   The position is the DRAWN one (SEAT-DRAW-X): identical to SEAT-WINDOW-X for every
   window a seat arranges for itself, and shifted onto this seat's copy of the owning
   window for a pop-up, which has only one."
  (when-let ((image (mcclim-render::image-mirror-image mirror)))
    (mcclim-render::with-image-locked (mirror)
      (let* ((arr (climi::pattern-array image))
             (ih (array-dimension arr 0)) (iw (array-dimension arr 1))
             (ox (seat-draw-x seat mirror)) (oy (seat-draw-y seat mirror))
             (dpx (glass:fb-pixels fb)) (fw (glass:fb-width fb)) (fh (glass:fb-height fb))
             (clip (glass:fb-clip fb))
             (cx0 (if clip (first clip) 0)) (cy0 (if clip (second clip) 0))
             (cx1 (if clip (third clip) fw)) (cy1 (if clip (fourth clip) fh)))
        (dotimes (iy ih)
          (let ((fy (+ oy iy)))
            (when (and (< -1 fy fh) (<= cy0 fy) (< fy cy1))
              (let ((frow (* fy fw)))
                (dotimes (ix iw)
                  (let ((fx (+ ox ix)))
                    (when (and (< -1 fx fw) (<= cx0 fx) (< fx cx1))
                      (setf (aref dpx (+ frow fx)) (logand (aref arr iy ix) #x00ffffff)))))))))))))

(defun composite-seat (seat &optional damage copy)
  "Redraw ONE SEAT's screen.  DAMAGE = (x y w h) IN THAT SEAT'S SCREEN COORDINATES
   confines the redraw (and the RFB sender's diff) to that rectangle — the compositor
   already knows what changed, so an idle move/blink doesn't rebuild + re-diff the
   whole 1280x800.  NIL means the whole screen (menus, resize, McCLIM updates, first
   paint).  COPY = (sx sy dx dy w h) marks a window MOVE so the sender can CopyRect it
   (near-free drag).

   A COPY may also come from the paint itself: a surface window whose content merely
   SCROLLED leaves a screen-space hint in SEAT-FRAME-COPY as WM-COMPOSITE draws it (see
   WM-SURFACE-SCREEN-COPY).  An explicit COPY — a window move, which is about this very
   composite — wins; only one CopyRect can ride an update.

   Damage and copy are per-seat and cannot be shared, which is the point: two seats
   holding the same window at different places translate it to different screen
   rectangles, so each seat's CopyRect hint is its own."
  (let ((port (seat-port seat)))
    (when-let ((fb (seat-fb seat)))
      (let ((%t0 (get-internal-real-time)))
        (glass:with-fb-locked (fb)
          (flet ((paint ()
                   (setf (seat-frame-copy seat) nil)   ; fresh: only THIS paint's hints count
                   (if (glass-port-wm-p port)
                       (wm-composite seat fb)
                       ;; mirrors is newest-first; composite oldest (main) first so newer are on top
                       (dolist (mirror (reverse (glass-port-mirrors port)))
                         (blit-mirror mirror fb seat)))))
            (if damage                          ; blit-mirror + blit-fb both honor the clip now
                (destructuring-bind (dx dy dw dh) damage
                  (glass:with-fb-clip (fb dx dy dw dh) (paint))
                  (glass:fb-mark-frame fb (list dx dy (+ dx dw) (+ dy dh))
                                       (or copy (seat-frame-copy seat))))
                (progn (paint) (glass:fb-mark-frame fb :full)))
            (setf (seat-frame-copy seat) nil))
          (glass:fb-touch fb))              ; content changed -> the sender should re-scan
        (glass:perf-record-composite (- (get-internal-real-time) %t0) damage))
      (glass:wake-signal (seat-wake seat)))))   ; …and wake it now, don't wait for its poll

(defun composite-all (port &optional damage copy)
  "Redraw the desktop.  With a DAMAGE box — which is in screen coordinates, and a screen
   belongs to a seat — this means the DEFAULT seat, exactly as it always did.  With no
   damage it means what it says: every seat rebuilds its whole screen, which is the
   right reading of the events that call it that way (a window closed, the background
   changed, Refresh).

   Kept under its old name and signature because it is the sentence the whole tree, the
   inspect harnesses and a running desktop's control socket already say.  New code
   inside the window manager should call COMPOSITE-SEAT and be explicit."
  (if (or damage copy)
      (composite-seat (glass-port-default-seat port) damage copy)
      (dolist (seat (glass-port-seats port)) (composite-seat seat))))

(defun port-damage-window (port window &optional local-box)
  "Mark that WINDOW's CONTENT changed — optionally only within LOCAL-BOX, an (x y w h)
   in the window's own content coordinates — for every seat.

   Content damage is the one kind that is session-wide: one window painted itself, and
   everybody watching has to see it.  Where that lands on a screen is not, because each
   seat holds the window somewhere else, so the box is carried in the window's own
   coordinates and converted per seat at the last moment — through SEAT-DRAW-X, the same
   arithmetic BLIT-MIRROR draws through, so a pop-up shifted onto this seat's copy of its
   owner damages the rectangle it is actually drawn in and not the one the driver sees."
  (dolist (seat (glass-port-seats port))
    (seat-accumulate-damage
     seat
     (if local-box
         (destructuring-bind (lx ly lw lh) local-box
           (list (+ (seat-draw-x seat window) lx) (+ (seat-draw-y seat window) ly) lw lh))
         (wm-window-box window seat)))))

(defun port-damage-popups (port)
  "Mark every posted pop-up's CURRENT screen rectangle as damaged on every seat.

   Called on both sides of anything that moves a pop-up WITHOUT repainting it: its
   position is derived from its owner's place on each screen and from which seat
   McCLIM's geometry follows, and neither of those is a repaint of the pop-up.  Called
   before the change it erases where the pop-up was; called after, it draws where it
   now is.  Nothing at all on a desktop with no pop-up open, which is nearly always."
  (dolist (m (glass-port-mirrors port))
    (when (and (typep m 'glass-mirror) (not (glass-mirror-managed m))
               (clim-popup-owner m port))
      (port-damage-window port m))))

(defun port-damage-all (port)
  "Mark every seat's whole screen as needing a rebuild on the next tick."
  (dolist (seat (glass-port-seats port)) (seat-accumulate-damage seat nil)))

(defun sync-fb-size (port mirror)
  "Keep the framebuffer the same size as the MAIN frame's image; on a change the
   RFB client is told the new size via DesktopSize."
  (let ((fb (glass-port-fb port))
        (image (mcclim-render::image-mirror-image mirror)))
    (when (and fb image (glass-mirror-main mirror) (not (glass-port-wm-p port)))
      (multiple-value-bind (w h) (image-wh image)
        (unless (and (= w (glass:fb-width fb)) (= h (glass:fb-height fb)))
          (glass:fb-resize fb w h))))))

(defgeneric present-mirror (port mirror)
  (:documentation
   "Push MIRROR's freshly rendered image to the display — the ONE seam between
    rendering (mcclim-render, per app) and the display (glass, shared).  The
    default composites into the local framebuffer and serves it over RFB; a
    MESSAGE-PORT overrides this to ship the pixels to a remote compositor over a
    mailbox, which is the entire actor boundary.")
  (:method ((port glass-port) mirror)
    (when (glass-mirror-main mirror)
      (ensure-fb-and-server port mirror)       ; main mirror creates the fb + starts the server
      (sync-fb-size port mirror))
    ;; DAMAGE-TRACK McCLIM repaints: recomposite only the mirror's dirty region
    ;; (what mcclim-render actually redrew), not the whole 1280x800.  And COALESCE:
    ;; a McCLIM app fires ~20 repaints per interaction, so in WM mode accumulate the
    ;; damage and let the tick loop composite the batch ONCE (eager per-repaint
    ;; compositing is what made CLIM apps jumpy).  Single-app mode has no tick, so
    ;; it composites eagerly.
    ;; Pop-up mirrors (pull-down menus, submenus, tooltips) render into their own image
    ;; but never flush their medium, so present-mirror is only ever called for the MAIN
    ;; frame — which flushes often during any interaction.  Fold in the other mirrors'
    ;; pending damage here so a just-opened submenu gets composited on the next
    ;; main-frame flush instead of staying invisible until the frame repaints.
    (if (glass-port-wm-p port)
        ;; WM mode: hand each mirror's damage over IN THE MIRROR'S OWN COORDINATES, so
        ;; every seat converts it against wherever IT is holding that window.  The seats'
        ;; accumulators then union exactly what one bbox-union used to.
        (dolist (m (cons mirror (remove mirror (glass-port-mirrors port))))
          (let ((mb (%mirror-dirty-local m)))          ; :empty / NIL=full / local (x y w h)
            (cond ((eq mb :empty))                     ; nothing drawn -> nothing to do
                  ((null mb) (port-damage-all port))   ; full repaint of an unknown extent
                  (t (port-damage-window port m mb)))))
        ;; Single-app mode: one seat, no tick loop, screen coordinates throughout.
        (let ((box (mirror-damage-box mirror)))
          (dolist (m (glass-port-mirrors port))
            (unless (eq m mirror)
              (let ((mb (mirror-damage-box m)))        ; mb: :empty / NIL=full / (x y w h)
                (unless (eq mb :empty)
                  (setf box (cond ((eq box :empty) mb) ; nothing yet -> take mb
                                  ((or (null box) (null mb)) nil)   ; either full -> full
                                  (t (bbox-union box mb))))))))
          (unless (eq box :empty)
            (composite-all port box))))))       ; BOX = (x y w h) damage, or NIL = full

(defun bbox-union (a b)
  "Union two (x y w h) boxes; NIL means full-screen (absorbs)."
  (if (or (null a) (null b)) nil
      (destructuring-bind (ax ay aw ah) a
        (destructuring-bind (bx by bw bh) b
          (let ((x0 (min ax bx)) (y0 (min ay by)))
            (list x0 y0 (- (max (+ ax aw) (+ bx bw)) x0) (- (max (+ ay ah) (+ by bh)) y0)))))))

(defun port-accumulate-damage (port box)
  "Union BOX (an (x y w h) rect in the DEFAULT seat's screen coordinates, or NIL =
   whole screen) into that seat's pending damage.  DEPRECATED in favour of
   PORT-DAMAGE-WINDOW, which says what changed rather than where it landed and so can
   be answered for every seat; kept because it is a screen-space sentence other code
   already says."
  (seat-accumulate-damage (glass-port-default-seat port) box))

(defun port-take-pending (port &optional seat)
  "Atomically read + clear SEAT's (default: the default seat's) pending damage:
   :full, an (x y w h) box, or NIL."
  (seat-take-pending (port-seat port seat)))

(defun %mirror-dirty-local (mirror)
  "MIRROR's accumulated dirty region as an (x y w h) in the mirror's OWN CONTENT
   coordinates — what a repaint actually redrew, before anybody decides where on a
   screen that is.  :EMPTY = nothing changed; NIL = unbounded/full.  CONSUMES (resets)
   the dirty region, so it is asked exactly once per flush; the box is clamped to the
   mirror image's bounds."
  (let ((dr (mcclim-render::image-dirty-region mirror))
        (image (mcclim-render::image-mirror-image mirror)))
    (setf (mcclim-render::image-dirty-region mirror) clim:+nowhere+)
    (cond
      ((clim:region-equal dr clim:+nowhere+) :empty)
      ((or (null image) (clim:region-equal dr clim:+everywhere+)) nil)     ; full repaint
      (t (multiple-value-bind (x1 y1 x2 y2) (clim:bounding-rectangle* dr)
           (multiple-value-bind (iw ih) (image-wh image)
             (let ((bx0 (max 0 (min iw (floor x1)))) (by0 (max 0 (min ih (floor y1))))
                   (bx1 (max 0 (min iw (ceiling x2)))) (by1 (max 0 (min ih (ceiling y2)))))
               (if (and (< bx0 bx1) (< by0 by1))
                   (list bx0 by0 (- bx1 bx0) (- by1 by0))
                   :empty))))))))

(defun mirror-damage-box (mirror &optional seat)
  "%MIRROR-DIRTY-LOCAL placed on SEAT's screen (NIL = the mirror's own position)."
  (let ((local (%mirror-dirty-local mirror)))
    (if (consp local)
        (destructuring-bind (bx by bw bh) local
          (list (+ (seat-draw-x seat mirror) bx) (+ (seat-draw-y seat mirror) by) bw bh))
        local)))

(defun %mirror-force-output (port mirror)
  (present-mirror port mirror))

(defmethod port-force-output ((port glass-port))
  (when-let* ((sheet (glass-port-top port))     ; drive through the main mirror so the server starts
              (mirror (sheet-direct-mirror sheet)))
    (%mirror-force-output port mirror)))

;;; ---- medium ----------------------------------------------------------------

(defclass glass-medium (mcclim-render::render-medium-mixin climi::basic-medium) ())

(defmethod make-medium ((port glass-port) sheet)
  (make-instance 'glass-medium :port port :sheet sheet))

(defmethod medium-finish-output :after ((medium glass-medium))
  (when-let ((mirror (medium-drawable medium)))
    (%mirror-force-output (port medium) mirror)))

(defmethod medium-force-output :after ((medium glass-medium))
  (when-let ((mirror (medium-drawable medium)))
    (%mirror-force-output (port medium) mirror)))

;;; ---- measuring text on a medium that has no backend -------------------------
;;;
;;; McCLIM's output-recording streams do not hand out the backend's medium while
;;; they record.  SHEET-MEDIUM on a recording stream returns a "faux medium" —
;;; MAKE-CONTEXT-MEDIUM in Core/extended-output/record-stream.lisp — which is a
;;; bare CLIMI::BASIC-MEDIUM carrying the right port and no backend class at all
;;; ("we are not interested in the medium specialized by the backend").  That is
;;; fine as long as text is measured through the STREAM, which has its own
;;; methods, and every measurement inside McCLIM goes that way.
;;;
;;; ESA's minibuffer does not.  It takes the medium out of WITH-SHEET-MEDIUM and
;;; asks IT:
;;;
;;;     (text-style-height (medium-merged-text-style medium) medium)   ; esa.lisp:151
;;;
;;; and TEXT-STYLE-ASCENT has methods for CLX-MEDIUM, TTF-MEDIUM-MIXIN, NULL and
;;; PostScript — and none for a plain BASIC-MEDIUM.  So COMPOSE-SPACE on the
;;; minibuffer dies inside ADOPT-FRAME with NO-APPLICABLE-METHOD, and Climacs
;;; cannot open.  Nothing about that is ours: the faux medium is made the same
;;; way on every backend, so CLX would land in the same hole.
;;;
;;; The measurement itself needs only the PORT — TTF-MEDIUM-MIXIN's methods are
;;; (font-ascent (text-style-mapping (port medium) text-style)) and nothing more.
;;; So rather than reimplement them against a font we would then have to keep in
;;; step, forward the question to a real medium of the same port.  One per port,
;;; made on demand, never drawn with: a ruler, not a canvas.
;;;
;;; The methods are on BASIC-MEDIUM, which is a core class, so they are careful
;;; to be invisible everywhere else: GLASS-MEDIUM inherits TTF-MEDIUM-MIXIN
;;; BEFORE BASIC-MEDIUM and so keeps its own methods, other backends' mediums are
;;; likewise more specific, and a medium whose port is not ours declines by
;;; CALL-NEXT-METHOD — which is exactly the error it would have signalled anyway.

(defun %port-ruler (medium)
  "A real GLASS-MEDIUM to answer text questions asked of MEDIUM, or NIL if MEDIUM
   is not one of ours to answer for."
  (let ((port (port medium)))
    (when (typep port 'glass-port)
      (or (glass-port-ruler port)
          (setf (glass-port-ruler port) (make-medium port nil))))))

(defmethod text-style-ascent (text-style (medium climi::basic-medium))
  (let ((ruler (%port-ruler medium)))
    (if ruler (text-style-ascent text-style ruler) (call-next-method))))

(defmethod text-style-descent (text-style (medium climi::basic-medium))
  (let ((ruler (%port-ruler medium)))
    (if ruler (text-style-descent text-style ruler) (call-next-method))))

;;; These two default their text style from the medium they are called on, so the
;;; forward has to carry MEDIUM's style across explicitly — the ruler's own is a
;;; different (and arbitrary) one, and letting it answer would silently measure
;;; the wrong font.

(defmethod text-size ((medium climi::basic-medium) string &rest args
                      &key (text-style (medium-merged-text-style medium))
                      &allow-other-keys)
  (let ((ruler (%port-ruler medium)))
    (if ruler
        (apply #'text-size ruler string :text-style text-style args)
        (call-next-method))))

(defmethod text-bounding-rectangle* ((medium climi::basic-medium) string &rest args
                                     &key (text-style (medium-merged-text-style medium))
                                     &allow-other-keys)
  (let ((ruler (%port-ruler medium)))
    (if ruler
        (apply #'text-bounding-rectangle* ruler string :text-style text-style args)
        (call-next-method))))

;;; ---- event injection (RFB callbacks -> CLIM events) ------------------------

(defparameter *modifier-keysyms*
  `((#xffe1 . ,+shift-key+)   (#xffe2 . ,+shift-key+)      ; Shift L/R
    (#xffe3 . ,+control-key+) (#xffe4 . ,+control-key+)    ; Control L/R
    (#xffe9 . ,+meta-key+)    (#xffea . ,+meta-key+)       ; Alt L/R
    (#xffe7 . ,+meta-key+)    (#xffe8 . ,+meta-key+)       ; Meta L/R
    (#xffeb . ,+super-key+)   (#xffec . ,+super-key+)))    ; Super L/R

(defparameter *special-keysyms*
  '((#xff08 :backspace #\Backspace) (#xff09 :tab #\Tab) (#xff0d :return #\Return)
    (#xff1b :escape #\Escape) (#xffff :delete #\Delete) (#xff8d :return #\Return)
    (#xff50 :home) (#xff51 :left) (#xff52 :up) (#xff53 :right) (#xff54 :down)
    (#xff55 :prior) (#xff56 :next) (#xff57 :end) (#xff63 :insert)
    (#xffbe :f1) (#xffbf :f2) (#xffc0 :f3) (#xffc1 :f4) (#xffc2 :f5) (#xffc3 :f6)
    (#xffc4 :f7) (#xffc5 :f8) (#xffc6 :f9) (#xffc7 :f10) (#xffc8 :f11) (#xffc9 :f12)))

(defun keysym->clim (k)
  "Translate an X/RFB keysym into (values key-name key-character)."
  (cond
    ((or (<= #x20 k #x7e) (<= #xa0 k #xff))          ; Latin-1 printables: keysym = codepoint
     (values (intern (string (code-char k)) :keyword) (code-char k)))
    (t (let ((e (assoc k *special-keysyms*)))
         (if e (values (second e) (third e)) (values nil nil))))))

(defun enqueue (port event)
  (sb-concurrency:send-message (glass-port-mailbox port) event))

(defmacro with-reported-errors (&body body)
  `(handler-case (progn ,@body)
     (error (e) (format *trace-output* "~&[glass] event error: ~a: ~a~%" (type-of e) e)
       (force-output *trace-output*))))

;;; ---- the McCLIM seam --------------------------------------------------------
;;;
;;; Everything below this line that touches McCLIM touches a SINGLE-POINTER window
;;; system.  A CLIM port has one CLIMI::PORT-POINTER and one PORT-KEYBOARD-INPUT-FOCUS,
;;; and DISTRIBUTE-EVENT routes every keyboard event to that focus regardless of what
;;; sheet the event names.  There is no per-pointer anything to hang a second seat on
;;; short of reimplementing McCLIM's event distribution, so we do not: McCLIM windows
;;; are ONE CONSOLIDATED SEAT, and which seat that is at any moment is a TOKEN, whose
;;; whole policy — how it is taken, when it goes free, why it is never handed over in the
;;; middle of a gesture, and how McCLIM's single-valued window GEOMETRY travels with it —
;;; is clim-token.lisp.  Read that file's header; this one only calls it.
;;;
;;; The rule is the one a single shared mouse already follows: THE LAST SEAT TO PRESS A
;;; BUTTON INSIDE A McCLIM WINDOW HOLDS THE TOKEN.  While it holds it, its pointer moves
;;; and its keystrokes become CLIM events; another seat's do not (its pointer motion
;;; would otherwise drag CLIM's one pointer around under the holder's hands), right up
;;; until it clicks, which is both a natural gesture for "I am driving now" and
;;; immediately visible to everyone.  No arbitration, no locking, no queue: seats
;;; cooperate, and a rule you can see is worth more here than a rule you cannot lose.
;;; What the token adds to that is the other half of "one at a time": it does not stay
;;; taken.  A holder who moves off the CLIM windows, goes quiet, or disconnects leaves it
;;; FREE, and a free token is taken silently by the next input from anybody — which is
;;; also, exactly, what a lone seat does with it all day.
;;;
;;; What a non-holding seat can still do with a McCLIM window is everything the WINDOW
;;; MANAGER does, because that is ours and not McCLIM's: see it, move it, raise it,
;;; lower it, resize it, close it, and hold it somewhere else than the other seat does.
;;; What it cannot do is type into it or move the pointer inside it without first
;;; clicking.  Native glass surfaces — terminals, the browser, warren, a nested remote
;;; desktop — carry none of this: they take (down keysym) and (mask x y) from whichever
;;; seat is addressing them, so they are per-seat all the way down.

;;; MCCLIM-SEAT-P (the pure "is SEAT driving?" question) and CLIM-TOKEN-CLAIM (the same
;;; question asked by input that may TAKE the token) are both in clim-token.lisp.

(defun glass-port-mcclim-seat (port)
  "The seat driving McCLIM, or NIL.  The name this had before the token was a thing of
   its own; kept because a running desktop's control socket says it."
  (clim-token-holder port))

(defun (setf glass-port-mcclim-seat) (seat port)
  (if seat (clim-token-grant port seat :contest) (clim-token-release port :gone))
  seat)

(defun take-mcclim-seat (port seat)
  "SEAT has clicked inside a McCLIM window: it drives McCLIM from now on."
  (when seat (clim-token-claim port seat :press t))
  seat)

(defun glass-key-sheet (port &optional seat)
  "The sheet a keystroke is addressed to when nothing has claimed the keyboard.

   Mostly nothing reads this.  DISTRIBUTE-EVENT ignores a keyboard event's sheet
   whenever PORT-KEYBOARD-INPUT-FOCUS is set (Core/windowing/ports.lisp), and on a
   desktop something has focus almost always — WM-RAISE gives it to whatever window
   you brought to the front.  This is the answer before the first raise: at startup,
   and after the focused window goes away.

   It used to be GLASS-PORT-TOP, the FIRST top-level sheet the port ever realized,
   which is a reasonable default only for a session that has one window.  The front
   window is the better guess for the same reason it is the right answer after a
   raise.  The pop-up tier (pull-downs, tooltips) is skipped: those are transient
   parts of the window that opened them and they are driven by the pointer.  With no
   managed window, or with no window manager at all, this is GLASS-PORT-TOP exactly
   as before."
  (or (and (glass-port-wm-p port)
           (let ((front (first (sort (remove-if-not #'glass-mirror-managed
                                                    (copy-list (glass-port-mirrors port)))
                                     #'> :key (lambda (m) (seat-window-z seat m))))))
             (and front (glass-mirror-sheet front))))
      (glass-port-top port)))

(defun glass-on-key (port down-p keysym &optional seat)
  (with-reported-errors
  ;; a focused surface window (e.g. a terminal) grabs the keyboard entirely — and
  ;; focus is THIS SEAT's, so two seats type into two terminals at once
  (let ((focus (seat-focus-surface (port-seat port seat))))
    (when (and (glass-port-wm-p port) focus)
      (funcall (wm-surface-on-key focus) down-p keysym)
      (return-from glass-on-key)))
  ;; Past here it is McCLIM's keyboard, which there is only one of: a seat that is not
  ;; holding the token types into nothing rather than into the holder's window.  A
  ;; keystroke is not a press, so it takes only a FREE token — it will not pull the
  ;; keyboard out of somebody's hands — but it does take a free one, which is what makes
  ;; a lone seat that has gone idle able to just start typing again.
  (unless (clim-token-claim port seat) (return-from glass-on-key))
  (let* ((seat (port-seat port seat))
         (mod (cdr (assoc keysym *modifier-keysyms*)))
         (sheet (glass-key-sheet port seat)))
    (cond
      (mod (setf (seat-mods seat)
                 (if down-p (logior (seat-mods seat) mod)
                     (logandc2 (seat-mods seat) mod))))
      (sheet
       (multiple-value-bind (name char) (keysym->clim keysym)
         ;; A keysym we have no CLIM name and no character for is not a key
         ;; press as far as anything upstream is concerned — it is an event
         ;; whose only two interesting slots are both NIL.  A VNC client sends
         ;; these for keys we do not model (Mode_switch, vendor keysyms a
         ;; particular keyboard emits alongside the real one), and passing them
         ;; on makes every gesture-matching loop in McCLIM and ESA consider a
         ;; keystroke that did not happen.  Modifiers went the other way above
         ;; and are already handled; drop the rest.
         (when (or name char)
           (enqueue port
                    (make-instance (if down-p 'key-press-event 'key-release-event)
                                   :key-name name
                                   :key-character (and down-p char)
                                   :sheet sheet
                                   :x (seat-px seat) :y (seat-py seat)
                                   :modifier-state (seat-mods seat)
                                   :timestamp (next-timestamp port))))))))))

(defparameter *button-bits*
  `((1 . ,+pointer-left-button+) (2 . ,+pointer-middle-button+) (4 . ,+pointer-right-button+)))

(defun glass-on-pointer (port mask x y &optional seat)
  (with-reported-errors
    (let* ((seat (port-seat port seat))
           (grab (seat-grab-sheet seat)))
      (setf (seat-px seat) x (seat-py seat) y)
      (cond
        ;; A McCLIM sheet (an open pull-down menu, a drag-tracker) has GRABBED the
        ;; pointer.  We are the window system, so we enforce the grab: route ALL
        ;; pointer events to that sheet (in its own coordinates), so its tracking loop
        ;; sees moves/clicks wherever the pointer is — and a click OUTSIDE it reads as
        ;; blank-area and DISMISSES it.  (X does this with a server-side grab; without
        ;; it, a click over the workspace never reached the menu, so it never closed.)
        ;; The grab is McCLIM's, so only the seat driving McCLIM may satisfy it; another
        ;; seat's pointer goes to the window manager as usual, which is what lets it keep
        ;; moving and raising windows while somebody else has a pull-down open.
        ;; GRAB is read from SEAT's own slot, so only the seat CLIM is tracking can be
        ;; here at all; the claim is what re-takes the token if it went free under it
        ;; (say its viewer blinked) and what refuses if somebody else has since taken it.
        ((and grab (clim-token-claim port seat :press (logtest mask 7))
              (climi::sheet-mirrored-ancestor grab))
         ;; Deliver to the leaf sheet under the pointer WITHIN the grabbing frame (so a
         ;; hover over a submenu button opens it — the tracker keys off event-sheet);
         ;; if the pointer is outside that frame's windows (workspace / another app),
         ;; deliver to the grab sheet so the tracker sees an outside event -> dismiss.
         (let ((frame (ignore-errors (pane-frame grab))))
           (multiple-value-bind (leaf lx ly)
               (when frame (grab-frame-leaf-at port frame x y seat))
             (if leaf
                 (emit-pointer-events port leaf mask (round lx) (round ly) seat)
                 (route-to-grabbed-sheet port grab mask x y seat)))))
        ((glass-port-wm-p port) (wm-on-pointer port mask x y seat))
        (t (glass-on-pointer/single port mask x y seat))))))

(defun leaf-sheet-at (top sx sy)
  "Descend from TOP (point SX,SY in TOP's local coordinates) to the innermost child
   containing the point; (values leaf local-x local-y)."
  (let ((sheet top) (x sx) (y sy))
    (loop (let ((child (ignore-errors (child-containing-position sheet x y))))
            (unless child (return (values sheet x y)))
            (multiple-value-setq (x y) (untransform-position (sheet-transformation child) x y))
            (setf sheet child)))))

(defun grab-frame-leaf-at (port frame x y &optional seat)
  "Topmost MIRROR belonging to FRAME whose screen region contains SCREEN (X,Y) as SEAT
   holds it, descended to its innermost child; (values leaf local-x local-y), or NIL.  A
   menu's pull-downs are separate mirrors but the SAME frame as its menu bar, so this
   reaches a hovered submenu button while excluding other apps / the bare workspace."
  (dolist (m (glass-port-mirrors port))
    (when (typep m 'glass-mirror)
      (let ((sheet (glass-mirror-sheet m))
            (img (ignore-errors (mcclim-render::image-mirror-image m))))
        (when (and sheet img (eq (ignore-errors (pane-frame sheet)) frame))
          (multiple-value-bind (iw ih) (image-wh img)
            (let ((gmx (seat-window-x seat m)) (gmy (seat-window-y seat m)))
              (when (and (<= gmx x (+ gmx iw)) (<= gmy y (+ gmy ih)))
                (return-from grab-frame-leaf-at (leaf-sheet-at sheet (- x gmx) (- y gmy)))))))))))

(defun route-to-grabbed-sheet (port grab mask x y &optional seat)
  "Deliver a pointer event at SCREEN (X,Y) to the grabbed sheet GRAB, mapped into its
   own coordinate system (via its mirror's screen position + native transformation)."
  (let* ((mirror (sheet-direct-mirror (climi::sheet-mirrored-ancestor grab)))
         (gmx (if (typep mirror 'glass-mirror) (seat-window-x seat mirror) 0))
         (gmy (if (typep mirror 'glass-mirror) (seat-window-y seat mirror) 0)))
    (multiple-value-bind (lx ly)
        (ignore-errors (untransform-position (sheet-native-transformation grab)
                                             (- x gmx) (- y gmy)))
      (if lx
          (emit-pointer-events port grab mask (round lx) (round ly) seat)
          (emit-pointer-events port grab mask (- x gmx) (- y gmy) seat)))))

(defun glass-on-pointer/single (port mask x y &optional seat)
  ;; No window manager here — one application filling the screen — so this is the whole
  ;; of the gate: a seat that is not driving may take a free token or press to contest,
  ;; and otherwise its pointer does not reach the one CLIM pointer.
  (when (clim-token-claim port (port-seat port seat) :press (logtest mask 7))
    (when-let ((sheet (glass-port-top port)))
      (emit-pointer-events port sheet mask x y seat))))

(defun emit-pointer-events (port sheet mask lx ly &optional seat)
  "Turn an RFB pointer state (MASK) at sheet-local (LX,LY) into CLIM motion/
   button/scroll events for SHEET.  Button transitions are diffed against SEAT's button
   mask — one physical mouse per seat, and CLIM's one pointer object for all of them,
   because a CLIM port has exactly one (see the McCLIM seam above).

   A BUTTON PRESS is what takes the McCLIM token: a seat that clicks inside a CLIM
   window is driving it from that moment, and a seat that merely moves the mouse over
   somebody else's CLIM window is refused before it gets here.  The claim below is
   therefore mostly a TOUCH — it records that the holder is still doing something, which
   is what keeps the idle sweep off it — and the backstop for the one caller that has no
   window manager above it to have asked first (GLASS-ON-POINTER/SINGLE)."
  (let ((seat (port-seat port seat)))
    (clim-token-claim port seat :press (logtest mask 7))
    ;; wheel (RFB buttons 4/5 = bits 8/16) arrives as a transient press
    (loop for (bit . delta) in '((8 . -1) (16 . 1))
          when (logtest mask bit)
          do (enqueue port (make-instance 'climi::pointer-scroll-event
                                          :pointer (climi::port-pointer port) :sheet sheet
                                          :x lx :y ly :delta-x 0 :delta-y delta
                                          :modifier-state (seat-mods seat)
                                          :timestamp (next-timestamp port))))
    (let ((real (logand mask 7)))
      (enqueue port (make-instance 'pointer-motion-event
                                   :pointer (climi::port-pointer port) :sheet sheet
                                   :x lx :y ly
                                   :modifier-state (seat-mods seat)
                                   :timestamp (next-timestamp port)))
      (let ((changed (logxor real (logand (seat-buttons seat) 7))))
        (loop for (rbit . cbtn) in *button-bits*
              when (logtest changed rbit)
              do (enqueue port
                          (make-instance (if (logtest real rbit)
                                             'pointer-button-press-event
                                             'pointer-button-release-event)
                                         :pointer (climi::port-pointer port) :sheet sheet
                                         :button cbtn :x lx :y ly
                                         :modifier-state (seat-mods seat)
                                         :timestamp (next-timestamp port)))))
      (setf (seat-buttons seat) real)
      ;; The buttons are up: whatever gesture this was is over, so a press another seat
      ;; made while it was running — deferred rather than dropped — takes effect now.
      (when (zerop real) (clim-token-settle port seat)))))

(defun glass-on-resize (port w h &optional seat)
  "Client asked (by resizing its VNC window) for a W x H desktop.  Relayout the
   frame to that size on the event thread; sync-fb-size then resizes the fb and
   the client is told the actual new size via DesktopSize.

   SEAT is accepted and deliberately unused, which is the honest state of it: this
   resizes the MAIN top-level sheet, and in WM mode that is one application's window,
   not the asking seat's screen.  Per-seat client-driven resize means resizing that
   seat's screen framebuffer and recompositing it, and it wants its own commit — until
   then this behaves exactly as it did before there were seats."
  (declare (ignorable seat))
  (with-reported-errors
    (when-let ((sheet (glass-port-top port)))
      (when (and (plusp w) (plusp h))
        ;; the same path the X backend uses for a user-driven window resize: a
        ;; window-configuration-event resizes the sheet (and, via render's
        ;; distribute-event :before, the image) and relays out the frame.
        (enqueue port (make-instance 'window-configuration-event
                                     :sheet sheet
                                     :region (make-bounding-rectangle 0 0 w h)))))))

;;; ---- event loop ------------------------------------------------------------

(defmethod process-next-event ((port glass-port) &key wait-function timeout)
  (let ((deadline (and timeout (+ (get-internal-real-time)
                                  (* timeout internal-time-units-per-second)))))
    (loop
      (when (maybe-funcall wait-function)
        (return (values nil :wait-function)))
      (multiple-value-bind (event ok)
          (sb-concurrency:receive-message-no-hang (glass-port-mailbox port))
        (when ok
          (if (functionp event)             ; a closure marshalled onto the event thread
              (funcall event)
              (distribute-event port event))
          (return t)))
      (when (and deadline (>= (get-internal-real-time) deadline))
        (return (values nil :timeout)))
      (sleep 1/200))))

;;; misc no-ops the frame machinery expects
(defmethod set-mirror-geometry ((port glass-port) sheet region)
  ;; REGION is the mirror's rectangle in screen coordinates — remember where to
  ;; composite this top-level sheet.  render-port-mixin's :after resizes the image
  ;; (always at a 0,0 origin), so position lives here, size in the image.
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* region)
    (when-let ((mirror (sheet-direct-mirror sheet)))
      ;; REGION is the mirror rect in screen coords — where we composite this sheet.
      ;; Managed windows are positioned by moving their SHEET (realize-mirror / wm-move),
      ;; so McCLIM's region already carries the WM slot; just read it back here.
      ;;
      ;; Two slots, because McCLIM's geometry follows whichever seat is DRIVING it and the
      ;; window's own position is the session's arrangement.
      ;;
      ;; This is one of exactly two writers of CLIM-X/Y (clim-token.lisp states the
      ;; invariant and names the other): it records the moves McCLIM makes ON ITS OWN
      ;; ACCOUNT — realising a mirror, a resize, posting a pop-up — where CLIM is telling
      ;; US where it put something.  When the update IS the aim-at-the-driver write it
      ;; writes NEITHER pair: CLIM-SHEET-GOTO has already recorded the target, and this
      ;; apply may be running a request that a later one has since superseded, so writing
      ;; back here would stamp a stale position over a fresh one (the race this replaced:
      ;; the next acquirer's resync then found nothing to move and drove on stale sheet
      ;; geometry).  And X/Y — what every non-diverged seat is looking at — must be left
      ;; alone regardless, or a second seat taking the token would drag the first seat's
      ;; windows across its screen.
      (when (and (typep mirror 'glass-mirror)
                 (not (eq mirror *clim-geometry-follows-driver*)))
        (setf (glass-mirror-clim-x mirror) (floor x1)
              (glass-mirror-clim-y mirror) (floor y1)
              (glass-mirror-x mirror) (floor x1)
              (glass-mirror-y mirror) (floor y1))))
    (values x1 y1 x2 y2)))
;; McCLIM asks the PORT for the modifier state, having one keyboard; the answer is the
;; modifier state of the seat currently driving McCLIM.
(defmethod port-modifier-state ((port glass-port))
  (seat-mods (clim-token-seat port)))
;; NB: keyboard-input-focus is handled by basic-port (it tracks the focused sheet
;; and distribute-event routes key events there) — we must NOT shadow it, or keys
;; never reach an interactor/editor.
(defmethod set-sheet-pointer-cursor ((port glass-port) sheet cursor)
  (declare (ignore sheet cursor)) nil)

;; Pointer grabbing: basic-port only WARNS "not implemented" (so the grab was never
;; recorded and menus had no grab).  We record the TRACKED SHEET ourselves — even for
;; a :multiple-window grab (menu bars use one), where climi::port-grabbed-sheet is
;; merely T — and glass-on-pointer routes every pointer event to it, so a menu's
;; tracking loop sees clicks anywhere and a click OUTSIDE it dismisses it.
;; The grab is recorded on the seat DRIVING McCLIM: the application asking for it has no
;; idea seats exist, and the person it is tracking is by definition the one whose click
;; opened the menu.  Another seat's pointer is not held by it (glass-on-pointer), so a
;; pull-down open on one screen does not freeze the other's mouse.
;; A live grab also PINS the token: while CLIM is tracking the one pointer through a
;; pull-down, another seat's press is remembered rather than obeyed, and it is obeyed the
;; moment the grab is dropped (CLIM-TOKEN-SETTLE below).  That is the case that corrupts
;; state if it is got wrong — the pointer teleporting out of a tracking loop.
(defmethod port-grab-pointer ((port glass-port) pointer sheet &key multiple-window)
  (declare (ignore pointer multiple-window))
  (setf (seat-grab-sheet (clim-token-seat port)) sheet)
  t)
(defmethod port-ungrab-pointer ((port glass-port) pointer sheet)
  (declare (ignore pointer sheet))
  (let ((seat (clim-token-seat port)))
    (setf (seat-grab-sheet seat) nil)
    (clim-token-settle port seat))
  t)

;;; ---- convenience: run a frame ----------------------------------------------

(defun find-glass-port (&key (port 5900))
  (find-port :server-path (list :glass :port port)))

(defun run-frame (frame-class &key (port 5900) (width 800) (height 600))
  "Make an application frame of FRAME-CLASS on a glass port serving on PORT and
   run its top-level loop (blocks).  Point any VNC client at localhost:PORT."
  (let* ((p (find-glass-port :port port))
         (fm (find-frame-manager :port p)))
    (climi::restart-port p)               ; start the port-io-loop thread that drives process-next-event
    (let ((frame (make-application-frame frame-class
                                         :frame-manager fm :width width :height height)))
      (run-frame-top-level frame))))
