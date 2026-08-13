;;;; seat.lisp — a SEAT: one person's view of, and hands on, a shared session.
;;;;
;;;; A session is WHAT RUNS: the applications, their windows, and the pixels each
;;;; window holds.  A seat is WHO IS WATCHING: a screen to composite onto, a pointer,
;;;; a keyboard, a keyboard focus, an open menu, a drag in progress, and — the part
;;;; that makes two seats feel like two desks rather than one screen shared — where
;;;; each window SITS and what is in FRONT of what.
;;;;
;;;; The split is deliberate and it is not the Qubes split.  Domains isolate what
;;;; runs; seats do the opposite — they COOPERATE, over one set of applications.  So
;;;; a window's CONTENT framebuffer is shared (there is one browser, and both people
;;;; are looking at it), and so is its SIZE: size is what the application lays its
;;;; content out to, so a per-seat size would be a per-seat layout, which is a second
;;;; copy of the application wearing the word "seat".  Position and stacking carry no
;;;; such consequence — nothing inside the window can tell — so they are per-seat.
;;;;
;;;; WHAT IS PER-SEAT (this class):
;;;;   the screen framebuffer + its size, the wallpaper rendered at that size, the
;;;;   TRANSPORTS carrying it (an RFB listener today; none, by default, until the seat
;;;;   opens one) and the wake that nudges their senders; an IDENTITY of its own — an
;;;;   npub naming THIS PLACE, so a way in can be asked for by seat rather than by wire;
;;;;   pointer position,
;;;;   button mask and modifier state; keyboard focus (the focused surface) and the
;;;;   McCLIM pointer grab; the drag in progress and its wireframe; the open menu
;;;;   chain; the composite's pending damage and its CopyRect hint; the VIEWS —
;;;;   this seat's position and z for each window — and the HEADSET: this person's mix
;;;;   out, microphone in, ear and dictation (src/headset.lisp), which decomposes the
;;;;   same way the screen does and for the same reason.  A window's pixels are painted
;;;;   once and composited per seat; a source's samples are pulled once and summed per
;;;;   seat.  What a seat gets is a composite, never a second copy of the content.
;;;;
;;;; WHAT STAYS ON THE PORT (session-wide): the mirrors and surfaces themselves (which
;;;;   windows EXIST is not a matter of opinion), the CLIM event mailbox and clock, the
;;;;   z ticket counter, the cascade counter (it places a window's DEFAULT position,
;;;;   and there is only one default), the root menu's app registry, and the text ruler.
;;;;
;;;; ---- how a seat's view of a window is stored -------------------------------
;;;;
;;;; Copy-on-write against the window's own slots.  WM-SURFACE-X/Y and
;;;; GLASS-MIRROR-X/Y (and WM-WINDOW-Z on both) go on meaning exactly what they meant
;;;; before there were seats: THE window's position and place in the stack.  A seat
;;;; that has never moved or raised a particular window has no record for it at all
;;;; and reads those slots; the first time it diverges it materialises a SEAT-VIEW
;;;; initialised from them.  So a seat that joins a running session sees the desktop
;;;; as it stands, and a session with one seat carries no view records whatsoever —
;;;; which is what makes the one-seat case not merely equivalent to the old code but
;;;; the same code path.
;;;;
;;;; One seat is the HOME SEAT (its PRIMARY slot; SEAT-HOME-P) and writes THROUGH to
;;;; those slots instead of shadowing them, so a window's own position goes on meaning
;;;; the SESSION's arrangement — the default a seat sees until it diverges, and what
;;;; somebody joining later finds on screen.  It is fixed at the first seat for the life
;;;; of the session, and the same seat inherits everything else the session has exactly
;;;; one of: GLASS:SESSION-CLIPBOARD, the session mixer's own mix, GLASS:*KEY-INJECTOR*,
;;;; the RFB desktop name, and PORT-SEAT's default.  All of that is about a one-seat
;;;; desktop being the old code path, and none of it follows anybody's mouse.
;;;;
;;;; It is NOT, any more, whose arrangement McCLIM believes in.  McCLIM's sheet geometry
;;;; is single-valued — a top-level sheet has one screen transformation, and its
;;;; pull-downs and dialogs are placed from it — so somebody has to own the one position
;;;; McCLIM believes in, and that is whoever is DRIVING McCLIM right now, which travels
;;;; with the CLIM token.  Those two were one seat and one word until clim-token.lisp
;;;; separated them; its header says why exactly one of the three should move.

(in-package #:clim-glass)

;;; ---- a window's own geometry, whatever kind of window it is -----------------
;;; Two unrelated classes are windows here — GLASS-MIRROR (a McCLIM top-level sheet)
;;; and WM-SURFACE (a framebuffer somebody else paints) — and the seat must not care
;;; which.  The methods live with the classes (backend.lisp, wm.lisp); these are the
;;; questions.

(defgeneric window-own-x (window)
  (:documentation "WINDOW's own content x — the position a seat sees until it moves it."))
(defgeneric (setf window-own-x) (value window))
(defgeneric window-own-y (window))
(defgeneric (setf window-own-y) (value window))
(defgeneric window-own-z (window)
  (:documentation "WINDOW's own place in the stacking order (a ticket from the port's ZCLOCK)."))
(defgeneric (setf window-own-z) (value window))

;;; ---- a wire that reaches this seat ------------------------------------------
;;;
;;; A SEAT is what you connect to; a TRANSPORT is what carries it.  They were one thing
;;; and one slot — the seat's PORT-NUM and the thread serving it — which is why a session
;;; could not decline to serve: RUN-WM ran the session AND opened the listener, in that
;;; order, in one call.  docs/seats-and-transports.md is the argument; this class is the
;;; two lines of state that make "a seat with no transport" a thing you can hold.
;;;
;;; A seat may have several (a VNC listener and, later, a WebRTC channel), none, or one it
;;; opens and closes again while the session runs.  What every transport of one seat
;;; shares is that seat's SELECTION, its KEYBOARD and its SCREEN; what it does not share
;;; with another SEAT's transport is any of them.
;;;
;;; The socket is held HERE rather than left inside the serving thread, because closing is
;;; the whole point and a socket only the parked accept can name cannot be closed — see
;;; GLASS:CLOSE-LISTENER, which also says why SOCKET-CLOSE alone leaves the port listening.

(defclass seat-transport ()
  ((seat     :initarg :seat     :initform nil   :reader transport-seat)
   ;; :RFB is a port anybody on this box can reach; :RFB-UNIX is a socket file only its owner
   ;; can open.  SIBLINGS, deliberately: the same protocol, carried differently, and a seat may
   ;; hold one of each.  KIND was already the discriminator, so a UNIX wire is another value of
   ;; it rather than a flag beside it — which also keeps SEAT-TRANSPORTS readable, because what
   ;; a reader wants to know about a wire is what it is.
   (kind     :initarg :kind     :initform :rfb  :reader transport-kind)
   (address  :initarg :address  :initform "0.0.0.0" :reader transport-address)
   (port-num :initarg :port-num :initform nil   :reader transport-port-num)
   (path     :initarg :path     :initform nil   :reader transport-path)      ; :RFB-UNIX only
   (socket   :initarg :socket   :initform nil   :accessor transport-socket)  ; the LISTENER
   (thread   :initform nil :accessor transport-thread)                       ; its accept loop
   ;; Set before the socket is torn down, so the accept loop's resulting error is
   ;; recognised as the thing we asked for rather than reported as a failure.
   (closing  :initform nil :accessor transport-closing-p))
  (:documentation
   "One wire onto one seat: an RFB listener today, and the shape the others will take.
    A class for the reason everything long-lived here is one — a desktop runs for weeks
    and grows slots while it runs, and redefining a structure strands the instances
    already made."))

(defmethod print-object ((tr seat-transport) stream)
  (print-unreadable-object (tr stream :type t)
    (format stream "~a ~a~:[~; closed~]"
            (slot-value tr 'kind) (transport-endpoint tr)
            (null (slot-value tr 'socket)))))

(defun transport-endpoint (transport)
  "Where TRANSPORT is, in one phrase: a socket path, or an address and port."
  (or (slot-value transport 'path)
      (format nil "~a:~a" (slot-value transport 'address) (slot-value transport 'port-num))))

(defun transport-open-p (transport)
  "Is TRANSPORT still listening?"
  (and transport (transport-socket transport) t))

;;; ---- this seat's identity ----------------------------------------------------
;;;
;;; A seat gets an npub of its own: `DM this npub and get a link' is a way in to A SEAT,
;;; not to a wire, so a seat reachable over VNC today and something else tomorrow is the
;;; same seat and a rotated transport key changes nothing about the destination.
;;;
;;; CORE GLASS CARRIES NO CRYPTO, and that is a property this file must not quietly
;;; spend.  cl-nostr and ironclad are dependencies of :glass/nostr ALONE; a desktop built
;;; without it must start, run and serve exactly as it does now.  So what lives here is an
;;; OPAQUE slot and the question `what is this seat's identity' — minting it, persisting
;;; it and interpreting it are :glass/nostr's, reached by name (GLASS:SEAT-IDENTITY-FOR),
;;; the same way START-SEAT-AUDIO reaches GLASS:MAKE-HEADSET.  Nothing here knows what an
;;; npub is, and nothing here can be made to sign anything.
;;;
;;; SEAT IDENTITY IS NOT PERSON IDENTITY.  The seat's key says WHICH PLACE THIS IS: it
;;; belongs to the session's configuration and persists across whoever is sitting there.
;;; The person's key — GLASS:*NOSTR-ALLOW* and the device enrolments in src/nostr.lisp —
;;; says WHO MAY SIT IN IT.  Both are npubs and they answer different questions; collapse
;;; them and `the owner sat down at the guest seat' stops being sayable, which is a thing
;;; people do.  They are kept in different stores for that reason and not by accident.

(defvar *seat-identity* t
  "Do new seats get an identity of their own?  T (the default) means ADD-SEAT asks for one,
   which does nothing at all unless :glass/nostr is loaded.  NIL suppresses it — for a
   test that must not touch a key store, and for a session that wants its seats anonymous.")

(defun seat-identity-provider ()
  "GLASS:SEAT-IDENTITY-FOR if this image has :glass/nostr, else NIL."
  (let ((s (find-symbol "SEAT-IDENTITY-FOR" '#:glass))) (and s (fboundp s) s)))

(defun ensure-seat-identity (seat)
  "Give SEAT an identity if it has none and this image can mint one.  Returns it, or NIL.

   NIL is an ordinary answer and not a failure: a desktop without :glass/nostr has seats
   with no identity, which is the same desktop it has always been."
  (or (seat-identity seat)
      (when *seat-identity*
        (let ((for (seat-identity-provider)))
          (when for
            ;; Never fatal.  A key store that cannot be read is a seat without a name, not
            ;; a desktop that fails to start — the identity is for addressing a seat, and
            ;; nothing about drawing on one depends on having it.
            (setf (seat-identity seat) (ignore-errors (funcall for (seat-name seat)))))))))

(defun seat-npub (seat)
  "SEAT's public name, or NIL if it has no identity (or this image cannot read one)."
  (let ((id (and seat (seat-identity seat)))
        (npub (let ((s (find-symbol "SEAT-IDENTITY-NPUB" '#:glass))) (and s (fboundp s) s))))
    (and id npub (ignore-errors (funcall npub id)))))

;;; WHERE A SIGNATURE WOULD GO.  A seat that signed its requests would be a seat the
;;; session could VERIFY, which is what would let a seat live in another process without
;;; the trust boundary being "whoever can reach the port".  It is deliberately not built:
;;; identity is given now because retrofitting it is a migration, and verification is
;;; added later because it is a feature — and there is no attacker today on a loopback
;;; call between two objects in one image.  The signature would be taken over the request
;;; a remote seat makes (open a transport, move a window, take the CLIM token) with
;;; SEAT-IDENTITY as the signing key, and checked here against the seat named in it.

;;; ---- one seat's divergent view of one window --------------------------------

(defclass seat-view ()
  ((x :initarg :x :initform 0 :accessor view-x)
   (y :initarg :y :initform 0 :accessor view-y)
   (z :initarg :z :initform 0 :accessor view-z))
  (:documentation
   "Where one window sits, and how high it stands, FOR ONE SEAT.  Exists only once
    that seat has moved or restacked that window; until then the window's own slots
    answer.  A class and not a structure for the reason everything long-lived here is
    one: a desktop runs for weeks and grows slots while it runs, and redefining a
    structure strands the instances already made."))

(defmethod print-object ((v seat-view) stream)
  (print-unreadable-object (v stream :type t)
    (format stream "~d,~d z~d" (view-x v) (view-y v) (view-z v))))

;;; ---- the seat ---------------------------------------------------------------

(defclass seat ()
  ((name     :initarg :name     :initform "seat"  :accessor seat-name)
   (port     :initarg :port     :initform nil     :accessor seat-port)   ; the session
   (primary  :initarg :primary  :initform nil     :accessor seat-primary-p)
   ;; --- who this seat IS ---
   ;; OPAQUE to everything in this file and in core glass: an object minted and understood
   ;; by :glass/nostr, or NIL on a desktop that never loaded it.  See the section above for
   ;; why it is opaque, and for why it is not the same question as who may sit here.
   (identity :initarg :identity :initform nil     :accessor seat-identity)
   ;; --- the wires this seat is watched through ---
   ;; TRANSPORTS is the list of them, and it is empty until somebody opens one: SERVING IS
   ;; A SEAT'S DECISION.  PORT-NUM is the port this seat serves on WHEN IT SERVES — a
   ;; setting, not a listener, and it has always been one; nothing listens because it is
   ;; set.  SERVER is kept because GLASS-PORT-SERVER delegates to it and callers outside
   ;; this tree read it; it holds the RFB transport's thread.
   (port-num   :initarg :port-num :initform 5900  :accessor seat-port-num)
   (transports :initform '() :accessor seat-transports)
   (server     :initform nil :accessor seat-server)                        ; the RFB one's thread
   (wake     :initform (glass:make-wake) :accessor seat-wake)              ; nudges ITS senders
   ;; --- the screen ---
   (fb       :initarg :fb       :initform nil     :accessor seat-fb)
   (screen-w :initarg :screen-w :initform 1000    :accessor seat-screen-w)
   (screen-h :initarg :screen-h :initform 720     :accessor seat-screen-h)
   ;; The wallpaper is per-seat because it is rasterised AT THE SCREEN SIZE: a phone
   ;; seat and a desktop seat looking at the same session want the same image and not
   ;; the same pixels.  The PATH is the session's taste; these are one seat's pixels.
   (bg       :initform nil :accessor seat-bg)
   ;; --- the hands ---
   (mods     :initform 0   :accessor seat-mods)        ; CLIM modifier state
   (buttons  :initform 0   :accessor seat-buttons)     ; RFB button mask
   (px       :initform 0   :accessor seat-px)          ; pointer position
   (py       :initform 0   :accessor seat-py)
   (focus-surface :initform nil :accessor seat-focus-surface)  ; surface holding THIS keyboard
   (grab-sheet    :initform nil :accessor seat-grab-sheet)     ; McCLIM sheet grabbing the pointer
   ;; What THIS person last copied.  Per-seat, not per-transport and not per-session: all of
   ;; one seat's transports (a VNC viewer and a WebRTC channel onto the same screen) share
   ;; one selection, and two seats do not, because two people copying must not clobber each
   ;; other.  See the header of src/clipboard.lisp, which draws that distinction.
   ;;
   ;; The PRIMARY seat's is GLASS:SESSION-CLIPBOARD itself, not a private one (see ADD-SEAT).
   ;; A one-seat desktop must keep having exactly one selection, and everything that reaches
   ;; the clipboard without going through a seat — dictation, a paste chord, loom, an app
   ;; calling SESSION-CLIPBOARD — must land on the same one the only person here is using.
   (clipboard :initarg :clipboard :initform nil :accessor seat-clipboard)
   ;; THIS person's sound: their mix out on a port of their own, their microphone in on
   ;; another, their ear behind it, and their dictation.  A GLASS:HEADSET, made by
   ;; START-SEAT-AUDIO, and NIL until something asks for one — a desktop can be watched
   ;; without being listened to, and the audio systems are optional (see src/headset.lisp
   ;; for why each of those four is one person's and not the session's).
   (headset  :initform nil :accessor seat-headset)
   ;; This seat's keyboard, as a function (DOWN-P KEYSYM): the very :ON-KEY its RFB
   ;; listener was started with, kept so that anything TYPING for this seat — dictation,
   ;; a paste — reaches the window THIS seat has focused.  GLASS:*KEY-INJECTOR* cannot
   ;; answer that question: there is one of it, and the last listener to start owns it.
   (injector :initform nil :accessor seat-injector)
   ;; --- what this seat is in the middle of ---
   (drag     :initform nil :accessor seat-drag)          ; (window mode . rest) while moving/resizing
   (drag-wire :initform nil :accessor seat-drag-wire)    ; this drag went wireframe (laggy link)?
   (drag-wire-box :initform nil :accessor seat-drag-wire-box)
   (menu     :initform nil :accessor seat-menu)          ; open menu chain, or nil
   ;; --- this seat's composite ---
   (frame-copy :initform nil :accessor seat-frame-copy)  ; CopyRect hint contributed by the paint
   (pending  :initform nil :accessor seat-pending)       ; :full / (x y w h) / nil
   (pending-lock :initform (sb-thread:make-mutex :name "glass-seat-pending")
                 :accessor seat-pending-lock)
   ;; window -> the content rect THIS SEAT's screen was last known to hold whole, which
   ;; is what makes a translation believable (see SEAT-COPY-BASE).  Per-seat because it
   ;; is a claim about one screen's pixels: two seats holding a window at different
   ;; places, under different clips, with different things stacked over it, do not have
   ;; the same claim to make.  The hint ITSELF is taken once per round and shared — see
   ;; WM-SURFACE-ROUND-HINT — so no seat loses the CopyRect to another seat's take.
   (copy-bases :initform (make-hash-table :test 'eq) :accessor seat-copy-bases)
   ;; window -> SEAT-VIEW, for the windows this seat has moved or restacked.  EQ, and
   ;; empty for a seat that has diverged from nothing.
   (views    :initform (make-hash-table :test 'eq) :accessor seat-views))
  (:documentation "One person's screen, hands, and arrangement of a shared session."))

;;; ---- the two questions that used to be one --------------------------------

(defun seat-home-p (seat)
  "Is SEAT the HOME seat — the one that inherits the resources a session has exactly one
   of (the session clipboard, the session mix, the key injector, the desktop name,
   PORT-SEAT's default) and whose window moves write the session's own arrangement?

   Fixed at the first seat for the life of the session.  The preferred name for the
   PRIMARY slot, which is kept under its old name because that is what every existing
   call site says.  Deliberately NOT the same question as `is SEAT driving McCLIM' —
   see CLIM-DRIVER-P and the header of clim-token.lisp."
  (and seat (seat-primary-p seat)))

(defun seat-mid-gesture-p (seat)
  "Is SEAT in the middle of something that must not be interrupted?  A CLIM grab (a
   pull-down or a tracking loop is following the one pointer), a window drag or resize in
   flight, a window-manager menu posted, or simply a button held down.

   This is what PINS the McCLIM token: a press from another seat during any of these
   would teleport CLIM's single pointer out from under a gesture already running."
  (and seat
       (or (seat-grab-sheet seat)
           (seat-drag seat)
           (seat-menu seat)
           (logtest (seat-buttons seat) 7))
       t))

(defmethod print-object ((seat seat) stream)
  (print-unreadable-object (seat stream :type t :identity t)
    (format stream "~s :~d ~dx~d~:[~; primary~]~:[ (no transport)~;~]"
            (slot-value seat 'name) (slot-value seat 'port-num)
            (slot-value seat 'screen-w) (slot-value seat 'screen-h)
            (slot-value seat 'primary)
            (slot-value seat 'transports))))

;;; ---- this seat's view of a window -------------------------------------------

(defun seat-view (seat window &optional create)
  "SEAT's divergent view of WINDOW, or NIL if it has never diverged.  With CREATE,
   materialise one from WINDOW's own geometry first — copy-on-write, so the window
   does not jump when a seat first touches it."
  (let ((tbl (seat-views seat)))
    (or (gethash window tbl)
        (when create
          (setf (gethash window tbl)
                (make-instance 'seat-view :x (window-own-x window)
                                          :y (window-own-y window)
                                          :z (window-own-z window)))))))

(defun seat-forget-window (seat window)
  "Drop SEAT's view of WINDOW — it closed, or its arrangement is to be reset to the
   session default."
  (remhash window (seat-views seat)))

(defun seat-window-x (seat window)
  (let ((v (and seat (seat-view seat window)))) (if v (view-x v) (window-own-x window))))
(defun seat-window-y (seat window)
  (let ((v (and seat (seat-view seat window)))) (if v (view-y v) (window-own-y window))))
(defun seat-window-z (seat window)
  (let ((v (and seat (seat-view seat window)))) (if v (view-z v) (window-own-z window))))

(defun seat-move-window (seat window x y)
  "Put WINDOW's content at (X,Y) FOR SEAT.  The HOME seat writes the window's own slots —
   the session's arrangement, which every seat that has not diverged is reading — and any
   other seat materialises a view and moves only its own picture.

   McCLIM's idea of where the window is no longer rides on this.  It follows whoever is
   DRIVING McCLIM, home seat or not, and is written separately (WM-SYNC-SHEET ->
   CLIM-SHEET-GOTO)."
  (if (or (null seat) (seat-primary-p seat))
      (setf (window-own-x window) x (window-own-y window) y)
      (let ((v (seat-view seat window t)))
        (setf (view-x v) x (view-y v) y)))
  window)

(defun seat-restack-window (seat window z)
  "Give WINDOW the stacking ticket Z for SEAT.  Tickets come from the session's one
   ZCLOCK whatever seat asked for them, so they stay comparable across seats: a window
   a second seat has never restacked keeps its session ticket and sorts sensibly among
   the ones this seat has."
  (if (or (null seat) (seat-primary-p seat))
      (setf (window-own-z window) z)
      (setf (view-z (seat-view seat window t)) z))
  window)

;;; ---- boxes -------------------------------------------------------------------
;;; (x y w h) rectangles are the currency of every damage, occlusion and hit question
;;; here.  WM-BOX-UNION lives with the seat rather than in the window manager because
;;; the damage accumulator below is the first thing that needs it.

(defun wm-box-union (boxes)
  "Bounding (x y w h) of BOXES, or NIL if empty."
  (let ((x0 nil) (y0 nil) (x1 nil) (y1 nil))
    (dolist (b (remove nil boxes))
      (destructuring-bind (x y w h) b
        (setf x0 (if x0 (min x0 x) x) y0 (if y0 (min y0 y) y)
              x1 (if x1 (max x1 (+ x w)) (+ x w)) y1 (if y1 (max y1 (+ y h)) (+ y h)))))
    (when x0 (list x0 y0 (- x1 x0) (- y1 y0)))))

;;; ---- this seat's pending damage ---------------------------------------------

(defun seat-accumulate-damage (seat box)
  "Union BOX (an (x y w h) rect in THIS SEAT's screen coordinates, or NIL = the whole
   screen) into SEAT's pending damage.  The WM tick loop drains and composites it, so a
   burst of repaints coalesces into one composite per seat."
  (sb-thread:with-mutex ((seat-pending-lock seat))
    (let ((cur (seat-pending seat)))
      (setf (seat-pending seat)
            (cond ((or (eq cur :full) (null box)) :full)
                  ((null cur) box)
                  (t (wm-box-union (list cur box))))))))

(defun seat-take-pending (seat)
  "Atomically read + clear SEAT's pending damage: :full, an (x y w h) box, or NIL."
  (sb-thread:with-mutex ((seat-pending-lock seat))
    (prog1 (seat-pending seat) (setf (seat-pending seat) nil))))
