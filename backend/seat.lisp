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
;;;;   the screen framebuffer + its size, the wallpaper rendered at that size, the RFB
;;;;   listener serving it and the wake that nudges its senders; pointer position,
;;;;   button mask and modifier state; keyboard focus (the focused surface) and the
;;;;   McCLIM pointer grab; the drag in progress and its wireframe; the open menu
;;;;   chain; the composite's pending damage and its CopyRect hint; and the VIEWS —
;;;;   this seat's position and z for each window.
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
;;;; One seat is PRIMARY and writes THROUGH to those slots instead of shadowing them.
;;;; That is not a special case bolted on: McCLIM's sheet geometry is single-valued —
;;;; a top-level sheet has one screen transformation, and its pull-downs and dialogs
;;;; are placed from it — so somebody has to own the one position McCLIM believes in.
;;;; The primary seat owns it.  See the McCLIM seam in backend.lisp.

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
   ;; --- the transport this seat is watched through ---
   (port-num :initarg :port-num :initform 5900    :accessor seat-port-num) ; RFB listener
   (server   :initform nil :accessor seat-server)                          ; its thread
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

(defmethod print-object ((seat seat) stream)
  (print-unreadable-object (seat stream :type t :identity t)
    (format stream "~s :~d ~dx~d~:[~; primary~]"
            (slot-value seat 'name) (slot-value seat 'port-num)
            (slot-value seat 'screen-w) (slot-value seat 'screen-h)
            (slot-value seat 'primary))))

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
  "Put WINDOW's content at (X,Y) FOR SEAT.  The primary seat writes the window's own
   slots (so McCLIM's one idea of where the window is follows it); any other seat
   materialises a view and moves only its own picture."
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
