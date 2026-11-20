;;;; src/wm/wm.lisp — the window manager: windows, z-order, focus, compositing, the pointer, seats.
;;;;
;;;; MOVED from backend/wm.lisp (and backend/backend.lisp's COMPOSITE-SEAT): the compositing
;;;; (damage-clipped, background skipped under an opaque window), the scroll CopyRect machinery,
;;;; the hit test, the adaptive drag (opaque with a CopyRect hint, WIREFRAME once the link lags),
;;;; resize, raise/lower/full-size/close, the root and window menus and their pointer handling,
;;;; and the tick.  REWRITTEN around them: the state.  The McCLIM desktop keeps windows on a
;;;; GLASS-PORT (a CLIM port) and arrangement on SEATs (backend/seat.lisp); here the session is
;;;; one struct, WM, its seats are SEAT objects, and every window is a surface -- no mirrors, no
;;;; pop-up tier, no McCLIM token.
;;;;
;;;; FROM THE LEAN DESK (src/desk.lisp): the keep-the-title-bar-on-screen clamp while dragging,
;;;; a content press that holds the pointer until the lift, and the on-screen-keyboard hint --
;;;; WM-KEYBOARD-FN is told whether the focused window takes text.
;;;;
;;;; THE APPLICATION CONTRACT (shared with glass.desk): MAKE-FN is called with the window's
;;;; content framebuffer and returns
;;;;     (values ON-KEY ON-POINTER DIRTY-P COPY-P CLOSE-FN RESIZE-FN TEXT-P)
;;;; all optional.  ON-KEY (down keysym), ON-POINTER (mask x y) in window-local pixels; DIRTY-P
;;;; ()->bool polled every tick (NIL: always repaint); COPY-P ()->(sx sy dx dy w h)|NIL, how the
;;;; content scrolled since last asked (consumed); CLOSE-FN ()->(); RESIZE-FN (w h)->() resizes
;;;; the content (the app calls FB-RESIZE on its own framebuffer), NIL = fixed size; TEXT-P true
;;;; when the app takes typing (a phone raises its keyboard while the window has focus).
;;;;
;;;; ONE THREAD.  Everything here runs on the thread that made the WM (SERVE-WM's compositor):
;;;; the windows, their framebuffers, their apps and every seat are that thread's own.  The only
;;;; things another thread sees are each seat's screen pixels and FB-MARK-FRAME's damage + copy.

(in-package #:glass.wm)

(defmacro when-let ((var form) &body body) `(let ((,var ,form)) (when ,var ,@body)))

;;; ---- tunables --------------------------------------------------------------------------------

(defparameter *drag-adaptive* t "Auto-switch a laggy opaque drag to wireframe.")

(defparameter *wireframe-queue-kb* 100.0d0
  "Socket send-queue backlog EWMA (KB, GLASS:*SEND-QUEUE*) past which an in-progress drag goes
   wireframe -- the McCLIM desktop's trigger, and the primary one here: bytes written that the
   client has not drained (SIOCOUTQ, on SBCL and modus, TCP and socket files alike).  A CopyRect
   client's cheap opaque drag never backs the queue up; a client re-receiving the whole window
   every frame does.  *WIREFRAME-LAG-MS* is the secondary trigger, for a client that stalls
   between requests rather than filling the socket.")

(defparameter *wireframe-lag-ms* 150
  "Send backlog (ms) past which an in-progress drag goes wireframe.  Read two ways: the
   sender's EWMA GLASS:*SEND-LAG* (how long each delivered change waited between this
   compositor marking it and a sender taking it), and the age of the change that is waiting
   RIGHT NOW (the screen's mark time while damage is untaken) -- the EWMA only moves when a
   frame is delivered, so a stalled client would otherwise read as fast.

   Why 150.  Under SERVE-MULTI the sender takes a frame when the client has asked for one, so
   the backlog is roughly one update's transfer plus the client's round trip.  A client keeping
   up -- localhost, or a CopyRect drag where each update is a CopyRect and two thin strips --
   reads ~1-30 ms; a link re-sending a whole window per frame reads hundreds.  150 ms sits above
   the loaded-machine noise of the first and well below where a drag visibly trails the
   pointer (the McCLIM desktop's first adaptive drag used 120 on the same clock).")

(defparameter *wm-scroll-copyrect* t
  "Honour a window's scroll CopyRect hint (COPY-P)?  NIL diffs instead -- always correct.")

(defparameter *wm-log* t
  "Log the window manager's mode switches (a drag going wireframe) to *ERROR-OUTPUT*.")

(defparameter *wm-surface-error-limit* 60
  "Consecutive per-tick errors from one window's poll or draw before it is closed.")

;;; ---- state -----------------------------------------------------------------------------------
;;;
;;; THE SESSION AND ITS SEATS.  A WM is the session: what runs -- the windows, each one's content
;;; framebuffer and SIZE, the app registry (the root menu), the cascade, and one z counter.  A SEAT
;;; is who is watching: a screen (framebuffer + size), a pointer and buttons, the keyboard focus,
;;; the content grab, a drag (and its wireframe) or a resize, the open menu, the pending damage and
;;; the CopyRect hint, the clipboard, and -- what makes two seats two desks rather than one screen
;;; shown twice -- where each window SITS and what is in FRONT of what.  Size is not per-seat: it
;;; is what the application lays its content out to, so a per-seat size would be a second copy of
;;; the application.  backend/seat.lisp is the design, built and accepted on the McCLIM desktop.
;;;
;;; COPY-ON-WRITE.  A window's own X, Y and Z go on meaning what they meant before there were
;;; seats: THE window's position and place in the stack.  A seat that never moved or restacked a
;;; window holds no record for it and reads those slots; the first time it diverges it materialises
;;; a SEAT-VIEW initialised from them.  The first seat is the HOME seat (MAKE-WM makes it): its
;;; moves and raises write the window's own slots -- the default every other seat inherits -- and
;;; its clipboard is GLASS:SESSION-CLIPBOARD.  So a one-seat session carries no view record at all
;;; and runs the same code it ran before there were seats, not an equivalent one.
;;;
;;; THE CURRENT SEAT.  Everything below that is one person's -- the screen, the focus, the drag,
;;; the stacking a hit test walks, where a window is drawn -- reads *SEAT*, bound by whoever acts
;;; for a seat (WM-POINTER, WM-KEY, WM-RESIZE-SCREEN, the tick's per-seat composite) and NIL
;;; otherwise, which means the home seat.  So the window manager's code reads as it did with one
;;; seat, and a caller that knows nothing of seats drives the home seat.
;;;
;;; DAMAGE IS CUT BY KIND.  Content damage (a window repainted itself) is the session's: the tick
;;; polls each window ONCE and damages it on every seat, at the box that seat draws it in -- the
;;; window's own coordinates converted per seat at the last moment.  View damage (a move, a raise,
;;; a menu, a drag's outline) is one seat's, screen-space from birth.  The one crossing: a change
;;; to a window's OWN slots (the home seat moving or raising a window; any seat opening, closing
;;; or resizing one) is also what every seat reading those slots sees, so those seats are damaged
;;; where it was and where it is (WITH-OTHERS-DAMAGED) -- with one seat, nobody.

(defvar *seat* nil
  "The seat being acted for: its screen, focus, drag, menu and arrangement are what the window
   manager's functions read.  NIL is the home seat.")

(defstruct (surf (:constructor %make-surf))
  fb (x 60) (y 60) (title "window")
  (z 0)                                 ; own place in the stack: a ticket from the WM's z counter
  (deco-fb nil) (deco-w -1)             ; cached title bar, and the width it was drawn for
  on-key on-pointer dirty-p copy-p close-fn resize-fn
  (text-p nil)
  (selection-fn nil)                    ; () -> what the window has highlighted now, or NIL
  (copy-base nil)                       ; HOME seat's: content box as of the last composite that drew it whole
  (copy-round nil) (copy-hint nil)      ; this tick's take of COPY-P, judged by every seat
  (saved-geom nil)                      ; (x y w h) saved by Full Size
  (span nil)                            ; rack units it takes in the RACK layout (rack.lisp), NIL = not placed yet
  (pos nil)                             ; the rack unit it starts at (rack.lisp), NIL = not placed yet
  (drag-p nil)                          ; (lambda (x y)): is a drag starting there the app's?  NIL: every one is
  (err-count 0))

(defmethod print-object ((s surf) stream)
  (print-unreadable-object (s stream :type t :identity t)
    (format stream "~s ~d,~d" (surf-title s) (surf-x s) (surf-y s))))

(defclass seat-view ()
  ((x :initarg :x :accessor view-x)
   (y :initarg :y :accessor view-y)
   (z :initarg :z :accessor view-z))
  (:documentation "Where one window sits, and how high, FOR ONE SEAT -- only once that seat has
   moved or restacked it; until then the window's own slots answer."))

(defmethod print-object ((v seat-view) stream)
  (print-unreadable-object (v stream :type t)
    (format stream "~d,~d z~d" (view-x v) (view-y v) (view-z v))))

(defclass seat ()
  ((id        :initarg :id        :initform 0   :reader seat-id)        ; the home seat is 0
   (key       :initarg :key       :initform nil :reader seat-key)       ; who asked for it (a device id)
   (home-p    :initarg :home-p    :initform nil :reader seat-home-p)
   (fb        :initarg :fb        :initform nil :accessor seat-fb)      ; the screen
   ;; the selection this seat's viewers copy into and are told about; NIL = GLASS:SESSION-CLIPBOARD
   (clipboard :initarg :clipboard :initform nil :accessor %seat-clipboard)
   ;; how many viewers it has right now (the network says).  A seat nobody watches is not
   ;; composited; the home seat always is, as it was before there were seats.
   (viewers   :initform 0   :accessor seat-viewers)
   ;; the hands
   (px        :initform 0   :accessor seat-px)          ; the pointer, as last reported
   (py        :initform 0   :accessor seat-py)
   (last-mask :initform 0   :accessor seat-last-mask)   ; the buttons as of the previous event
   (focus     :initform nil :accessor seat-focus)       ; the surface keys go to
   (grab      :initform nil :accessor seat-grab)        ; a content press's surface, until the lift
   (keyboard-fn :initform nil :accessor seat-keyboard-fn) ; (lambda (on)): the host's on-screen keyboard
   (keyboard-up :initform nil :accessor seat-keyboard-up) ; what KEYBOARD-FN was last told
   ;; what it is in the middle of
   (drag      :initform nil :accessor seat-drag)        ; (surf :move dx dy) | (surf :resize x0 y0 cw0 ch0)
   (drag-wire :initform nil :accessor seat-drag-wire)   ; this drag went wireframe (the link lagged)
   (drag-wire-box :initform nil :accessor seat-drag-wire-box)
   (menu      :initform nil :accessor seat-menu)        ; the open menu tree's root
   ;; its composite
   (pending   :initform :full :accessor seat-pending)   ; NIL, :FULL or (x y w h); first, everything
   (frame-copy :initform nil :accessor seat-frame-copy) ; a scroll CopyRect found while painting
   ;; surf -> SEAT-VIEW, and surf -> its content box as THIS screen last held it whole.  NIL until
   ;; the first entry, and never anything but NIL on the home seat.
   (views      :initform nil :accessor seat-views)
   (copy-bases :initform nil :accessor seat-copy-bases)
   (wire-drags :initform 0 :accessor seat-wire-drags)   ; counts, for a test or a curious host
   (opaque-moves :initform 0 :accessor seat-opaque-moves))
  (:documentation "One person's screen, hands and arrangement of the session's windows."))

(defmethod print-object ((seat seat) stream)
  (print-unreadable-object (seat stream :type t :identity t)
    (format stream "~d~@[ ~s~]~:[~; home~] ~ax~a" (seat-id seat) (seat-key seat) (seat-home-p seat)
            (ignore-errors (glass:fb-width (seat-fb seat)))
            (ignore-errors (glass:fb-height (seat-fb seat))))))

(defun seat-clipboard (seat)
  "SEAT's selection: the home seat's is the session's, any other seat's its own."
  (or (%seat-clipboard seat) (glass:session-clipboard)))

(defun seat-view-count (seat)
  "How many windows SEAT has its own view of (0 for the home seat, always)."
  (let ((tbl (seat-views seat))) (if tbl (hash-table-count tbl) 0)))

(defstruct (wm (:constructor %make-wm))
  (surfaces '())        ; the session's windows by their OWN z, topmost first (the home seat's stack)
  (apps '())            ; ((label make-fn w h) ...), in menu order
  (actions '())         ; ((label . thunk) ...): root-menu entries that open no window
  (cascade 0)
  (zclock 0)            ; the one monotonic z counter: every seat's tickets come from it
  home                  ; the home seat
  (seats '())           ; every seat, the home seat first
  ;; THE RACK (rack.lisp): NIL lays windows out as they are put (floating), :RACK in rack units
  (layout nil)
  (rack-order '())      ; the windows in rack order, first at the top (portrait) or left (landscape)
  (rack-scroll 0)       ; how far the rack is scrolled along its axis, in pixels
  (rack-drag nil)       ; the overflow bar dragged: (nil along0 scroll0 moved-p)
  (rack-press nil)      ; a press in a unit, not yet a tap, a drag or a long press: (s x y t0 mask)
  (rack-lift nil)       ; a unit picked up by a long press: (s grab-offset moved-p)
  (rack-want nil)       ; the empty slot the root menu was opened on: where its app goes
  (obscured 0))         ; pixels at the bottom of the screen hidden by the host's keyboard (WM-SET-OBSCURED)

(defun make-wm (fb)
  "A window manager drawing into FB, the home seat's screen."
  (let ((home (make-instance 'seat :id 0 :home-p t :fb fb)))
    (%make-wm :home home :seats (list home))))

(declaim (inline cur-seat))
(defun cur-seat (wm) (or *seat* (wm-home wm)))

;; The current seat's hands and screen -- the home seat's, unless one is being acted for -- under
;; the names the one-seat window manager gave them.
(macrolet ((by-seat (&rest pairs)
             `(progn
                ,@(loop for (name acc) on pairs by #'cddr
                        collect `(defun ,name (wm) (,acc (cur-seat wm)))
                        collect `(defun (setf ,name) (v wm) (setf (,acc (cur-seat wm)) v))))))
  (by-seat wm-fb seat-fb  wm-focus seat-focus  wm-grab seat-grab  wm-drag seat-drag
           wm-drag-wire seat-drag-wire  wm-drag-wire-box seat-drag-wire-box  wm-menu seat-menu
           wm-last-mask seat-last-mask  wm-pending seat-pending  wm-frame-copy seat-frame-copy
           wm-keyboard-fn seat-keyboard-fn  wm-keyboard-up seat-keyboard-up
           wm-wire-drags seat-wire-drags  wm-opaque-moves seat-opaque-moves))

(defun wm-find-seat (wm id)
  "The seat numbered ID (NIL or 0: the home seat), or NIL if there is none."
  (if (or (null id) (eql id 0)) (wm-home wm) (find id (wm-seats wm) :key #'seat-id)))

(defun wm-add-seat (wm id fb &key key clipboard)
  "Seat number ID onto the session: its screen FB, its selection CLIPBOARD (NIL: the session's).
   It sees the session as it stands -- every window where its own slots say -- and holds no view
   record until it moves or raises something.  Returns the seat; an ID that is already a seat
   returns that seat unchanged."
  (or (wm-find-seat wm id)
      (let ((seat (make-instance 'seat :id id :key key :fb fb :clipboard clipboard)))
        (setf (wm-seats wm) (append (wm-seats wm) (list seat)))
        seat)))

(defun composited-p (seat)
  "Is SEAT's screen kept up to date?  The home seat's always; another's while somebody watches."
  (or (seat-home-p seat) (plusp (seat-viewers seat))))

(defun wm-seat-viewers (wm seat n)
  "The network says SEAT has N viewers.  A seat coming into view is repainted whole: nothing
   was composited onto it while nobody watched, so its screen holds nothing a copy can trust."
  (declare (ignore wm))
  (when (and (plusp n) (not (composited-p seat)))
    (setf (seat-pending seat) :full (seat-copy-bases seat) nil))
  (setf (seat-viewers seat) n))

(defun wm-register-app (wm label make-fn &key (width 400) (height 300))
  "Put LABEL on the root menu; choosing it opens a WIDTH x HEIGHT window running MAKE-FN."
  (setf (wm-apps wm)
        (append (remove label (wm-apps wm) :key #'first :test #'string=)
                (list (list label make-fn width height))))
  label)

(defun wm-register-action (wm label thunk)
  "Put LABEL on the root menu, after the applications; choosing it calls THUNK and opens no window
   -- what a host's own screens hang off (a phone's native views, drawn over the desktop)."
  (setf (wm-actions wm)
        (append (remove label (wm-actions wm) :key #'car :test #'string=)
                (list (cons label thunk))))
  label)

(defun wm-redraw (wm) (setf (wm-pending wm) :full))

(defun screen-w (wm) (glass:fb-width (wm-fb wm)))
(defun screen-h (wm) (glass:fb-height (wm-fb wm)))

;;; ---- boxes -----------------------------------------------------------------------------------

(defun box-intersect (a b)
  "Intersection of two (x y w h) boxes, or NIL."
  (when (and a b)
    (destructuring-bind (ax ay aw ah) a
      (destructuring-bind (bx by bw bh) b
        (let ((x0 (max ax bx)) (y0 (max ay by))
              (x1 (min (+ ax aw) (+ bx bw))) (y1 (min (+ ay ah) (+ by bh))))
          (when (and (< x0 x1) (< y0 y1)) (list x0 y0 (- x1 x0) (- y1 y0))))))))

(defun boxes-overlap-p (a b) (and (box-intersect a b) t))
(defun box-inside-p (inner outer) (and inner outer (equal inner (box-intersect inner outer))))

(defun box-union (boxes)
  "The bounding box of BOXES (NILs ignored), or NIL."
  (let ((bs (remove nil boxes)))
    (when bs
      (let ((x0 most-positive-fixnum) (y0 most-positive-fixnum)
            (x1 most-negative-fixnum) (y1 most-negative-fixnum))
        (dolist (b bs)
          (destructuring-bind (x y w h) b
            (setf x0 (min x0 x) y0 (min y0 y) x1 (max x1 (+ x w)) y1 (max y1 (+ y h)))))
        (list x0 y0 (- x1 x0) (- y1 y0))))))

(defun box-difference (a b)
  "The parts of A that B does not cover, as up to four rectangles."
  (let ((i (box-intersect a b)))
    (if (null i)
        (list a)
        (destructuring-bind (ax ay aw ah) a
          (destructuring-bind (bx by bw bh) i
            (let ((out '()))
              (when (> by ay) (push (list ax ay aw (- by ay)) out))
              (when (< (+ by bh) (+ ay ah)) (push (list ax (+ by bh) aw (- (+ ay ah) by bh)) out))
              (when (> bx ax) (push (list ax by (- bx ax) bh) out))
              (when (< (+ bx bw) (+ ax aw)) (push (list (+ bx bw) by (- (+ ax aw) bx bw) bh) out))
              out))))))

(defun screen-box (wm) (list 0 0 (screen-w wm) (screen-h wm)))

(defun clip-box (fb)
  "FB's clip as an (x y w h) box, or NIL when unclipped."
  (when-let (c (glass:fb-clip fb))
    (list (first c) (second c) (- (third c) (first c)) (- (fourth c) (second c)))))

(defun clip-copy (copy screen)
  "COPY (sx sy dx dy w h) trimmed so its source and destination both lie in SCREEN (x y w h),
   or NIL if nothing is left.  A CopyRect reaching off the screen is refused whole by the
   sender; trimmed, a window dragged half off an edge still moves by CopyRect."
  (when copy
    (destructuring-bind (sx sy dx dy w h) copy
      (let* ((ddx (- dx sx)) (ddy (- dy sy))
             (dst (box-intersect (box-intersect (list dx dy w h) screen)
                                 (destructuring-bind (x y sw sh) screen
                                   (list (+ x ddx) (+ y ddy) sw sh)))))
        (when (and dst (or (/= ddx 0) (/= ddy 0)))
          (destructuring-bind (x y w2 h2) dst
            (list (- x ddx) (- y ddy) x y w2 h2)))))))

;;; ---- this seat's view of a window ------------------------------------------------------------

(declaim (inline view-of))
(defun view-of (s)
  "The current seat's divergent view of S, or NIL."
  (let ((seat *seat*))
    (and seat (let ((tbl (seat-views seat))) (and tbl (gethash s tbl))))))

(defun sx (s) "S's content x on the current seat." (let ((v (view-of s))) (if v (view-x v) (surf-x s))))
(defun sy (s) "S's content y on the current seat." (let ((v (view-of s))) (if v (view-y v) (surf-y s))))
(defun sz (s) "S's z on the current seat."         (let ((v (view-of s))) (if v (view-z v) (surf-z s))))

(defun %view (seat s)
  "SEAT's view of S, materialised from S's own slots (copy-on-write) if it has none."
  (let ((tbl (or (seat-views seat) (setf (seat-views seat) (make-hash-table :test 'eq)))))
    (or (gethash s tbl)
        (setf (gethash s tbl) (make-instance 'seat-view :x (surf-x s) :y (surf-y s) :z (surf-z s))))))

(defun seat-copy-base (seat s)
  "The content box SEAT's screen last held S whole at, or NIL: what makes a scroll believable."
  (if (seat-home-p seat)
      (surf-copy-base s)
      (let ((tbl (seat-copy-bases seat))) (and tbl (gethash s tbl)))))

(defun (setf seat-copy-base) (v seat s)
  (if (seat-home-p seat)
      (setf (surf-copy-base s) v)
      (setf (gethash s (or (seat-copy-bases seat)
                           (setf (seat-copy-bases seat) (make-hash-table :test 'eq))))
            v)))

(defun forget-copy-bases (wm s)
  "S's content changed size: no seat's screen holds it as it now is."
  (dolist (seat (wm-seats wm)) (setf (seat-copy-base seat s) nil)))

(defun stack (wm)
  "The windows as the current seat stacks them, topmost first.  A seat that has restacked
   nothing -- the home seat always -- stacks them by their own z, which is the session's list."
  (if (zerop (seat-view-count (cur-seat wm)))
      (wm-surfaces wm)
      (stable-sort (copy-list (wm-surfaces wm)) #'> :key #'sz)))

;;; ---- geometry --------------------------------------------------------------------------------

(defun surf-cw (s) (glass:fb-width (surf-fb s)))
(defun surf-ch (s) (glass:fb-height (surf-fb s)))

(defun window-box-at (s cx cy)
  "The decorated (x y w h) S would occupy with its content at (CX,CY)."
  (list (- cx (wm-border)) (- cy (wm-titleh) (wm-border))
        (+ (surf-cw s) (* 2 (wm-border))) (+ (wm-titleh) (surf-ch s) (* 2 (wm-border)))))

(defun window-box (s) (window-box-at s (sx s) (sy s)))

(defun surf-deco (s)
  (when (zerop (wm-titleh)) (return-from surf-deco nil))     ; no title bars (the rack)
  (let ((cw (surf-cw s)))
    (when (or (null (surf-deco-fb s)) (/= cw (surf-deco-w s)))
      (setf (surf-deco-fb s) (wm-render-titlebar (surf-title s) cw) (surf-deco-w s) cw))
    (surf-deco-fb s)))

(defun wm-topmost (wm) (first (stack wm)))

(defun boxes-above (wm s)
  "Every box drawn after S: windows above it, the drag wireframe, the open menu chain."
  (let ((boxes '()))
    (loop for w in (stack wm) until (eq w s) do (push (window-box w) boxes))
    (when (wm-drag-wire wm) (when-let (b (wm-drag-wire-box wm)) (push b boxes)))
    (when-let (menu (wm-menu wm))
      (dolist (m (wm-menu-chain menu)) (push (wm-menu-box m) boxes)))
    boxes))

(defun obstructed-p (wm s boxes)
  "Does anything drawn after S land on any of BOXES?"
  (let ((above (boxes-above wm s)))
    (and above (some (lambda (b) (some (lambda (a) (boxes-overlap-p a b)) above)) (remove nil boxes)) t)))

(defun covered-p (wm box)
  "Is BOX entirely inside one window (each paints its whole decorated rectangle)?"
  (and box (some (lambda (s) (box-inside-p box (window-box s))) (wm-surfaces wm))))

;;; ---- what other seats see of a change to a window's own slots ---------------------------------

(defun seat-damage (seat box)
  "Add BOX (SEAT's screen coordinates; NIL = all of it) to what SEAT's next composite repaints.
   A seat nobody watches collects nothing: it is repainted whole when it comes into view."
  (when (composited-p seat)
    (let ((p (seat-pending seat)))
      (setf (seat-pending seat) (cond ((eq p :full) :full) ((null box) :full) ((null p) box)
                                      (t (box-union (list p box))))))))

(defun seat-window-box (seat s) (let ((*seat* seat)) (window-box s)))

(defun others-boxes (wm s)
  "(SEAT . S's box there) for every seat but the current one -- NIL in a one-seat session."
  (when (cdr (wm-seats wm))
    (let ((me (cur-seat wm)))
      (loop for seat in (wm-seats wm)
            unless (eq seat me) collect (cons seat (seat-window-box seat s))))))

(defun damage-others (wm s before &key restack)
  "After a change to S other seats may see: damage each (BEFORE, from OTHERS-BOXES) where S was
   and where it is now.  With RESTACK (S's own z changed) a seat reading S's own z repaints it
   even though it did not move."
  (loop for (seat . old) in before
        for new = (and (member s (wm-surfaces wm)) (seat-window-box seat s))
        do (when (or (not (equal old new))
                     (and restack (not (let ((*seat* seat)) (view-of s)))))
             (seat-damage seat (box-union (list old new))))))

(defmacro with-others-damaged ((wm s &rest keys) &body body)
  "BODY changes S in a way the other seats may see; damage them where it was and where it is."
  (let ((before (gensym "BEFORE")))
    `(let ((,before (others-boxes ,wm ,s)))
       (multiple-value-prog1 (progn ,@body) (damage-others ,wm ,s ,before ,@keys)))))

(defun seat-move (wm s x y)
  "Put S's content at (X,Y) for the current seat: the home seat writes S's own position -- the
   default every seat that never moved S reads, so they are damaged too -- any other seat its
   own view."
  (let ((seat (cur-seat wm)))
    (if (seat-home-p seat)
        (with-others-damaged (wm s) (setf (surf-x s) x (surf-y s) y))
        (let ((v (%view seat s))) (setf (view-x v) x (view-y v) y)))))

(defun seat-restack (wm s z)
  "Give S the stacking ticket Z for the current seat: the home seat's is S's own z (and the
   session's list is kept in that order), any other seat's its own view."
  (let ((seat (cur-seat wm)))
    (if (seat-home-p seat)
        (with-others-damaged (wm s :restack t)
          (setf (surf-z s) z
                (wm-surfaces wm) (stable-sort (copy-list (wm-surfaces wm)) #'> :key #'surf-z)))
        (setf (view-z (%view seat s)) z))))

;;; ---- compositing -----------------------------------------------------------------------------
;;;
;;; WM-COMPOSITE is COMPOSITE-SEAT: redraw a damaged box (or the screen) of the CURRENT seat under
;;; its screen's pixel lock and tell the senders where, with a CopyRect hint when the composite
;;; was a window move or a window's scroll.  The lock is held across the mark, so a sender taking
;;; the frame sees the copy and the pixels it describes together.
;;;
;;; A window reports its scroll CONSUMINGLY (COPY-P: what moved since you last asked), so if each
;;; seat asked in turn the first would take it and the rest would lose the CopyRect.  The tick
;;; therefore takes each window's hint once per round (*COPY-ROUND*) and every seat judges that
;;; one against its own screen, stack and clip.  Outside a round (a drag's composite) each ask
;;; takes afresh, as it always did.  The apps run on this thread, so no window repaints between
;;; one seat's blit and the next one's within a round.

(defvar *copy-round* nil "The tick's compositing round, or NIL outside one: see above.")
(defvar *tick-round* 0)

(defun take-copy-hint (s)
  (let ((take (surf-copy-p s)))
    (cond ((null take) nil)
          ((null *copy-round*) (funcall take))
          ((eql (surf-copy-round s) *copy-round*) (surf-copy-hint s))
          (t (setf (surf-copy-round s) *copy-round*
                   (surf-copy-hint s) (funcall take))))))

(defun surf-screen-copy (wm s fb)
  "Take S's pending content translation and judge it as a screen CopyRect for this composite.
   (values WIRE SCREEN ALLOWED): the hint for the RFB update, the hint the compositor may also
   apply to the screen itself (refused more often: nothing repairs a wrong in-RAM translation),
   and S's visible content in this composite.  See backend/wm.lisp WM-SURFACE-SCREEN-COPY."
  (let* ((hint (take-copy-hint s))
         (seat (cur-seat wm))
         (content (list (sx s) (sy s) (surf-cw s) (surf-ch s)))
         (clipb (or (clip-box fb) (screen-box wm)))
         (base (seat-copy-base seat s)))
    (setf (seat-copy-base seat s) (and (equal content (box-intersect content clipb)) content))
    (when (and hint *wm-scroll-copyrect* (equal base content))
      (destructuring-bind (hsx hsy hdx hdy hw hh) hint
        (let* ((ddx (- hdx hsx)) (ddy (- hdy hsy))
               (allowed (box-intersect (box-intersect content clipb) (screen-box wm)))
               (dst (and allowed
                         (box-intersect
                          (box-intersect (list (+ (first content) hdx) (+ (second content) hdy) hw hh)
                                         allowed)
                          (destructuring-bind (ax ay aw ah) allowed
                            (list (+ ax ddx) (+ ay ddy) aw ah))))))
          (when (and dst (or (/= ddx 0) (/= ddy 0)))
            (destructuring-bind (dx dy dw dh) dst
              (let* ((src (list (- dx ddx) (- dy ddy) dw dh))
                     (copy (list (first src) (second src) dx dy dw dh))
                     (wire (unless (obstructed-p wm s (list src dst)) copy))
                     (screen (unless (obstructed-p wm s (list allowed)) copy)))
                (values wire screen allowed)))))))))

(defun note-surface-error (wm s e)
  "Log a window's poll/draw error; close the window once it has failed too many ticks running."
  (ignore-errors (format *error-output* "~&glass wm: window ~s: ~a~%" (surf-title s) e))
  (when (>= (incf (surf-err-count s)) *wm-surface-error-limit*)
    (ignore-errors (%forget wm s))
    (dolist (seat (wm-seats wm)) (seat-damage seat nil))))

(defun draw-surface (wm s fb)
  (let ((cx (sx s)) (cy (sy s)))
    (wm-frame fb cx cy (surf-cw s) (surf-ch s) (surf-deco s)
              (lambda ()
                (let ((c (ignore-errors (surf-screen-copy wm s fb)))
                      (cur (wm-frame-copy wm)))
                  (when (and c (or (null cur) (> (* (fifth c) (sixth c)) (* (fifth cur) (sixth cur)))))
                    (setf (wm-frame-copy wm) c)))
                (blit-fb (surf-fb s) cx cy fb)))))

(defun composite-scroll (wm fb box)
  "Paint this composite as a window's scroll -- move what the screen holds, blit only the strip
   it exposed -- when the topmost window containing BOX reports a believable translation.  True
   if it did; the wire hint is kept either way."
  (when-let (s (and *wm-scroll-copyrect* box
                    (find-if (lambda (w) (and (surf-copy-p w) (box-inside-p box (window-box w))))
                             (stack wm))))
    (multiple-value-bind (wire screen allowed)
        (handler-case (surf-screen-copy wm s fb) (error () (values nil nil nil)))
      (when wire (setf (wm-frame-copy wm) wire))
      (when screen
        (handler-case
            (destructuring-bind (sx sy dx dy w h) screen
              (glass:fb-move-rect fb sx sy dx dy w h)
              (dolist (band (box-difference allowed (list dx dy w h)) t)
                (destructuring-bind (bx by bw bh) band
                  (glass:with-fb-clip (fb bx by bw bh) (blit-fb (surf-fb s) (sx s) (sy s) fb)))))
          (error (e) (note-surface-error wm s e) nil))))))

(defun composite-whole (wm fb box)
  "Rebuild BOX (NIL = the screen) bottom up: workspace, windows, wireframe, menus."
  (unless (covered-p wm box)
    (glass:fb-fill fb (if (rack-p wm) *rack-panel* *wm-bg*)))   ; the rack: filler, not a workspace
  (if (rack-p wm)
      (rack-draw-units wm fb)
      (dolist (s (reverse (stack wm)))
        (handler-case (draw-surface wm s fb)
          (error (e) (note-surface-error wm s e)))))
  (when (rack-p wm) (rack-draw-bar wm fb) (rack-draw-lift wm fb))
  (when-let (b (and (wm-drag-wire wm) (wm-drag-wire-box wm)))
    (destructuring-bind (x y w h) b
      (let ((t2 (max 2 (round (* 2 *wm-scale*)))) (t1 (max 1 (round *wm-scale*))))
        (glass:fb-frame fb x y w h glass:+white+ t2)
        (glass:fb-frame fb (+ x t1) (+ y t1) (max 0 (- w (* 2 t1))) (max 0 (- h (* 2 t1)))
                        glass:+black+ t1))))
  (when-let (menu (wm-menu wm))
    (dolist (m (wm-menu-chain menu))
      (blit-fb (wm-menu-fb m) (wm-menu-x m) (wm-menu-y m) fb))))

(defun wm-composite (wm &optional damage copy)
  "Redraw the current seat's screen -- only DAMAGE (x y w h) when given -- and mark the frame for
   the senders, with COPY (sx sy dx dy w h) when the composite moved a window.  A scroll hint
   found while painting rides when there is no explicit COPY."
  (let* ((fb (wm-fb wm))
         (box (if damage (box-intersect damage (screen-box wm)) (screen-box wm))))
    (when box
      (glass:with-fb-locked (fb)
        (setf (wm-frame-copy wm) nil)
        (if damage
            (destructuring-bind (dx dy dw dh) box
              (glass:with-fb-clip (fb dx dy dw dh)
                (unless (composite-scroll wm fb box) (composite-whole wm fb box)))
              (glass:fb-mark-frame fb (list dx dy (+ dx dw) (+ dy dh))
                                   (clip-copy (or copy (wm-frame-copy wm)) box)))
            (progn (composite-whole wm fb nil) (glass:fb-mark-frame fb :full)))
        (setf (wm-frame-copy wm) nil)
        (glass:fb-touch fb)))))

(defun damage (wm box)
  "Add BOX to what the current seat's next tick repaints."
  (let ((p (wm-pending wm)))
    (setf (wm-pending wm) (cond ((eq p :full) :full) ((null box) :full) (t (box-union (list p box)))))))

(defun damage-content (wm s)
  "S repainted itself: CONTENT damage, the session's -- every watched seat, where it draws S."
  (dolist (seat (wm-seats wm))
    (when (composited-p seat) (seat-damage seat (seat-window-box seat s)))))

(defun wm-tick (wm)
  "Poll every window once and repaint what changed on every seat that is watched.  Returns T if
   it painted."
  (when (rack-p wm) (rack-tick wm))
  (dolist (s (wm-surfaces wm))
    (handler-case
        (let ((dp (surf-dirty-p s)))
          (when (or (null dp) (funcall dp)) (damage-content wm s))
          (setf (surf-err-count s) 0))
      (error (e) (note-surface-error wm s e))))
  (let ((*copy-round* (incf *tick-round*)) (painted nil))
    (dolist (seat (wm-seats wm) painted)
      (let ((p (seat-pending seat)))
        (when (and p (composited-p seat))
          (setf (seat-pending seat) nil painted t)
          (let ((*seat* seat)) (wm-composite wm (unless (eq p :full) p))))))))

;;; ---- focus and the host keyboard --------------------------------------------------------------

(defun %focus (wm s &key (keyboard t) force)
  "The current seat's keys now go to S (or nowhere).  With KEYBOARD the host's on-screen keyboard
   follows: up while the focused window takes text, down otherwise; FORCE says it again even if
   unchanged (a tap into a text window after the keyboard was swiped away by hand should bring it
   back).  Without KEYBOARD (a title-bar press) the hint is not raised: picking a window up to
   move it is not asking to type, and taking it from another window puts the keyboard away."
  (let ((was (wm-focus wm)))
    (setf (wm-focus wm) s)
    (let ((want (cond ((null s) nil)
                      (keyboard (and (surf-text-p s) t))
                      ((eq s was) (wm-keyboard-up wm))
                      (t nil))))
      (when (and (wm-keyboard-fn wm) (or force (not (eq want (wm-keyboard-up wm)))))
        (setf (wm-keyboard-up wm) want)
        (ignore-errors (funcall (wm-keyboard-fn wm) want))))))

;;; ---- windows ---------------------------------------------------------------------------------

(defun wm-open (wm label)
  "Open the application LABEL in a new window on top -- for EVERY seat, its z ticket being its
   own -- with the current seat's keys (only: a window appearing on my screen must not take the
   keys from under you).  Placed for the current seat's screen.  Returns the window."
  (when-let (app (find label (wm-apps wm) :key #'first :test #'string=))
    (destructuring-bind (lbl make-fn w h) app
      (let* ((sw (screen-w wm)) (sh (screen-h wm))
             (w (max 32 (min w (- sw (* 2 (wm-border))))))
             (h (max 32 (min h (- sh (wm-titleh) (* 2 (wm-border))))))
             (fb (glass:make-framebuffer w h glass:+white+))
             (c (wm-cascade wm))
             (x (max (wm-border) (min (+ 40 c) (- sw w (wm-border)))))
             (y (max (+ (wm-titleh) (wm-border)) (min (+ 40 c (wm-titleh)) (- sh h (wm-border))))))
        (multiple-value-bind (on-key on-pointer dirty-p copy-p close-fn resize-fn text-p selection-fn drag-p)
            (funcall make-fn fb)
          (setf (wm-cascade wm) (mod (+ c 28) 200))
          (let ((s (%make-surf :fb fb :x x :y y :title lbl :on-key on-key :on-pointer on-pointer
                               :dirty-p dirty-p :copy-p copy-p :close-fn close-fn
                               :resize-fn resize-fn :text-p (and text-p t)
                               :selection-fn selection-fn :drag-p drag-p
                               :z (incf (wm-zclock wm)))))
            (push s (wm-surfaces wm))
            (%focus wm s)
            (if (rack-p wm)
                (rack-opened wm s w h)
                (progn
                  (damage wm (window-box s))
                  (loop for (seat . box) in (others-boxes wm s) do (seat-damage seat box))))
            s))))))

(defun %forget (wm s)
  "S is gone for everybody: every seat's hands come off it and no seat keeps a view of it."
  (setf (wm-surfaces wm) (remove s (wm-surfaces wm)))
  (when (eq (first (wm-rack-press wm)) s) (setf (wm-rack-press wm) nil))
  (when (eq (first (wm-rack-lift wm)) s) (setf (wm-rack-lift wm) nil))
  (dolist (seat (wm-seats wm))
    (let ((*seat* seat))
      (when (eq (wm-grab wm) s) (setf (wm-grab wm) nil))
      (when (and (wm-drag wm) (eq (first (wm-drag wm)) s))
        (setf (wm-drag wm) nil (wm-drag-wire wm) nil (wm-drag-wire-box wm) nil))
      (when (eq (wm-focus wm) s) (%focus wm nil)))
    (when (seat-views seat) (remhash s (seat-views seat)))
    (when (seat-copy-bases seat) (remhash s (seat-copy-bases seat))))
  (when (surf-close-fn s) (ignore-errors (funcall (surf-close-fn s)))))

(defun wm-close (wm s)
  (let ((box (window-box s)))
    (with-others-damaged (wm s) (%forget wm s))
    (if (rack-p wm) (rack-closed wm) (wm-composite wm box))))

(defun wm-raise (wm s)
  (unless (eq s (wm-topmost wm))
    (seat-restack wm s (incf (wm-zclock wm)))
    (wm-composite wm (window-box s))))

(defun wm-lower (wm s)
  "Back: behind every other window (a ticket below every other: the counter's next, negated)."
  (seat-restack wm s (- (incf (wm-zclock wm))))
  (wm-composite wm (window-box s)))

(defun wm-resize (wm s w h)
  "Resize S's content to W x H (clamped), through its RESIZE-FN, and repaint old and new -- on
   every seat, a window's size being the session's."
  (when (surf-resize-fn s)
    (let ((w (max 32 (min w (- (screen-w wm) (* 2 (wm-border))))))
          (h (max 32 (min h (- (screen-h wm) (wm-titleh) (* 2 (wm-border)))))))
      (unless (and (= w (surf-cw s)) (= h (surf-ch s)))
        (let ((old (window-box s)))
          (with-others-damaged (wm s)
            (handler-case (funcall (surf-resize-fn s) w h)
              (error (e) (note-surface-error wm s e))))
          (forget-copy-bases wm s)
          (wm-composite wm (box-union (list old (window-box s)))))))))

(defun wm-fullsize (wm s)
  "Full Size / Restore Size for a resizable window: the current seat's screen, its position."
  (when (surf-resize-fn s)
    (let ((old (window-box s)))
      (with-others-damaged (wm s)
        (if (surf-saved-geom s)
            (destructuring-bind (x y w h) (surf-saved-geom s)
              (seat-move wm s x y)
              (setf (surf-saved-geom s) nil)
              (ignore-errors (funcall (surf-resize-fn s) w h)))
            (progn
              (setf (surf-saved-geom s) (list (sx s) (sy s) (surf-cw s) (surf-ch s)))
              (seat-move wm s (wm-border) (+ (wm-titleh) (wm-border)))
              (ignore-errors (funcall (surf-resize-fn s) (- (screen-w wm) (* 2 (wm-border)))
                                      (- (screen-h wm) (wm-titleh) (* 2 (wm-border))))))))
      (forget-copy-bases wm s)
      (wm-composite wm (box-union (list old (window-box s)))))))

(defparameter *screen-min* 240 "The smallest screen side a client may ask for, in pixels.")
(defparameter *screen-max* 2560 "The largest: a framebuffer is W x H words, and every one is sent.")

(defun wm-resize-screen (wm w h &optional seat)
  "Make SEAT's screen (default: the current seat's, i.e. the home seat's) W x H, clamped to
   *SCREEN-MIN*..*SCREEN-MAX*: what a client's SetDesktopSize asks.  Every window is kept
   reachable on the new screen -- its position clamped as a drag's is, for this seat -- an open
   menu is closed (it was placed for the old screen), a drag in progress is let go, and the whole
   screen is repainted.  The RFB senders see the framebuffer's new size and tell their clients.

   ONLY THE HOME SEAT SHRINKS WINDOWS.  A window wider or taller than the home seat's new screen
   is resized to fit (through its RESIZE-FN), as before there were seats.  A window's size is the
   session's, so another seat -- a phone joining a laptop's desktop -- shrinking every window to
   its own screen would re-lay-out everybody's; it gets the window at its top left, title bar
   showing, and its own drag and Full Size.  Returns T if the size changed."
  (let* ((*seat* (or seat *seat*))
         (me (cur-seat wm))
         (w (max *screen-min* (min *screen-max* w)))
         (h (max *screen-min* (min *screen-max* h)))
         (fb (wm-fb wm)))
    (unless (and (= w (glass:fb-width fb)) (= h (glass:fb-height fb)))
      (glass:fb-resize fb w h *wm-bg*)
      (setf (wm-menu wm) nil (wm-drag wm) nil (wm-drag-wire wm) nil (wm-drag-wire-box wm) nil)
      ;; THE RACK turns with the screen: laid out again the other way (rack.lisp)
      (when (and (rack-p wm) (seat-home-p me))
        (setf (wm-rack-drag wm) nil (wm-rack-press wm) nil (wm-rack-lift wm) nil)
        (rack-layout wm)
        (return-from wm-resize-screen t))
      (dolist (s (wm-surfaces wm))
        (setf (seat-copy-base me s) nil)
        (when (seat-home-p me)
          (setf (surf-saved-geom s) nil)
          (let ((mw (- w (* 2 (wm-border)))) (mh (- h (wm-titleh) (* 2 (wm-border)))))
            (when (and (surf-resize-fn s) (or (> (surf-cw s) mw) (> (surf-ch s) mh)))
              (with-others-damaged (wm s)
                (handler-case (funcall (surf-resize-fn s) (max 32 (min mw (surf-cw s)))
                                       (max 32 (min mh (surf-ch s))))
                  (error (e) (note-surface-error wm s e))))
              (forget-copy-bases wm s))))
        ;; a window that still does not fit starts at the top left, its title bar showing
        (let ((x (if (> (+ (sx s) (surf-cw s) (wm-border)) w) (wm-border) (sx s)))
              (y (if (> (+ (sy s) (surf-ch s) (wm-border)) h) (+ (wm-titleh) (wm-border)) (sy s))))
          (multiple-value-bind (cx cy) (clamp-position wm s x y)
            (unless (and (= cx (sx s)) (= cy (sy s)))
              (seat-move wm s cx cy)))))
      (wm-composite wm)
      t)))

;;; ---- hit test --------------------------------------------------------------------------------

(defun wm-hit (wm x y)
  "The topmost window at (X,Y) on the current seat: (values S REGION CX CY CW CH), REGION one of
   :WINMENU (the wedge), :CLOSE (the close box), :RESIZE (the bottom-right corner of a resizable
   window), :TITLE or :CONTENT; NIL over the workspace."
  (dolist (s (stack wm) nil)
    (let* ((cx (sx s)) (cy (sy s)) (cw (surf-cw s)) (ch (surf-ch s))
           (ty (- cy (wm-titleh))) (b (wm-border))
           (rz (max 6 (round (* 16 *wm-scale*)))))
      (multiple-value-bind (bx by bs) (wm-menu-button-box)
        (multiple-value-bind (kx ky ks) (wm-close-button-box cw)
          (let ((region
                  (cond
                    ((not (and (<= (- cx b) x (+ cx cw b -1)) (<= (- ty b) y (+ cy ch b -1)))) nil)
                    ((and (<= (+ cx bx) x (+ cx bx bs)) (<= (+ ty by) y (+ ty by bs))) :winmenu)
                    ((and (> cw (* 4 (wm-titleh)))
                          (<= (+ cx kx) x (+ cx kx ks)) (<= (+ ty ky) y (+ ty ky ks))) :close)
                    ((and (surf-resize-fn s)
                          (<= (- (+ cx cw) rz) x) (<= (- (+ cy ch) rz) y)) :resize)
                    ((and (<= cx x (1- (+ cx cw))) (<= cy y (1- (+ cy ch)))) :content)
                    ((< y cy) :title)
                    (t :frame))))                       ; the border: a press there just raises
            (when region (return (values s region cx cy cw ch)))))))))

;;; ---- the drag: opaque when the link keeps up, wireframe when it lags --------------------------
;;;
;;; An opaque move composites old+new with a CopyRect hint: near-free on a client that can
;;; CopyRect, but a client that cannot re-receives the whole window every frame.  So the drag
;;; starts opaque and switches to a WIREFRAME outline once the link is behind -- only the
;;; outline's thin tiles change from then on -- and the window lands on release.  Once
;;; wireframe, it stays wireframe for the rest of that drag (no flapping).  Per seat: the drag,
;;; its outline and the age of the change waiting are the dragging seat's; the send-lag and
;;; send-queue EWMAs are the process's (glass's senders keep one of each).

(defun pending-age-ms (fb)
  "How long the change waiting for a sender has waited (ms), or 0 when none is waiting."
  (if (glass:fb-damage fb)
      (let ((mt (glass::fb-mark-time fb)))
        (if (plusp mt)
            (/ (* 1000 (- (get-internal-real-time) mt)) internal-time-units-per-second)
            0))
      0))

(defun link-lagging-p (wm)
  "Is the link behind?  The socket send queue (Linux TCP), the sender's backlog EWMA, or the age
   of the change waiting now on this seat's screen -- see *WIREFRAME-LAG-MS*."
  (or (> glass:*send-queue* *wireframe-queue-kb*)
      (> glass:*send-lag* *wireframe-lag-ms*)
      (> (pending-age-ms (wm-fb wm)) *wireframe-lag-ms*)))

(defun clamp-position (wm s ncx ncy)
  "(values X Y): content position (NCX,NCY) kept so 40 pixels of the title bar stay on screen
   -- a window dragged out of reach could never be dragged back, and on a touch screen there is
   nothing else to grab it by (from the lean desk)."
  (let ((sw (screen-w wm)) (sh (screen-h wm)) (keep (round (* 40 *wm-scale*))))
    (values (max (- keep (surf-cw s)) (min (- sw keep) ncx))
            (max (+ (wm-titleh) (wm-border)) (min (- sh (round (* 4 *wm-scale*))) ncy)))))

(defun drag-move-opaque (wm s ncx ncy)
  "Move S to (NCX,NCY) and composite old+new with a CopyRect hint for the move."
  (let ((old (window-box s)))
    (seat-move wm s ncx ncy)
    (let ((new (window-box s)))
      (unless (equal old new)
        (incf (wm-opaque-moves wm))
        (wm-composite wm (box-union (list old new))
                      (list (first old) (second old) (first new) (second new)
                            (third old) (fourth old)))))))

(defun drag-wire-to (wm s ncx ncy)
  "Move only the outline; the window stays put (its pixels stay on the client)."
  (let ((old (wm-drag-wire-box wm)) (new (window-box-at s ncx ncy)))
    (unless (equal old new)
      (setf (wm-drag-wire-box wm) new)
      (wm-composite wm (box-union (list old new))))))

(defun drag-move (wm s ncx ncy)
  "One drag step: opaque, unless the link is behind -- then this drag goes wireframe."
  (if (and *drag-adaptive* (link-lagging-p wm))
      (progn
        (when *wm-log*
          (ignore-errors
           (format *error-output* "~&glass wm: drag -> wireframe (lag ~,1f ms, waiting ~,1f ms, queue ~,1f KB)~%"
                   glass:*send-lag* (float (pending-age-ms (wm-fb wm))) glass:*send-queue*)))
        (incf (wm-wire-drags wm))
        (setf (wm-drag-wire wm) t (wm-drag-wire-box wm) (window-box s))
        (drag-wire-to wm s ncx ncy))
      (drag-move-opaque wm s ncx ncy)))

(defun drag-wire-drop (wm s ncx ncy)
  "End a wireframe drag: land the window and repaint where it was, the outline, and where it is."
  (let ((old (window-box s)) (wire (wm-drag-wire-box wm)))
    (seat-move wm s ncx ncy)
    (setf (wm-drag-wire wm) nil (wm-drag-wire-box wm) nil)
    (wm-composite wm (box-union (list old wire (window-box s))))))

;;; ---- menus -----------------------------------------------------------------------------------

(defun menu-chain-box (wm)
  (when-let (m (wm-menu wm)) (box-union (mapcar #'wm-menu-box (wm-menu-chain m)))))

;;; SPEAKING.  The desktop's voice (glass/voice-apps' *VOICE*, by default the session's chord) says
;;; what is highlighted in a window, or what was last copied -- the McCLIM desktop's Speak selection
;;; and Speak clipboard.  Found by name: a desktop without glass/voice-apps offers neither.
(defun %voice-say-fn ()
  (let ((say (and (find-package "GLASS.DESK") (find-symbol "VOICE-SAY" "GLASS.DESK")))
        (voice (and (find-package "GLASS.DESK") (find-symbol "*VOICE*" "GLASS.DESK"))))
    (and say (fboundp say) voice (boundp voice)
         (lambda (text) (funcall say (symbol-value voice) text)))))

(defun %speakable (text)
  (and (stringp text) (string/= "" (string-trim '(#\Space #\Tab #\Newline #\Return) text)) text))

(defun clipboard-menu-items (wm)
  "Speak clipboard, when there is a voice and this seat copied something (a viewer's paste lands
in its seat's clipboard -- the home seat's is the session's -- rfb.lisp READ-CLIENT-CUT-TEXT)."
  (let ((say (%voice-say-fn))
        (text (%speakable (ignore-errors (glass:clipboard-text (seat-clipboard (cur-seat wm)))))))
    (when (and say text)
      (list (cons "Speak clipboard" (lambda () (funcall say text)))))))

(defun selection-menu-items (s)
  "Speak selection, when there is a voice and window S has something highlighted right now."
  (let ((say (%voice-say-fn))
        (text (and (surf-selection-fn s) (%speakable (ignore-errors (funcall (surf-selection-fn s)))))))
    (when (and say text)
      (list (cons "Speak selection" (lambda () (funcall say text)))))))

(defun wm-open-root-menu (wm x y mask)
  ;; The ORIGIN, as the window menu has: the opening press's own release, where it was pressed,
  ;; only arms the menu.  Without it a TAP -- a finger's press and lift in one place -- chose
  ;; whatever item the menu had opened under it; with it a tap opens the menu and a second tap
  ;; chooses, while a mouse's press-drag-release still chooses in one gesture.
  (setf (wm-menu wm)
        (wm-place-menu (make-wm-menu :items (append
                                             (mapcar (lambda (a) (cons (first a) (list :app (first a))))
                                                     (wm-apps wm))
                                             (mapcar (lambda (a) (cons (car a) (cdr a)))
                                                     (wm-actions wm))
                                             (clipboard-menu-items wm)))
                       (screen-w wm) (screen-h wm) x y mask)
        (wm-menu-origin (wm-menu wm)) (cons x y))
  (wm-composite wm (menu-chain-box wm)))

(defun window-menu-items (wm s)
  (when (rack-p wm)
    (return-from window-menu-items
      (append (selection-menu-items s)
              (rack-menu-items wm s)
              (list (cons "Close" (lambda () (wm-close wm s)))))))
  (append
   (selection-menu-items s)
   (when (surf-resize-fn s)
     (list (cons (if (surf-saved-geom s) "Restore Size" "Full Size") (lambda () (wm-fullsize wm s)))))
   (list (cons "Back" (lambda () (wm-lower wm s)))
         (cons "Refresh" (lambda () (wm-composite wm nil)))
         (cons "Quit" (lambda () (wm-close wm s))))))

(defun wm-open-window-menu (wm s cx cy mask px py)
  "The Window Menu just below S's title bar (content top-left CX,CY), opened by a press at PX,PY."
  (setf (wm-menu wm) (wm-place-menu (make-wm-menu :title "Window" :items (window-menu-items wm s))
                                    (screen-w wm) (screen-h wm) cx cy mask)
        (wm-menu-origin (wm-menu wm)) (cons px py))
  (wm-composite wm (menu-chain-box wm)))

(defun open-submenu (wm parent idx action)
  (let ((sub (make-wm-menu :title (car (nth idx (wm-menu-items parent))) :items (cdr action))))
    (setf (wm-menu-child parent)
          (wm-place-menu sub (screen-w wm) (screen-h wm)
                         (+ (wm-menu-x parent) (glass:fb-width (wm-menu-fb parent)) -2)
                         (+ (wm-menu-y parent) (menu-titleh) (* idx (menu-itemh)) -1)
                         (wm-menu-last-mask parent)))))

(defun menu-run (wm action)
  (cond ((functionp action) (funcall action))
        ((and (consp action) (eq (first action) :app)) (wm-open wm (second action)))))

(defun menu-pointer (wm root mask x y)
  "A pointer event while a menu tree is open (see backend/wm.lisp %WM-MENU-POINTER): hover
   highlights and opens submenus, RELEASING on a leaf runs it, a new press or a release off every
   menu dismisses.  Whichever of left and right opened it, both choose."
  (let* ((before (menu-chain-box wm))
         (was (wm-menu-last-mask root))
         (release (logtest (logandc2 was mask) 5))
         (pressed (logandc2 mask was))
         (held (logtest mask 5))
         (chain (wm-menu-chain root))
         (menu (find-if (lambda (m) (not (eq :outside (wm-menu-index m x y)))) (reverse chain)))
         (hovering (or (find-if (lambda (m) (/= -1 (wm-menu-hover m))) (reverse chain)) root))
         (run nil) (changed nil))
    (setf (wm-menu-last-mask root) mask)
    ;; the opening click's own release, where it was pressed: arm, and nothing else
    (when (and release (not (wm-menu-armed root)))
      (setf (wm-menu-armed root) t)
      (let ((o (wm-menu-origin root)) (slop (max 6 (round (* 6 *wm-scale*)))))
        (when (and o (<= (abs (- x (car o))) slop) (<= (abs (- y (cdr o))) slop))
          (return-from menu-pointer nil))))
    (flet ((rerender (m) (let ((*wm-scale* (wm-menu-scale m))) (wm-menu-render m)) (setf changed t)))
      (if (null menu)
          (cond ((or (logtest pressed 5) release) (setf (wm-menu wm) nil changed t))
                ((/= -1 (wm-menu-hover hovering)) (setf (wm-menu-hover hovering) -1) (rerender hovering)))
          (let ((idx (wm-menu-index menu x y)))
            (if (integerp idx)
                (let ((action (wm-item-action (nth idx (wm-menu-items menu)))))
                  (unless (eql idx (wm-menu-hover menu))
                    (setf (wm-menu-hover menu) idx (wm-menu-child menu) nil)
                    (when (wm-submenu-p action) (open-submenu wm menu idx action))
                    (rerender menu))
                  (cond ((wm-submenu-p action)
                         (when (and (or held release) (not (wm-menu-child menu)))
                           (open-submenu wm menu idx action) (setf changed t)))
                        (release (setf (wm-menu wm) nil changed t run (or action :none)))))
                (unless (eql (wm-menu-hover menu) -1)
                  (setf (wm-menu-hover menu) -1) (rerender menu))))))
    (when changed (wm-composite wm (box-union (list before (menu-chain-box wm)))))
    (when (and run (not (eq run :none))) (menu-run wm run))))

;;; ---- the pointer -----------------------------------------------------------------------------

(defun %to-app (s mask x y)
  (when (surf-on-pointer s)
    (funcall (surf-on-pointer s) mask (- x (sx s)) (- y (sy s)))))

(defun wm-pointer (wm mask x y &optional seat)
  "One pointer event of SEAT's (default: the current seat, i.e. the home seat), in its screen's
   pixels.  MASK bit 0 is the left button (a touch), bit 2 the right."
  (let* ((*seat* (or seat *seat*))
         (was (wm-last-mask wm))
         (pressed (logandc2 mask was))
         (down (logtest mask 1)))
    (setf (wm-last-mask wm) mask
          (seat-px (cur-seat wm)) x (seat-py (cur-seat wm)) y)
    (cond
      ;; an open menu has the pointer
      ((wm-menu wm) (menu-pointer wm (wm-menu wm) mask x y))
      ;; THE RACK's gestures (rack.lisp): a unit picked up, a press not yet decided, the bar dragged
      ((and (rack-p wm) (wm-rack-lift wm)) (rack-lift-pointer wm x y down))
      ((and (rack-p wm) (wm-rack-press wm)) (rack-press-pointer wm mask x y down))
      ((and (rack-p wm) (wm-rack-drag wm)) (rack-drag-to wm x y down))
      ((and (rack-p wm) (logtest pressed 1) (rack-on-bar-p wm x y)) (rack-press-bar wm x y))
      ;; a press anywhere in a unit's slot -- its window or the infill round it -- is the unit's;
      ;; only the screen past the last unit opens the root menu
      ((and (rack-p wm) (logtest pressed 1) (rack-unit-at wm x y))
       (rack-begin-press wm (rack-unit-at wm x y) mask x y))
      ;; an EMPTY SLOT: no root menu in the rack -- a long press there is its apps menu, a drag
      ;; scrolls, a tap does nothing (rack.lisp)
      ((and (rack-p wm) (logtest pressed 5)) (rack-begin-press wm nil mask x y))
      ;; a move or resize in flight
      ((wm-drag wm)
       (destructuring-bind (s mode &rest rest) (wm-drag wm)
         (ecase mode
           (:move
            (destructuring-bind (dx dy) rest
              (multiple-value-bind (ncx ncy) (clamp-position wm s (- x dx) (- y dy))
                (cond ((wm-drag-wire wm)
                       (if down (drag-wire-to wm s ncx ncy) (drag-wire-drop wm s ncx ncy)))
                      (down (drag-move wm s ncx ncy))
                      (t (drag-move-opaque wm s ncx ncy))))))
           (:resize
            (destructuring-bind (x0 y0 cw0 ch0) rest
              (wm-resize wm s (+ cw0 (- x x0)) (+ ch0 (- y y0))))))
         (unless down (setf (wm-drag wm) nil))))
      ;; a content press holds the pointer until every button is up, wherever it goes
      ((wm-grab wm)
       (let ((s (wm-grab wm)))
         (when (zerop (logand mask 7)) (setf (wm-grab wm) nil))
         (%to-app s mask x y)))
      (t
       (multiple-value-bind (s region cx cy cw ch) (wm-hit wm x y)
         (declare (ignore cw))
         ;; in the rack a window is neither moved nor resized by hand: its corner is content
         (when (and (rack-p wm) (eq region :resize)) (setq region :content))
         (cond
           ((null s)
            (when (logtest pressed 5)                ; a press on the workspace: the root menu
              (%focus wm nil)
              (wm-open-root-menu wm x y mask)))
           ((not (logtest pressed 7))                 ; no new press: motion goes to the app below
            (when (eq region :content) (%to-app s mask x y)))
           ((member region '(:winmenu :close :resize :title :frame))
            (when (logtest pressed 1)
              (case region
                (:close (wm-close wm s))
                (t
                 (%focus wm s :keyboard nil)
                 (wm-raise wm s)
                 (case region
                   (:winmenu (wm-open-window-menu wm s cx cy mask x y))
                   (:resize (setf (wm-drag wm) (list s :resize x y (surf-cw s) ch)))
                   (:title (setf (wm-drag wm) (list s :move (- x (sx s)) (- y (sy s))))))))))
           ((and (rack-p wm) (logtest pressed 1))      ; a unit: a tap, a drag or a long press
            (rack-begin-press wm s mask x y))
           (t                                         ; a press in a window's content
            (%focus wm s :force (surf-text-p s))
            (wm-raise wm s)
            (setf (wm-grab wm) s)
            (%to-app s mask x y))))))
    nil))

(defun wm-key (wm down keysym &optional seat)
  "A key of SEAT's (default: the current seat, i.e. the home seat) goes to that seat's focus."
  (let* ((*seat* (or seat *seat*))
         (s (wm-focus wm)))
    (when (and s (surf-on-key s)) (funcall (surf-on-key s) down keysym))))
