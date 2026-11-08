;;;; src/wm/wm.lisp — the window manager: windows, z-order, focus, compositing, the pointer.
;;;;
;;;; MOVED from backend/wm.lisp (and backend/backend.lisp's COMPOSITE-SEAT): the compositing
;;;; (damage-clipped, background skipped under an opaque window), the scroll CopyRect machinery,
;;;; the hit test, the adaptive drag (opaque with a CopyRect hint, WIREFRAME once the link lags),
;;;; resize, raise/lower/full-size/close, the root and window menus and their pointer handling,
;;;; and the tick.  REWRITTEN around them: the state.  The McCLIM desktop keeps windows on a
;;;; GLASS-PORT (a CLIM port) and arrangement on SEATs; here there is one seat, so it is one
;;;; struct, WM, and every window is a surface -- no mirrors, no pop-up tier, no McCLIM token.
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
;;;; the windows, their framebuffers and their apps are that thread's own.  The only things
;;;; another thread sees are the screen framebuffer's pixels and FB-MARK-FRAME's damage + copy.

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

(defstruct (surf (:constructor %make-surf))
  fb (x 60) (y 60) (title "window")
  (deco-fb nil) (deco-w -1)             ; cached title bar, and the width it was drawn for
  on-key on-pointer dirty-p copy-p close-fn resize-fn
  (text-p nil)
  (copy-base nil)                       ; content box as of the last composite that drew it whole
  (saved-geom nil)                      ; (x y w h) saved by Full Size
  (err-count 0))

(defmethod print-object ((s surf) stream)
  (print-unreadable-object (s stream :type t :identity t)
    (format stream "~s ~d,~d" (surf-title s) (surf-x s) (surf-y s))))

(defstruct (wm (:constructor %make-wm))
  fb
  (surfaces '())        ; topmost first: THE stacking order, read by drawing, hit test, occlusion
  (apps '())            ; ((label make-fn w h) ...), in menu order
  (focus nil)           ; the surface keys go to
  (grab nil)            ; the surface a content press landed in: it has the pointer until the lift
  (drag nil)            ; (surf :move dx dy) | (surf :resize x0 y0 cw0 ch0)
  (drag-wire nil)       ; this drag went wireframe (the link lagged)
  (drag-wire-box nil)   ; the outline's (x y w h)
  (menu nil)            ; the open menu tree's root, or NIL
  (last-mask 0)         ; the buttons as of the previous pointer event, for press edges
  (cascade 0)
  (pending :full)       ; damage for the next tick: NIL, :FULL, or (x y w h); first, everything
  (frame-copy nil)      ; a scroll CopyRect found while painting this composite
  (keyboard-fn nil)     ; (lambda (on)): show or hide the host's on-screen keyboard; NIL = none
  (keyboard-up nil)     ; what KEYBOARD-FN was last told
  (wire-drags 0) (opaque-moves 0))   ; counts, for a test or a curious host

(defun make-wm (fb)
  "A window manager drawing into FB, the screen."
  (%make-wm :fb fb))

(defun wm-register-app (wm label make-fn &key (width 400) (height 300))
  "Put LABEL on the root menu; choosing it opens a WIDTH x HEIGHT window running MAKE-FN."
  (setf (wm-apps wm)
        (append (remove label (wm-apps wm) :key #'first :test #'string=)
                (list (list label make-fn width height))))
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

;;; ---- geometry --------------------------------------------------------------------------------

(defun surf-cw (s) (glass:fb-width (surf-fb s)))
(defun surf-ch (s) (glass:fb-height (surf-fb s)))

(defun window-box-at (s cx cy)
  "The decorated (x y w h) S would occupy with its content at (CX,CY)."
  (list (- cx (wm-border)) (- cy (wm-titleh) (wm-border))
        (+ (surf-cw s) (* 2 (wm-border))) (+ (wm-titleh) (surf-ch s) (* 2 (wm-border)))))

(defun window-box (s) (window-box-at s (surf-x s) (surf-y s)))

(defun surf-deco (s)
  (let ((cw (surf-cw s)))
    (when (or (null (surf-deco-fb s)) (/= cw (surf-deco-w s)))
      (setf (surf-deco-fb s) (wm-render-titlebar (surf-title s) cw) (surf-deco-w s) cw))
    (surf-deco-fb s)))

(defun wm-topmost (wm) (first (wm-surfaces wm)))

(defun boxes-above (wm s)
  "Every box drawn after S: windows above it, the drag wireframe, the open menu chain."
  (let ((boxes '()))
    (loop for w in (wm-surfaces wm) until (eq w s) do (push (window-box w) boxes))
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

;;; ---- compositing -----------------------------------------------------------------------------
;;;
;;; WM-COMPOSITE is COMPOSITE-SEAT: redraw a damaged box (or the screen) under the screen's pixel
;;; lock and tell the senders where, with a CopyRect hint when the composite was a window move or
;;; a window's scroll.  The lock is held across the mark, so a sender taking the frame sees the
;;; copy and the pixels it describes together.

(defun surf-screen-copy (wm s fb)
  "Take S's pending content translation and judge it as a screen CopyRect for this composite.
   (values WIRE SCREEN ALLOWED): the hint for the RFB update, the hint the compositor may also
   apply to the screen itself (refused more often: nothing repairs a wrong in-RAM translation),
   and S's visible content in this composite.  See backend/wm.lisp WM-SURFACE-SCREEN-COPY."
  (let* ((take (surf-copy-p s))
         (hint (and take (funcall take)))
         (content (list (surf-x s) (surf-y s) (surf-cw s) (surf-ch s)))
         (clipb (or (clip-box fb) (screen-box wm)))
         (base (surf-copy-base s)))
    (setf (surf-copy-base s) (and (equal content (box-intersect content clipb)) content))
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
    (setf (wm-pending wm) :full)))

(defun draw-surface (wm s fb)
  (let ((cx (surf-x s)) (cy (surf-y s)))
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
                             (wm-surfaces wm))))
    (multiple-value-bind (wire screen allowed)
        (handler-case (surf-screen-copy wm s fb) (error () (values nil nil nil)))
      (when wire (setf (wm-frame-copy wm) wire))
      (when screen
        (handler-case
            (destructuring-bind (sx sy dx dy w h) screen
              (glass:fb-move-rect fb sx sy dx dy w h)
              (dolist (band (box-difference allowed (list dx dy w h)) t)
                (destructuring-bind (bx by bw bh) band
                  (glass:with-fb-clip (fb bx by bw bh) (blit-fb (surf-fb s) (surf-x s) (surf-y s) fb)))))
          (error (e) (note-surface-error wm s e) nil))))))

(defun composite-whole (wm fb box)
  "Rebuild BOX (NIL = the screen) bottom up: workspace, windows, wireframe, menus."
  (unless (covered-p wm box)
    (glass:fb-fill fb *wm-bg*))
  (dolist (s (reverse (wm-surfaces wm)))
    (handler-case (draw-surface wm s fb)
      (error (e) (note-surface-error wm s e))))
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
  "Redraw the screen -- only DAMAGE (x y w h) when given -- and mark the frame for the senders,
   with COPY (sx sy dx dy w h) when the composite moved a window.  A scroll hint found while
   painting rides when there is no explicit COPY."
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
  "Add BOX to what the next tick repaints."
  (let ((p (wm-pending wm)))
    (setf (wm-pending wm) (cond ((eq p :full) :full) ((null box) :full) (t (box-union (list p box)))))))

(defun wm-tick (wm)
  "Poll every window and repaint what changed.  Returns T if it painted."
  (dolist (s (wm-surfaces wm))
    (handler-case
        (let ((dp (surf-dirty-p s)))
          (when (or (null dp) (funcall dp)) (damage wm (window-box s)))
          (setf (surf-err-count s) 0))
      (error (e) (note-surface-error wm s e))))
  (let ((p (wm-pending wm)))
    (when p
      (setf (wm-pending wm) nil)
      (wm-composite wm (unless (eq p :full) p))
      t)))

;;; ---- focus and the host keyboard --------------------------------------------------------------

(defun %focus (wm s &key (keyboard t) force)
  "Keys now go to S (or nowhere).  With KEYBOARD the host's on-screen keyboard follows: up while
   the focused window takes text, down otherwise; FORCE says it again even if unchanged (a tap
   into a text window after the keyboard was swiped away by hand should bring it back).  Without
   KEYBOARD (a title-bar press) the hint is not raised: picking a window up to move it is not
   asking to type, and taking it from another window puts the keyboard away."
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
  "Open the application LABEL in a new window on top, with the keys.  Returns the window."
  (when-let (app (find label (wm-apps wm) :key #'first :test #'string=))
    (destructuring-bind (lbl make-fn w h) app
      (let* ((sw (screen-w wm)) (sh (screen-h wm))
             (w (max 32 (min w (- sw (* 2 (wm-border))))))
             (h (max 32 (min h (- sh (wm-titleh) (* 2 (wm-border))))))
             (fb (glass:make-framebuffer w h glass:+white+))
             (c (wm-cascade wm))
             (x (max (wm-border) (min (+ 40 c) (- sw w (wm-border)))))
             (y (max (+ (wm-titleh) (wm-border)) (min (+ 40 c (wm-titleh)) (- sh h (wm-border))))))
        (multiple-value-bind (on-key on-pointer dirty-p copy-p close-fn resize-fn text-p)
            (funcall make-fn fb)
          (setf (wm-cascade wm) (mod (+ c 28) 200))
          (let ((s (%make-surf :fb fb :x x :y y :title lbl :on-key on-key :on-pointer on-pointer
                               :dirty-p dirty-p :copy-p copy-p :close-fn close-fn
                               :resize-fn resize-fn :text-p (and text-p t))))
            (push s (wm-surfaces wm))
            (%focus wm s)
            (damage wm (window-box s))
            s))))))

(defun %forget (wm s)
  (setf (wm-surfaces wm) (remove s (wm-surfaces wm)))
  (when (eq (wm-grab wm) s) (setf (wm-grab wm) nil))
  (when (and (wm-drag wm) (eq (first (wm-drag wm)) s))
    (setf (wm-drag wm) nil (wm-drag-wire wm) nil (wm-drag-wire-box wm) nil))
  (when (eq (wm-focus wm) s) (%focus wm nil))
  (when (surf-close-fn s) (ignore-errors (funcall (surf-close-fn s)))))

(defun wm-close (wm s)
  (let ((box (window-box s)))
    (%forget wm s)
    (wm-composite wm box)))

(defun wm-raise (wm s)
  (unless (eq s (wm-topmost wm))
    (setf (wm-surfaces wm) (cons s (remove s (wm-surfaces wm))))
    (wm-composite wm (window-box s))))

(defun wm-lower (wm s)
  "Back: behind every other window."
  (setf (wm-surfaces wm) (append (remove s (wm-surfaces wm)) (list s)))
  (wm-composite wm (window-box s)))

(defun wm-resize (wm s w h)
  "Resize S's content to W x H (clamped), through its RESIZE-FN, and repaint old and new."
  (when (surf-resize-fn s)
    (let ((w (max 32 (min w (- (screen-w wm) (* 2 (wm-border))))))
          (h (max 32 (min h (- (screen-h wm) (wm-titleh) (* 2 (wm-border)))))))
      (unless (and (= w (surf-cw s)) (= h (surf-ch s)))
        (let ((old (window-box s)))
          (handler-case (funcall (surf-resize-fn s) w h)
            (error (e) (note-surface-error wm s e)))
          (setf (surf-copy-base s) nil)
          (wm-composite wm (box-union (list old (window-box s)))))))))

(defun wm-fullsize (wm s)
  "Full Size / Restore Size for a resizable window."
  (when (surf-resize-fn s)
    (let ((old (window-box s)))
      (if (surf-saved-geom s)
          (destructuring-bind (x y w h) (surf-saved-geom s)
            (setf (surf-x s) x (surf-y s) y (surf-saved-geom s) nil)
            (ignore-errors (funcall (surf-resize-fn s) w h)))
          (progn
            (setf (surf-saved-geom s) (list (surf-x s) (surf-y s) (surf-cw s) (surf-ch s))
                  (surf-x s) (wm-border) (surf-y s) (+ (wm-titleh) (wm-border)))
            (ignore-errors (funcall (surf-resize-fn s) (- (screen-w wm) (* 2 (wm-border)))
                                    (- (screen-h wm) (wm-titleh) (* 2 (wm-border)))))))
      (setf (surf-copy-base s) nil)
      (wm-composite wm (box-union (list old (window-box s)))))))

;;; ---- hit test --------------------------------------------------------------------------------

(defun wm-hit (wm x y)
  "The topmost window at (X,Y): (values S REGION CX CY CW CH), REGION one of :WINMENU (the
   wedge), :CLOSE (the close box), :RESIZE (the bottom-right corner of a resizable window),
   :TITLE or :CONTENT; NIL over the workspace."
  (dolist (s (wm-surfaces wm) nil)
    (let* ((cx (surf-x s)) (cy (surf-y s)) (cw (surf-cw s)) (ch (surf-ch s))
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
;;; wireframe, it stays wireframe for the rest of that drag (no flapping).

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
   of the change waiting now -- see *WIREFRAME-LAG-MS*."
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
    (setf (surf-x s) ncx (surf-y s) ncy)
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
    (setf (surf-x s) ncx (surf-y s) ncy
          (wm-drag-wire wm) nil (wm-drag-wire-box wm) nil)
    (wm-composite wm (box-union (list old wire (window-box s))))))

;;; ---- menus -----------------------------------------------------------------------------------

(defun menu-chain-box (wm)
  (when-let (m (wm-menu wm)) (box-union (mapcar #'wm-menu-box (wm-menu-chain m)))))

(defun wm-open-root-menu (wm x y mask)
  (setf (wm-menu wm)
        (wm-place-menu (make-wm-menu :items (mapcar (lambda (a) (cons (first a) (list :app (first a))))
                                                    (wm-apps wm)))
                       (screen-w wm) (screen-h wm) x y mask))
  (wm-composite wm (menu-chain-box wm)))

(defun window-menu-items (wm s)
  (append
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
    (funcall (surf-on-pointer s) mask (- x (surf-x s)) (- y (surf-y s)))))

(defun wm-pointer (wm mask x y)
  "One pointer event, in screen pixels.  MASK bit 0 is the left button (a touch), bit 2 the right."
  (let* ((was (wm-last-mask wm))
         (pressed (logandc2 mask was))
         (down (logtest mask 1)))
    (setf (wm-last-mask wm) mask)
    (cond
      ;; an open menu has the pointer
      ((wm-menu wm) (menu-pointer wm (wm-menu wm) mask x y))
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
                   (:title (setf (wm-drag wm) (list s :move (- x (surf-x s)) (- y (surf-y s))))))))))
           (t                                         ; a press in a window's content
            (%focus wm s :force (surf-text-p s))
            (wm-raise wm s)
            (setf (wm-grab wm) s)
            (%to-app s mask x y))))))
    nil))

(defun wm-key (wm down keysym)
  "A key goes to the focused window."
  (let ((s (wm-focus wm)))
    (when (and s (surf-on-key s)) (funcall (surf-on-key s) down keysym))))
