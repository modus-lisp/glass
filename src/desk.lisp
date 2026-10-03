;;;; desk.lisp — a minimal desktop: windows you drag, a root menu, and nothing that needs CLIM.
;;;;
;;;; mcclim-glass's window manager is the full desktop, and it is built on a McCLIM port.  This is
;;;; the part of a desktop that a screen with no McCLIM still wants -- a phone running modus, today
;;;; -- and it is deliberately small: windows with a title bar to drag them by and a box to close
;;;; them, raise on touch, and a menu of applications on the background.
;;;;
;;;; AN APPLICATION IS THE SAME CONTRACT THE WM'S SURFACE APPS USE, so one written for the full
;;;; desktop runs here unchanged: a MAKE-FN called with the window's content framebuffer, returning
;;;; (values ON-KEY ON-POINTER DIRTY-P [COPY-P CLOSE-FN]).  ON-KEY (down keysym) and ON-POINTER
;;;; (mask x y) take window-local coordinates; DIRTY-P is polled every tick and answers whether
;;;; the content changed; CLOSE-FN, if given, runs when the window closes.
;;;;
;;;; DRIVING IT: the host owns the screen and the clock.  It makes a DESK over a framebuffer,
;;;; registers applications, forwards pointer and key events (DESK-POINTER, DESK-KEY), and calls
;;;; DESK-TICK, which answers T when the screen framebuffer changed and should be shown.  Pointer
;;;; MASK bit 0 is the (one) button: a touch down is 1, the lift is 0, a drag is moves with 1.

(defpackage #:glass.desk
  (:use #:cl)
  (:export #:make-desk #:desk-register-app #:desk-open #:desk-pointer #:desk-key #:desk-tick
           #:desk-fb #:desk-windows #:desk-apps #:desk-redraw))

(in-package #:glass.desk)

(defparameter *title-h* 24 "Title bar height.")
(defparameter *border* 1)
(defparameter *menu-item-h* 30)
(defparameter *menu-w* 180)
(defparameter *bg* #x1e2530)
(defparameter *title-bg* #x3a4556)
(defparameter *title-bg-top* #x4f6a8f)
(defparameter *title-fg* #xe6ebf2)
(defparameter *frame* #x0d1117)
(defparameter *menu-bg* #x2a3240)
(defparameter *menu-hi* #x4f6a8f)
(defparameter *menu-fg* #xe6ebf2)

(defstruct (window (:conc-name win-))
  title x y w h            ; content size; the frame adds the title bar and border
  fb on-key on-pointer dirty-p close-fn
  (pressed nil))           ; the content holds the pointer (down landed in it)

(defstruct (desk (:constructor %make-desk))
  fb
  (windows '())            ; topmost first
  (apps '())               ; ((label make-fn w h) ...), in menu order
  (menu nil)               ; NIL, or (x y) of the open root menu
  (drag nil)               ; (window dx dy) while a title bar is held
  (cascade 0)
  (damage '())             ; ((x y w h) ...) to repaint at the next tick
  (dirty t))               ; T = repaint everything

(defun make-desk (fb)
  "A desktop drawing into FB (the screen, in the desk's own pixels)."
  (%make-desk :fb fb))

(defun desk-register-app (desk label make-fn &key (width 400) (height 300))
  "Put LABEL on the root menu; choosing it opens a WIDTH x HEIGHT window running MAKE-FN."
  (setf (desk-apps desk)
        (append (remove label (desk-apps desk) :key #'first :test #'string=)
                (list (list label make-fn width height))))
  (when (desk-menu desk) (setf (desk-dirty desk) t))
  label)

(defun desk-redraw (desk) (setf (desk-dirty desk) t))

(defun %damage (desk x y w h)
  (when (and (plusp w) (plusp h)) (push (list x y w h) (desk-damage desk))))

(defun %damage-frame (desk win)
  (multiple-value-bind (fx fy fw fh) (%frame-box win) (%damage desk fx fy fw fh)))

;;; ---- windows -------------------------------------------------------------------------------------

(defun %frame-box (w)
  "(values x y w h) of window W's whole frame, title bar and border included."
  (values (- (win-x w) *border*) (- (win-y w) *title-h* *border*)
          (+ (win-w w) (* 2 *border*)) (+ (win-h w) *title-h* (* 2 *border*))))

(defun desk-open (desk label)
  "Open the application LABEL in a new window, on top.  Returns the window."
  (let ((app (find label (desk-apps desk) :key #'first :test #'string=)))
    (when app
      (destructuring-bind (lbl make-fn w h) app
        (let* ((sw (glass:fb-width (desk-fb desk))) (sh (glass:fb-height (desk-fb desk)))
               (w (min w (- sw (* 2 *border*))))
               (h (min h (- sh *title-h* (* 2 *border*))))
               (fb (glass:make-framebuffer w h))
               (n (desk-cascade desk))
               (x (+ *border* (mod (* n 24) (max 1 (- sw w (* 2 *border*))))))
               (y (+ *title-h* *border* (mod (* n 24) (max 1 (- sh h *title-h* (* 2 *border*)))))))
          (multiple-value-bind (on-key on-pointer dirty-p copy-p close-fn) (funcall make-fn fb)
            (declare (ignore copy-p))
            (incf (desk-cascade desk))
            (let ((win (make-window :title lbl :x x :y y :w w :h h :fb fb :on-key on-key
                                    :on-pointer on-pointer :dirty-p dirty-p :close-fn close-fn)))
              (let ((old-top (first (desk-windows desk))))
                (push win (desk-windows desk))
                (when old-top (%damage-frame desk old-top)))   ; its title bar dims
              (%damage-frame desk win)
              win)))))))

(defun %close (desk win)
  (%damage-frame desk win)
  (setf (desk-windows desk) (remove win (desk-windows desk)))
  (when (first (desk-windows desk)) (%damage-frame desk (first (desk-windows desk))))  ; new top
  (when (win-close-fn win) (ignore-errors (funcall (win-close-fn win)))))

(defun %raise (desk win)
  (let ((old-top (first (desk-windows desk))))
    (unless (eq win old-top)
      (setf (desk-windows desk) (cons win (remove win (desk-windows desk))))
      (%damage-frame desk win)
      (when old-top (%damage-frame desk old-top)))))   ; its title bar dims

(defun %hit (desk x y)
  "(values WINDOW PART) for the topmost window under (X,Y); PART is :close, :title or :content."
  (dolist (w (desk-windows desk) (values nil nil))
    (multiple-value-bind (fx fy fw fh) (%frame-box w)
      (when (and (<= fx x) (< x (+ fx fw)) (<= fy y) (< y (+ fy fh)))
        (return
          (values w (cond ((>= y (win-y w)) :content)
                          ((>= x (- (+ fx fw) *title-h*)) :close)
                          (t :title))))))))

;;; ---- the root menu -------------------------------------------------------------------------------

(defun %menu-items (desk) (mapcar #'first (desk-apps desk)))

(defun %menu-box (desk)
  "(values x y w h) of the open menu, kept on screen."
  (destructuring-bind (mx my) (desk-menu desk)
    (let* ((h (* *menu-item-h* (max 1 (length (%menu-items desk)))))
           (fb (desk-fb desk))
           (x (max 0 (min mx (- (glass:fb-width fb) *menu-w*))))
           (y (max 0 (min my (- (glass:fb-height fb) h)))))
      (values x y *menu-w* h))))

(defun %menu-item-at (desk x y)
  (multiple-value-bind (mx my mw mh) (%menu-box desk)
    (when (and (<= mx x) (< x (+ mx mw)) (<= my y) (< y (+ my mh)))
      (nth (floor (- y my) *menu-item-h*) (%menu-items desk)))))

;;; ---- input ---------------------------------------------------------------------------------------

(defun desk-pointer (desk mask x y)
  "A pointer event in desk pixels.  MASK bit 0 is the button."
  (let ((down (logbitp 0 mask)))
    (cond
      ;; a title bar held: follow the pointer, let go on lift
      ((desk-drag desk)
       (destructuring-bind (win dx dy) (desk-drag desk)
         ;; keep 40 pixels of title bar on screen: a window dragged out of reach could never
         ;; be dragged back, and on a touch screen there is nothing else to grab it by
         (let ((sw (glass:fb-width (desk-fb desk))) (sh (glass:fb-height (desk-fb desk))))
           ;; where it was and where it is now: everything a move can change
           (%damage-frame desk win)
           (setf (win-x win) (max (- 40 (win-w win)) (min (- sw 40) (- x dx)))
                 (win-y win) (max (+ *title-h* *border*) (min (+ sh -4) (- y dy))))
           (%damage-frame desk win))
         (unless down (setf (desk-drag desk) nil))))
      ;; a window's content holds the pointer until the lift, wherever it goes
      ((find-if #'win-pressed (desk-windows desk))
       (let ((win (find-if #'win-pressed (desk-windows desk))))
         (unless down (setf (win-pressed win) nil))
         (when (win-on-pointer win)
           (funcall (win-on-pointer win) mask (- x (win-x win)) (- y (win-y win))))))
      ;; the menu is open: a press on an item opens it, anywhere else closes the menu
      ((desk-menu desk)
       (when down
         (let ((label (%menu-item-at desk x y)))
           (multiple-value-bind (mx my mw mh) (%menu-box desk)
             (%damage desk (1- mx) (1- my) (+ mw 2) (+ mh 2)))
           (setf (desk-menu desk) nil)
           (when label (desk-open desk label)))))
      (down
       (multiple-value-bind (win part) (%hit desk x y)
         (cond
           ((null win)
            (setf (desk-menu desk) (list x y))
            (multiple-value-bind (mx my mw mh) (%menu-box desk)
              (%damage desk (1- mx) (1- my) (+ mw 2) (+ mh 2))))
           ((eq part :close) (%close desk win))
           ((eq part :title)
            (%raise desk win)
            (setf (desk-drag desk) (list win (- x (win-x win)) (- y (win-y win)))))
           (t
            (%raise desk win)
            (setf (win-pressed win) t)
            (when (win-on-pointer win)
              (funcall (win-on-pointer win) mask (- x (win-x win)) (- y (win-y win))))))))
      ;; a move with nothing held goes to whatever window it is over
      (t
       (multiple-value-bind (win part) (%hit desk x y)
         (when (and win (eq part :content) (win-on-pointer win))
           (funcall (win-on-pointer win) mask (- x (win-x win)) (- y (win-y win)))))))
    nil))

(defun desk-key (desk down keysym)
  "A key goes to the topmost window."
  (let ((win (first (desk-windows desk))))
    (when (and win (win-on-key win)) (funcall (win-on-key win) down keysym))))

;;; ---- drawing -------------------------------------------------------------------------------------

(defun %blit (dst src dx dy)
  "SRC into DST at (DX,DY), clipped, a row at a time."
  (let* ((sw (glass:fb-width src)) (sh (glass:fb-height src))
         (clip (glass:fb-clip dst))
         (tw (glass:fb-width dst)) (th (glass:fb-height dst))
         (cy0 (if clip (max 0 (second clip)) 0)) (cy1 (if clip (min th (fourth clip)) th))
         (sp (glass:fb-pixels src)) (dp (glass:fb-pixels dst))
         (x0 (max 0 dx (if clip (first clip) 0))) (x1 (min tw (+ dx sw) (if clip (third clip) tw))))
    (when (< x0 x1)
      (dotimes (sy sh)
        (let ((ty (+ dy sy)))
          (when (and (>= ty cy0) (< ty cy1))
            (replace dp sp :start1 (+ (* ty tw) x0) :end1 (+ (* ty tw) x1)
                           :start2 (+ (* sy sw) (- x0 dx)))))))))

(defun %draw-window (fb win top)
  (multiple-value-bind (fx fy fw fh) (%frame-box win)
    (glass:fb-rect fb fx fy fw fh *frame*)
    (glass:fb-rect fb (win-x win) (- (win-y win) *title-h*) (win-w win) *title-h*
                   (if top *title-bg-top* *title-bg*))
    (glass:fb-text fb (+ (win-x win) 8) (+ (- (win-y win) *title-h*) 4) (win-title win)
                   :size 13 :color *title-fg*)
    ;; the close box: an X in the title bar's right-hand square
    (let ((cx (- (+ fx fw) *title-h*)) (cy fy))
      (glass:fb-text fb (+ cx 7) (+ cy 4) "x" :size 13 :color *title-fg*))
    (%blit fb (win-fb win) (win-x win) (win-y win))))

(defun %draw-menu (desk)
  (let ((fb (desk-fb desk)))
    (multiple-value-bind (mx my mw mh) (%menu-box desk)
      (glass:fb-rect fb (1- mx) (1- my) (+ mw 2) (+ mh 2) *frame*)
      (glass:fb-rect fb mx my mw mh *menu-bg*)
      (let ((items (%menu-items desk)))
        (if items
            (loop for label in items for i from 0
                  do (glass:fb-text fb (+ mx 12) (+ my (* i *menu-item-h*) 8) label
                                    :size 13 :color *menu-fg*))
            (glass:fb-text fb (+ mx 12) (+ my 8) "(no applications)" :size 13 :color *menu-fg*))))))

(defun %overlaps-p (ax ay aw ah bx by bw bh)
  (and (< ax (+ bx bw)) (< bx (+ ax aw)) (< ay (+ by bh)) (< by (+ ay ah))))

(defun %covered-p (desk win)
  "Does anything drawn above WIN -- a window over it, or the open menu -- overlap its content?"
  (let ((x (win-x win)) (y (win-y win)) (w (win-w win)) (h (win-h win)))
    (or (loop for above in (desk-windows desk)
              until (eq above win)
              thereis (multiple-value-bind (fx fy fw fh) (%frame-box above)
                        (%overlaps-p x y w h fx fy fw fh)))
        (and (desk-menu desk)
             (multiple-value-bind (mx my mw mh) (%menu-box desk)
               (%overlaps-p x y w h (1- mx) (1- my) (+ mw 2) (+ mh 2)))))))

(defun %redraw-all (desk)
  (let ((fb (desk-fb desk)))
    (glass:fb-fill fb *bg*)
    (let ((top (first (desk-windows desk))))
      (dolist (w (reverse (desk-windows desk)))
        (%draw-window fb w (eq w top))))
    (when (desk-menu desk) (%draw-menu desk))
    (glass:fb-touch fb)))

(defun %redraw-region (desk x y w h)
  "Repaint (X,Y,W,H) of the screen: background, the windows that cross it bottom to top, the
   menu -- every stroke clipped to it."
  (let ((fb (desk-fb desk)) (top (first (desk-windows desk))))
    (glass:with-fb-clip (fb x y w h)
      (glass:fb-rect fb x y w h *bg*)
      (dolist (win (reverse (desk-windows desk)))
        (multiple-value-bind (fx fy fw fh) (%frame-box win)
          (when (%overlaps-p x y w h fx fy fw fh)
            (%draw-window fb win (eq win top)))))
      (when (desk-menu desk)
        (multiple-value-bind (mx my mw mh) (%menu-box desk)
          (when (%overlaps-p x y w h (1- mx) (1- my) (+ mw 2) (+ mh 2))
            (%draw-menu desk)))))))

(defun desk-tick (desk)
  "Poll the windows and bring the screen up to date.  Returns NIL when nothing changed, else
   (values T X Y W H): the rectangle of FB that changed, so a host need only show that.

   ONLY WHAT CHANGED.  A window whose content changed and that nothing covers is copied on its
   own.  Everything else is DAMAGE -- a dragged window's old and new frames, a raised window, the
   menu's box, content under another window -- and only the rectangle around it is repainted,
   clipped.  Dragging a small window repainted the whole desk and magnified the whole screen,
   and stuttered; now it costs about the window.  A host's DESK-REDRAW still repaints all."
  (let ((fb (desk-fb desk)) (copied '()))
    (dolist (w (desk-windows desk))
      (when (and (win-dirty-p w) (funcall (win-dirty-p w)))
        (if (%covered-p desk w)
            (%damage desk (win-x w) (win-y w) (win-w w) (win-h w))
            (progn (%blit fb (win-fb w) (win-x w) (win-y w)) (push w copied)))))
    (let ((x0 most-positive-fixnum) (y0 most-positive-fixnum) (x1 most-negative-fixnum)
          (y1 most-negative-fixnum) (any nil))
      (flet ((grow (x y w h)
               (setf any t x0 (min x0 x) y0 (min y0 y) x1 (max x1 (+ x w)) y1 (max y1 (+ y h)))))
        (cond
          ((desk-dirty desk)
           (setf (desk-dirty desk) nil (desk-damage desk) '())
           (%redraw-all desk)
           (grow 0 0 (glass:fb-width fb) (glass:fb-height fb)))
          (t
           (when (desk-damage desk)
             (dolist (r (desk-damage desk)) (apply #'grow r))
             (setf (desk-damage desk) '())
             (%redraw-region desk x0 y0 (- x1 x0) (- y1 y0)))
           (dolist (w copied) (grow (win-x w) (win-y w) (win-w w) (win-h w))))))
      (when any
        (glass:fb-touch fb)
        (let ((x0 (max 0 x0)) (y0 (max 0 y0))
              (x1 (min (glass:fb-width fb) x1)) (y1 (min (glass:fb-height fb) y1)))
          (when (and (< x0 x1) (< y0 y1))
            (values t x0 y0 (- x1 x0) (- y1 y0))))))))
