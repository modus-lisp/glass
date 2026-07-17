;;;; wm.lisp — a tiny OPEN LOOK window manager for the glass McCLIM backend.
;;;;
;;;; When the port runs in WM mode, the framebuffer is a fixed-size "screen" with
;;;; the classic Sun teal workspace, and every managed application frame is a
;;;; window on it: a thin black border, a light-grey title bar with the OPEN LOOK
;;;; abbreviated-menu button at the left and a centred title, and L-shaped resize
;;;; corner marks.  The compositor draws the decorations (the title bars are
;;;; rendered with mcclim-render, so we get real fonts); the pointer router
;;;; hit-tests title bar vs content, drags windows by their title bar, raises +
;;;; focuses on click, and forwards content events to the right frame with
;;;; screen->content translated coordinates.  Look: olwm, SunOS 4.x.

(in-package #:clim-glass)

(defparameter +wm-teal+ (glass:rgb 61 122 138) "The Sun workspace background.")

;;; ---- decoration rendering (via mcclim-render, cached per window) ------------

(defun wm-sheet-title (sheet)
  (or (ignore-errors (let ((n (clime:sheet-pretty-name sheet))) (and n (string n))))
      (ignore-errors (string (clim:frame-pretty-name (clim:pane-frame sheet))))
      "window"))

(defun wm-render-titlebar (title width)
  "A glass framebuffer of an OPEN LOOK title bar WIDTH px wide — drawn entirely
   with glass primitives + scribe text (no McCLIM)."
  (let ((tb (glass:make-framebuffer (max 1 width) +wm-titleh+ (glass:rgb 204 204 204))))
    (glass:fb-hline tb 0 (1- +wm-titleh+) width (glass:rgb 120 120 120))       ; bottom shadow line
    ;; menu button box: raised bevel + abbreviated-menu wedge
    (let* ((bs (- +wm-titleh+ 8)) (bx 4) (by 4))
      (glass:fb-rect tb bx by bs bs (glass:rgb 188 188 188))
      (glass:fb-hline tb bx by bs glass:+white+)                                ; top light
      (glass:fb-vline tb bx by bs glass:+white+)                               ; left light
      (glass:fb-hline tb bx (+ by bs -1) bs (glass:rgb 77 77 77))              ; bottom dark
      (glass:fb-vline tb (+ bx bs -1) by bs (glass:rgb 77 77 77))             ; right dark
      (let ((cx (+ bx (floor bs 2))) (cy (+ by (floor bs 2) -2)))              ; downward wedge
        (dotimes (i 4) (glass:fb-hline tb (- cx (- 3 i)) (+ cy i) (max 1 (- 7 (* 2 i))) glass:+black+))))
    ;; centred bold title, anti-aliased via scribe
    (let ((tw (glass:text-width title :size 12 :font (glass:default-font t))))
      (glass:fb-text tb (max (+ +wm-titleh+ 6) (floor (- width tw) 2)) 3 title
                     :size 12 :color glass:+black+ :font (glass:default-font t)))
    tb))

(defun wm-deco (mirror cw)
  "Cached title-bar framebuffer for MIRROR at content width CW."
  (when (or (null (glass-mirror-deco mirror)) (/= cw (glass-mirror-deco-w mirror)))
    (setf (glass-mirror-deco mirror) (wm-render-titlebar (glass-mirror-title mirror) cw)
          (glass-mirror-deco-w mirror) cw))
  (glass-mirror-deco mirror))

;;; ---- compositing ------------------------------------------------------------

(defun blit-fb (src ox oy dst)
  "Copy glass framebuffer SRC into DST with its top-left at (OX,OY)."
  (let* ((sw (glass:fb-width src)) (sh (glass:fb-height src))
         (spx (glass:fb-pixels src)) (dpx (glass:fb-pixels dst))
         (dw (glass:fb-width dst)) (dh (glass:fb-height dst)))
    (dotimes (sy sh)
      (let ((dy (+ oy sy)))
        (when (< -1 dy dh)
          (let ((drow (* dy dw)) (srow (* sy sw)))
            (dotimes (sx sw)
              (let ((dx (+ ox sx)))
                (when (< -1 dx dw)
                  (setf (aref dpx (+ drow dx)) (aref spx (+ srow sx))))))))))))

(defun wm-corners (fb x y w h)
  "OPEN LOOK L-shaped resize corner marks."
  (let ((n 7) (c glass:+black+))
    (flet ((h* (px py len) (glass:fb-rect fb px py len 1 c))
           (v* (px py len) (glass:fb-rect fb px py 1 len c)))
      (h* x y n) (v* x y n)                                   ; top-left
      (h* (- (+ x w) n) y n) (v* (- (+ x w) 1) y n)           ; top-right
      (h* x (- (+ y h) 1) n) (v* x (- (+ y h) n) n)           ; bottom-left
      (h* (- (+ x w) n) (- (+ y h) 1) n) (v* (- (+ x w) 1) (- (+ y h) n) n)))) ; bottom-right

;;; A surface window is a non-McCLIM window: a glass framebuffer somebody else
;;; renders into (e.g. a terminal) plus input callbacks.  The WM decorates,
;;; composites, drags, raises and focuses it just like a McCLIM window.
(defstruct wm-surface
  fb (x 60) (y 60) (title "window")
  (deco nil) (deco-w -1) on-key on-pointer)

(defun wm-surface-deco* (surf cw)
  (when (or (null (wm-surface-deco surf)) (/= cw (wm-surface-deco-w surf)))
    (setf (wm-surface-deco surf) (wm-render-titlebar (wm-surface-title surf) cw)
          (wm-surface-deco-w surf) cw))
  (wm-surface-deco surf))

(defun wm-frame (fb cx cy cw ch deco content-fn)
  "Draw a decorated window: DECO title bar above the content at (cx,cy) size
   (cw,ch), the content via CONTENT-FN, then a border + corner marks."
  (let* ((ty (- cy +wm-titleh+)) (wx (- cx +wm-border+)) (wy (- ty +wm-border+))
         (ww (+ cw (* 2 +wm-border+))) (wh (+ +wm-titleh+ ch (* 2 +wm-border+))))
    (blit-fb deco cx ty fb)
    (funcall content-fn)
    (glass:fb-frame fb wx wy ww wh glass:+black+ +wm-border+)
    (wm-corners fb wx wy ww wh)))

(defun wm-draw-window (mirror fb)
  (when-let ((image (mcclim-render::image-mirror-image mirror)))
    (multiple-value-bind (cw ch) (image-wh image)
      (wm-frame fb (glass-mirror-x mirror) (glass-mirror-y mirror) cw ch
                (wm-deco mirror cw) (lambda () (blit-mirror mirror fb))))))

(defun wm-draw-surface (surf fb)
  (let* ((sfb (wm-surface-fb surf)) (cw (glass:fb-width sfb)) (ch (glass:fb-height sfb)))
    (wm-frame fb (wm-surface-x surf) (wm-surface-y surf) cw ch (wm-surface-deco* surf cw)
              (lambda () (blit-fb sfb (wm-surface-x surf) (wm-surface-y surf) fb)))))

(defun wm-composite (port fb)
  (glass:fb-fill fb +wm-teal+)
  (dolist (mirror (reverse (glass-port-mirrors port)))        ; McCLIM windows (bottom-to-top)
    (if (glass-mirror-managed mirror) (wm-draw-window mirror fb) (blit-mirror mirror fb)))
  (dolist (surf (reverse (glass-port-surfaces port)))         ; surface windows, on top
    (wm-draw-surface surf fb)))

;;; ---- pointer routing --------------------------------------------------------

(defun wm-pos-x (obj) (if (wm-surface-p obj) (wm-surface-x obj) (glass-mirror-x obj)))
(defun wm-pos-y (obj) (if (wm-surface-p obj) (wm-surface-y obj) (glass-mirror-y obj)))
(defun wm-move (obj x y)
  (if (wm-surface-p obj) (setf (wm-surface-x obj) x (wm-surface-y obj) y)
      (setf (glass-mirror-x obj) x (glass-mirror-y obj) y)))

(defun wm-hit (port x y)
  "Topmost window (surfaces are above mirrors) whose title bar or content contains
   (X,Y): (values obj :title|:content cx cy cw ch), or NIL over the workspace."
  (flet ((test (cx cy cw ch obj)
           (cond ((and (<= cx x (+ cx cw)) (<= cy y (+ cy ch))) (list obj :content cx cy cw ch))
                 ((and (<= cx x (+ cx cw)) (<= (- cy +wm-titleh+) y cy)) (list obj :title cx cy cw ch)))))
    (dolist (surf (glass-port-surfaces port))
      (let ((hit (test (wm-surface-x surf) (wm-surface-y surf)
                       (glass:fb-width (wm-surface-fb surf)) (glass:fb-height (wm-surface-fb surf)) surf)))
        (when hit (return-from wm-hit (values-list hit)))))
    (dolist (mirror (glass-port-mirrors port))
      (when (glass-mirror-managed mirror)
        (when-let ((image (mcclim-render::image-mirror-image mirror)))
          (multiple-value-bind (cw ch) (image-wh image)
            (let ((hit (test (glass-mirror-x mirror) (glass-mirror-y mirror) cw ch mirror)))
              (when hit (return-from wm-hit (values-list hit))))))))))

(defun wm-raise (port obj)
  "Move OBJ to the top of its z-order (surfaces and mirrors are separate stacks)."
  (if (wm-surface-p obj)
      (setf (glass-port-surfaces port) (cons obj (remove obj (glass-port-surfaces port))))
      (setf (glass-port-mirrors port) (cons obj (remove obj (glass-port-mirrors port))))))

(defun wm-on-pointer (port mask x y)
  (let ((down (logtest mask 1)))
    (cond
      ((glass-port-drag port)                                 ; dragging a title bar
       (destructuring-bind (obj dx dy) (glass-port-drag port)
         (wm-move obj (- x dx) (- y dy))
         (composite-all port)
         (unless down (setf (glass-port-drag port) nil))))
      (t
       (multiple-value-bind (obj region cx cy) (wm-hit port x y)
         (cond
           ((null obj))                                       ; workspace: ignore
           ((eq region :title)
            (when down
              (wm-raise port obj)
              (when (wm-surface-p obj) (setf (glass-port-focus-surface port) obj))
              (setf (glass-port-drag port) (list obj (- x (wm-pos-x obj)) (- y (wm-pos-y obj))))
              (composite-all port)))
           ((wm-surface-p obj)                                ; content of a surface window
            (when down (setf (glass-port-focus-surface port) obj) (wm-raise port obj) (composite-all port))
            (when (wm-surface-on-pointer obj) (funcall (wm-surface-on-pointer obj) mask (- x cx) (- y cy))))
           (t                                                 ; content of a McCLIM window
            (when down (setf (glass-port-focus-surface port) nil))   ; keyboard back to CLIM
            (when (and down (not (eq obj (first (glass-port-mirrors port)))))
              (wm-raise port obj) (composite-all port))
            (emit-pointer-events port (glass-mirror-sheet obj) mask (- x cx) (- y cy)))))))))

;;; ---- run ---------------------------------------------------------------------

(defun wm-add-terminal (port &key (cols 80) (rows 24) (ppem 14))
  "Create a terminal (shell in a pty) and add it as a WM surface window."
  (let* ((tm (glass-term:make-terminal :cols cols :rows rows :ppem ppem))
         (c (glass-port-cascade port))
         (surf (make-wm-surface :fb (glass-term:terminal-fb tm)
                                :x (+ 40 c) :y (+ 40 c +wm-titleh+) :title "terminal"
                                :on-key (lambda (down k) (glass-term:on-key tm down k))
                                :on-pointer (lambda (mask lx ly) (glass-term:on-mouse tm mask lx ly)))))
    (glass-term:start-pump tm)
    (setf (glass-port-cascade port) (mod (+ c 28) 200))
    (push surf (glass-port-surfaces port))
    (setf (glass-port-focus-surface port) surf)
    surf))

(defun run-wm (specs &key (port 5900) (width 1000) (height 720))
  "Run a mini OPEN LOOK desktop over VNC.  Each spec is a decorated window:
   (FRAME-CLASS &key WIDTH HEIGHT) for a McCLIM app, or (:terminal &key COLS ROWS
   PPEM) for a shell terminal.  Serves on PORT; point any VNC client there."
  (let ((p (find-glass-port :port port)))
    (setf (glass-port-wm-p p) t
          (glass-port-screen-w p) width (glass-port-screen-h p) height
          (glass-port-fb p) (glass:make-framebuffer width height +wm-teal+))
    (start-glass-server p)
    (climi::restart-port p)                                   ; event-loop thread
    (let ((fm (find-frame-manager :port p)))
      (dolist (spec specs)
        (if (eq (car spec) :terminal)
            (apply #'wm-add-terminal p (cdr spec))
            (destructuring-bind (class &key (width 480) (height 320)) spec
              (sb-thread:make-thread
               (lambda ()
                 (handler-case
                     (run-frame-top-level (make-application-frame class :frame-manager fm
                                                                  :width width :height height))
                   (error (e) (format *trace-output* "~&[wm] frame ~a: ~a~%" class e))))
               :name (format nil "wm-~a" class))))
        (sleep 0.7)                                           ; stagger for distinct cascade slots
        (composite-all p)))
    (loop (sleep 10))))
