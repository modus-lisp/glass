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
  "An mcclim-render image of an OPEN LOOK title bar WIDTH px wide."
  (clime:with-output-to-drawing-stream (s :raster :pattern :width (max 1 width) :height +wm-titleh+)
    (clim:draw-rectangle* s 0 0 width +wm-titleh+ :ink (clim:make-gray-color 0.80))
    (clim:draw-line* s 0 (1- +wm-titleh+) width (1- +wm-titleh+) :ink (clim:make-gray-color 0.45))
    ;; menu button box, raised bevel, with the abbreviated-menu wedge
    (let* ((bs (- +wm-titleh+ 8)) (bx 4) (by 4) (bx2 (+ bx bs)) (by2 (+ by bs)))
      (clim:draw-rectangle* s bx by bx2 by2 :ink (clim:make-gray-color 0.72))
      (clim:draw-line* s bx by bx2 by :ink clim:+white+)
      (clim:draw-line* s bx by bx by2 :ink clim:+white+)
      (clim:draw-line* s bx by2 bx2 by2 :ink (clim:make-gray-color 0.30))
      (clim:draw-line* s bx2 by bx2 by2 :ink (clim:make-gray-color 0.30))
      (let ((cx (+ bx (floor bs 2))) (cy (+ by (floor bs 2))))
        (clim:draw-polygon* s (list (- cx 3) (- cy 1) (+ cx 3) (- cy 1) cx (+ cy 3)) :ink clim:+black+)))
    ;; centred bold title
    (let ((tw (* (length title) 7)))
      (clim:draw-text* s title (max (+ +wm-titleh+ 6) (floor (- width tw) 2)) 15
                       :ink clim:+black+ :text-size 12 :text-face :bold))))

(defun wm-deco (mirror cw)
  "Cached title-bar image for MIRROR at content width CW."
  (when (or (null (glass-mirror-deco mirror)) (/= cw (glass-mirror-deco-w mirror)))
    (setf (glass-mirror-deco mirror) (wm-render-titlebar (glass-mirror-title mirror) cw)
          (glass-mirror-deco-w mirror) cw))
  (glass-mirror-deco mirror))

;;; ---- compositing ------------------------------------------------------------

(defun blit-argb (arr ox oy fb)
  "Composite the ARGB 2D array ARR into FB with its top-left at (OX,OY), opaque."
  (let* ((ih (array-dimension arr 0)) (iw (array-dimension arr 1))
         (dpx (glass:fb-pixels fb)) (fw (glass:fb-width fb)) (fh (glass:fb-height fb)))
    (dotimes (iy ih)
      (let ((fy (+ oy iy)))
        (when (< -1 fy fh)
          (let ((frow (* fy fw)))
            (dotimes (ix iw)
              (let ((fx (+ ox ix)))
                (when (< -1 fx fw)
                  (setf (aref dpx (+ frow fx)) (logand (aref arr iy ix) #x00ffffff)))))))))))

(defun wm-corners (fb x y w h)
  "OPEN LOOK L-shaped resize corner marks."
  (let ((n 7) (c glass:+black+))
    (flet ((h* (px py len) (glass:fb-rect fb px py len 1 c))
           (v* (px py len) (glass:fb-rect fb px py 1 len c)))
      (h* x y n) (v* x y n)                                   ; top-left
      (h* (- (+ x w) n) y n) (v* (- (+ x w) 1) y n)           ; top-right
      (h* x (- (+ y h) 1) n) (v* x (- (+ y h) n) n)           ; bottom-left
      (h* (- (+ x w) n) (- (+ y h) 1) n) (v* (- (+ x w) 1) (- (+ y h) n) n)))) ; bottom-right

(defun wm-draw-window (mirror fb)
  (when-let ((image (mcclim-render::image-mirror-image mirror)))
    (multiple-value-bind (cw ch) (image-wh image)
      (let* ((cx (glass-mirror-x mirror)) (cy (glass-mirror-y mirror))
             (tx cx) (ty (- cy +wm-titleh+))                  ; title bar just above content
             (wx (- cx +wm-border+)) (wy (- ty +wm-border+))
             (ww (+ cw (* 2 +wm-border+))) (wh (+ +wm-titleh+ ch (* 2 +wm-border+))))
        (blit-argb (climi::pattern-array (wm-deco mirror cw)) tx ty fb)   ; title bar
        (blit-mirror mirror fb)                                           ; content at (cx,cy)
        (glass:fb-frame fb wx wy ww wh glass:+black+ +wm-border+)         ; window border
        (wm-corners fb wx wy ww wh)))))

(defun wm-composite (port fb)
  (glass:fb-fill fb +wm-teal+)
  (dolist (mirror (reverse (glass-port-mirrors port)))        ; bottom-to-top
    (if (glass-mirror-managed mirror)
        (wm-draw-window mirror fb)
        (blit-mirror mirror fb))))                            ; menus: undecorated, at their pos

;;; ---- pointer routing --------------------------------------------------------

(defun wm-window-under (port x y)
  "Topmost managed window whose DECORATION (title bar or content) contains (X,Y),
   with the region — (values mirror :title|:content cx cy cw ch) — or NIL."
  (dolist (mirror (glass-port-mirrors port))                 ; newest-first = topmost
    (when (glass-mirror-managed mirror)
      (when-let ((image (mcclim-render::image-mirror-image mirror)))
        (multiple-value-bind (cw ch) (image-wh image)
          (let ((cx (glass-mirror-x mirror)) (cy (glass-mirror-y mirror)))
            (cond
              ((and (<= cx x (+ cx cw)) (<= cy y (+ cy ch)))
               (return (values mirror :content cx cy cw ch)))
              ((and (<= cx x (+ cx cw)) (<= (- cy +wm-titleh+) y cy))
               (return (values mirror :title cx cy cw ch))))))))))

(defun wm-raise (port mirror)
  "Move MIRROR to the front (top) of the z-order."
  (setf (glass-port-mirrors port)
        (cons mirror (remove mirror (glass-port-mirrors port)))))

(defun wm-on-pointer (port mask x y)
  (let ((down (logtest mask 1)))
    (cond
      ;; dragging a title bar: follow the mouse
      ((glass-port-drag port)
       (destructuring-bind (mirror dx dy) (glass-port-drag port)
         (setf (glass-mirror-x mirror) (- x dx)
               (glass-mirror-y mirror) (- y dy))
         (composite-all port)
         (unless down (setf (glass-port-drag port) nil))))    ; released
      (t
       (multiple-value-bind (mirror region cx cy) (wm-window-under port x y)
         (cond
           ((null mirror))                                    ; over the workspace: ignore
           ((eq region :title)
            (when down                                        ; grab: raise + begin move
              (wm-raise port mirror)
              (setf (glass-port-drag port) (list mirror (- x (glass-mirror-x mirror))
                                                        (- y (glass-mirror-y mirror))))
              (composite-all port)))
           ((eq region :content)
            (when (and down (not (eq mirror (first (glass-port-mirrors port)))))
              (wm-raise port mirror) (composite-all port))    ; click-to-raise
            (emit-pointer-events port (glass-mirror-sheet mirror) mask (- x cx) (- y cy)))))))))

;;; ---- run ---------------------------------------------------------------------

(defun run-wm (specs &key (port 5900) (width 1000) (height 720))
  "Run a mini OPEN LOOK desktop over VNC.  SPECS is a list of frame specs, each
   (FRAME-CLASS &key WIDTH HEIGHT) — one decorated window per spec.  Serves on
   PORT; point any VNC client at localhost:PORT.  Blocks until interrupted."
  (let ((p (find-glass-port :port port)))
    (setf (glass-port-wm-p p) t
          (glass-port-screen-w p) width (glass-port-screen-h p) height
          (glass-port-fb p) (glass:make-framebuffer width height +wm-teal+))
    (start-glass-server p)
    (climi::restart-port p)                                   ; event-loop thread
    (let ((fm (find-frame-manager :port p)))
      (dolist (spec specs)
        (destructuring-bind (class &key (width 480) (height 320)) spec
          (sb-thread:make-thread
           (lambda ()
             (handler-case
                 (run-frame-top-level (make-application-frame class :frame-manager fm
                                                              :width width :height height))
               (error (e) (format *trace-output* "~&[wm] frame ~a: ~a~%" class e))))
           :name (format nil "wm-~a" class))
          (sleep 0.6))))                                      ; stagger so cascade positions differ
    ;; keep the driver thread alive; the frames run in their own threads
    (loop (sleep 10))))
