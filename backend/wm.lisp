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

;;; ---- workspace root menu (OPEN LOOK) ---------------------------------------
;;; Right-click the bare workspace to pop up a small grey menu of things to run
;;; (Terminal, Tabbed Terminal, ...).  It follows the pointer with a hover
;;; highlight; a left-click on an item runs its thunk, a click off it dismisses.

(defparameter +menu-bg+ (glass:rgb 208 208 208) "Menu background grey.")
(defparameter +menu-title-bg+ (glass:rgb 188 188 188) "Menu title strip.")
(defparameter +menu-hi+ (glass:rgb 61 122 138) "Highlighted item (teal).")
(defconstant +menu-itemh+ 20 "Height of one menu item (px).")
(defconstant +menu-titleh+ 20 "Height of the menu title strip (px).")

(defstruct wm-menu
  (x 0) (y 0) (hover -1) (title "Workspace") items fb)

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
    (wm-draw-surface surf fb))
  (when-let ((menu (glass-port-menu port)))                   ; root menu above everything
    (blit-fb (wm-menu-fb menu) (wm-menu-x menu) (wm-menu-y menu) fb)))

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

;;; ---- workspace root menu ----------------------------------------------------

(defun wm-menu-width (menu)
  (let ((w (+ 24 (glass:text-width (wm-menu-title menu) :size 12 :font (glass:default-font t)))))
    (dolist (it (wm-menu-items menu) (max 96 w))
      (setf w (max w (+ 28 (glass:text-width (car it) :size 12 :font (glass:default-font t))))))))

(defun wm-menu-render (menu)
  "(Re)build the menu's framebuffer, drawing the current hover highlight."
  (let* ((n (length (wm-menu-items menu)))
         (w (wm-menu-width menu))
         (h (+ +menu-titleh+ (* n +menu-itemh+)))
         (fb (glass:make-framebuffer w h +menu-bg+))
         (font (glass:default-font t)))
    (glass:fb-rect fb 0 0 w +menu-titleh+ +menu-title-bg+)                 ; title strip
    (glass:fb-text fb 8 3 (wm-menu-title menu) :size 12 :color glass:+black+ :font font)
    (glass:fb-hline fb 0 (1- +menu-titleh+) w (glass:rgb 120 120 120))
    (loop for it in (wm-menu-items menu) for i from 0
          for yy = (+ +menu-titleh+ (* i +menu-itemh+))
          for hot = (= i (wm-menu-hover menu))
          do (when hot (glass:fb-rect fb 1 yy (- w 2) +menu-itemh+ +menu-hi+))
             (glass:fb-text fb 14 (+ yy 3) (car it) :size 12
                            :color (if hot glass:+white+ glass:+black+) :font font))
    (glass:fb-frame fb 0 0 w h glass:+black+ 1)
    (setf (wm-menu-fb menu) fb)))

(defun wm-menu-index (menu x y)
  "For screen (X,Y): an item index, :title over the title strip, or :outside."
  (let* ((mx (wm-menu-x menu)) (my (wm-menu-y menu)) (fb (wm-menu-fb menu)))
    (if (and (<= mx x (+ mx (glass:fb-width fb) -1)) (<= my y (+ my (glass:fb-height fb) -1)))
        (let ((yl (- y my)))
          (if (< yl +menu-titleh+) :title
              (let ((i (floor (- yl +menu-titleh+) +menu-itemh+)))
                (if (< i (length (wm-menu-items menu))) i :title))))
        :outside)))

(defun wm-open-menu (port x y)
  (let ((menu (make-wm-menu :x x :y y :hover -1 :items (glass-port-menu-items port))))
    (wm-menu-render menu)
    (setf (wm-menu-x menu) (max 0 (min x (- (glass-port-screen-w port) (glass:fb-width (wm-menu-fb menu)))))
          (wm-menu-y menu) (max 0 (min y (- (glass-port-screen-h port) (glass:fb-height (wm-menu-fb menu)))))
          (glass-port-menu port) menu)))

(defun wm-menu-pointer (port menu mask x y)
  "Route a pointer event to the open MENU.  Returns having handled it."
  (let ((left (logtest mask 1))
        (idx (wm-menu-index menu x y)))
    (cond
      ((eq idx :outside)
       (when (logtest mask 5)                                  ; any click off the menu dismisses
         (setf (glass-port-menu port) nil) (composite-all port)))
      ((integerp idx)
       (unless (eql idx (wm-menu-hover menu))                  ; hover follows the pointer
         (setf (wm-menu-hover menu) idx) (wm-menu-render menu) (composite-all port))
       (when left                                              ; left-press selects the item
         (let ((action (cdr (nth idx (wm-menu-items menu)))))
           (setf (glass-port-menu port) nil) (composite-all port)
           (when action (wm-menu-run port action)))))
      (t                                                       ; over the title strip
       (unless (eql (wm-menu-hover menu) -1)
         (setf (wm-menu-hover menu) -1) (wm-menu-render menu) (composite-all port))))))

(defun wm-on-pointer (port mask x y)
  (when-let ((menu (glass-port-menu port)))                    ; an open menu grabs the pointer
    (wm-menu-pointer port menu mask x y)
    (return-from wm-on-pointer))
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
           ((and (null obj) (logtest mask 4))                 ; right-press on workspace: root menu
            (wm-open-menu port x y) (composite-all port))
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

(defun wm-add-surface* (port surf)
  (setf (glass-port-cascade port) (mod (+ (glass-port-cascade port) 28) 200))
  (push surf (glass-port-surfaces port))
  (setf (glass-port-focus-surface port) surf)
  surf)

(defun wm-add-terminal (port &key (cols 80) (rows 24) (ppem 14))
  "Create a terminal (shell in a pty) and add it as a WM surface window."
  (let* ((tm (glass-term:make-terminal :cols cols :rows rows :ppem ppem))
         (c (glass-port-cascade port)))
    (glass-term:start-pump tm)
    (wm-add-surface* port
      (make-wm-surface :fb (glass-term:terminal-fb tm)
                       :x (+ 40 c) :y (+ 40 c +wm-titleh+) :title "terminal"
                       :on-key (lambda (down k) (glass-term:on-key tm down k))
                       :on-pointer (lambda (mask lx ly) (glass-term:on-mouse tm mask lx ly))))))

(defun wm-add-tabterm (port &key (cols 80) (rows 24) (ppem 14))
  "A tabbed terminal (several shells, a tab bar) as a WM surface window."
  (let ((tt (glass-term:make-tabbed-terminal :cols cols :rows rows :ppem ppem))
        (c (glass-port-cascade port)))
    (wm-add-surface* port
      (make-wm-surface :fb (glass-term:tabterm-fb tt)
                       :x (+ 40 c) :y (+ 40 c +wm-titleh+) :title "terminal"
                       :on-key (lambda (down k) (glass-term:tabterm-on-key tt down k))
                       :on-pointer (lambda (mask lx ly) (glass-term:tabterm-on-mouse tt mask lx ly))))))

(defun wm-run-frame (port frame &optional (name "frame"))
  "Host a McCLIM application FRAME in its own thread on PORT (realize-mirror
   decorates it as a managed window)."
  (sb-thread:make-thread
   (lambda ()
     (handler-case (run-frame-top-level frame)
       (error (e) (format *trace-output* "~&[wm] ~a: ~a~%" name e))))
   :name (format nil "wm-~a" name)))

(defun wm-inspect (port form)
  "Open Clouseau (the McCLIM object inspector, Genera's lineage) on the value of
   FORM — a decorated window with clickable slot drill-down.  Clouseau is an
   OPTIONAL runtime dependency, resolved by name so the backend needn't load it."
  (let ((fn (and (find-package '#:clouseau) (find-symbol "INSPECT" '#:clouseau))))
    (unless (and fn (fboundp fn)) (error "clouseau is not loaded — (ql:quickload :clouseau)"))
    (let ((object (eval form))
          (fm (find-frame-manager :port port)))
      (sb-thread:make-thread
       (lambda ()
         (handler-case
             (let ((climi::*default-frame-manager* fm))   ; land on OUR glass port
               (funcall fn object :new-process nil))       ; runs the frame in this thread
           (error (e) (format *trace-output* "~&[wm] inspect: ~a~%" e))))
       :name "wm-inspect"))))

;;; A window spec is the shared launch vocabulary — used both for run-wm's
;;; initial windows AND for the root-menu items, so a menu is just a list of
;;; labelled specs:
;;;   (:terminal &key cols rows ppem)   a shell terminal (surface window)
;;;   (:tabterm  &key cols rows ppem)   a tabbed terminal
;;;   (:inspect FORM)                   Clouseau inspecting the value of FORM
;;;   (FRAME-CLASS &key width height)   any McCLIM application frame
(defun wm-spawn-spec (port spec)
  (case (car spec)
    (:terminal (apply #'wm-add-terminal port (cdr spec)))
    (:tabterm  (apply #'wm-add-tabterm  port (cdr spec)))
    (:inspect  (wm-inspect port (cadr spec)))
    (t (destructuring-bind (class &key (width 480) (height 320)) spec
         (wm-run-frame port (make-application-frame class :frame-manager (find-frame-manager :port port)
                                                    :width width :height height)
                       (princ-to-string class))))))

(defun wm-menu-run (port action)
  "Run a chosen menu ACTION: a window spec (launch it) or, as an escape hatch, a
   thunk (call it)."
  (if (functionp action)
      (funcall action)
      (progn (wm-spawn-spec port action) (composite-all port))))

(defun wm-default-menu ()
  "The default workspace root menu: labelled window specs (LABEL . SPEC)."
  '(("Terminal"        :terminal)
    ("Terminal (tabs)" :tabterm)))

(defun run-wm (specs &key (port 5900) (width 1000) (height 720) menu)
  "Run a mini OPEN LOOK desktop over VNC.  Each spec is a decorated window:
   (FRAME-CLASS &key WIDTH HEIGHT) for a McCLIM app, or (:terminal &key COLS ROWS
   PPEM) for a shell terminal.  Right-click the workspace for a root menu; pass
   MENU — a list of (LABEL . SPEC) labelled window specs (same vocabulary as
   SPECS), or (LABEL . THUNK) for an arbitrary action — to override its items.
   Serves on PORT."
  (let ((p (find-glass-port :port port)))
    (setf (glass-port-wm-p p) t
          (glass-port-screen-w p) width (glass-port-screen-h p) height
          (glass-port-fb p) (glass:make-framebuffer width height +wm-teal+)
          (glass-port-menu-items p) (or menu (wm-default-menu)))
    (start-glass-server p)
    (climi::restart-port p)                                   ; event-loop thread
    (dolist (spec specs)
      (wm-spawn-spec p spec)
      (sleep 0.7)                                             ; stagger for distinct cascade slots
      (composite-all p))
    ;; Surface windows (terminals) render asynchronously in their own threads, so
    ;; tick the compositor to pick up shell output; glass's dirty diff ships only
    ;; changed tiles, so an idle desktop costs next to nothing.
    (loop (sleep 1/20)
          (when (glass-port-surfaces p) (ignore-errors (composite-all p))))))
