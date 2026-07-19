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
;;; (Browse, Inspect, Debug, Terminal, Apps...).  It follows the pointer with a
;;; hover highlight; a left-click on an item runs it, a click off it dismisses.
;;; An item whose action is (:submenu ITEM...) opens a child menu to its right
;;; on hover — arbitrarily deep via the CHILD slot chain.

(defparameter +menu-bg+ (glass:rgb 208 208 208) "Menu background grey.")
(defparameter +menu-title-bg+ (glass:rgb 188 188 188) "Menu title strip.")
(defparameter +menu-hi+ (glass:rgb 61 122 138) "Highlighted item (teal).")
(defconstant +menu-itemh+ 20 "Height of one menu item (px).")
(defconstant +menu-titleh+ 20 "Height of the menu title strip (px).")

(defstruct wm-menu
  (x 0) (y 0) (hover -1) (title "Workspace") items fb (child nil))

(defun wm-submenu-p (action) (and (consp action) (eq (car action) :submenu)))
(defun wm-item-action (item) (cdr item))
(defun wm-menu-chain (root) (loop for m = root then (wm-menu-child m) while m collect m))

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
  "Copy glass framebuffer SRC into DST with its top-left at (OX,OY).  Each visible
   row is copied in one REPLACE (memcpy) — so a full-screen wallpaper blit is fast."
  (declare (optimize (speed 3) (safety 0)))
  (let* ((sw (glass:fb-width src)) (sh (glass:fb-height src))
         (spx (glass:fb-pixels src)) (dpx (glass:fb-pixels dst))
         (dw (glass:fb-width dst)) (dh (glass:fb-height dst))
         (clip (glass:fb-clip dst))
         (cx0 (if clip (the fixnum (first clip)) 0)) (cy0 (if clip (the fixnum (second clip)) 0))
         (cx1 (if clip (the fixnum (third clip)) dw)) (cy1 (if clip (the fixnum (fourth clip)) dh)))
    (declare (type (simple-array (unsigned-byte 32) (*)) spx dpx)
             (fixnum sw sh dw dh ox oy cx0 cy0 cx1 cy1))
    (dotimes (sy sh)
      (declare (fixnum sy))
      (let ((dy (+ oy sy)))
        (declare (fixnum dy))
        (when (and (< -1 dy dh) (<= cy0 dy) (< dy cy1))     ; row within fb and clip
          (let* ((drow (* dy dw)) (srow (* sy sw))
                 (dx0 (max 0 ox cx0)) (dx1 (min dw (+ ox sw) cx1)))
            (declare (fixnum drow srow dx0 dx1))
            (when (< dx0 dx1)
              (replace dpx spx :start1 (+ drow dx0) :end1 (+ drow dx1)
                               :start2 (+ srow (- dx0 ox))))))))))

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
  (deco nil) (deco-w -1) on-key on-pointer
  (dirty-p nil)                         ; ()->bool: did the content fb change? (nil = always redraw)
  (resize-fn nil)                       ; (px-w px-h)->() : resize the content, or nil = not resizable
  (close-fn nil)                        ; ()->() : tear down the content on window close
  (saved-geom nil))                     ; (x y w h) saved by Full Size, for Restore Size

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

(defun wm-render-background (port path &key (mode :cover))
  "Rasterise the image at PATH (any format pigment decodes — PNG/JPEG/GIF/WebP/SVG)
   into a screen-sized framebuffer for use as the desktop background.  MODE places
   it: :cover (fill, centre-crop — default), :fit (whole image, teal letterbox),
   :stretch (distort to fill), :center (1:1), or :tile.  For a crisp SVG, author
   it at the screen size (it rasterises at its intrinsic size, then scales)."
  (multiple-value-bind (iw ih samp) (%decode-image path)
    (let* ((sw (glass-port-screen-w port)) (sh (glass-port-screen-h port))
           (fb (glass:make-framebuffer sw sh +wm-teal+))
           (px (glass:fb-pixels fb)))
      (flet ((put (dx dy sx sy)
               (multiple-value-bind (r g b a) (funcall samp (min (1- ih) (max 0 sy)) (min (1- iw) (max 0 sx)))
                 (setf (aref px (+ (* dy sw) dx))
                       (if (>= a 255) (glass:rgb r g b)
                           (glass:rgb (round (+ (* r a) (* 61 (- 255 a))) 255)     ; over teal
                                      (round (+ (* g a) (* 122 (- 255 a))) 255)
                                      (round (+ (* b a) (* 138 (- 255 a))) 255)))))))
        (ecase mode
          (:stretch (dotimes (dy sh) (dotimes (dx sw) (put dx dy (floor (* dx iw) sw) (floor (* dy ih) sh)))))
          (:tile    (dotimes (dy sh) (dotimes (dx sw) (put dx dy (mod dx iw) (mod dy ih)))))
          ((:cover :fit :center)
           (let* ((scale (case mode (:fit (min (/ sw iw) (/ sh ih))) (:center 1) (t (max (/ sw iw) (/ sh ih)))))
                  (ox (/ (- sw (* iw scale)) 2)) (oy (/ (- sh (* ih scale)) 2)))
             (dotimes (dy sh) (dotimes (dx sw)
               (let ((sx (floor (- dx ox) scale)) (sy (floor (- dy oy) scale)))
                 (when (and (<= 0 sx) (< sx iw) (<= 0 sy) (< sy ih)) (put dx dy sx sy)))))))))
      fb)))

(defun wm-set-background (port path &key (mode :cover))
  "Set the desktop background to the image at PATH (NIL clears it -> flat teal)."
  (setf (glass-port-bg port)
        (and path (ignore-errors (wm-render-background port path :mode mode))))
  (when (glass-port-fb port) (composite-all port))
  (glass-port-bg port))

(defun wm-composite (port fb)
  (if (glass-port-bg port)
      (blit-fb (glass-port-bg port) 0 0 fb)                    ; desktop wallpaper
      (glass:fb-fill fb +wm-teal+))
  (dolist (mirror (reverse (glass-port-mirrors port)))        ; McCLIM windows (bottom-to-top)
    (if (glass-mirror-managed mirror) (wm-draw-window mirror fb) (blit-mirror mirror fb)))
  (dolist (surf (reverse (glass-port-surfaces port)))         ; surface windows, on top
    (wm-draw-surface surf fb))
  (when-let ((b (and (glass-port-drag-wire port) (glass-port-drag-wire-box port))))  ; wireframe outline
    (destructuring-bind (x y w h) b
      (glass:fb-frame fb x y w h glass:+white+ 2)             ; white + inner black = visible on any bg
      (glass:fb-frame fb (1+ x) (1+ y) (max 0 (- w 2)) (max 0 (- h 2)) glass:+black+ 1)))
  (when-let ((menu (glass-port-menu port)))                   ; root menu (+ submenu chain) on top
    (dolist (m (wm-menu-chain menu))
      (blit-fb (wm-menu-fb m) (wm-menu-x m) (wm-menu-y m) fb))))

;;; ---- pointer routing --------------------------------------------------------

(defun wm-window-box (obj)
  "(x y w h) of OBJ's whole decorated window — title bar + border + content — for
   damage accounting."
  (multiple-value-bind (cx cy cw ch)
      (if (wm-surface-p obj)
          (values (wm-surface-x obj) (wm-surface-y obj)
                  (glass:fb-width (wm-surface-fb obj)) (glass:fb-height (wm-surface-fb obj)))
          (when-let ((img (mcclim-render::image-mirror-image obj)))
            (multiple-value-bind (w h) (image-wh img)
              (values (glass-mirror-x obj) (glass-mirror-y obj) w h))))
    (when cx
      (list (- cx +wm-border+) (- cy +wm-titleh+ +wm-border+)
            (+ cw (* 2 +wm-border+)) (+ +wm-titleh+ ch (* 2 +wm-border+))))))

(defun wm-window-box-at (obj cx cy)
  "The decorated (x y w h) OBJ WOULD occupy if its content were at (CX,CY) — for the
   wireframe outline, which shows a hypothetical position without moving the window."
  (multiple-value-bind (cw ch)
      (if (wm-surface-p obj)
          (values (glass:fb-width (wm-surface-fb obj)) (glass:fb-height (wm-surface-fb obj)))
          (when-let ((img (mcclim-render::image-mirror-image obj)))
            (image-wh img)))
    (when cw
      (list (- cx +wm-border+) (- cy +wm-titleh+ +wm-border+)
            (+ cw (* 2 +wm-border+)) (+ +wm-titleh+ ch (* 2 +wm-border+))))))

;;; ---- adaptive drag: opaque when the link keeps up, wireframe when it can't -----
;;; Moving a window OPAQUELY re-encodes it each frame — cheap on a client that can
;;; CopyRect (TigerVNC) or for small moves, but a big drag on a no-CopyRect client
;;; (macOS Screen Sharing) re-sends the whole window every frame and lags.  So a
;;; drag starts opaque (small moves look great) and switches to a WIREFRAME outline
;;; only once the send backlog (glass:*send-lag*) shows the link falling behind —
;;; the outline is a few thin rects, near-free to encode; the real window snaps to
;;; the final spot on release.
(defparameter *drag-adaptive* t "Auto-switch a laggy opaque drag to wireframe.")
(defparameter *wireframe-queue-kb* 100.0d0
  "Socket send-queue backlog EWMA (KB, glass:*send-queue*) past which an in-progress
   drag switches to wireframe — the real 'client can't keep up' signal.  A CopyRect
   client's cheap opaque drags never back the queue up, so it stays opaque; a
   no-CopyRect client (macOS) re-encoding the whole window backs it up and trips
   wireframe.  Tune live over the control socket.")

(defun wm-drag-move-opaque (port obj ncx ncy)
  "Opaque move step: move the real window to content-position (NCX,NCY) and composite
   old+new with a CopyRect hint (near-free on a client that can CopyRect)."
  (let ((old (wm-window-box obj)))
    (wm-move obj ncx ncy)
    (let ((new (wm-window-box obj)))
      (composite-all port (wm-box-union (list old new))
                     (when (and old new)
                       (list (first old) (second old) (first new) (second new)
                             (third old) (fourth old)))))))

(defun wm-drag-move (port obj ncx ncy)
  "A drag move: opaque, unless the socket send-queue (glass:*send-queue*) shows the
   client can't keep up — then switch THIS drag to wireframe (outline starts at the
   window's current box) and don't move the real window."
  (if (and *drag-adaptive* (> glass:*send-queue* *wireframe-queue-kb*))
      (progn
        (setf (glass-port-drag-wire port) t
              (glass-port-drag-wire-box port) (wm-window-box obj))
        (wm-drag-wire-to port obj ncx ncy))
      (wm-drag-move-opaque port obj ncx ncy)))

(defun wm-drag-wire-to (port obj ncx ncy)
  "Wireframe drag step: move only the OUTLINE to content-position (NCX,NCY); the real
   window stays put (its pixels stay on the client), so only the thin outline tiles
   change — cheap even with no CopyRect."
  (let ((old (glass-port-drag-wire-box port))
        (new (wm-window-box-at obj ncx ncy)))
    (setf (glass-port-drag-wire-box port) new)
    (composite-all port (wm-box-union (list old new)))))

(defun wm-drag-wire-drop (port obj ncx ncy)
  "End a wireframe drag: move the real window to (NCX,NCY) and composite the union of
   the window's OLD position (it stayed put through the drag, so its pixels are still
   on the client and must be ERASED — otherwise a ghost window lingers there), the
   last outline, and the window's NEW box.  The one time the moved content is re-sent."
  (let ((old (wm-window-box obj))            ; real position BEFORE the move (the ghost source)
        (wire (glass-port-drag-wire-box port)))
    (wm-move obj ncx ncy)
    (setf (glass-port-drag-wire port) nil (glass-port-drag-wire-box port) nil)
    (composite-all port (wm-box-union (list old wire (wm-window-box obj))))))

(defun wm-box-union (boxes)
  "Bounding (x y w h) of BOXES, or NIL if empty."
  (let ((x0 nil) (y0 nil) (x1 nil) (y1 nil))
    (dolist (b (remove nil boxes))
      (destructuring-bind (x y w h) b
        (setf x0 (if x0 (min x0 x) x) y0 (if y0 (min y0 y) y)
              x1 (if x1 (max x1 (+ x w)) (+ x w)) y1 (if y1 (max y1 (+ y h)) (+ y h)))))
    (when x0 (list x0 y0 (- x1 x0) (- y1 y0)))))

(defun wm-pos-x (obj) (if (wm-surface-p obj) (wm-surface-x obj) (glass-mirror-x obj)))
(defun wm-pos-y (obj) (if (wm-surface-p obj) (wm-surface-y obj) (glass-mirror-y obj)))
(defun wm-move (obj x y)
  (if (wm-surface-p obj) (setf (wm-surface-x obj) x (wm-surface-y obj) y)
      (setf (glass-mirror-x obj) x (glass-mirror-y obj) y)))

(defun wm-hit (port x y)
  "Topmost window (surfaces are above mirrors) whose decoration or content contains
   (X,Y): (values obj REGION cx cy cw ch), REGION one of :winmenu (title-bar menu
   button) / :resize (bottom-right corner grab) / :title / :content; NIL over the
   workspace."
  (flet ((test (cx cy cw ch obj)
           (let ((ty (- cy +wm-titleh+)) (rz 16))
             (cond
               ((and (<= (+ cx 4) x (+ cx 18)) (<= (+ ty 4) y (+ ty 18)))           ; wedge = Window Menu
                (list obj :winmenu cx cy cw ch))
               ((and (<= (- (+ cx cw) rz) x (+ cx cw 1)) (<= (- (+ cy ch) rz) y (+ cy ch 1)))  ; resize corner
                (list obj :resize cx cy cw ch))
               ((and (<= cx x (+ cx cw)) (<= cy y (+ cy ch))) (list obj :content cx cy cw ch))
               ((and (<= cx x (+ cx cw)) (<= ty y cy)) (list obj :title cx cy cw ch))))))
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

(defun wm-close (port obj)
  "Close window OBJ: tear down a surface's content (kill its shell) and drop it, or
   drop a McCLIM window's mirror (the window vanishes; its frame thread lingers,
   idle).  Recomposites."
  (cond
    ((wm-surface-p obj)
     (when (wm-surface-close-fn obj) (ignore-errors (funcall (wm-surface-close-fn obj))))
     (setf (glass-port-surfaces port) (remove obj (glass-port-surfaces port)))
     (when (eq (glass-port-focus-surface port) obj) (setf (glass-port-focus-surface port) nil)))
    (t
     (setf (glass-port-mirrors port) (remove obj (glass-port-mirrors port)))))
  (composite-all port))

(defun wm-resize (port obj px-w px-h)
  "Resize window OBJ's content to PX-W x PX-H pixels — surfaces with a resize-fn
   (e.g. a terminal re-grids); others (incl. McCLIM windows) don't resize yet."
  (declare (ignore port))
  (when (and (wm-surface-p obj) (wm-surface-resize-fn obj))
    (ignore-errors (funcall (wm-surface-resize-fn obj) (max 32 px-w) (max 32 px-h)))))

;;; ---- OPEN LOOK window menu (the title-bar wedge) ----------------------------
;;; The wedge button was never a close box — it opened the Window Menu.  olwm's
;;; base-window menu was: Close (iconify) / Full Size / Move / Resize (keyboard) /
;;; Back / Refresh / Quit (kill).  We honour the names we can act on; iconify and
;;; the mouseless Move/Resize are out of scope (no icon strip; we drag/corner).

(defun wm-lower (port obj)
  "Back: send OBJ behind all other windows in its stack."
  (if (wm-surface-p obj)
      (setf (glass-port-surfaces port) (append (remove obj (glass-port-surfaces port)) (list obj)))
      (setf (glass-port-mirrors port) (append (remove obj (glass-port-mirrors port)) (list obj))))
  (composite-all port))

(defun wm-fullsize (port obj)
  "Toggle Full Size / Restore Size for a resizable surface (fills the workspace,
   below the title bar; a second time restores the saved geometry)."
  (when (and (wm-surface-p obj) (wm-surface-resize-fn obj))
    (if (wm-surface-saved-geom obj)
        (destructuring-bind (x y w h) (wm-surface-saved-geom obj)          ; Restore Size
          (setf (wm-surface-x obj) x (wm-surface-y obj) y (wm-surface-saved-geom obj) nil)
          (funcall (wm-surface-resize-fn obj) w h))
        (progn                                                             ; Full Size
          (setf (wm-surface-saved-geom obj)
                (list (wm-surface-x obj) (wm-surface-y obj)
                      (glass:fb-width (wm-surface-fb obj)) (glass:fb-height (wm-surface-fb obj)))
                (wm-surface-x obj) +wm-border+
                (wm-surface-y obj) (+ +wm-titleh+ +wm-border+))
          (funcall (wm-surface-resize-fn obj)
                   (- (glass-port-screen-w port) (* 2 +wm-border+))
                   (- (glass-port-screen-h port) +wm-titleh+ (* 2 +wm-border+)))))
    (composite-all port)))

(defun wm-window-menu-items (port obj)
  "The Window Menu items (LABEL . THUNK) for OBJ."
  (append
   (when (and (wm-surface-p obj) (wm-surface-resize-fn obj))
     (list (cons (if (wm-surface-saved-geom obj) "Restore Size" "Full Size")
                 (lambda () (wm-fullsize port obj)))))
   (list (cons "Back"    (lambda () (wm-lower port obj)))
         (cons "Refresh" (lambda () (composite-all port)))
         (cons "Quit"    (lambda () (wm-close port obj))))))

(defun wm-open-window-menu (port obj cx cy)
  "Pop the Window Menu just below OBJ's title bar (at content top-left CX,CY)."
  (let ((menu (make-wm-menu :hover -1 :title "Window" :items (wm-window-menu-items port obj))))
    (setf (glass-port-menu port) (wm-place-menu menu port cx cy))))

;;; ---- workspace root menu ----------------------------------------------------

(defun wm-menu-width (menu)
  (let ((w (+ 24 (glass:text-width (wm-menu-title menu) :size 12 :font (glass:default-font t)))))
    (dolist (it (wm-menu-items menu) (max 108 w))
      (let ((pad (if (wm-submenu-p (wm-item-action it)) 46 28)))     ; room for the ▸ arrow
        (setf w (max w (+ pad (glass:text-width (car it) :size 12 :font (glass:default-font t)))))))))

(defun wm-submenu-arrow (fb x y color)
  "A small right-pointing triangle (▸) marking a submenu item, top-left at (X,Y)."
  (dotimes (k 5) (glass:fb-vline fb (+ x k) (+ y k) (max 1 (- 9 (* 2 k))) color)))

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
          for ink = (if hot glass:+white+ glass:+black+)
          do (when hot (glass:fb-rect fb 1 yy (- w 2) +menu-itemh+ +menu-hi+))
             (glass:fb-text fb 14 (+ yy 3) (car it) :size 12 :color ink :font font)
             (when (wm-submenu-p (wm-item-action it))
               (wm-submenu-arrow fb (- w 13) (+ yy 6) ink)))
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

(defun wm-place-menu (menu port x y)
  "Render MENU and position it on-screen, top-left near (X,Y) but kept in bounds."
  (wm-menu-render menu)
  (setf (wm-menu-x menu) (max 0 (min x (- (glass-port-screen-w port) (glass:fb-width (wm-menu-fb menu)))))
        (wm-menu-y menu) (max 0 (min y (- (glass-port-screen-h port) (glass:fb-height (wm-menu-fb menu))))))
  menu)

(defun wm-open-menu (port x y)
  (let ((menu (make-wm-menu :x x :y y :hover -1 :items (glass-port-menu-items port))))
    (setf (glass-port-menu port) (wm-place-menu menu port x y))))

(defun wm-open-submenu (parent idx action port)
  "Open ACTION's submenu as PARENT's child, to the right of PARENT's item IDX."
  (let ((sub (make-wm-menu :hover -1 :title (car (nth idx (wm-menu-items parent)))
                           :items (cdr action))))                ; (:submenu ITEM...) -> ITEMs
    (wm-menu-render sub)
    (setf (wm-menu-child parent)
          (wm-place-menu sub port
                         (+ (wm-menu-x parent) (glass:fb-width (wm-menu-fb parent)) -2)
                         (+ (wm-menu-y parent) +menu-titleh+ (* idx +menu-itemh+) -1)))))

(defun wm-menu-pointer (port root mask x y)
  "Route a pointer event to the open menu tree ROOT (a menu + its submenu chain).
   Hover opens/closes submenus; a left-press on a leaf runs it and dismisses all;
   a click off every menu dismisses."
  (let* ((left (logtest mask 1))
         (chain (wm-menu-chain root))
         (menu (find-if (lambda (m) (not (eq :outside (wm-menu-index m x y)))) (reverse chain))))
    (cond
      ((null menu)                                              ; off every menu
       (when (logtest mask 5) (setf (glass-port-menu port) nil) (composite-all port)))
      (t
       (let ((idx (wm-menu-index menu x y)))
         (cond
           ((integerp idx)
            (let ((action (wm-item-action (nth idx (wm-menu-items menu)))))
              (unless (eql idx (wm-menu-hover menu))            ; hover moved within this menu
                (setf (wm-menu-hover menu) idx
                      (wm-menu-child menu) nil)                 ; drop any sibling's submenu
                (when (wm-submenu-p action) (wm-open-submenu menu idx action port))
                (wm-menu-render menu)
                (composite-all port))
              (when left
                (cond
                  ((wm-submenu-p action)                        ; keep it open, don't dismiss
                   (unless (wm-menu-child menu)
                     (wm-open-submenu menu idx action port) (composite-all port)))
                  (t                                            ; leaf: run + dismiss the whole tree
                   (setf (glass-port-menu port) nil) (composite-all port)
                   (when action (wm-menu-run port action)))))))
           (t                                                   ; over the title strip
            (unless (eql (wm-menu-hover menu) -1)
              (setf (wm-menu-hover menu) -1) (wm-menu-render menu) (composite-all port)))))))))

(defun wm-on-pointer (port mask x y)
  (when-let ((menu (glass-port-menu port)))                    ; an open menu grabs the pointer
    (wm-menu-pointer port menu mask x y)
    (return-from wm-on-pointer))
  (let ((down (logtest mask 1)))
    (cond
      ((glass-port-drag port)                                 ; a move or resize in progress
       (destructuring-bind (obj mode . rest) (glass-port-drag port)
         (ecase mode
           (:move
            (destructuring-bind (dx dy) rest
              (let ((ncx (- x dx)) (ncy (- y dy)))
                (cond
                  ((glass-port-drag-wire port)               ; already wireframe (no flapping)
                   (if down (wm-drag-wire-to port obj ncx ncy)
                       (wm-drag-wire-drop port obj ncx ncy)))  ; release -> land the window
                  (down (wm-drag-move port obj ncx ncy))     ; opaque; switch to wireframe if laggy
                  (t (wm-drag-move-opaque port obj ncx ncy)))))) ; release -> final opaque, no switch
           (:resize (destructuring-bind (x0 y0 cw0 ch0) rest
                      (wm-resize port obj (+ cw0 (- x x0)) (+ ch0 (- y y0))) (composite-all port))))
         (unless down (setf (glass-port-drag port) nil))))
      (t
       (multiple-value-bind (obj region cx cy cw ch) (wm-hit port x y)
         (cond
           ((and (null obj) (logtest mask 4))                 ; right-press on workspace: root menu
            (wm-open-menu port x y) (composite-all port))
           ((null obj))                                       ; workspace: ignore
           ((eq region :winmenu)                              ; title-bar wedge: the Window Menu
            (when down
              (wm-raise port obj)
              (when (wm-surface-p obj) (setf (glass-port-focus-surface port) obj))
              (wm-open-window-menu port obj cx cy) (composite-all port)))
           ((eq region :resize)                               ; bottom-right corner: start a resize
            (when down
              (wm-raise port obj)
              (when (wm-surface-p obj) (setf (glass-port-focus-surface port) obj))
              (setf (glass-port-drag port) (list obj :resize x y cw ch))
              (composite-all port)))
           ((eq region :title)                                ; title bar: start a move
            (when down
              (wm-raise port obj)
              (when (wm-surface-p obj) (setf (glass-port-focus-surface port) obj))
              (setf (glass-port-drag port) (list obj :move (- x (wm-pos-x obj)) (- y (wm-pos-y obj))))
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
                       :on-pointer (lambda (mask lx ly) (glass-term:on-mouse tm mask lx ly))
                       :dirty-p (lambda () (glass-term:terminal-take-dirty tm))
                       :resize-fn (lambda (w h) (glass-term:resize-terminal-px tm w h))
                       :close-fn (lambda () (glass-term:kill-terminal tm))))))

(defun wm-add-tabterm (port &key (cols 80) (rows 24) (ppem 14))
  "A tabbed terminal (several shells, a tab bar) as a WM surface window."
  (let ((tt (glass-term:make-tabbed-terminal :cols cols :rows rows :ppem ppem))
        (c (glass-port-cascade port)))
    (wm-add-surface* port
      (make-wm-surface :fb (glass-term:tabterm-fb tt)
                       :x (+ 40 c) :y (+ 40 c +wm-titleh+) :title "terminal"
                       :on-key (lambda (down k) (glass-term:tabterm-on-key tt down k))
                       :on-pointer (lambda (mask lx ly) (glass-term:tabterm-on-mouse tt mask lx ly))
                       :dirty-p (lambda () (glass-term:tabterm-take-dirty tt))
                       :close-fn (lambda () (glass-term:tabterm-kill tt))))))

(defun %loom-fn (pkg name)
  "The bound function named NAME in package PKG, or NIL — used to reach loom/glass
   by name so glass need not depend on it."
  (let ((p (find-package pkg)))
    (and p (let ((s (find-symbol name p))) (and s (fboundp s) s)))))

(defun wm-browse-default-url ()
  "A generic start page for (:browse) with no URL: loom's bundled home page if
   loom is loadable, else a well-known site."
  (let ((dir (ignore-errors (asdf:system-source-directory '#:loom))))
    (if dir (namestring (merge-pathnames "assets/home.html" dir)) "https://example.com")))

(defun wm-add-browser (port &optional url &key (width 900) (height 620))
  "Open URL (default: a generic start page) as a loom browser surface window:
   loom/glass renders weft into a glass framebuffer, the WM decorates it, and RFB
   input routes to the live page (links navigate in place).  loom/glass is an
   OPTIONAL runtime dependency, resolved by name (no .asd dep — glass must not
   depend on loom, which depends on glass)."
  (unless url (setf url (wm-browse-default-url)))
  (let ((load-url  (%loom-fn '#:loom "LOAD-URL"))
        (load-file (%loom-fn '#:loom "LOAD-FILE"))
        (render    (%loom-fn '#:loom "RENDER-PAGE"))
        (title-of  (%loom-fn '#:loom "PAGE-TITLE"))
        (attach    (%loom-fn '#:loom.glass "ATTACH"))
        (onk       (%loom-fn '#:loom.glass "ON-KEY"))
        (onp       (%loom-fn '#:loom.glass "ON-POINTER"))
        (pump      (%loom-fn '#:loom.glass "PUMP-LOOP"))
        (stop      (%loom-fn '#:loom.glass "STOP")))
    (unless (and attach onk onp pump (or load-url load-file))
      (error "loom/glass not loaded — (ql:quickload :loom/glass)"))
    (let* ((u (string-downcase url))
           (httpp (or (and (>= (length u) 5) (string= u "http:"  :end1 5))
                      (and (>= (length u) 6) (string= u "https:" :end1 6))))
           (page (if httpp (funcall load-url url :width width :viewport-height height)
                     (funcall load-file url :width width :viewport-height height)))
           (fb (glass:make-framebuffer width height (glass:rgb 255 255 255)))
           (c (glass-port-cascade port)))
      (when render (funcall render page))
      (let ((app (funcall attach page fb)))
        (prog1
            (wm-add-surface* port
              (make-wm-surface :fb fb :x (+ 40 c) :y (+ 40 c +wm-titleh+)
                               :title (or (and title-of (funcall title-of page)) "browser")
                               :on-key (lambda (down k) (funcall onk app down k))
                               :on-pointer (lambda (mask lx ly) (funcall onp app mask lx ly))
                               ;; on close, stop weft's render pump (else it re-renders forever)
                               :close-fn (and stop (lambda () (funcall stop app)))))
          (sb-thread:make-thread (lambda () (funcall pump app)) :name "wm-browse-pump"))))))

(defun %read-file-bytes (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((b (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence b s) b)))

(defun %decode-rgba (pkg path)
  "Decode PATH via PKG's decode-image-bytes + img-w/h/rgba (pigment and — for compat
   — weft.render share this API), to (values W H SAMPLER), or NIL if PKG isn't loaded."
  (let ((dec (%loom-fn pkg "DECODE-IMAGE-BYTES")))
    (when dec
      (let ((img (funcall dec (%read-file-bytes path))))
        (unless img (error "~a could not decode ~a" pkg path))
        (let ((w (funcall (%loom-fn pkg "IMG-W") img))
              (h (funcall (%loom-fn pkg "IMG-H") img))
              (rgba (funcall (%loom-fn pkg "IMG-RGBA") img)))     ; w*h*4 straight-alpha
          (values w h (lambda (y x)
                        (let ((o (* (+ (* y w) x) 4)))
                          (values (aref rgba o) (aref rgba (+ o 1)) (aref rgba (+ o 2)) (aref rgba (+ o 3)))))))))))

(defun %decode-image (path)
  "Decode PATH to (values W H SAMPLER), where SAMPLER is (fn Y X) -> (values R G B
   A).  Prefers pigment (our pure-CL PNG/JPEG/GIF/WebP/SVG codecs, split out of weft),
   then weft.render (compat), then opticl — all OPTIONAL runtime deps, by name."
  (cond
    ((%loom-fn '#:pigment "DECODE-IMAGE-BYTES")     (%decode-rgba '#:pigment path))
    ((%loom-fn '#:weft.render "DECODE-IMAGE-BYTES") (%decode-rgba '#:weft.render path))
    ((%loom-fn '#:opticl "READ-IMAGE-FILE")          ; --- opticl (fallback) ---
     (let* ((img (funcall (%loom-fn '#:opticl "READ-IMAGE-FILE") path)) (dims (array-dimensions img))
            (ih (first dims)) (iw (second dims)) (ch (if (cddr dims) (third dims) 1))
            (p16 (equal (array-element-type img) '(unsigned-byte 16))))
       (values iw ih
               (lambda (y x)
                 (flet ((c (k) (let ((v (if (= ch 1) (aref img y x) (aref img y x k)))) (if p16 (ash v -8) v))))
                   (if (= ch 1) (let ((v (c 0))) (values v v v 255))
                       (values (c 0) (c 1) (c 2) (if (>= ch 4) (c 3) 255))))))))
    (t (error "no image decoder — (ql:quickload :pigment) or :opticl"))))

(defun wm-add-image (port path &key (max-w 960) (max-h 660))
  "Open the image file at PATH as a WM surface window — decoded (pigment, else opticl)
   into a glass framebuffer, nearest-neighbour scaled to fit MAX-W x MAX-H, with
   any alpha composited over the dark window background."
  (multiple-value-bind (iw ih samp) (%decode-image path)
    (let* ((scale (min 1 (/ max-w iw) (/ max-h ih)))
           (ow (max 1 (floor (* iw scale)))) (oh (max 1 (floor (* ih scale))))
           (bg 24)
           (fb (glass:make-framebuffer ow oh (glass:rgb bg bg bg)))
           (px (glass:fb-pixels fb)))
      (dotimes (oy oh)                                   ; nearest-neighbour scale
        (let ((sy (min (1- ih) (floor (* oy ih) oh))) (row (* oy ow)))
          (dotimes (ox ow)
            (let ((sx (min (1- iw) (floor (* ox iw) ow))))
              (multiple-value-bind (r g b a) (funcall samp sy sx)
                (setf (aref px (+ row ox))
                      (if (>= a 255) (glass:rgb r g b)
                          (glass:rgb (round (+ (* r a) (* bg (- 255 a))) 255)
                                     (round (+ (* g a) (* bg (- 255 a))) 255)
                                     (round (+ (* b a) (* bg (- 255 a))) 255)))))))))
      (let ((c (glass-port-cascade port)))
        (wm-add-surface* port
          (make-wm-surface :fb fb :x (+ 40 c) :y (+ 40 c +wm-titleh+)
                           :title (file-namestring path)
                           :dirty-p (constantly nil)))))))     ; static: never needs recompositing

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

(defun wm-debug (port form)
  "Evaluate FORM with McCLIM's graphical debugger installed, so any UNHANDLED
   error pops up clim-debugger (condition + restarts + inspectable backtrace) as
   a decorated window — the Genera debugger.  clim-debugger is an OPTIONAL runtime
   dependency, resolved by name.  NB: we must NOT wrap FORM in an error handler
   (that would preempt the debugger); instead we provide a clean ABORT restart."
  (let ((dbg (and (find-package '#:clim-debugger) (find-symbol "DEBUGGER" '#:clim-debugger))))
    (unless (and dbg (fboundp dbg)) (error "clim-debugger not loaded — (ql:quickload :clim-debugger)"))
    (let ((fm (find-frame-manager :port port)))
      (sb-thread:make-thread
       (lambda ()
         (let ((*debugger-hook* dbg)                  ; ANSI hook…
               (sb-ext:*invoke-debugger-hook* dbg)    ; …and SBCL's (takes precedence)
               (climi::*default-frame-manager* fm))   ; land the debugger on OUR port
           (with-simple-restart (abort "Close the debugger")
             (eval form))))
       :name "wm-debug"))))

(defun wm-run-app (port name-string invoker)
  "Host a self-contained CLIM application launched by INVOKER — a thunk that, run
   with our frame-manager as the default, opens the app's frame in THIS thread
   (the clouseau/climacs 'inspect'/':new-process nil' pattern).  Used for apps
   that own their frame rather than exposing a class to make-application-frame."
  (let ((fm (find-frame-manager :port port)))
    (sb-thread:make-thread
     (lambda ()
       (handler-case
           (let ((climi::*default-frame-manager* fm))   ; land on OUR glass port
             (funcall invoker))
         (error (e) (format *trace-output* "~&[wm] ~a: ~a~%" name-string e))))
     :name (format nil "wm-~a" name-string))))

(defun wm-edit (port &optional file)
  "Open Climacs — the McCLIM Emacs-family editor (Zmacs' lineage) — optionally on
   FILE.  Climacs is an OPTIONAL runtime dependency, resolved by name."
  (let ((fn (and (find-package '#:climacs) (find-symbol "CLIMACS" '#:climacs))))
    (unless (and fn (fboundp fn)) (error "climacs not loaded — (ql:quickload :climacs)"))
    (wm-run-app port "climacs"
                (lambda () (if file (funcall fn :new-process nil :buffers (list file))
                               (funcall fn :new-process nil))))))

;;; A window spec is the shared launch vocabulary — used both for run-wm's
;;; initial windows AND for the root-menu items, so a menu is just a list of
;;; labelled specs:
;;;   (:terminal &key cols rows ppem)   a shell terminal (surface window)
;;;   (:tabterm  &key cols rows ppem)   a tabbed terminal
;;;   (:inspect FORM)                   Clouseau inspecting the value of FORM
;;;   (:debug FORM)                     evaluate FORM under the CLIM debugger
;;;   (:edit &optional FILE)            Climacs, the McCLIM editor
;;;   (:browse URL &key width height)   a loom/weft browser window
;;;   (:image PATH &key max-w max-h)    an image (pigment's decoder, else opticl)
;;;   (FRAME-CLASS &key width height)   any McCLIM application frame
(defun wm-spawn-spec (port spec)
  (case (car spec)
    (:terminal (apply #'wm-add-terminal port (cdr spec)))
    (:tabterm  (apply #'wm-add-tabterm  port (cdr spec)))
    (:inspect  (wm-inspect port (cadr spec)))
    (:debug    (wm-debug   port (cadr spec)))
    (:edit     (apply #'wm-edit port (cdr spec)))
    (:browse   (apply #'wm-add-browser port (cdr spec)))
    (:image    (apply #'wm-add-image port (cdr spec)))
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

(defun wm-app-item (label pkg class-name &rest args)
  "A menu item launching McCLIM frame CLASS-NAME in PKG, or NIL if that package/
   class isn't loaded (so the Apps menu only offers what's actually available)."
  (let ((sym (and (find-package pkg) (find-symbol class-name pkg))))
    (and sym (find-class sym nil) (list* label sym args))))

(defun wm-sample-image ()
  "The path of glass's bundled sample image, if present."
  (let ((dir (ignore-errors (asdf:system-source-directory '#:mcclim-glass))))
    (and dir (let ((p (merge-pathnames "assets/sample.png" dir))) (and (probe-file p) (namestring p))))))

(defun wm-default-menu ()
  "The default workspace root menu: generic Browse / Inspect / Debug / Terminal,
   plus an Apps submenu of whatever McCLIM apps are loaded.  Built at call time so
   app class symbols only appear when their packages exist."
  (list*
   '("Browse"   :browse)                                  ; generic start page
   '("Inspect"  :inspect (list-all-packages))             ; generic: the environment
   '("Debug"    :debug (break "Workspace debugger"))      ; generic: enter the debugger
   '("Terminal" :terminal)
   (list
    (list* "Apps" :submenu
           (remove nil
                   (list '("Tabbed Terminal" :tabterm)
                         (wm-app-item "Calculator" '#:clim-demo.calculator "CALCULATOR-APP" :width 360 :height 320)
                         (wm-app-item "Gadget Demo" '#:clim-demo "GADGET-TEST" :width 380 :height 320)
                         (wm-app-item "Listener" '#:clim-listener "LISTENER" :width 720 :height 480)
                         '("Editor (Climacs)" :edit)
                         (let ((img (wm-sample-image))) (and img (list "Image Viewer" :image img)))
                         '("Browse example.com" :browse "https://example.com")))))))

(defun run-wm (specs &key (port 5900) (width 1000) (height 720) menu
                          background (background-mode :cover))
  "Run a mini OPEN LOOK desktop over VNC.  Each spec is a decorated window:
   (FRAME-CLASS &key WIDTH HEIGHT) for a McCLIM app, or (:terminal &key COLS ROWS
   PPEM) for a shell terminal.  Right-click the workspace for a root menu; pass
   MENU — a list of (LABEL . SPEC) labelled window specs (same vocabulary as
   SPECS), or (LABEL . THUNK) for an arbitrary action — to override its items.
   BACKGROUND is a desktop-wallpaper image path (any format pigment decodes, incl.
   SVG), placed per BACKGROUND-MODE (:cover/:fit/:stretch/:center/:tile).  Serves
   on PORT."
  (let ((p (find-glass-port :port port)))
    (setf (glass-port-wm-p p) t
          (glass-port-screen-w p) width (glass-port-screen-h p) height
          (glass-port-fb p) (glass:make-framebuffer width height +wm-teal+)
          (glass-port-menu-items p) (or menu (wm-default-menu)))
    (when background (wm-set-background p background :mode background-mode))
    (start-glass-server p)
    (climi::restart-port p)                                   ; event-loop thread
    (dolist (spec specs)
      (wm-spawn-spec p spec)
      (sleep 0.7)                                             ; stagger for distinct cascade slots
      (composite-all p))
    ;; Surface windows (terminals) render asynchronously in their own threads.
    ;; DAMAGE TRACKING: only recomposite when a surface actually changed (its
    ;; dirty-p reports so and clears) — an idle desktop does ZERO compositing, so
    ;; no wasted full-screen redraws.  A NIL dirty-p means "always redraw" (safe
    ;; default); a static surface (image) reports NIL forever.  WM operations
    ;; (move/resize/menu/...) recomposite directly, so they're not gated here.
    (loop (sleep 1/60)
          (let ((boxes '()) (full nil))
            (dolist (s (glass-port-surfaces p))
              (cond ((null (wm-surface-dirty-p s)) (setf full t))        ; unknown extent -> whole screen
                    ((funcall (wm-surface-dirty-p s)) (push (wm-window-box s) boxes))))
            ;; McCLIM app repaints accumulated by present-mirror since the last tick,
            ;; coalesced into ONE composite here (a burst of ~20 repaints -> 1 paint)
            (let ((pend (port-take-pending p)))
              (cond ((eq pend :full) (setf full t))
                    (pend (push pend boxes))))
            (when (or full boxes)
              ;; damage only the changed windows (a full-screen surface with no
              ;; dirty-p, or a :full McCLIM repaint, forces a whole-screen recomposite)
              (ignore-errors (composite-all p (unless full (wm-box-union boxes)))))))))
