;;;; src/wm/chrome.lisp — how the window manager looks: density, title bars, blits, menus.
;;;;
;;;; Copied from backend/wm.lisp and backend/backend.lisp (the McCLIM desktop), which drew all of
;;;; this with glass primitives and scribe already.  What changed: no McCLIM mirrors, so no
;;;; per-mirror decoration cache and no holder-tinted title bar; and a CLOSE BOX at the right of
;;;; the title bar, which OPEN LOOK never had (Quit lives on the wedge's Window Menu) but which a
;;;; touch screen wants -- the lean desk had one, and a phone has no other way to close a window
;;;; in one tap.

(in-package #:glass.wm)

;;; ---- density -------------------------------------------------------------------------------

(defconstant +wm-titleh-1x+ 22 "OPEN LOOK title-bar height, in DESIGN pixels (1x).")
(defconstant +wm-border-1x+ 1  "Window border thickness, in DESIGN pixels (1x).")
(defconstant +menu-itemh-1x+ 20 "Height of one menu item, in DESIGN pixels (1x).")
(defconstant +menu-titleh-1x+ 20 "Height of the menu title strip, in DESIGN pixels (1x).")

(defvar *wm-scale* 1
  "The density the chrome is drawn and hit-tested at: 1 is the McCLIM desktop's 1x.  A host on
   a dense screen (a phone at one desk pixel per device pixel) sets it to 2 for touch-sized
   title bars and menu rows.  Drawing and hit testing both read it, so they cannot disagree.")

(declaim (inline wm-titleh wm-border))
(defun wm-titleh () (max 1 (round (* +wm-titleh-1x+ *wm-scale*))))
(defun wm-border () (max 1 (round (* +wm-border-1x+ *wm-scale*))))
(defun wm-size (n) "A chrome text size N at the current density." (max 1 (round (* n *wm-scale*))))
(defun menu-itemh () (max 1 (round (* +menu-itemh-1x+ *wm-scale*))))
(defun menu-titleh () (max 1 (round (* +menu-titleh-1x+ *wm-scale*))))

;;; ---- colours ---------------------------------------------------------------------------------

(defparameter +wm-teal+ (glass:rgb 61 122 138) "The Sun workspace background.")
(defparameter *wm-bg* +wm-teal+ "The workspace colour (milestone 1 has no wallpaper).")
(defparameter +wm-title-bg+ (glass:rgb 204 204 204) "OPEN LOOK title-bar grey.")
(defparameter +menu-bg+ (glass:rgb 208 208 208) "Menu background grey.")
(defparameter +menu-title-bg+ (glass:rgb 188 188 188) "Menu title strip.")
(defparameter +menu-hi+ (glass:rgb 61 122 138) "Highlighted item (teal).")

;;; ---- the title bar ---------------------------------------------------------------------------

(defun wm-menu-button-box ()
  "The window-menu wedge's (values X Y SIZE) inside a title bar, at the current density.  One
   definition, read by the renderer and the hit test, so the two cannot drift."
  (let* ((h (wm-titleh))
         (pad (max 2 (round (* 4 *wm-scale*))))
         (size (max 4 (- h (* 2 pad)))))
    (values pad pad size)))

(defun wm-close-button-box (width)
  "The close box's (values X Y SIZE) inside a title bar WIDTH wide: the wedge's square, mirrored
   to the right-hand end."
  (multiple-value-bind (pad py size) (wm-menu-button-box)
    (values (- width pad size) py size)))

(defun %bevel (tb bx by bs)
  "A raised OPEN LOOK button square at (BX,BY), BS on a side."
  (glass:fb-rect tb bx by bs bs (glass:rgb 188 188 188))
  (glass:fb-hline tb bx by bs glass:+white+)
  (glass:fb-vline tb bx by bs glass:+white+)
  (glass:fb-hline tb bx (+ by bs -1) bs (glass:rgb 77 77 77))
  (glass:fb-vline tb (+ bx bs -1) by bs (glass:rgb 77 77 77)))

(defun wm-render-titlebar (title width &key (close t))
  "A framebuffer of an OPEN LOOK title bar WIDTH px wide: the window-menu wedge at the left, the
   bold title centred, and (CLOSE) a close box at the right.  Glass primitives and scribe only."
  (let* ((h (wm-titleh))
         (tb (glass:make-framebuffer (max 1 width) h +wm-title-bg+)))
    (glass:fb-hline tb 0 (1- h) width (glass:rgb 120 120 120))                ; bottom shadow line
    (multiple-value-bind (bx by bs) (wm-menu-button-box)
      (%bevel tb bx by bs)
      ;; the wedge, sized from the box
      (let* ((wh (max 2 (round (* 4 *wm-scale*))))
             (ww (max 3 (1- (* 2 wh))))
             (cx (+ bx (floor bs 2)))
             (cy (+ by (floor (- bs wh) 2))))
        (dotimes (i wh)
          (glass:fb-hline tb (- cx (floor (- ww (* 2 i)) 2)) (+ cy i)
                          (max 1 (- ww (* 2 i))) glass:+black+))))
    (when (and close (> width (* 4 h)))
      (multiple-value-bind (bx by bs) (wm-close-button-box width)
        (%bevel tb bx by bs)
        ;; an X, inset a quarter of the box, a stroke as thick as the density
        (let* ((in (max 2 (floor bs 4))) (n (- bs (* 2 in))) (th (max 1 (round *wm-scale*))))
          (dotimes (i n)
            (glass:fb-rect tb (+ bx in i) (+ by in i) th th glass:+black+)
            (glass:fb-rect tb (+ bx in (- n 1 i)) (+ by in i) th th glass:+black+)))))
    (let* ((size (wm-size 12))
           (font (glass:default-font t))
           (tw (glass:text-width title :size size :font font))
           (ty (max 0 (floor (- h size) 2))))
      (glass:fb-text tb (max (+ h (round (* 6 *wm-scale*))) (floor (- width tw) 2)) ty title
                     :size size :color glass:+black+ :font font))
    tb))

;;; ---- blits -----------------------------------------------------------------------------------

(defun blit-fb (src ox oy dst)
  "Copy framebuffer SRC into DST with its top-left at (OX,OY), honouring DST's clip.  Each visible
   row is one REPLACE.  A non-:XRGB framebuffer on either side goes through FB-BLIT-CLIPPED."
  (unless (and (eq (glass:fb-format src) :xrgb) (eq (glass:fb-format dst) :xrgb))
    (return-from blit-fb (glass:fb-blit-clipped dst src ox oy)))
  (locally (declare (optimize (speed 3) (safety 0)))
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
          (when (and (< -1 dy dh) (<= cy0 dy) (< dy cy1))
            (let* ((drow (* dy dw)) (srow (* sy sw))
                   (dx0 (max 0 ox cx0)) (dx1 (min dw (+ ox sw) cx1)))
              (declare (fixnum drow srow dx0 dx1))
              (when (< dx0 dx1)
                (replace dpx spx :start1 (+ drow dx0) :end1 (+ drow dx1)
                                 :start2 (+ srow (- dx0 ox)))))))))))

(defun wm-corners (fb x y w h)
  "OPEN LOOK L-shaped resize corner marks."
  (let ((n (max 7 (round (* 7 *wm-scale*)))) (c glass:+black+))
    (flet ((h* (px py len) (glass:fb-rect fb px py len 1 c))
           (v* (px py len) (glass:fb-rect fb px py 1 len c)))
      (h* x y n) (v* x y n)
      (h* (- (+ x w) n) y n) (v* (- (+ x w) 1) y n)
      (h* x (- (+ y h) 1) n) (v* x (- (+ y h) n) n)
      (h* (- (+ x w) n) (- (+ y h) 1) n) (v* (- (+ x w) 1) (- (+ y h) n) n))))

(defun wm-frame (fb cx cy cw ch deco content-fn)
  "Draw a decorated window: DECO title bar above the content at (CX,CY) size (CW,CH), the content
   via CONTENT-FN, then the border and the corner marks."
  (let* ((ty (- cy (wm-titleh))) (wx (- cx (wm-border))) (wy (- ty (wm-border)))
         (ww (+ cw (* 2 (wm-border)))) (wh (+ (wm-titleh) ch (* 2 (wm-border)))))
    (blit-fb deco cx ty fb)
    (funcall content-fn)
    (glass:fb-frame fb wx wy ww wh glass:+black+ (wm-border))
    (wm-corners fb wx wy ww wh)))

;;; ---- menus -----------------------------------------------------------------------------------
;;; Right-press the bare workspace for the root menu; the title-bar wedge for the Window Menu.  It
;;; follows the pointer with a hover highlight; RELEASING on an item runs it.  An item whose
;;; action is (:submenu ITEM...) opens a child menu to its right on hover.

(defstruct wm-menu
  (x 0) (y 0) (hover -1) (title "Workspace") items fb (child nil)
  (scale 1)          ; the density the FB was drawn at, so the hit test divides by the same rows
  (last-mask 0)      ; the buttons as of the previous event, for edges
  ;; Has the click that OPENED this menu been let go?  Until then a release where it was pressed
  ;; is that click's tail and does nothing: the wedge sits above the Window Menu, so a click on
  ;; it releases outside (which would dismiss), and a menu clamped near the screen's edge puts an
  ;; item under the pointer (which would choose it).  A tap opens a menu and leaves it open,
  ;; which a touch screen needs; press-drag-release -- the release somewhere else -- still works.
  (armed nil)
  (origin nil))      ; (x . y) of the press that opened it

(defun wm-submenu-p (action) (and (consp action) (eq (car action) :submenu)))
(defun wm-item-action (item) (cdr item))
(defun wm-menu-chain (root) (loop for m = root then (wm-menu-child m) while m collect m))

(defun wm-menu-width (menu)
  (let* ((font (glass:default-font t))
         (w (+ (round (* 24 *wm-scale*))
               (glass:text-width (wm-menu-title menu) :size (wm-size 12) :font font))))
    (dolist (it (wm-menu-items menu) (max (round (* 108 *wm-scale*)) w))
      (let ((pad (round (* (if (wm-submenu-p (wm-item-action it)) 46 28) *wm-scale*))))
        (setf w (max w (+ pad (glass:text-width (car it) :size (wm-size 12) :font font))))))))

(defun wm-submenu-arrow (fb x y color)
  (let* ((half (max 2 (round (* 4 *wm-scale*)))) (hgt (1+ (* 2 half))))
    (dotimes (k (1+ half))
      (glass:fb-vline fb (+ x k) (+ y k) (max 1 (- hgt (* 2 k))) color))))

(defun wm-menu-render (menu)
  "(Re)build MENU's framebuffer with the current hover highlight."
  (let* ((n (length (wm-menu-items menu)))
         (w (wm-menu-width menu))
         (ih (menu-itemh)) (th (menu-titleh))
         (size (wm-size 12))
         (h (+ th (* (max n 1) ih)))
         (fb (glass:make-framebuffer w h +menu-bg+))
         (font (glass:default-font t))
         (padx (round (* 8 *wm-scale*)))
         (itemx (round (* 14 *wm-scale*))))
    (setf (wm-menu-scale menu) *wm-scale*)
    (glass:fb-rect fb 0 0 w th +menu-title-bg+)
    (glass:fb-text fb padx (max 0 (floor (- th size) 2)) (wm-menu-title menu)
                   :size size :color glass:+black+ :font font)
    (glass:fb-hline fb 0 (1- th) w (glass:rgb 120 120 120))
    (if (zerop n)
        (glass:fb-text fb itemx (+ th (max 0 (floor (- ih size) 2))) "(nothing)"
                       :size size :color (glass:rgb 90 90 90) :font font)
        (loop for it in (wm-menu-items menu) for i from 0
              for yy = (+ th (* i ih))
              for hot = (= i (wm-menu-hover menu))
              for ink = (if hot glass:+white+ glass:+black+)
              do (when hot (glass:fb-rect fb 1 yy (- w 2) ih +menu-hi+))
                 (glass:fb-text fb itemx (+ yy (max 0 (floor (- ih size) 2))) (car it)
                                :size size :color ink :font font)
                 (when (wm-submenu-p (wm-item-action it))
                   (wm-submenu-arrow fb (- w (round (* 13 *wm-scale*)))
                                     (+ yy (floor (- ih (round (* 9 *wm-scale*))) 2)) ink))))
    (glass:fb-frame fb 0 0 w h glass:+black+ (wm-border))
    (setf (wm-menu-fb menu) fb)))

(defun wm-menu-index (menu x y)
  "For screen (X,Y): an item index, :TITLE over the title strip, or :OUTSIDE -- measured at the
   density MENU was rendered at."
  (let* ((*wm-scale* (wm-menu-scale menu))
         (mx (wm-menu-x menu)) (my (wm-menu-y menu)) (fb (wm-menu-fb menu)))
    (if (and (<= mx x (+ mx (glass:fb-width fb) -1)) (<= my y (+ my (glass:fb-height fb) -1)))
        (let ((yl (- y my)))
          (if (< yl (menu-titleh)) :title
              (let ((i (floor (- yl (menu-titleh)) (menu-itemh))))
                (if (< i (length (wm-menu-items menu))) i :title))))
        :outside)))

(defun wm-menu-box (menu)
  (list (wm-menu-x menu) (wm-menu-y menu)
        (glass:fb-width (wm-menu-fb menu)) (glass:fb-height (wm-menu-fb menu))))

(defun wm-place-menu (menu sw sh x y &optional (mask 0))
  "Render MENU and put it near (X,Y), kept inside a SW x SH screen.  MASK is the buttons as the
   menu opens: without it the first held motion looks like a fresh press (see backend/wm.lisp's
   %SEED-MENU-MASK)."
  (wm-menu-render menu)
  (setf (wm-menu-x menu) (max 0 (min x (- sw (glass:fb-width (wm-menu-fb menu)))))
        (wm-menu-y menu) (max 0 (min y (- sh (glass:fb-height (wm-menu-fb menu)))))
        (wm-menu-last-mask menu) mask
        (wm-menu-origin menu) (cons x y))
  menu)
