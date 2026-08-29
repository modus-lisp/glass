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

(defparameter +wm-title-bg+ (glass:rgb 204 204 204) "OPEN LOOK title-bar grey.")
(defparameter +wm-title-other-bg+ (glass:rgb 178 188 204)
  "Title-bar grey for a CLIM window ON A SEAT THAT IS NOT DRIVING McCLIM — the holder
   indicator (*CLIM-TOKEN-INDICATOR*, off by default).  The same value cooled towards
   blue: enough to read as a state at a glance, not enough to look like a different
   window manager.  It tints only the title bar, and only on the screens of the people
   who are NOT driving, so the person actually using the application sees the desktop
   they have always seen.")

(defun wm-render-titlebar (title width &optional (bg +wm-title-bg+))
  "A glass framebuffer of an OPEN LOOK title bar WIDTH px wide — drawn entirely
   with glass primitives + scribe text (no McCLIM).

   CENTRED BY ARITHMETIC, NOT BY A CONSTANT.  The title used to sit at y=3, which is
   centred for a 22-pixel bar holding 12-pixel text and is the top of a 44-pixel bar
   holding 24-pixel text.  Scaling that 3 would only move the wrongness around, because
   the number was never a position — it was the answer to a centring sum done once, by
   hand, for one size.  So the sum is written down instead, and every density gets it
   right including the fractional ones where no single constant could."
  (let* ((h (wm-titleh))
         (pad (max 2 (round (* 4 *wm-scale*))))                ; the bevel's inset
         (tb (glass:make-framebuffer (max 1 width) h bg)))
    (glass:fb-hline tb 0 (1- h) width (glass:rgb 120 120 120))                  ; bottom shadow line
    ;; menu button box: raised bevel + abbreviated-menu wedge
    (let* ((bs (max 4 (- h (* 2 pad)))) (bx pad) (by pad))
      (glass:fb-rect tb bx by bs bs (glass:rgb 188 188 188))
      (glass:fb-hline tb bx by bs glass:+white+)                                ; top light
      (glass:fb-vline tb bx by bs glass:+white+)                               ; left light
      (glass:fb-hline tb bx (+ by bs -1) bs (glass:rgb 77 77 77))              ; bottom dark
      (glass:fb-vline tb (+ bx bs -1) by bs (glass:rgb 77 77 77))             ; right dark
      ;; the wedge, sized from the box rather than from 4 and 3 and 7 — same reasoning as
      ;; the title: those were one size's answer, and a 2x box wants a 2x wedge.
      (let* ((wh (max 2 (round (* 4 *wm-scale*))))            ; wedge height in rows
             (ww (max 3 (1- (* 2 wh))))                       ; and its width at the top
             (cx (+ bx (floor bs 2)))
             (cy (+ by (floor (- bs wh) 2))))
        (dotimes (i wh)
          (glass:fb-hline tb (- cx (floor (- ww (* 2 i)) 2)) (+ cy i)
                          (max 1 (- ww (* 2 i))) glass:+black+))))
    ;; centred bold title, anti-aliased via scribe — centred in BOTH axes now
    (let* ((size (wm-size 12))
           (tw (glass:text-width title :size size :font (glass:default-font t)))
           ;; scribe's y is the top of the em box, so the ink sits a little below it; the
           ;; ascent is about 4/5 of the size, which is what makes the visual centre and
           ;; the box centre differ.  Splitting the leftover space is close enough to right
           ;; that it holds at every scale, which a hand-tuned offset did not.
           (ty (max 0 (floor (- h size) 2))))
      (glass:fb-text tb (max (+ h (round (* 6 *wm-scale*))) (floor (- width tw) 2)) ty title
                     :size size :color glass:+black+ :font (glass:default-font t)))
    tb))

(defun wm-deco (mirror cw &optional other)
  "Cached title-bar framebuffer for MIRROR at content width CW.  With OTHER, the tinted
   one a seat that is not driving McCLIM sees.  Both are cached and a width change drops
   both; the tinted one is never rendered at all unless somebody asks for it, which with
   the indicator off is never."
  (unless (eql cw (glass-mirror-deco-w mirror))
    (setf (glass-mirror-deco mirror) nil
          (glass-mirror-deco-other mirror) nil
          (glass-mirror-deco-w mirror) cw))
  (if other
      (or (glass-mirror-deco-other mirror)
          (setf (glass-mirror-deco-other mirror)
                (wm-render-titlebar (glass-mirror-title mirror) cw +wm-title-other-bg+)))
      (or (glass-mirror-deco mirror)
          (setf (glass-mirror-deco mirror)
                (wm-render-titlebar (glass-mirror-title mirror) cw)))))

(defun wm-driven-elsewhere-p (seat)
  "Should SEAT's copy of a CLIM window's title bar say that somebody ELSE has the
   application?  Always NIL unless *CLIM-TOKEN-INDICATOR* is on, and NIL for the driver
   and for a free token — nobody driving is not somebody else driving."
  (and *clim-token-indicator*
       seat (seat-port seat)
       (clim-token-elsewhere-p (seat-port seat) seat)
       t))

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
;;;
;;; A CLASS rather than a structure, and deliberately: a desktop runs for weeks and
;;; grows new slots while it runs.  Redefining a structure strands every instance
;;; already made — the live windows on a running desktop become obsolete objects and
;;; the compositor dies on the next tick — whereas redefining a class migrates them
;;; through UPDATE-INSTANCE-FOR-REDEFINED-CLASS, so a running desktop simply keeps
;;; going with the new slot unbound-or-defaulted.  The accessor and constructor names
;;; are the ones a DEFSTRUCT would have made, so nothing else changes.
(defclass wm-surface ()
  ((fb        :initarg :fb        :initform nil        :accessor wm-surface-fb)
   (x         :initarg :x         :initform 60         :accessor wm-surface-x)
   (y         :initarg :y         :initform 60         :accessor wm-surface-y)
   (title     :initarg :title     :initform "window"   :accessor wm-surface-title)
   (deco      :initarg :deco      :initform nil        :accessor wm-surface-deco)
   (deco-w    :initarg :deco-w    :initform -1         :accessor wm-surface-deco-w)
   (on-key    :initarg :on-key    :initform nil        :accessor wm-surface-on-key)
   (on-pointer :initarg :on-pointer :initform nil      :accessor wm-surface-on-pointer)
   ;; ()->bool: did the content fb change?  (nil = no answer, so always redraw)
   (dirty-p   :initarg :dirty-p   :initform nil        :accessor wm-surface-dirty-p)
   ;; ()->(sx sy dx dy w h) | nil : how the content TRANSLATED, in surface-local pixels,
   ;; since this was last called — a scrolling window moves a block of its own pixels by
   ;; a fixed offset, and the compositor can turn that into a CopyRect instead of
   ;; re-encoding the window.  CONSUMING, exactly like DIRTY-P: each call reports the
   ;; change since the previous one, so a hint is never replayed against pixels it no
   ;; longer describes.  NIL (the default, and every non-scrolling window) means "I never
   ;; translate anything" — the compositor just diffs, which is always correct.
   (copy-p    :initarg :copy-p    :initform nil        :accessor wm-surface-copy-p)
   ;; The content rect (x y w h) as of the last composite that redrew this window WHOLE.
   ;; A translation may only be believed when the screen still holds that same rect: if
   ;; the window moved, resized, or was last drawn under a clip that cut it, the pixels a
   ;; copy would read are not the ones the hint is about.  Compositor-owned.
   ;;
   ;; VESTIGIAL as of seats: the claim is about ONE SCREEN's pixels, so it moved to the
   ;; seat (SEAT-COPY-BASE).  The slot stays because it is initarg-visible and somebody
   ;; may read it; nothing in the compositor writes it any more.
   (copy-base :initarg :copy-base :initform nil        :accessor wm-surface-copy-base)
   ;; This round's take of COPY-P, so that every seat judges the SAME translation instead
   ;; of the first one consuming it — see WM-SURFACE-ROUND-HINT.
   (copy-round :initform nil :accessor wm-surface-copy-round)
   (copy-hint  :initform nil :accessor wm-surface-copy-hint)
   ;; (px-w px-h)->() : resize the content, or nil = not resizable
   (resize-fn :initarg :resize-fn :initform nil        :accessor wm-surface-resize-fn)
   ;; ()->() : tear down the content on window close
   (close-fn  :initarg :close-fn  :initform nil        :accessor wm-surface-close-fn)
   ;; (x y w h) saved by Full Size, for Restore Size
   (saved-geom :initarg :saved-geom :initform nil      :accessor wm-surface-saved-geom)
   ;; ()->string|nil : the text this window has SELECTED RIGHT NOW — the live
   ;; highlight, asked for at the moment somebody wants to act on it, and never the
   ;; clipboard.  A clipboard outlives the highlight by design (that is what makes
   ;; paste-later work), so reading one to answer "what is selected?" answers a
   ;; different question and eventually a stale one.  NIL (the default, and every
   ;; window with no notion of a selection) means "I never have one", which is what
   ;; keeps the selection context menu off windows with nothing to offer it.
   (selection-fn :initarg :selection-fn :initform nil  :accessor wm-surface-selection-fn)
   ;; consecutive per-frame poll/draw errors (see WM-NOTE-SURFACE-ERROR)
   (err-count :initarg :err-count :initform 0          :accessor wm-surface-err-count)
   ;; Place in the ONE stacking order shared with McCLIM windows.  Same slot, same
   ;; accessor name as GLASS-MIRROR's, so WM-STACKING-ORDER can order a list holding
   ;; both kinds without a single type test.
   (z         :initarg :z         :initform 0          :accessor wm-window-z)))

;;; A surface's OWN geometry — the position and stacking every seat sees until it
;;; arranges the window for itself.  GLASS-MIRROR answers the same three questions in
;;; backend.lisp, which is what lets a seat hold either species without a type test.
(defmethod window-own-x ((s wm-surface)) (wm-surface-x s))
(defmethod (setf window-own-x) (v (s wm-surface)) (setf (wm-surface-x s) v))
(defmethod window-own-y ((s wm-surface)) (wm-surface-y s))
(defmethod (setf window-own-y) (v (s wm-surface)) (setf (wm-surface-y s) v))
(defmethod window-own-z ((s wm-surface)) (wm-window-z s))
(defmethod (setf window-own-z) (v (s wm-surface)) (setf (wm-window-z s) v))

(defun make-wm-surface (&rest initargs)
  "Make a surface window.  Keyword-for-keyword what the DEFSTRUCT constructor took,
   so every call site reads the same as before the class conversion."
  (apply #'make-instance 'wm-surface initargs))

(defun wm-surface-p (object) (typep object 'wm-surface))

(defmethod print-object ((surf wm-surface) stream)
  (print-unreadable-object (surf stream :type t :identity t)
    (format stream "~s ~d,~d" (slot-value surf 'title)
            (slot-value surf 'x) (slot-value surf 'y))))

(defparameter *wm-surface-error-limit* 60
  "Consecutive per-frame errors from a surface's dirty-p poll or draw that the
   compositor tolerates before dropping the surface.  A misbehaving window is skipped
   each frame and, if it keeps failing (~1s at 60fps), removed — so one broken window
   can never wedge or kill the desktop.  A surface that recovers has its count reset.")

(defun wm-note-surface-error (port surf e)
  "Record a per-frame error from SURF's poll/draw: log it, count it, and once it has
   failed *WM-SURFACE-ERROR-LIMIT* frames in a row, drop it from the compositor so the
   loop stays alive.  Never signals (called from inside the compositor's guards)."
  (ignore-errors
   (format *trace-output* "~&[wm] surface ~s poll/draw error: ~a~%"
           (ignore-errors (wm-surface-title surf)) e))
  (when (>= (incf (wm-surface-err-count surf)) *wm-surface-error-limit*)
    (ignore-errors
     (setf (glass-port-surfaces port) (remove surf (glass-port-surfaces port)))
     ;; The window is gone for everybody, so every seat's keyboard comes off it and no
     ;; seat keeps a view of it.
     (dolist (seat (glass-port-seats port))
       (when (eq (seat-focus-surface seat) surf) (setf (seat-focus-surface seat) nil)))
     (port-forget-window port surf)
     (when (wm-surface-close-fn surf) (ignore-errors (funcall (wm-surface-close-fn surf)))))))

(defun wm-surface-deco* (surf cw)
  (when (or (null (wm-surface-deco surf)) (/= cw (wm-surface-deco-w surf)))
    (setf (wm-surface-deco surf) (wm-render-titlebar (wm-surface-title surf) cw)
          (wm-surface-deco-w surf) cw))
  (wm-surface-deco surf))

(defun wm-frame (fb cx cy cw ch deco content-fn)
  "Draw a decorated window: DECO title bar above the content at (cx,cy) size
   (cw,ch), the content via CONTENT-FN, then a border + corner marks."
  (let* ((ty (- cy (wm-titleh))) (wx (- cx (wm-border))) (wy (- ty (wm-border)))
         (ww (+ cw (* 2 (wm-border)))) (wh (+ (wm-titleh) ch (* 2 (wm-border)))))
    (blit-fb deco cx ty fb)
    (funcall content-fn)
    (glass:fb-frame fb wx wy ww wh glass:+black+ (wm-border))
    (wm-corners fb wx wy ww wh)))

(defun wm-draw-window (mirror fb &optional seat)
  (when-let ((image (mcclim-render::image-mirror-image mirror)))
    (multiple-value-bind (cw ch) (image-wh image)
      (wm-frame fb (seat-window-x seat mirror) (seat-window-y seat mirror) cw ch
                (wm-deco mirror cw (wm-driven-elsewhere-p seat))
                (lambda () (blit-mirror mirror fb seat))))))

;;; ---- scroll CopyRect: a surface's content translation, mapped onto the screen ----
;;;
;;; A window whose content merely SCROLLED has moved a block of its own pixels by a
;;; fixed offset, and the screen framebuffer already holds that block — so the update
;;; can go out as one CopyRect plus the newly exposed strip instead of re-encoding the
;;; whole window.  The window reports the translation in ITS OWN coordinates (see the
;;; COPY-P slot); the compositor's job is the part only it can know: where that block
;;; lands on the screen, and whether the screen pixels there are still exclusively this
;;; window's.  Because a CopyRect moves whatever is ON THE SCREEN in the rectangle —
;;; including a terminal stacked on top, an open menu, or the desktop showing through
;;; past the screen edge — every doubtful case is REFUSED.  Refusing is free of
;;; consequence: the ordinary diff then re-encodes the region, which is always correct.
;;;
;;; Two of those checks do different jobs, and it is worth keeping them apart.  Clipping
;;; the copy into the window's own visible content, the screen, AND THIS COMPOSITE'S
;;; DAMAGE BOX is load-bearing: the RFB sender applies the copy to its picture of the
;;; client and then re-diffs only the damage box, so a copy that reached outside it
;;; would leave pixels nobody ever corrects.  The z-order check on top of that is a
;;; policy: a copy that drags an overlapping window along lands inside the damage box,
;;; so the diff does repair it in the very same update — measurably, at about twice the
;;; bytes of letting the copy through (see *WM-COPYRECT-OCCLUSION-GUARD*).  It is on by
;;; default because it is right without depending on that argument, or on a client
;;; implementing an overlapping CopyRect the way we model it.

(defparameter *wm-scroll-copyrect* t
  "Honour a surface window's scroll CopyRect hint?  NIL falls every window back to the
   plain damage diff — always correct, just more bytes — which is both the emergency
   switch and the control arm when measuring what the hint is worth.  Live-tunable.")

(defparameter *wm-copyrect-occlusion-guard* t
  "Refuse a scroll CopyRect whose moved rectangle has another window or an open menu
   stacked over it?  The copy would drag those pixels along; the sender's diff then
   repairs them in the same update, so what this buys is not correctness but not having
   to rely on that repair.  It is not free: on a 1280x800 desktop with a terminal over a
   scrolling browser, refusing measured ~417 KB/frame against ~213 KB/frame for letting
   the copy through (both pixel-identical on the client).  Refusing is never worse than
   the no-CopyRect behaviour it falls back to.  Live-tunable.

   NB this parameter governs the WIRE copy only.  The same translation done in the
   SCREEN framebuffer (*WM-SCROLL-STRIP*) is guarded unconditionally, because there the
   argument above does not hold — see WM-COMPOSITE-SCROLL.")

(defparameter *wm-scroll-strip* t
  "Composite a scrolling window by TRANSLATING what the screen already holds and
   redrawing only the strip the translation exposed, instead of blitting the whole
   window again?  The same move the wire gets as a CopyRect, done in RAM: the screen
   already holds those pixels, so re-reading them out of the window's framebuffer is
   the second time the same copy is paid.  NIL restores the plain whole-window blit,
   which is always correct.  Live-tunable.")

(defparameter *wm-skip-covered-background* t
  "Skip the desktop background under a region an opaque window completely covers?
   The wallpaper (or the flat teal) is drawn first and every pixel of it inside a window
   is then overwritten before anybody sees it — so for the common damage box, ONE
   window's own rectangle, drawing it is pure waste.  NIL always draws it.
   Live-tunable.")

(defun wm-box-intersect (a b)
  "Intersection of two (x y w h) boxes, or NIL when they don't overlap."
  (when (and a b)
    (destructuring-bind (ax ay aw ah) a
      (destructuring-bind (bx by bw bh) b
        (let ((x0 (max ax bx)) (y0 (max ay by))
              (x1 (min (+ ax aw) (+ bx bw))) (y1 (min (+ ay ah) (+ by bh))))
          (when (and (< x0 x1) (< y0 y1)) (list x0 y0 (- x1 x0) (- y1 y0))))))))

(defun wm-boxes-overlap-p (a b) (and (wm-box-intersect a b) t))

(defun wm-box-difference (a b)
  "The parts of box A that box B does not cover, as up to four rectangles: the bands
   above and below B (full width of A) plus the bands left and right of it (height of B).
   B need not lie inside A — it is intersected first — and a B that misses A entirely
   leaves A whole.  Used to redraw exactly what a translation did NOT carry."
  (let ((i (wm-box-intersect a b)))
    (if (null i)
        (list a)
        (destructuring-bind (ax ay aw ah) a
          (destructuring-bind (bx by bw bh) i
            (let ((out '()))
              (when (> by ay) (push (list ax ay aw (- by ay)) out))
              (when (< (+ by bh) (+ ay ah))
                (push (list ax (+ by bh) aw (- (+ ay ah) by bh)) out))
              (when (> bx ax) (push (list ax by (- bx ax) bh) out))
              (when (< (+ bx bw) (+ ax aw))
                (push (list (+ bx bw) by (- (+ ax aw) bx bw) bh) out))
              out))))))

(defun wm-clip-box (fb)
  "FB's clip rectangle as an (x y w h) box — the region this composite is responsible
   for — or NIL when it is unclipped, i.e. the whole screen is being rebuilt."
  (when-let ((c (glass:fb-clip fb)))
    (list (first c) (second c) (- (third c) (first c)) (- (fourth c) (second c)))))

(defun wm-box-inside-p (inner outer)
  "Is box INNER entirely within box OUTER?"
  (and inner outer (equal inner (wm-box-intersect inner outer))))

;;; ---- one z-order ------------------------------------------------------------
;;;
;;; There used to be two: the McCLIM mirrors were composited, and then the surface
;;; windows were composited on top of them, unconditionally.  Two stacks is not a
;;; z-order — it is two z-orders plus a rule about which one wins, and no amount of
;;; clicking can argue with the rule.  A McCLIM window could not be raised above a
;;; terminal, however recently you had touched it, because "above" was decided by what
;;; kind of window it was.  Both kinds now carry a Z and there is one order.

(defun wm-stacking-order (port &optional seat)
  "Every window on the screen, topmost first, AS SEAT SEES IT.  THE answer to what is
   above what — asked by the compositor, the pointer, and the occlusion guards, so that
   the pixels, the clicks and the CopyRect refusals cannot disagree about the stack.

   WHICH windows exist is the session's; WHERE THEY STAND is the seat's.  So membership
   comes from the port's MIRRORS and SURFACES, unchanged, and only the sort key is the
   seat's — SEAT-WINDOW-Z, which answers with the window's own ticket for any window
   this seat has never restacked.  A session with one seat therefore sorts by exactly
   the same numbers it always did.

   Unmanaged McCLIM mirrors — pull-down menus, submenus, tooltips — are not in the
   shared order at all: they sit above it, always.  They are transient parts of the
   window that opened them and are dismissed by the next click, and a menu that can be
   covered is not a menu.  Under the old two-stack rule they were the same bug in a
   worse form, since a pull-down opened over a terminal rendered UNDER the terminal."
  (let ((mirrors (glass-port-mirrors port)))
    (append (remove-if #'glass-mirror-managed mirrors)               ; pop-up tier, on top
            (sort (append (remove-if-not #'glass-mirror-managed mirrors)
                          (copy-list (glass-port-surfaces port)))
                  #'> :key (lambda (w) (seat-window-z seat w))))))

(defun wm-topmost (port &optional seat)
  "The frontmost window a click can raise FOR SEAT: the top of that seat's order,
   skipping the pop-up tier, which nobody raises and which is gone by the time the
   click lands."
  (find-if (lambda (w) (or (wm-surface-p w) (glass-mirror-managed w)))
           (wm-stacking-order port seat)))

(defun wm-boxes-above (port obj &optional seat)
  "The (x y w h) boxes of everything WM-COMPOSITE draws AFTER OBJ — i.e. everything
   whose pixels can sit on top of OBJ's on the screen.  WM-STACKING-ORDER is topmost
   first, so the windows listed BEFORE OBJ are the ones above it, of either kind; the
   drag wireframe and the open menu chain go on last of all.  A window that somehow
   isn't in the order yields the whole list, which errs towards refusing.

   This used to be WM-BOXES-ABOVE-SURFACE and used to say `every McCLIM window is drawn
   before any surface, so none is ever above one'.  That was true, and it was the bug:
   it is what made a McCLIM window unable to come to the front.  Now that a mirror CAN
   be above a surface, it has to be counted here, or a terminal scrolling under a text
   window would CopyRect the window's pixels along with its own.

   Every box here is in SEAT's screen coordinates and the order is SEAT's, because
   occlusion is: the same two windows overlap on one seat's screen and not on
   another's, so a CopyRect one seat must refuse is one the other may take."
  (let ((boxes '()))
    (loop for w in (wm-stacking-order port seat)
          until (eq w obj)
          do (when-let ((b (wm-window-box w seat))) (push b boxes)))
    (let ((seat (port-seat port seat)))
      (when (seat-drag-wire seat)
        (when-let ((b (seat-drag-wire-box seat))) (push b boxes)))
      (when-let ((menu (seat-menu seat)))
        (dolist (m (wm-menu-chain menu))
          (push (list (wm-menu-x m) (wm-menu-y m)
                      (glass:fb-width (wm-menu-fb m)) (glass:fb-height (wm-menu-fb m)))
                boxes))))
    boxes))

(defun wm-obstructed-p* (port surf boxes &optional seat)
  "Does anything WM-COMPOSITE draws AFTER SURF ON SEAT'S SCREEN — a window stacked over
   it, the drag wireframe, an open menu — land on any of BOXES (a LIST of boxes)?  THE
   occlusion question, asked once and answered in one place: both the wire CopyRect and
   the in-RAM screen translation ask it, of different rectangles, so that the two
   verdicts can differ in strictness without ever differing in what 'above' means."
  (let ((above (wm-boxes-above port surf seat)))
    (and above
         (some (lambda (b) (some (lambda (a) (wm-boxes-overlap-p a b)) above))
               (remove nil boxes))
         t)))

(defun wm-obstructed-p (port surf &rest boxes)
  "WM-OBSTRUCTED-P* for the default seat, spelled the way the rest of the tree and the
   inspect harnesses already spell it: the boxes as trailing arguments.  A &REST cannot
   also carry a trailing seat, which is the whole reason the starred one exists."
  (wm-obstructed-p* port surf boxes))

(defvar *wm-copy-tally* nil
  "NIL, or a plist WM-SURFACE-SCREEN-COPY tallies its verdicts into — the only way to
   tell a hint that was never offered from one the occlusion guard refused, which the
   byte counters cannot distinguish.  Off by default and one test when off; a
   measurement turns it on over the control socket (SETF it to a fresh list).  Keys:
   :offered a hint arrived; :stale the window moved or resized under it so the screen
   is no longer a base to translate; :empty it clipped away to nothing; :wire /
   :wire-px it went on the RFB update; :wire-obstructed the guard refused it because
   something is stacked over an end of the move; :screen / :screen-obstructed the same
   verdict for the in-RAM strip path, which refuses more often.")

(defun wm-copy-tally (key &optional (n 1))
  "Add N to KEY's count, and return NIL always — this is called in tail position of a
   branch that must answer 'no hint'."
  (when *wm-copy-tally*
    (incf (getf *wm-copy-tally* key 0) n))
  nil)

;;; ---- ONE take, MANY seats ----------------------------------------------------
;;;
;;; A surface reports its translation CONSUMINGLY: WM-SURFACE-COPY-P answers "how did
;;; my content move since you last asked", so the second asker is told "not at all" and
;;; the second seat loses the CopyRect entirely — which on a window drag is 24-57x the
;;; bytes on a nested hop, far too much to hand away.
;;;
;;; So the take happens ONCE PER COMPOSITING ROUND and every seat judges the same hint.
;;; A round is a dynamic extent, marked by *WM-COPY-ROUND*: the WM tick loop binds it to
;;; a fresh number and composites every seat inside it, so the first seat to reach a
;;; surface takes the hint and the rest read what it took.  Outside a round — a drag, a
;;; menu, an explicit COMPOSITE-SEAT — the variable is NIL and every call takes afresh,
;;; which is exactly what a single-seat session did before there were rounds.
;;;
;;; The take alone is not enough: a hint describes the pixels AS THEY WERE WHEN IT WAS
;;; TAKEN, so if the surface repaints between seat A's blit and seat B's, B applies A's
;;; translation to newer pixels and smears.  The tick loop therefore holds the scrolling
;;; surfaces' framebuffer locks across the WHOLE round (WM-WITH-SCROLL-LOCKS), which is
;;; the same lock WM-DRAW-SURFACE has always taken around take-and-blit, merely widened
;;; from one seat to all of them.  There is normally at most one scrolling window, so
;;; the round holds one lock.

(defvar *wm-copy-round* nil
  "The current compositing round's identity while every seat is being composited from
   one set of surface translations, or NIL outside such a round (take per call).")

(defun wm-surface-round-hint (surf)
  "SURF's content translation for this round: taken from the surface the first time it
   is asked within a round and remembered for the rest of it, so every seat judges the
   same move.  Outside a round, taken afresh — the pre-seat behaviour."
  (let ((take (wm-surface-copy-p surf)))
    (cond
      ((null take) nil)
      ((null *wm-copy-round*) (funcall take))
      ((eql (wm-surface-copy-round surf) *wm-copy-round*) (wm-surface-copy-hint surf))
      (t (setf (wm-surface-copy-round surf) *wm-copy-round*
               (wm-surface-copy-hint surf) (funcall take))))))

(defun wm-with-scroll-locks (surfaces thunk)
  "Run THUNK holding every surface in SURFACES' framebuffer lock, so a round's take and
   all the seats' blits see one set of pixels.  Nested rather than looped because that
   is what a scoped lock macro gives; the list is normally empty or one long."
  (if (null surfaces)
      (funcall thunk)
      (glass:with-fb-locked ((wm-surface-fb (first surfaces)))
        (wm-with-scroll-locks (rest surfaces) thunk))))

(defun seat-copy-base (seat surf)
  "The content rect SEAT's screen was last known to hold whole for SURF, or NIL.
   Per-seat, and it has to be: it is a claim about ONE SCREEN's pixels, and two seats
   holding the window at different places, under different clips, with different things
   stacked over it do not have the same claim to make."
  (gethash surf (seat-copy-bases seat)))

(defun (setf seat-copy-base) (value seat surf)
  (setf (gethash surf (seat-copy-bases seat)) value))

(defun wm-surface-screen-copy (port surf fb &optional seat)
  "Take SURF's pending content translation and return it as a screen-space CopyRect
   hint (sx sy dx dy w h) for framebuffer FB — or NIL to refuse it.

   Returns (values WIRE SCREEN ALLOWED): the same hint judged twice, plus the rectangle
   both verdicts are about.  WIRE is the hint to put on the RFB update; SCREEN is the
   hint the compositor may also apply to the screen framebuffer itself (see
   *WM-SCROLL-STRIP*), which is refused more often, and ALLOWED is SURF's visible
   content within this composite — the region the strip path is responsible for.

   The two verdicts differ because the consequences do.  A wrong CopyRect on the WIRE is
   self-repairing: the sender re-diffs the damage box afterwards, so a dragged-along
   window is corrected inside the same update, and the z-order refusal there is a bytes
   policy (*WM-COPYRECT-OCCLUSION-GUARD*, switchable).  A wrong translation of the
   SCREEN is not: the strip path deliberately does not redraw the rest, so a smear
   nothing repairs it.  SCREEN therefore refuses whenever ANYTHING above SURF touches
   ALLOWED — which contains the source, the destination AND the exposed strip — and it
   refuses whatever that parameter says.  Falling back is free: the whole-window blit
   the compositor does instead is always correct.

   The hint is CONSUMED either way (that is what makes the next one mean \"since this
   composite\"), and SURF's COPY-BASE is refreshed either way.  A hint survives only if:

     * the screen still holds the pixels it talks about — SURF's content rect is
       unchanged since the last composite that drew it whole (COPY-BASE), so it has not
       moved or resized underneath the translation;
     * the moved rectangle lies inside SURF's own visible CONTENT area — not the title
       bar, border or corner marks, which the frame redraws over;
     * it lies inside the screen, so a window hanging off an edge copies only the part
       that is really there;
     * it lies inside THIS composite's damage box (the fb clip), which is the region the
       RFB sender will diff afterwards — a copy reaching outside it could not be
       corrected;
     * and, while *WM-COPYRECT-OCCLUSION-GUARD* holds, nothing is stacked above SURF
       over either end of it (WM-BOXES-ABOVE-SURFACE): another window, or an open menu,
       would otherwise be dragged along by the copy.

   Both ends matter: the source must be this window's own content (or the copy reads
   somebody else's pixels) and so must the destination (or it overwrites them)."
  (let* ((seat (port-seat port seat))
         (hint (wm-surface-round-hint surf))     ; one take per round, judged by every seat
         (sfb (wm-surface-fb surf))
         (content (list (seat-window-x seat surf) (seat-window-y seat surf)
                        (glass:fb-width sfb) (glass:fb-height sfb)))
         (clip (glass:fb-clip fb))
         (clip-box (if clip
                       (list (first clip) (second clip)
                             (- (third clip) (first clip)) (- (fourth clip) (second clip)))
                       (list 0 0 (glass:fb-width fb) (glass:fb-height fb))))
         (base (seat-copy-base seat surf)))
    ;; This composite redraws the window WHOLE only if the clip contains all of it;
    ;; that — and only that — makes the screen a base the next translation can read.
    (setf (seat-copy-base seat surf)
          (and (equal content (wm-box-intersect content clip-box)) content))
    (when hint
      (wm-copy-tally :offered)
      (unless (equal base content) (wm-copy-tally :stale)))
    (when (and hint *wm-scroll-copyrect* (equal base content))
      (destructuring-bind (hsx hsy hdx hdy hw hh) hint
        (let* ((ddx (- hdx hsx)) (ddy (- hdy hsy))
               (allowed (wm-box-intersect
                         (wm-box-intersect content clip-box)
                         (list 0 0 (glass:fb-width fb) (glass:fb-height fb))))
               ;; Clip the destination so BOTH it and the source it reads from land
               ;; inside ALLOWED: intersect with ALLOWED and with ALLOWED shifted by the
               ;; translation.  Rectangles stay rectangles, so one intersection does it.
               (dst (and allowed
                         (wm-box-intersect
                          (wm-box-intersect
                           (list (+ (first content) hdx) (+ (second content) hdy) hw hh)
                           allowed)
                          (destructuring-bind (ax ay aw ah) allowed
                            (list (+ ax ddx) (+ ay ddy) aw ah))))))
          (if (and dst (or (/= ddx 0) (/= ddy 0)))
              (destructuring-bind (dx dy dw dh) dst
                (let* ((src (list (- dx ddx) (- dy ddy) dw dh))
                       (copy (list (first src) (second src) dx dy dw dh))
                       ;; wire: refused only while the guard holds and something above
                       ;; sits on one end of the move — the diff repairs it either way
                       (wire (unless (and *wm-copyrect-occlusion-guard*
                                          (wm-obstructed-p* port surf (list src dst) seat))
                               copy))
                       ;; screen: refused whenever anything above touches the region the
                       ;; strip path writes, guard or no guard — nothing repairs that
                       (screen (unless (wm-obstructed-p* port surf (list allowed) seat) copy)))
                  (when *wm-copy-tally*
                    (wm-copy-tally :clipped-px (- (* hw hh) (* dw dh)))
                    (if wire
                        (progn (wm-copy-tally :wire) (wm-copy-tally :wire-px (* dw dh)))
                        (wm-copy-tally :wire-obstructed))
                    (if screen (wm-copy-tally :screen) (wm-copy-tally :screen-obstructed)))
                  (values wire screen allowed)))
              (wm-copy-tally :empty)))))))               ; clipped to nothing / a null move

(defvar *wm-surface-lock-skips* 0
  "Frames in which a surface was skipped because its framebuffer was busy.  Zero on a
   healthy desktop; climbing means something holds a surface's lock long enough to
   matter, which is worth knowing before it becomes a freeze.")

(defun wm-draw-surface (surf fb &optional port seat)
  "Draw SURF's decorated window into SEAT's screen framebuffer FB, at the position SEAT
   holds it.  Given a PORT, also collect SURF's scroll CopyRect hint for this composite
   (SEAT-FRAME-COPY).

   The hint is taken under the SURFACE's lock, in the same breath as the blit that
   copies its pixels onto the screen: the two describe each other, and a paint landing
   between them would leave the hint one translation behind the pixels it names.  Only
   one CopyRect can ride an RFB update, so if two windows scroll at once the larger
   block wins and the other simply rides the diff."
  (let* ((sfb (wm-surface-fb surf)) (cw (glass:fb-width sfb)) (ch (glass:fb-height sfb))
         (sx (seat-window-x seat surf)) (sy (seat-window-y seat surf)))
    (wm-frame fb sx sy cw ch (wm-surface-deco* surf cw)
              (lambda ()
                ;; THE COMPOSITOR MUST NOT BLOCK HERE.  It already holds the SEAT's framebuffer
                ;; lock — that is what it is drawing into — and this reaches for a SURFACE's.  A
                ;; thread walking those two the other way round deadlocks the pair, and the one
                ;; that loses is the thread painting the screen: the whole desktop stops, gateway
                ;; included.  Not a hypothesis — it is what a resize with the browser open did.
                ;;
                ;; Declining to wait turns that into a surface one frame stale, which the next
                ;; composite corrects.  It does not repair the lock ORDER; it removes the
                ;; compositor from the cycle.
                (glass:with-fb-locked-or (sfb :seconds 0.05)
                    (progn (incf *wm-surface-lock-skips*) nil)
                  (when port
                    (when-let ((c (ignore-errors (wm-surface-screen-copy port surf fb seat))))
                      (let* ((seat (port-seat port seat))
                             (cur (seat-frame-copy seat)))
                        (when (or (null cur)
                                  (> (* (fifth c) (sixth c)) (* (fifth cur) (sixth cur))))
                          (setf (seat-frame-copy seat) c)))))
                  (blit-fb sfb sx sy fb))))))

(defun %svg-path-p (path)
  (let ((s (string-downcase (princ-to-string path))))
    (and (>= (length s) 4) (string= ".svg" (subseq s (- (length s) 4))))))

(defun %bg-render-size (mode sw sh iw ih)
  "The pixel size to rasterise a VECTOR image at so MODE displays it crisply (the
   display size for :stretch; the aspect-preserving cover/fit size otherwise), so the
   sample loop then runs ~1:1 instead of upscaling.  NIL for modes that don't scale."
  (flet ((r (s) (values (max 1 (round (* iw s))) (max 1 (round (* ih s))))))
    (case mode
      (:stretch (values sw sh))
      (:cover   (r (max (/ sw iw) (/ sh ih))))
      (:fit     (r (min (/ sw iw) (/ sh ih))))
      (t        (values nil nil)))))

(defun wm-render-background (seat path &key (mode :cover))
  "Rasterise the image at PATH (any format pigment decodes — PNG/JPEG/GIF/WebP/SVG)
   into a screen-sized framebuffer for use as the desktop background.  MODE places
   it: :cover (fill, centre-crop — default), :fit (whole image, teal letterbox),
   :stretch (distort to fill), :center (1:1), or :tile.  An SVG is re-rasterised at
   the display size (vector — crisp at any resolution, not an upscaled intrinsic)."
  (multiple-value-bind (iw ih samp) (%decode-image path)
    (let ((sw (seat-screen-w seat)) (sh (seat-screen-h seat)))
      ;; SVG: re-render at the size MODE will show it -> crisp, no upscale blur
      (when (%svg-path-p path)
        (multiple-value-bind (tw th) (%bg-render-size mode sw sh iw ih)
          (when (and tw (or (/= tw iw) (/= th ih)))
            (multiple-value-setq (iw ih samp) (%decode-image path :width tw :height th)))))
     (let* ((fb (glass:make-framebuffer sw sh +wm-teal+))
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
      fb))))

(defun seat-wallpaper (seat)
  "SEAT's wallpaper, rasterised for the screen SIZE IT HAS NOW, or NIL for none.

   The picture is the session's; the pixels are this seat's; and the size those pixels
   were cut for is a fact that can go stale.  It goes stale exactly when a seat's screen
   is resized, which is why this re-renders rather than returning the cached image
   blindly: the alternative is a wallpaper drawn for a screen that no longer exists,
   blitted at 0,0 into one that does — too small and leaving a margin, or too large and
   silently cropped.

   Cached until the size changes, so the common path is one comparison."
  (let ((fb (seat-fb seat)))
    (when fb
      (let ((bg (seat-bg seat)))
        (if (and bg
                 (= (glass:fb-width bg) (glass:fb-width fb))
                 (= (glass:fb-height bg) (glass:fb-height fb)))
            bg
            (setf (seat-bg seat)
                  (let ((path (seat-bg-path seat)))
                    (and path (ignore-errors
                               (wm-render-background seat path :mode (seat-bg-mode seat)))))))))))

(defun resize-seat-screen (seat width height)
  "Resize SEAT's screen to WIDTH x HEIGHT and repaint it.  Returns T if it changed.

   This is the per-seat resize that GLASS-ON-RESIZE explicitly is not: that one resizes
   the CLIM main top-level sheet, which in WM mode is one application's window and not
   anybody's screen.  A seat's screen is its framebuffer, and resizing it is this.

   The wallpaper needs no attention here — SEAT-WALLPAPER notices the new size and cuts
   the picture again — which is the whole point of keeping the picture."
  (let ((fb (seat-fb seat)))
    (when (and fb (plusp width) (plusp height)
               (not (and (= width (glass:fb-width fb)) (= height (glass:fb-height fb)))))
      (glass:fb-resize fb width height +wm-teal+)
      (setf (seat-screen-w seat) width (seat-screen-h seat) height)
      ;; Whole-screen: COMPOSITE-SEAT's own docstring lists resize among the cases that
      ;; are not a believable partial update.
      (composite-seat seat)
      t)))

(defun wm-set-background (port path &key (mode :cover) seat)
  "Set the desktop background to the image at PATH (NIL clears it -> flat teal).  With
   no SEAT this is the session's taste and every seat gets it, each rasterised AT ITS OWN
   SCREEN SIZE — the same picture, not the same pixels, which is the whole reason the
   wallpaper is a per-seat slot.  With a SEAT, only that seat's changes."
  ;; The session's taste, so a seat added later inherits the picture rather than the
  ;; absence of one.
  (unless seat
    (setf (glass-port-bg-path port) path (glass-port-bg-mode port) mode))
  (dolist (s (if seat (list (port-seat port seat)) (glass-port-seats port)))
    ;; ...and on the seat, WHAT it was rendered from -- that is what makes the pixels
    ;; reproducible at another size instead of being the only copy of the picture.
    (setf (seat-bg-path s) path
          (seat-bg-mode s) mode
          (seat-bg s) (and path (ignore-errors (wm-render-background s path :mode mode)))))
  (when (glass-port-fb port)
    (if seat (composite-seat (port-seat port seat)) (composite-all port)))
  (seat-bg (port-seat port seat)))

;;; ---- compositing a scroll: translate the screen, redraw only the strip ----------
;;;
;;; The wire stopped re-sending a scrolled window when the CopyRect hint landed; the
;;; screen framebuffer did not.  A scroll composite still cleared the damage box to the
;;; wallpaper and blitted all 900x620 of the window over it, to produce a picture that
;;; differs from the one already there by a ~48 px strip.  So the same copy is paid
;;; twice in RAM — once when the window paints its own translation, once when the
;;; compositor reads the result back out — and the wallpaper is drawn where nothing can
;;; ever see it.
;;;
;;; When a surface reports a translation the compositor can believe, this does on the
;;; screen what the CopyRect does on the client: move the block the screen already
;;; holds, then blit ONLY what the move did not cover.  The rest of the damage box —
;;; title bar, border, corner marks, the wallpaper under the window — is not redrawn at
;;; all, because a full repaint would have produced exactly the pixels already there.
;;;
;;; THE OCCLUSION GUARD IS LOAD-BEARING HERE, which it is not on the wire.  A wrong
;;; CopyRect on the wire is repaired by the sender's own diff of the damage box in the
;;; same update; a wrong translation of the screen is repaired by nothing, because the
;;; whole point of the path is that the rest is not redrawn.  So this refuses on the
;;; strict verdict (WM-SURFACE-SCREEN-COPY's second value) whatever the wire policy
;;; says, and refusing simply falls back to the whole-window blit below.

(defun wm-covered-p (port box &optional seat)
  "Is BOX entirely inside one window that this composite draws opaquely ON SEAT'S
   SCREEN?  Every window, McCLIM or surface, paints its whole decorated rectangle —
   title bar, content, border — so a region inside one never shows a pixel of whatever
   was drawn under it.  Used to skip the desktop background, which is otherwise drawn
   and immediately buried."
  (and box
       (or (some (lambda (s) (wm-box-inside-p box (wm-window-box s seat)))
                 (glass-port-surfaces port))
           (some (lambda (m) (and (glass-mirror-managed m)
                                  (wm-box-inside-p box (wm-window-box m seat))))
                 (glass-port-mirrors port)))))

(defun wm-scroll-candidate (port box &optional seat)
  "The one surface a composite of region BOX could be painted as a translation of: the
   topmost one, IN SEAT'S ORDER, that answers the translation question at all and whose
   decorated window CONTAINS the whole box on SEAT's screen.

   Containment is what makes skipping the rest of the paint sound.  A window is opaque
   over its own box, so everything below it inside BOX is invisible and redrawing it
   changes nothing; and if two windows had both changed, the tick loop would have
   unioned their boxes into something no single window contains, and there would be no
   candidate.  What is left is whatever is stacked ABOVE this surface, which is exactly
   what the occlusion verdict rules on."
  (when box
    (find-if (lambda (w) (and (wm-surface-p w)
                              (wm-surface-copy-p w)
                              (wm-box-inside-p box (wm-window-box w seat))))
             (wm-stacking-order port seat))))

(defun wm-paint-strip (surf fb copy allowed &optional seat)
  "Paint SURF's window as the translation COPY (sx sy dx dy w h) of what FB already
   holds, plus a blit of everything in ALLOWED the translation did not carry — the
   newly exposed strip, and (for a scroll, which never moves the chrome) the rows above
   the moved block.  Both are in SEAT's screen coordinates; ALLOWED is SURF's visible
   content."
  (destructuring-bind (sx sy dx dy w h) copy
    (glass:fb-move-rect fb sx sy dx dy w h)
    (let ((sfb (wm-surface-fb surf))
          (cx (seat-window-x seat surf)) (cy (seat-window-y seat surf)))
      (dolist (band (wm-box-difference allowed (list dx dy w h)))
        (destructuring-bind (bx by bw bh) band
          (glass:with-fb-clip (fb bx by bw bh) (blit-fb sfb cx cy fb)))))))

(defun wm-composite-scroll (port fb box &optional seat)
  "Try to paint this composite as SURF's scroll instead of a redraw: true if it did.
   The hint is taken under the surface's lock and the translation applied in the same
   breath, so the copy and the pixels it names cannot drift apart — and within a round
   (see WM-SURFACE-ROUND-HINT) the tick loop holds that lock across every seat, so the
   same is true of the second screen.  Refused or absent, the hint is still handed to
   the wire (SEAT-FRAME-COPY) before returning NIL, so falling back here never costs the
   CopyRect the update would otherwise have had."
  (when-let ((surf (and *wm-scroll-copyrect* *wm-scroll-strip*
                        (wm-scroll-candidate port box seat))))
    (glass:with-fb-locked ((wm-surface-fb surf))
      (multiple-value-bind (wire screen allowed)
          (handler-case (wm-surface-screen-copy port surf fb seat)
            (error () (values nil nil nil)))                 ; no hint, not a half-hint
        (when wire (setf (seat-frame-copy (port-seat port seat)) wire))
        (when screen
          ;; A translation that dies half-applied simply reports failure: the caller
          ;; then paints the region WHOLE, which overwrites whatever it managed to do.
          (handler-case (progn (wm-paint-strip surf fb screen allowed seat) t)
            (error (e) (wm-note-surface-error port surf e) nil)))))))

(defun wm-composite (seat fb)
  "Draw the desktop into SEAT's screen framebuffer FB, within whatever region FB's clip
   marks as this composite's responsibility.  Everything below reads the arrangement
   from SEAT: which window is where, what is above what, whose menu is open, whose drag
   is in flight."
  (let ((port (seat-port seat))
        (box (wm-clip-box fb)))
    (unless (wm-composite-scroll port fb box seat)
      (wm-composite-whole port fb box seat))))

(defparameter *wm-session-label-size* 34
  "Point size of the desktop's own name.  Large enough to read without looking for it.")

(defparameter *wm-session-label-alpha* 0.38d0
  "How present that name is.  Faint on purpose: a desktop should say which one it is
   without competing with anything actually on it.")

(defparameter *wm-session-label-p* t
  "Whether the desktop writes its own name in the corner.  On, because the moment there
   is more than one of these the screen itself is the only thing that says which one you
   are looking at — a VNC window and an SDL window look identical.")

(defun wm-draw-session-name (fb)
  "Write GLASS:*DESKTOP-NAME* large and faint in FB's lower-left.

   BIG AND TRANSLUCENT rather than small and solid.  It is a label on a screen, not a
   caption: at a glance it should answer `which desktop is this' from across a room,
   and it must not read as something to click.  Faint gets both — the eye finds it when
   it is looking and slides off it when it is not — where small-and-solid was hard to
   read AND looked like content.

   A shadow under it because a wallpaper can be any colour: one dark pixel offset costs
   nothing and keeps it legible on a light picture, where faint grey would disappear
   exactly when somebody is trying to read it.  The shadow is fainter still, or it
   becomes an outline."
  (when (and *wm-session-label-p*
             (stringp glass:*desktop-name*)
             (plusp (length glass:*desktop-name*)))
    (ignore-errors
     (let* ((size *wm-session-label-size*)
            (x (round size 2))
            ;; Bottom-left, clear of the edge by half a line, and it follows the
            ;; framebuffer's height so a resized screen keeps it in the corner.
            (y (- (glass:fb-height fb) size (round size 2))))
       (when (plusp y)
         (glass:fb-text fb (+ x 2) (+ y 2) glass:*desktop-name*
                        :size size :color (glass:rgb 0 0 0)
                        :alpha (* *wm-session-label-alpha* 0.6d0))
         (glass:fb-text fb x y glass:*desktop-name*
                        :size size :color (glass:rgb 255 255 255)
                        :alpha *wm-session-label-alpha*))))))

(defun wm-composite-whole (port fb box &optional seat)
  "Rebuild region BOX (NIL = the whole screen) from the bottom up: desktop, McCLIM
   windows, surface windows, drag wireframe, menus.  Always correct, and what every
   composite that is not a believable scroll does."
  (let ((seat (port-seat port seat)))
    (unless (and *wm-skip-covered-background* (wm-covered-p port box seat))
      (let ((bg (seat-wallpaper seat)))                       ; cut for THIS screen size
        (if bg
            (blit-fb bg 0 0 fb)                                 ; desktop wallpaper
            (glass:fb-fill fb +wm-teal+)))
      ;; ...and whose desktop this is, in the corner.  Part of the BACKGROUND layer on
      ;; purpose: it is then clipped, damaged and repainted exactly like the pixels
      ;; behind it, and it follows a resize without being told.  Drawn whether or not
      ;; there is a wallpaper — the question "which desktop am I looking at" does not
      ;; depend on somebody having set a picture.
      (wm-draw-session-name fb))
    (dolist (w (reverse (wm-stacking-order port seat)))         ; every window, bottom-to-top
      (if (wm-surface-p w)
          (handler-case (wm-draw-surface w fb port seat)        ; isolate each surface's draw
            (error (e) (wm-note-surface-error port w e)))       ; skip it this frame; cull if persistent
          (ignore-errors                                        ; a bad window draws nothing, not a crash
           (if (glass-mirror-managed w) (wm-draw-window w fb seat) (blit-mirror w fb seat)))))
    (when-let ((b (and (seat-drag-wire seat) (seat-drag-wire-box seat))))   ; wireframe outline
      (destructuring-bind (x y w h) b
        (glass:fb-frame fb x y w h glass:+white+ 2)             ; white + inner black = visible on any bg
        (glass:fb-frame fb (1+ x) (1+ y) (max 0 (- w 2)) (max 0 (- h 2)) glass:+black+ 1)))
    (when-let ((menu (seat-menu seat)))                         ; root menu (+ submenu chain) on top
      (dolist (m (wm-menu-chain menu))
        (blit-fb (wm-menu-fb m) (wm-menu-x m) (wm-menu-y m) fb)))))

;;; ---- pointer routing --------------------------------------------------------

(defun wm-window-box (obj &optional seat)
  "(x y w h) of OBJ's whole decorated window — title bar + border + content — AS SEAT
   HOLDS IT (NIL = the window's own position), for damage accounting.

   The position is SEAT-DRAW-X, i.e. where the pixels actually land on THAT screen.  For
   every window a seat arranges for itself that is SEAT-WINDOW-X exactly; for a pop-up —
   one position, session-wide, placed in the CLIM driver's arrangement — it is that
   position shifted onto this seat's copy of the window that opened it.  Damage and
   occlusion both come through here, so both follow the pixels rather than the slot."
  ;; THE DENSITY COMES FROM THE SEAT, HERE, because this is where geometry is decided and
  ;; it is asked from places that are not drawing.  PORT-DAMAGE-WINDOW asks it once per
  ;; seat from WM-TICK, outside any binding, and got a 1x box for a 2x frame — so the top
  ;; of every title bar was drawn and never marked dirty, and the stale pixels stayed.
  ;; That is the smear: not a compositor bug, a bug in what the compositor was told had
  ;; changed.  Binding at the choke point means damage, occlusion and drawing cannot
  ;; disagree, whatever route the caller took to get here.
  (with-seat-scale (seat)
  (multiple-value-bind (cx cy cw ch)
      (if (wm-surface-p obj)
          (values (seat-draw-x seat obj) (seat-draw-y seat obj)
                  (glass:fb-width (wm-surface-fb obj)) (glass:fb-height (wm-surface-fb obj)))
          (when-let ((img (mcclim-render::image-mirror-image obj)))
            (multiple-value-bind (w h) (image-wh img)
              (values (seat-draw-x seat obj) (seat-draw-y seat obj) w h))))
    (when cx
      (list (- cx (wm-border)) (- cy (wm-titleh) (wm-border))
            (+ cw (* 2 (wm-border))) (+ (wm-titleh) ch (* 2 (wm-border))))))))

(defun wm-window-box-at (obj cx cy)
  "The decorated (x y w h) OBJ WOULD occupy if its content were at (CX,CY) — for the
   wireframe outline, which shows a hypothetical position without moving the window.
   Seat-free: the caller has already decided the position it is asking about."
  (multiple-value-bind (cw ch)
      (if (wm-surface-p obj)
          (values (glass:fb-width (wm-surface-fb obj)) (glass:fb-height (wm-surface-fb obj)))
          (when-let ((img (mcclim-render::image-mirror-image obj)))
            (image-wh img)))
    (when cw
      (list (- cx (wm-border)) (- cy (wm-titleh) (wm-border))
            (+ cw (* 2 (wm-border))) (+ (wm-titleh) ch (* 2 (wm-border)))))))

;;; ---- adaptive drag: opaque when the link keeps up, wireframe when it can't -----
;;; Moving a window OPAQUELY re-encodes it each frame — cheap on a client that can
;;; CopyRect (TigerVNC) or for small moves, but a big drag on a no-CopyRect client
;;; (macOS Screen Sharing) re-sends the whole window every frame and lags.  So a
;;; drag starts opaque (small moves look great) and switches to a WIREFRAME outline
;;; only once the send backlog (glass:*send-lag*) shows the link falling behind —
;;; the outline is a few thin rects, near-free to encode; the real window snaps to
;;; the final spot on release.
;;;
;;; A drag is one seat's, all the way down: the seat that grabbed the title bar moves
;;; the window ON ITS OWN SCREEN, and the composite, the CopyRect hint and the wireframe
;;; are that seat's.  Nobody else's picture moves, and nobody else's link decides whether
;;; this drag goes wireframe.
(defparameter *drag-adaptive* t "Auto-switch a laggy opaque drag to wireframe.")
(defparameter *wireframe-queue-kb* 100.0d0
  "Socket send-queue backlog EWMA (KB, glass:*send-queue*) past which an in-progress
   drag switches to wireframe — the real 'client can't keep up' signal.  A CopyRect
   client's cheap opaque drags never back the queue up, so it stays opaque; a
   no-CopyRect client (macOS) re-encoding the whole window backs it up and trips
   wireframe.  Tune live over the control socket.")

(defun wm-owned-popup-boxes (port owner &optional seat)
  "The (x y w h) boxes, on SEAT's screen, of the pop-ups OWNER has posted.  Empty on a
   desktop with no menu open, which is nearly always — so the drag paths below union in
   nothing and compute exactly the boxes they always did.

   A pop-up is drawn relative to the seat's copy of its owner (SEAT-DRAW-X), so moving
   the owner moves the pop-up on that screen without the pop-up repainting: asked before
   and after the move, this is what says which pixels the move dirtied."
  (let ((boxes '()))
    (dolist (m (glass-port-mirrors port) boxes)
      (when (and (typep m 'glass-mirror) (not (glass-mirror-managed m))
                 (eq (clim-popup-owner m port) owner))
        (when-let ((b (wm-window-box m seat))) (push b boxes))))))

(defun wm-drag-move-opaque (port obj ncx ncy &optional seat)
  "Opaque move step: move the real window to content-position (NCX,NCY) on SEAT's screen
   and composite old+new with a CopyRect hint (near-free on a client that can CopyRect)."
  (let* ((seat (port-seat port seat))
         (old (wm-window-box obj seat))
         (old-pops (wm-owned-popup-boxes port obj seat)))
    (wm-move obj ncx ncy seat)
    (let ((new (wm-window-box obj seat))
          (new-pops (wm-owned-popup-boxes port obj seat)))
      ;; No CopyRect while a pop-up of this window is posted: the copy would drag the
      ;; menu's pixels along with the window's.  The diff of the (now larger) damage box
      ;; would repair that in the same update, but a menu is open for a second or two and
      ;; refusing costs nothing measurable — it is the same trade WM-COMPOSITE-SCROLL's
      ;; occlusion guard makes, and for the same reason.
      (composite-seat seat (wm-box-union (list* old new (append old-pops new-pops)))
                      (when (and old new (null old-pops) (null new-pops))
                        (list (first old) (second old) (first new) (second new)
                              (third old) (fourth old)))))))

(defun wm-drag-move (port obj ncx ncy &optional seat)
  "A drag move: opaque, unless the socket send-queue (glass:*send-queue*) shows the
   client can't keep up — then switch THIS drag to wireframe (outline starts at the
   window's current box) and don't move the real window."
  (let ((seat (port-seat port seat)))
    (if (and *drag-adaptive* (> glass:*send-queue* *wireframe-queue-kb*))
        (progn
          (setf (seat-drag-wire seat) t
                (seat-drag-wire-box seat) (wm-window-box obj seat))
          (wm-drag-wire-to port obj ncx ncy seat))
        (wm-drag-move-opaque port obj ncx ncy seat))))

(defun wm-drag-wire-to (port obj ncx ncy &optional seat)
  "Wireframe drag step: move only the OUTLINE to content-position (NCX,NCY); the real
   window stays put (its pixels stay on the client), so only the thin outline tiles
   change — cheap even with no CopyRect."
  (let* ((seat (port-seat port seat))
         (old (seat-drag-wire-box seat))
         (new (wm-window-box-at obj ncx ncy)))
    (setf (seat-drag-wire-box seat) new)
    (composite-seat seat (wm-box-union (list old new)))))

(defun wm-drag-wire-drop (port obj ncx ncy &optional seat)
  "End a wireframe drag: move the real window to (NCX,NCY) and composite the union of
   the window's OLD position (it stayed put through the drag, so its pixels are still
   on the client and must be ERASED — otherwise a ghost window lingers there), the
   last outline, and the window's NEW box.  The one time the moved content is re-sent."
  (let* ((seat (port-seat port seat))
         (old (wm-window-box obj seat))       ; real position BEFORE the move (the ghost source)
         (old-pops (wm-owned-popup-boxes port obj seat))
         (wire (seat-drag-wire-box seat)))
    (wm-move obj ncx ncy seat)
    (setf (seat-drag-wire seat) nil (seat-drag-wire-box seat) nil)
    (composite-seat seat (wm-box-union (list* old wire (wm-window-box obj seat)
                                             (append old-pops
                                                     (wm-owned-popup-boxes port obj seat)))))))

(defun wm-pos-x (obj &optional seat) (seat-window-x seat obj))
(defun wm-pos-y (obj &optional seat) (seat-window-y seat obj))

(defun wm-move (obj x y &optional seat)
  "Put OBJ's content at (X,Y) for SEAT.  With no seat — which is how the harnesses, the
   control socket and every pre-seat call site say it — this writes the window's OWN
   position, the one every seat sees until it diverges, exactly as it always did."
  (if seat
      (seat-move-window seat obj x y)
      (setf (window-own-x obj) x (window-own-y obj) y)))

(defun wm-sync-sheet (port obj &optional seat)
  "After a WM move settles, resync a McCLIM window's SHEET transformation to its new
   position so McCLIM positions its pull-down menus/dialogs at the window's new spot
   (wm-move only pokes the position, cheap, for per-motion blitting).  Runs on the event
   thread (marshalled via the mailbox) — update-mirror-geometry touches sheet state.
   Surfaces have no McCLIM sheet, so they're skipped.

   ONLY THE SEAT DRIVING McCLIM SYNCS, and it syncs to ITS OWN VIEW.  A sheet
   transformation is single-valued and it is what McCLIM places pull-downs and dialogs
   from, so it can track exactly one seat's idea of where the window is — but that seat
   is whoever has their hand on the application, not a seat fixed at session start.  This
   used to be the home seat unconditionally, which is where 'one at a time' turned into
   'one forever': a second person could drag a CLIM window across their own screen and
   still get its pull-downs at the first person's window position.  A NON-driving seat's
   drag moves the window on its own screen and leaves McCLIM alone, and the geometry
   comes to it the moment it takes the token (CLIM-RESYNC-GEOMETRY)."
  (unless (wm-surface-p obj)
    (when (clim-driver-p port seat)
      (clim-sheet-goto port obj (seat-window-x seat obj) (seat-window-y seat obj)))))

(defun wm-hit (port x y &optional seat)
  "What is at (X,Y) on SEAT's screen — the chrome half of the pair that must agree with
the compositor.  See *WM-SCALE*: a hit test that used 1x chrome against a 2x drawing would
put every title bar somewhere other than where it is shown, so the density is bound here
for the same reason and in the same way COMPOSITE-SEAT binds it."
  (with-seat-scale ((port-seat port seat))
    (%wm-hit port x y seat)))

(defun %wm-hit (port x y &optional seat)
  "Topmost window whose decoration or content contains (X,Y) ON SEAT'S SCREEN: (values obj REGION cx cy
   cw ch), REGION one of :winmenu (title-bar menu button) / :resize (bottom-right corner
   grab) / :title / :content; NIL over the workspace.

   Walks WM-STACKING-ORDER, the same order the compositor draws in, so the window you
   click is the window you can see.  It walked surfaces first and mirrors second when
   the compositor drew them that way; the two had to change together, since a pointer
   that disagrees with the pixels sends clicks to a window that is not there.

   Unmanaged mirrors are skipped, as they always were: a CLIM pull-down gets its events
   through the grab-sheet path, not through the WM's hit test."
  (flet ((test (cx cy cw ch obj)
           (let ((ty (- cy (wm-titleh))) (rz 16))
             (cond
               ((and (<= (+ cx 4) x (+ cx 18)) (<= (+ ty 4) y (+ ty 18)))           ; wedge = Window Menu
                (list obj :winmenu cx cy cw ch))
               ((and (<= (- (+ cx cw) rz) x (+ cx cw 1)) (<= (- (+ cy ch) rz) y (+ cy ch 1)))  ; resize corner
                (list obj :resize cx cy cw ch))
               ((and (<= cx x (+ cx cw)) (<= cy y (+ cy ch))) (list obj :content cx cy cw ch))
               ((and (<= cx x (+ cx cw)) (<= ty y cy)) (list obj :title cx cy cw ch))))))
    (dolist (w (wm-stacking-order port seat))
      (let ((hit (cond
                   ((wm-surface-p w)
                    (test (seat-window-x seat w) (seat-window-y seat w)
                          (glass:fb-width (wm-surface-fb w)) (glass:fb-height (wm-surface-fb w)) w))
                   ((glass-mirror-managed w)
                    (when-let ((image (mcclim-render::image-mirror-image w)))
                      (multiple-value-bind (cw ch) (image-wh image)
                        (test (seat-window-x seat w) (seat-window-y seat w) cw ch w)))))))
        (when hit (return-from %wm-hit (values-list hit)))))))

(defun wm-raise (port obj &optional seat)
  "Move OBJ to the front of SEAT's stacking order, whichever kind of window it is,
   and — for a McCLIM window — give it the keyboard.

   The ticket comes from the SESSION's one ZCLOCK even though the stack it goes into is
   the seat's: one monotonic counter keeps tickets comparable between a window this seat
   has restacked and one it has never touched, which is what lets a seat diverge from
   the session arrangement one window at a time instead of all at once.

   The second half is the window manager's job and there was nobody else to do it.
   McCLIM routes keyboard events by PORT-KEYBOARD-INPUT-FOCUS, ignoring the sheet on
   the event whenever that is set (Core/windowing/ports.lisp), and the only thing
   that ever set it was an application grabbing focus for itself.  On CLX the X
   server is what tells CLIM the focus moved; here there is no X server, so a second
   application could come up, draw, and never receive a key — the first one to grab
   focus kept it for the session.  Climacs is what made that plain, having the manners
   to grab focus onto its minibuffer the moment it started reading.

   A WINDOW-MANAGER-FOCUS-EVENT is the sanctioned way to say it: TOP-LEVEL-SHEET-MIXIN
   handles it by setting the port focus, and it does so on the CLIM thread rather than
   from whatever thread noticed the click.  We post it rather than setting the focus
   here for exactly that reason.

   Surfaces are not McCLIM windows and take the keyboard the other way, through
   GLASS-PORT-FOCUS-SURFACE, which the caller sets."
  (seat-restack-window (port-seat port seat) obj (incf (glass-port-zclock port)))
  (unless (wm-surface-p obj)
    (when-let ((sheet (glass-mirror-sheet obj)))
      (when (typep sheet 'climi::top-level-sheet-mixin)
        ;; McCLIM's keyboard focus is single-valued, so only the seat driving McCLIM may
        ;; move it; another seat raising a CLIM window raises it on its own screen and
        ;; leaves the focus with whoever is typing.
        (when (clim-driver-p port seat)
          (enqueue port (make-instance 'climi::window-manager-focus-event :sheet sheet)))))))

(defun wm-close (port obj)
  "Close window OBJ: tear down a surface's content (kill its shell) and drop it, or
   drop a McCLIM window's mirror (the window vanishes; its frame thread lingers,
   idle).  Recomposites EVERY seat — the window is gone for all of them, whichever one
   asked, and each drops its view and its keyboard along with it."
  (cond
    ((wm-surface-p obj)
     (when (wm-surface-close-fn obj) (ignore-errors (funcall (wm-surface-close-fn obj))))
     (setf (glass-port-surfaces port) (remove obj (glass-port-surfaces port)))
     (dolist (seat (glass-port-seats port))
       (when (eq (seat-focus-surface seat) obj) (setf (seat-focus-surface seat) nil))))
    (t
     (setf (glass-port-mirrors port) (remove obj (glass-port-mirrors port)))))
  (port-forget-window port obj)
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

(defun wm-lower (port obj &optional seat)
  "Back: send OBJ behind every other window ON SEAT'S SCREEN — of either kind, now that there is only
   one stack for it to go to the back of.

   One below the current minimum rather than a counter of its own: Back is rare, the
   windows are few, and a second counter is a second thing that can drift out of step
   with the first."
  (let ((seat (port-seat port seat)))
    (seat-restack-window
     seat obj
     (1- (reduce #'min (wm-stacking-order port seat)
                 :key (lambda (w) (seat-window-z seat w)) :initial-value 0)))
    (composite-seat seat)))

(defun wm-fullsize (port obj &optional seat)
  "Toggle Full Size / Restore Size for a resizable surface (fills SEAT's workspace,
   below the title bar; a second time restores the saved geometry).

   Full Size is where the shared/per-seat line shows: the POSITION it sets is the asking
   seat's, but the SIZE it sets is the window's, and a window has one size for everybody
   because that is what the application laid its content out to.  So a seat filling ITS
   screen with a window resizes that window on every screen.  The alternative is a
   per-seat layout, which is a second copy of the application."
  (let ((seat (port-seat port seat)))
    (when (and (wm-surface-p obj) (wm-surface-resize-fn obj))
      (if (wm-surface-saved-geom obj)
          (destructuring-bind (x y w h) (wm-surface-saved-geom obj)          ; Restore Size
            (seat-move-window seat obj x y)
            (setf (wm-surface-saved-geom obj) nil)
            (funcall (wm-surface-resize-fn obj) w h))
          (progn                                                             ; Full Size
            (setf (wm-surface-saved-geom obj)
                  (list (seat-window-x seat obj) (seat-window-y seat obj)
                        (glass:fb-width (wm-surface-fb obj)) (glass:fb-height (wm-surface-fb obj))))
            (seat-move-window seat obj (wm-border) (+ (wm-titleh) (wm-border)))
            (funcall (wm-surface-resize-fn obj)
                     (- (seat-screen-w seat) (* 2 (wm-border)))
                     (- (seat-screen-h seat) (wm-titleh) (* 2 (wm-border))))))
      ;; the size changed for everybody, so everybody repaints
      (composite-all port))))

(defun wm-window-menu-items (port obj &optional seat)
  "The Window Menu items (LABEL . THUNK) for OBJ, closed over the SEAT that opened it —
   Back and Refresh act on that seat's screen and nobody else's."
  (append
   (when (and (wm-surface-p obj) (wm-surface-resize-fn obj))
     (list (cons (if (wm-surface-saved-geom obj) "Restore Size" "Full Size")
                 (lambda () (wm-fullsize port obj seat)))))
   (list (cons "Back"    (lambda () (wm-lower port obj seat)))
         (cons "Refresh" (lambda () (composite-seat (port-seat port seat))))
         (cons "Quit"    (lambda () (wm-close port obj))))))

(defun wm-open-window-menu (port obj cx cy &optional seat)
  "Pop the Window Menu just below OBJ's title bar (at content top-left CX,CY)."
  (let ((seat (port-seat port seat))
        (menu (make-wm-menu :hover -1 :title "Window"
                            :items (wm-window-menu-items port obj seat))))
    (setf (seat-menu seat) (wm-place-menu menu seat cx cy))))

;;; ---- workspace root menu ----------------------------------------------------

(defun wm-menu-width (menu)
  (let ((w (+ 24 (glass:text-width (wm-menu-title menu) :size (wm-size 12) :font (glass:default-font t)))))
    (dolist (it (wm-menu-items menu) (max 108 w))
      (let ((pad (if (wm-submenu-p (wm-item-action it)) 46 28)))     ; room for the ▸ arrow
        (setf w (max w (+ pad (glass:text-width (car it) :size (wm-size 12) :font (glass:default-font t)))))))))

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
    (glass:fb-text fb 8 3 (wm-menu-title menu) :size (wm-size 12) :color glass:+black+ :font font)
    (glass:fb-hline fb 0 (1- +menu-titleh+) w (glass:rgb 120 120 120))
    (loop for it in (wm-menu-items menu) for i from 0
          for yy = (+ +menu-titleh+ (* i +menu-itemh+))
          for hot = (= i (wm-menu-hover menu))
          for ink = (if hot glass:+white+ glass:+black+)
          do (when hot (glass:fb-rect fb 1 yy (- w 2) +menu-itemh+ +menu-hi+))
             (glass:fb-text fb 14 (+ yy 3) (car it) :size (wm-size 12) :color ink :font font)
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

(defun wm-place-menu (menu seat x y)
  "Render MENU and position it on SEAT's screen, top-left near (X,Y) but kept in that
   seat's bounds — a menu clamps to the screen it opened on, which is why this needs a
   seat and not a port: the two screens are different sizes."
  (wm-menu-render menu)
  (setf (wm-menu-x menu) (max 0 (min x (- (seat-screen-w seat) (glass:fb-width (wm-menu-fb menu)))))
        (wm-menu-y menu) (max 0 (min y (- (seat-screen-h seat) (glass:fb-height (wm-menu-fb menu))))))
  menu)

(defun wm-open-menu (port x y &optional seat)
  "Open the workspace root menu at (X,Y): the port's items, plus whatever the SESSION
   itself can offer from the workspace — today, speaking the clipboard, and deciding
   whether this seat is on a plain VNC port.  Those are appended here rather than
   registered as apps because they are not apps and because
   they change: the items are built at every open, so \"Stop speaking\" is on the menu
   exactly while something is being said.  A port whose menu was overridden still gets
   them, for the same reason it still gets a window menu — they are the desktop's, not
   the menu list's."
  (let* ((seat (port-seat port seat))
         (menu (make-wm-menu :x x :y y :hover -1
                             :items (append (glass-port-menu-items port)
                                            (wm-clipboard-menu-items seat)
                                            ;; …and whether THIS seat is on a VNC port,
                                            ;; which is a question about the seat whose
                                            ;; menu this is and not about the session
                                            (wm-vnc-menu-items port seat)))))
    (setf (seat-menu seat) (wm-place-menu menu seat x y))))

;;; ---- the selection menu (right-click ON the thing you selected) --------------
;;;
;;; The workspace menu is where you go to start something; this is where you go to
;;; act on what you just did.  Making the desktop read a selection aloud used to mean
;;; selecting the text, then travelling to the bare workspace to right-click — asking
;;; the hand to leave the words behind in order to talk about them.  Here the menu
;;; comes up over the selection.
;;;
;;; It lives in the WM because the ability to act on a selection belongs to the DESKTOP,
;;; not to whoever happens to have one.  Any window that ever has one gets read-aloud
;;; without knowing that speech exists — the terminal on the day it learns to select, an
;;; editor, the next app nobody has written.  Building it inside the browser instead
;;; would buy a second menu renderer, a second hit-tester, and a menu that could not
;;; escape the browser's own window to clamp to the screen.
;;;
;;; THE SELECTION IS NOT THE CLIPBOARD.  This menu asks the window under the pointer
;;; what it has HIGHLIGHTED right now (WM-SURFACE-SELECTION-FN) and speaks that.  It
;;; used to read the session clipboard instead, which is a different thing wearing the
;;; same word: a clipboard deliberately OUTLIVES the highlight — that is what makes
;;; paste-later work — so the menu would come up over a page with nothing selected and
;;; cheerfully read out whatever was copied ten minutes ago.  What is on the clipboard
;;; is a fine question; it is just the ROOT menu's question (WM-CLIPBOARD-MENU-ITEMS),
;;; where there is no window to ask and "whatever was last copied" is exactly the answer
;;; wanted.
;;;
;;; THE INTERCEPTION RULE.  Button 3 belongs to the application.  The WM takes it iff
;;; the surface under the pointer reports a non-empty live selection; everything else —
;;; every other window, and this one the moment the highlight is dismissed — falls
;;; through to the app untouched.  The menu appears exactly where the words are, and
;;; only while they are still words.

(defun wm-speech-fn (name)
  "The bound GLASS symbol NAME, or NIL — speech is an optional system (:glass/speech),
   so the selection menu offers only what this image can actually do."
  (let ((s (find-symbol name '#:glass))) (and s (fboundp s) s)))

(defun wm-speaking-p ()
  "Is the session saying something?  Asked only when there IS a voice to ask, because
   SPEAKING-P's default speaker is SESSION-SPEAKER, which CREATES one — thread, mixer
   source and all — merely by being asked.  Opening a menu must not be the thing that
   gives the desktop a voice, so a session that has never spoken answers NIL here and
   stays as silent as it was."
  (let ((busy (wm-speech-fn "SPEAKING-P"))
        (var (find-symbol "*SESSION-SPEAKER*" '#:glass)))
    (and busy var (boundp var) (symbol-value var)
         (ignore-errors (funcall busy (symbol-value var))))))

(defun wm-surface-live-selection (surf)
  "The text SURF has highlighted RIGHT NOW, or NIL — the test behind the interception
   rule, returning the text so the caller needn't ask twice.  The window is asked at
   the instant of the click, so a selection that has been replaced or dismissed cannot
   answer.  A window that never has a selection has no SELECTION-FN and says NIL
   without being called; one whose answer signals is treated as no answer, because a
   busy or broken window must not be able to swallow a button press."
  (let ((fn (and (wm-surface-p surf) (wm-surface-selection-fn surf))))
    (when fn
      (let ((text (ignore-errors (funcall fn))))
        (and (stringp text) (plusp (length text)) text)))))

(defun wm-say (speak text &optional seat)
  "Say TEXT — to SEAT if it has a mix of its own, and to the session if it does not.

   Which is the same rule the menu that calls this already follows: a SELECTION is one
   person's (they highlighted it), so reading it aloud is for that person, and a seat
   that has no headset has no private mix to be addressed at, so the desktop simply
   says it out loud — which is what a one-seat desktop has always done and still does.

   The voice itself stays SESSION-wide: one chord engine, one queue, utterances in turn.
   Making it per-seat would be two engines and two synthesis threads to buy privacy that
   the audience already buys for the price of a list."
  (let ((mix (seat-mix seat)))
    (ignore-errors
     (if mix
         (funcall speak text :audience (list mix))
         (funcall speak text)))))

(defun wm-selection-menu-items (text &optional seat)
  "Items for the selection menu over TEXT.  Deliberately two lines long: this is a menu
   about one selection, not a second place to put applications."
  (let ((speak (wm-speech-fn "SPEAK")) (hush (wm-speech-fn "HUSH")))
    (when speak
      (append
       (list (cons "Speak selection"
                   (lambda ()
                     ;; A new selection replaces what is being said — picking this twice
                     ;; means "read THIS", not "read it again after the last one".
                     (when hush (ignore-errors (funcall hush)))
                     (wm-say speak text seat))))
       (when (and hush (wm-speaking-p))
         (list (cons "Stop speaking" (lambda () (ignore-errors (funcall hush))))))))))

(defun wm-clipboard-menu-items (&optional seat)
  "The CLIPBOARD speech items, appended to the workspace root menu (see WM-OPEN-MENU).
   The counterpart of the selection menu, and deliberately named for what it does: this
   reads whatever THIS SEAT last COPIED, which outlives any one window's highlight and
   is still there to be read from the bare workspace where there is nobody to ask what
   is selected.  A seat's clipboard and not the session's, so the item on my menu reads
   back what I copied and not what you did.  An empty clipboard says so rather
   than saying nothing — silence is indistinguishable from a broken voice.

   Empty when this image has no speech at all, so the root menu grows nothing it cannot
   do.  Built fresh on every menu open, which is what lets \"Stop speaking\" appear only
   while there is something to stop."
  (let ((speak (wm-speech-fn "SPEAK")) (hush (wm-speech-fn "HUSH")))
    (when speak
      (append
       (list (cons "Speak clipboard"
                   (lambda ()
                     (let ((text (ignore-errors
                                  (glass:clipboard-text (if seat (seat-clipboard seat)
                                                            (glass:session-clipboard))))))
                       (when hush (ignore-errors (funcall hush)))
                       ;; THIS seat's clipboard, said to THIS seat: the item exists because
                       ;; the selection is one person's, and so is the answer.
                       (wm-say speak (if (and (stringp text) (plusp (length text)))
                                         text
                                         "The clipboard is empty.")
                               seat)))))
       (when (and hush (wm-speaking-p))
         (list (cons "Stop speaking" (lambda () (ignore-errors (funcall hush))))))))))

;;; ---- plain VNC, from the root menu, per seat ---------------------------------
;;;
;;; A desktop may be reachable only over socket files — the gateway's wire and the
;;; capture's — and then a person holding a VNC viewer has no way in at all.  These are
;;; the two items that let one seat decide otherwise while the session runs, and the
;;; small window that answers the question a menu cannot ask: what is the password?
;;;
;;; PER SEAT, and the items are closed over the seat whose menu they are on, because
;;; serving is a seat's decision (docs/seats-and-transports.md) and "the desktop" is not
;;; a thing that can be exposed — a session has seats, and one of them can be on a port
;;; while another is not.

(defparameter *vnc-window-size* '(460 . 300)
  "The credential window's size.  Big enough that an 8-character password at 26px and a
   full address line are not truncated on a small screen, small enough not to cover the
   desktop somebody has just been given.")

(defun wm-vnc-window-lines (tr credential note)
  "The lines the credential window shows for transport TR: what to connect to, what to
   type, and — where they differ — what was actually bound.

   THE ADDRESS IS THE ROUTABLE ONE AND NOT THE BOUND ONE, which is the difference
   between a window somebody can act on and a window that says `0.0.0.0'.  The bound
   address is still shown, on its own line, because `every interface' is a fact about
   exposure and this window is the only place it is ever stated."
  (let* ((port (transport-port-num tr))
         (bound (transport-address tr))
         (host (if (loopback-address-p bound) "127.0.0.1" (local-address bound))))
    (append
     (list (list :head (if credential "VNC is on, and asking for a password"
                           "VNC is on — no password"))
           (list :field "Connect to" (format nil "~a:~d" host port)))
     (if credential
         (list (list :secret "Password" credential)
               (list :note "It is on this seat's clipboard as well."))
         (list (list :note "Anyone who can reach this port can use this desktop.")))
     (list (list :field "Bound to" (format nil "~a:~d" bound port))
           (list :note (if (loopback-address-p bound)
                           "Loopback only: nothing outside this machine can reach it."
                           "Every interface: this port faces whatever the box does.")))
     (when note (list (list :warn note)))
     (list (list :note "The same menu item stops it again.")))))

(defun wm-render-vnc-window (fb lines)
  "Draw LINES (from WM-VNC-WINDOW-LINES) onto FB."
  (let ((font (glass:default-font)) (bold (glass:default-font t))
        (w (glass:fb-width fb)) (y 14))
    (glass:fb-fill fb (glass:rgb 248 248 245))
    (flet ((wrap (text size limit)
             ;; A note is a sentence, not a label; it wraps rather than running off the
             ;; edge, because the one that gets truncated is always the one saying why
             ;; this is on loopback.
             (let ((words (let ((out '()) (start 0))
                            (dotimes (i (1+ (length text)) (nreverse out))
                              (when (or (= i (length text)) (char= (char text i) #\Space))
                                (when (> i start) (push (subseq text start i) out))
                                (setf start (1+ i))))))
                   (lines '()) (cur ""))
               (dolist (word words (nreverse (if (plusp (length cur)) (cons cur lines) lines)))
                 (let ((try (if (plusp (length cur)) (concatenate 'string cur " " word) word)))
                   (if (or (zerop (length cur)) (<= (glass:text-width try :size size :font font) limit))
                       (setf cur try)
                       (progn (push cur lines) (setf cur word))))))))
      (dolist (line lines fb)
        (destructuring-bind (kind a &optional b) line
          (ecase kind
            (:head (glass:fb-text fb 16 y a :size (wm-size 15) :color (glass:rgb 20 20 20) :font bold)
                   (glass:fb-hline fb 16 (+ y 22) (- w 32) (glass:rgb 200 200 195))
                   (incf y 32))
            (:field (glass:fb-text fb 16 y a :size (wm-size 13) :color (glass:rgb 100 100 100) :font font)
                    (glass:fb-text fb 110 y b :size (wm-size 13) :color (glass:rgb 20 20 20) :font bold)
                    (incf y 22))
            (:secret (glass:fb-text fb 16 (+ y 8) a :size (wm-size 13) :color (glass:rgb 100 100 100) :font font)
                     (glass:fb-text fb 110 y b :size (wm-size 26) :color (glass:rgb 20 60 120) :font bold)
                     (incf y 38))
            (:note (dolist (l (wrap a 12 (- w 32)))
                     (glass:fb-text fb 16 y l :size (wm-size 12) :color (glass:rgb 110 110 110) :font font)
                     (incf y 16))
                   (incf y 4))
            (:warn (dolist (l (wrap a 12 (- w 32)))
                     (glass:fb-text fb 16 y l :size (wm-size 12) :color (glass:rgb 150 60 20) :font font)
                     (incf y 16))
                   (incf y 4))))))))

(defun wm-show-vnc-window (port lines &optional (title "VNC"))
  "Put LINES on the screen as an ordinary WM window — a surface with no input, drawn
   once.  An ordinary window on purpose: it can be moved, raised and closed like
   everything else, and it does not grab anything."
  (add-surface port
               (lambda (fb)
                 (wm-render-vnc-window fb lines)
                 (let ((first t))
                   (values (lambda (down k) (declare (ignore down k)) nil)   ; swallows keys
                           nil
                           (lambda () (prog1 first (setf first nil))))))
               :title title :width (car *vnc-window-size*) :height (cdr *vnc-window-size*)))

(defun wm-start-vnc (port &optional seat)
  "Start plain VNC for SEAT and show the credential.  Returns the transport, or NIL if
   it could not be opened — in which case the window says why, because an item that
   silently does nothing is worse than one that reports a bound port."
  (let ((seat (port-seat port seat)))
    (handler-case
        (multiple-value-bind (tr credential note) (serve-seat-vnc seat)
          ;; The seat's clipboard, not the session's: this is one person's password,
          ;; and pasting it should not hand it to whoever else is sitting here.
          (when credential
            (ignore-errors (glass:clipboard-set (seat-clipboard seat) credential
                                                :owner :vnc :name "vnc password")))
          (wm-show-vnc-window port (wm-vnc-window-lines tr credential note))
          tr)
      (error (e)
        (wm-show-vnc-window port (list (list :head "VNC could not start")
                                       (list :warn (princ-to-string e)))
                            "VNC")
        nil))))

(defun wm-vnc-menu-items (port &optional seat)
  "The serving items for SEAT's root menu, built at every open so they say what is true
   now: a toggle, and — while a credential is in force — a way to see it again.

   THE LABEL NAMES THE PORT WHILE IT IS OPEN, and says when there is no password on it.
   That is the whole point of putting this on the menu rather than in a launcher: the
   item is also the indicator, so `am I exposed right now, and to whom' is answered by
   opening the menu and reading one line, rather than by running `ss' on a box somebody
   may not have a shell on.

   The `show it again' item exists because the password is generated: a person who
   closes the window has no way back to a credential nothing else knows, and stopping
   and restarting VNC to see it would mint a DIFFERENT one and break the viewer that
   had saved the first."
  (when seat
    (let ((tr (seat-vnc-transport seat)))
      (if tr
          (let ((credential (transport-credential tr)))
            (cons (cons (format nil "Stop serving VNC (~a~:[ — NO PASSWORD~;~])"
                                (transport-endpoint tr) credential)
                        (lambda () (stop-seat-vnc seat)))
                  (when credential
                    (list (cons "Show the VNC password"
                                (lambda ()
                                  (wm-show-vnc-window
                                   port (wm-vnc-window-lines tr credential nil))))))))
          (list (cons "Serve this seat over VNC…"
                      (lambda () (wm-start-vnc port seat))))))))

(defun wm-open-selection-menu (port surf x y &optional seat)
  "Open the selection menu over SURF at (X,Y), or return NIL if there is nothing to
   offer — a NIL return is the caller's signal to let the press through to the app."
  (when-let* ((text (wm-surface-live-selection surf))
              (seat (port-seat port seat))
              ;; the items close over the seat that opened the menu, so what they say is
              ;; said to that person — the selection was theirs
              (items (wm-selection-menu-items text seat)))
    (let ((menu (make-wm-menu :x x :y y :hover -1 :title "Selection" :items items)))
      (setf (seat-menu seat) (wm-place-menu menu seat x y)))))

(defun wm-open-submenu (parent idx action seat)
  "Open ACTION's submenu as PARENT's child, to the right of PARENT's item IDX."
  (let ((sub (make-wm-menu :hover -1 :title (car (nth idx (wm-menu-items parent)))
                           :items (cdr action))))                ; (:submenu ITEM...) -> ITEMs
    (wm-menu-render sub)
    (setf (wm-menu-child parent)
          (wm-place-menu sub seat
                         (+ (wm-menu-x parent) (glass:fb-width (wm-menu-fb parent)) -2)
                         (+ (wm-menu-y parent) +menu-titleh+ (* idx +menu-itemh+) -1)))))

(defun wm-menu-pointer (port root mask x y &optional seat)
  "Route a pointer event to the open menu tree ROOT (a menu + its submenu chain).
   Hover opens/closes submenus; a left-press on a leaf runs it and dismisses all;
   a click off every menu dismisses.

   The whole tree belongs to the seat that opened it: the hover highlight, the submenu
   chain and the dismiss all recomposite THAT seat.  Another seat never sees this menu
   and its screen is not touched by any of it."
  (let* ((seat (port-seat port seat))
         (left (logtest mask 1))
         (chain (wm-menu-chain root))
         (menu (find-if (lambda (m) (not (eq :outside (wm-menu-index m x y)))) (reverse chain))))
    (cond
      ((null menu)                                              ; off every menu
       (when (logtest mask 5)
         (setf (seat-menu seat) nil) (composite-seat seat)
         (clim-token-settle port seat)))       ; the menu pinned the token; it is gone
      (t
       (let ((idx (wm-menu-index menu x y)))
         (cond
           ((integerp idx)
            (let ((action (wm-item-action (nth idx (wm-menu-items menu)))))
              (unless (eql idx (wm-menu-hover menu))            ; hover moved within this menu
                (setf (wm-menu-hover menu) idx
                      (wm-menu-child menu) nil)                 ; drop any sibling's submenu
                (when (wm-submenu-p action) (wm-open-submenu menu idx action seat))
                (wm-menu-render menu)
                (composite-seat seat))
              (when left
                (cond
                  ((wm-submenu-p action)                        ; keep it open, don't dismiss
                   (unless (wm-menu-child menu)
                     (wm-open-submenu menu idx action seat) (composite-seat seat)))
                  (t                                            ; leaf: run + dismiss the whole tree
                   (setf (seat-menu seat) nil) (composite-seat seat)
                   (clim-token-settle port seat)                ; …and it no longer pins
                   (when action (wm-menu-run port action seat)))))))
           (t                                                   ; over the title strip
            (unless (eql (wm-menu-hover menu) -1)
              (setf (wm-menu-hover menu) -1) (wm-menu-render menu) (composite-seat seat)))))))))

(defun wm-on-pointer (port mask x y &optional seat)
  "Route one seat's pointer.  Every decision here — which window is under the pointer,
   whether it is already on top, whose menu is open, whose drag is in flight, which
   window has this keyboard — is read from and written to SEAT, so two people can be
   dragging two windows, or the same window to two different places, at once."
  (let ((seat (port-seat port seat)))
    (when-let ((menu (seat-menu seat)))                      ; an open menu grabs the pointer
      (wm-menu-pointer port menu mask x y seat)
      (return-from wm-on-pointer))
    (let ((down (logtest mask 1)))
      (cond
        ((seat-drag seat)                                    ; a move or resize in progress
         (destructuring-bind (obj mode . rest) (seat-drag seat)
           (ecase mode
             (:move
              (destructuring-bind (dx dy) rest
                (let ((ncx (- x dx)) (ncy (- y dy)))
                  (cond
                    ((seat-drag-wire seat)                   ; already wireframe (no flapping)
                     (if down (wm-drag-wire-to port obj ncx ncy seat)
                         (wm-drag-wire-drop port obj ncx ncy seat)))  ; release -> land the window
                    (down (wm-drag-move port obj ncx ncy seat))  ; opaque; wireframe if laggy
                    (t (wm-drag-move-opaque port obj ncx ncy seat)))))) ; release -> final opaque
             ;; A resize changes the window's SIZE, which is the session's — so it
             ;; recomposites every seat, not just this one.
             (:resize (destructuring-bind (x0 y0 cw0 ch0) rest
                        (wm-resize port obj (+ cw0 (- x x0)) (+ ch0 (- y y0)))
                        (composite-all port))))
           (unless down                                      ; release: end the drag
             (setf (seat-drag seat) nil)
             (when (eq mode :move) (wm-sync-sheet port obj seat))   ; McCLIM's menu coords
             ;; The drag pinned the token; it has landed, so a press somebody else made
             ;; while it was in flight — held rather than obeyed — takes effect now.
             (clim-token-settle port seat))))
        (t
         (multiple-value-bind (obj region cx cy cw ch) (wm-hit port x y seat)
           ;; Two token questions, both answered from the hit test that was needed anyway.
           ;; First: a driver whose pointer has left every CLIM window is no longer
           ;; driving anything, so the token goes free for whoever is still in one.
           (clim-token-follow-pointer port seat obj)
           ;; Second: a press anywhere on a CLIM window — its title bar and its resize
           ;; corner included — takes a FREE token, so that raising a window after a lull
           ;; still moves CLIM's keyboard focus to it (WM-RAISE asks CLIM-DRIVER-P).  It
           ;; does NOT contest a held one: taking the application out of somebody's hands
           ;; is a press INSIDE the window, and grabbing a title bar is not that.
           (when (and obj (not (wm-surface-p obj)) (logtest mask 7))
             (clim-token-claim port seat))
           (cond
             ((and (null obj) (logtest mask 5))              ; press on workspace: root menu
              (wm-open-menu port x y seat) (composite-seat seat))
             ((null obj))                                    ; workspace: ignore
             ((eq region :winmenu)                           ; title-bar wedge: the Window Menu
              (when down
                (wm-raise port obj seat)
                (when (wm-surface-p obj) (setf (seat-focus-surface seat) obj))
                (wm-open-window-menu port obj cx cy seat) (composite-seat seat)))
             ((eq region :resize)                            ; bottom-right corner: start a resize
              (when down
                (wm-raise port obj seat)
                (when (wm-surface-p obj) (setf (seat-focus-surface seat) obj))
                (setf (seat-drag seat) (list obj :resize x y cw ch))
                (composite-seat seat)))
             ((eq region :title)                             ; title bar: start a move
              (when down
                (wm-raise port obj seat)
                (when (wm-surface-p obj) (setf (seat-focus-surface seat) obj))
                (setf (seat-drag seat)
                      (list obj :move (- x (wm-pos-x obj seat)) (- y (wm-pos-y obj seat))))
                (composite-seat seat)))
             ;; Right-press inside the window that is holding the selection: the selection
             ;; menu, over the words themselves.  WM-OPEN-SELECTION-MENU answers NIL when
             ;; this window owns nothing worth a menu, and then the clause fails and the
             ;; press goes to the app below exactly as it always did — button 3 is the
             ;; application's until there is something of the session's to say about it.
             ((and (wm-surface-p obj) (logtest mask 4)
                   (wm-open-selection-menu port obj x y seat))
              (composite-seat seat))
             ((wm-surface-p obj)                             ; content of a surface window
              (when down
                (setf (seat-focus-surface seat) obj)
                ;; Raise only if it is not already the frontmost window — the same guard
                ;; the McCLIM branch below has, and for a bigger reason.  DOWN is true of
                ;; every motion event with a button held, so a drag or a text selection
                ;; INSIDE this window used to run a full-screen composite per event: a
                ;; whole-desktop repaint, marked as :FULL damage, which then makes the
                ;; RFB sender diff the entire screen instead of the box that changed.  It
                ;; painted the pixels that were already there, every time.  A window that
                ;; is already on top has nothing to raise and nothing to repaint.
                ;;
                ;; Both halves are THIS SEAT's: frontmost-for-this-seat, raised for this
                ;; seat, recomposited on this seat's screen.  Seat A raising a window does
                ;; not reorder seat B's stack, so it must not skip a raise because the
                ;; window happens to be on top of B's.
                (unless (eq obj (wm-topmost port seat))
                  (wm-raise port obj seat) (composite-seat seat)))
              (when (wm-surface-on-pointer obj)
                (funcall (wm-surface-on-pointer obj) mask (- x cx) (- y cy))))
             (t                                              ; content of a McCLIM window
              ;; ASKED FIRST, before the raise: this is the gesture that takes the McCLIM
              ;; token, and WM-RAISE just below hands CLIM's keyboard focus to the window
              ;; only if the asking seat is the driver.  Claiming after the raise (which
              ;; is where the take used to happen, down inside EMIT-POINTER-EVENTS) meant
              ;; a second seat's first click got the token but not the focus, and its
              ;; typing went to whatever window the previous driver had raised.
              ;;
              ;; MAY is NIL when the driver is mid-gesture: the press is remembered and
              ;; applied when that gesture ends, and meanwhile it reaches the window
              ;; manager (this seat may still raise and move the window on its own screen)
              ;; but not McCLIM.
              (let ((may (clim-token-claim port seat :press (logtest mask 7))))
              (when down (setf (seat-focus-surface seat) nil))   ; keyboard back to CLIM
              ;; Raise unless it is already the frontmost window — which now means
              ;; frontmost of ALL windows, not merely of the McCLIM ones.  Against the
              ;; old mirrors-only test, clicking a McCLIM window that sat under a
              ;; terminal did nothing: it was already first among mirrors, so no raise
              ;; and no composite, and the window stayed buried.
              (when (and down (not (eq obj (wm-topmost port seat))))
                (wm-raise port obj seat) (composite-seat seat))
              ;; The window manager's half of the click is done and it was this seat's.
              ;; The CLIM half is McCLIM's, which has one pointer, so it happens only if
              ;; the claim above said this seat may drive it.
              (when may
                (emit-pointer-events port (glass-mirror-sheet obj) mask
                                     (- x cx) (- y cy) seat)))))))))))

;;; ---- run ---------------------------------------------------------------------

(defvar *wm-spawn-seat* nil
  "The seat whose menu pick is launching a window, or NIL.  A spec-spawning call chain is
   long and passes through user code (an app's own invoker), so the seat that asked rides
   a dynamic binding rather than an extra argument on eight launchers — it is read once,
   by WM-ADD-SURFACE*, to decide whose keyboard the new window takes.")

(defun wm-add-surface* (port surf &optional seat)
  "Add SURF to the session and raise it.  A NEW WINDOW OPENS ON TOP FOR EVERYBODY: the z
   ticket goes on the window itself, which is what every seat reads for a window it has
   never restacked, so nobody has to be told about it.  The KEYBOARD, though, goes only
   to the seat that opened it — a window appearing on my screen must not take the keys
   out from under you mid-sentence."
  (setf (glass-port-cascade port) (mod (+ (glass-port-cascade port) 28) 200))
  (push surf (glass-port-surfaces port))
  (setf (window-own-z surf) (incf (glass-port-zclock port)))
  (setf (seat-focus-surface (port-seat port (or seat *wm-spawn-seat*))) surf)
  surf)

(defun add-surface (port make-fn &key (title "surface") (width 800) (height 600))
  "GENERIC, app-agnostic surface launcher — the public extension point for hosting
   ANY external glass-surface app as a decorated WM window.  Makes a fresh WIDTH x
   HEIGHT framebuffer, calls MAKE-FN on it to build the app over that fb; MAKE-FN
   returns (values ON-KEY ON-POINTER DIRTY-P) — the same surface contract the
   terminal exposes (on-key/on-pointer forward RFB input, dirty-p repaints into the
   fb and reports change).  The WM decorates, composites, drags, raises and closes
   it like any window.  Nothing here knows about any particular app.

   Three more values are OPTIONAL and default to what an app that returns three has
   always had: COPY-P (how the content translated — a scrolling app hands its
   CopyRect to the compositor instead of being re-encoded; see the COPY-P slot),
   CLOSE-FN (tear the app down when the window closes) and RESIZE-FN.  They are
   extra return values rather than extra arguments because MAKE-FN is what builds
   the app: it is the only thing that knows whether the app HAS a translation to
   report, and it does not know that until it has made it."
  (let* ((fb (glass:make-framebuffer width height (glass:rgb 255 255 255)))
         (c (glass-port-cascade port)))
    (multiple-value-bind (on-key on-pointer dirty-p copy-p close-fn resize-fn)
        (funcall make-fn fb)
      (wm-add-surface* port
        (make-wm-surface :fb fb :x (+ 40 c) :y (+ 40 c (wm-titleh)) :title title
                         :on-key on-key :on-pointer on-pointer :dirty-p dirty-p
                         :copy-p copy-p :close-fn close-fn :resize-fn resize-fn)))))

(defun wm-add-terminal (port &key (cols 80) (rows 24) (ppem 14))
  "Create a terminal (shell in a pty) and add it as a WM surface window.

   PPEM is scaled by the density of the seat that asked for the window — the FIRST place
   glass consults SEAT-SCALE, and the cheapest.  scribe rasterises glyphs from outlines at
   whatever ppem it is handed, so on a 2x seat this is a sharper terminal rather than a
   bigger one; on a 1x seat the arithmetic is the identity and nothing moves."
  (let* ((ppem (seat-ppem (port-seat port *wm-spawn-seat*) ppem))
         (tm (glass-term:make-terminal :cols cols :rows rows :ppem ppem))
         (c (glass-port-cascade port)))
    (glass-term:start-pump tm)
    (wm-add-surface* port
      (make-wm-surface :fb (glass-term:terminal-fb tm)
                       :x (+ 40 c) :y (+ 40 c (wm-titleh)) :title "terminal"
                       :on-key (lambda (down k) (glass-term:on-key tm down k))
                       :on-pointer (lambda (mask lx ly) (glass-term:on-mouse tm mask lx ly))
                       :dirty-p (lambda () (glass-term:terminal-take-dirty tm))
                       :resize-fn (lambda (w h) (glass-term:resize-terminal-px tm w h))
                       :close-fn (lambda () (glass-term:kill-terminal tm))))))

(defun wm-add-tabterm (port &key (cols 80) (rows 24) (ppem 14))
  "A tabbed terminal (several shells, a tab bar) as a WM surface window.

   Density-scaled the same way as WM-ADD-TERMINAL; see there."
  (let ((tt (glass-term:make-tabbed-terminal
             :cols cols :rows rows
             :ppem (seat-ppem (port-seat port *wm-spawn-seat*) ppem)))
        (c (glass-port-cascade port)))
    (wm-add-surface* port
      (make-wm-surface :fb (glass-term:tabterm-fb tt)
                       :x (+ 40 c) :y (+ 40 c (wm-titleh)) :title "terminal"
                       :on-key (lambda (down k) (glass-term:tabterm-on-key tt down k))
                       :on-pointer (lambda (mask lx ly) (glass-term:tabterm-on-mouse tt mask lx ly))
                       :dirty-p (lambda () (glass-term:tabterm-take-dirty tt))
                       :close-fn (lambda () (glass-term:tabterm-kill tt))))))

(defun %loom-fn (pkg name)
  "The bound function named NAME in package PKG, or NIL — used to reach loom/glass
   by name so glass need not depend on it."
  (let ((p (find-package pkg)))
    (and p (let ((s (find-symbol name p))) (and s (fboundp s) s)))))

(defun fb-generation-poll (fb)
  "A DIRTY-P thunk for a surface whose content FB is written by somebody else's render
   loop: true exactly when FB's generation has moved since the last poll.  Every glass
   drawing primitive bumps the generation (FB-TOUCH), so this needs no cooperation from
   the renderer beyond drawing through — or explicitly touching — the framebuffer.

   The alternative the compositor falls back to for a surface with NO dirty-p is 'assume
   the whole screen changed', which for a permanently-open window means a full-screen
   recomposite every tick forever, whether or not anything moved."
  (let ((seen -1))
    (lambda ()
      (let ((g (glass:fb-generation fb)))
        (and (not (eql g seen)) (setf seen g) t)))))

(defun wm-browse-default-url ()
  "The start page for (:browse) with no URL: about:blank, so the window appears
   instantly (no page fetch) — type a URL in the address bar to go somewhere."
  "about:blank")

(defun wm-add-browser (port &optional url &key (width 900) (height 620))
  "Open URL (default: about:blank) as a loom browser surface window WITH CHROME —
   loom.glass:attach-browser renders a toolbar (back / forward / reload + address
   bar) over weft's page into a glass framebuffer; the WM decorates it and routes
   RFB input to the live browser.  loom/glass is an OPTIONAL runtime dependency,
   resolved by name (no .asd dep — glass must not depend on loom, which depends on
   glass)."
  (unless url (setf url (wm-browse-default-url)))
  (let ((attach-b (%loom-fn '#:loom.glass "ATTACH-BROWSER"))
        (onk      (%loom-fn '#:loom.glass "ON-KEY"))
        (onp      (%loom-fn '#:loom.glass "ON-POINTER"))
        (pump     (%loom-fn '#:loom.glass "PUMP-LOOP"))
        (sel      (%loom-fn '#:loom.glass "SELECTION-TEXT"))
        (stop     (%loom-fn '#:loom.glass "STOP")))
    (unless (and attach-b onk onp pump)
      (error "loom/glass not loaded — (ql:quickload :loom/glass)"))
    (let* ((fb (glass:make-framebuffer width height (glass:rgb 255 255 255)))
           (c (glass-port-cascade port))
           (app (funcall attach-b url fb)))
      (prog1
          (wm-add-surface* port
            (make-wm-surface :fb fb :x (+ 40 c) :y (+ 40 c (wm-titleh))
                             :title "browser"
                             ;; What this window has highlighted at the moment somebody
                             ;; asks — loom reads its live page selection, the same state
                             ;; the highlight is painted from, so the answer is gone the
                             ;; instant the highlight is.  An older loom without
                             ;; SELECTION-TEXT simply has no selection to offer.
                             :selection-fn (and sel (lambda () (funcall sel app)))
                             :on-key (lambda (down k) (funcall onk app down k))
                             :on-pointer (lambda (mask lx ly) (funcall onp app mask lx ly))
                             ;; loom's pump paints into FB only when the page or the chrome
                             ;; actually changed, and touches it when it does — so the fb
                             ;; generation is exactly this window's damage signal.  Without
                             ;; a dirty-p the compositor must assume "unknown extent" and
                             ;; recomposite the whole screen every tick for as long as the
                             ;; window is open, browsing or not.
                             :dirty-p (fb-generation-poll fb)
                             ;; loom's PAINT already works out, per frame, whether it was
                             ;; a pure scroll of the previous one, and leaves that
                             ;; translation on the fb for whoever serves it.  Under the WM
                             ;; nobody serves this fb directly, so the compositor takes the
                             ;; hint here and maps it onto the screen — a scroll then goes
                             ;; out as a CopyRect instead of a whole-window re-encode.
                             :copy-p (lambda () (glass:fb-take-copy fb))
                             ;; on close, stop weft's render pump (else it re-renders forever)
                             :close-fn (and stop (lambda () (funcall stop app)))))
        (sb-thread:make-thread (lambda () (funcall pump app)) :name "wm-browse-pump")))))

(defun %read-file-bytes (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((b (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence b s) b)))

(defun %img-sampler (pkg img)
  "Extract (values W H SAMPLER) from a decoded IMG via PKG's img-w/h/rgba, where
   SAMPLER is (fn Y X) -> (values R G B A) over the straight-alpha RGBA."
  (let ((w (funcall (%loom-fn pkg "IMG-W") img))
        (h (funcall (%loom-fn pkg "IMG-H") img))
        (rgba (funcall (%loom-fn pkg "IMG-RGBA") img)))     ; w*h*4 straight-alpha
    (values w h (lambda (y x)
                  (let ((o (* (+ (* y w) x) 4)))
                    (values (aref rgba o) (aref rgba (+ o 1)) (aref rgba (+ o 2)) (aref rgba (+ o 3))))))))

(defun %decode-rgba (pkg path)
  "Decode PATH via PKG's decode-image-bytes (pigment and — for compat — weft.render
   share this API), to (values W H SAMPLER), or NIL if PKG isn't loaded."
  (let ((dec (%loom-fn pkg "DECODE-IMAGE-BYTES")))
    (when dec
      (let ((img (funcall dec (%read-file-bytes path))))
        (unless img (error "~a could not decode ~a" pkg path))
        (%img-sampler pkg img)))))

(defun %decode-image (path &key width height)
  "Decode PATH to (values W H SAMPLER), SAMPLER = (fn Y X) -> (values R G B A).
   Prefers pigment (our pure-CL PNG/JPEG/GIF/WebP/SVG codecs, split out of weft),
   then weft.render (compat), then opticl — all OPTIONAL, by name.  WIDTH/HEIGHT
   rasterise a VECTOR image (SVG) at that size (crisp — no upscale); raster formats
   ignore them (fixed size)."
  (cond
    ((and (or width height) (%loom-fn '#:pigment "DECODE-IMAGE-BYTES"))   ; sized SVG render
     (let ((img (funcall (%loom-fn '#:pigment "DECODE-IMAGE-BYTES")
                         (%read-file-bytes path) nil :width width :height height)))
       (unless img (error "pigment could not decode ~a" path))
       (%img-sampler '#:pigment img)))
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
          (make-wm-surface :fb fb :x (+ 40 c) :y (+ 40 c (wm-titleh))
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
;;;   (:surface MAKE-FN &key title width height)  ANY external glass-surface app —
;;;       MAKE-FN, called with a fresh fb, returns (values on-key on-pointer
;;;       dirty-p) (the generic, warren-agnostic extension hook; see ADD-SURFACE)
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
    (:surface  (apply #'add-surface port (cdr spec)))
    ;; A McCLIM frame class.  TITLE is the window's name — it goes in as the frame's
    ;; PRETTY-NAME, which is where WM-SHEET-TITLE reads the title bar from (the third
    ;; argument below only names the thread).  Default is the class name, which is
    ;; what the generic apps want and what a registered app rarely does: a person
    ;; reading a title bar wants "Speak", not SPEAK-BOX.
    (t (destructuring-bind (class &key (width 480) (height 320) title) spec
         (wm-run-frame port (apply #'make-application-frame class
                                   :frame-manager (find-frame-manager :port port)
                                   :width width :height height
                                   (when title (list :pretty-name title)))
                       (or title (princ-to-string class)))))))

(defun wm-menu-run (port action &optional seat)
  "Run a chosen menu ACTION: a window spec (launch it) or, as an escape hatch, a
   thunk (call it).  A launched window is the SESSION's — it appears for everybody — but
   its keyboard goes to the seat that picked it off the menu."
  (if (functionp action)
      (funcall action)
      (let* ((*wm-spawn-seat* (port-seat port seat))
             ;; ...and its density, so a window placed by this seat is placed with the
             ;; same chrome the compositor will draw around it.
             (*wm-scale* (if *wm-spawn-seat* (seat-scale *wm-spawn-seat*) *wm-scale*)))
        (wm-spawn-spec port action)
        (composite-all port))))

(defun wm-app-item (label pkg class-name &rest args)
  "A menu item launching McCLIM frame CLASS-NAME in PKG, or NIL if that package/
   class isn't loaded (so the menu only offers what's actually available)."
  (let ((sym (and (find-package pkg) (find-symbol class-name pkg))))
    (and sym (find-class sym nil) (list* label sym args))))

(defun wm-surface-item (label pkg fn-name &rest args)
  "A menu item launching the glass-surface app FN-NAME in PKG — warren and anything
   else that speaks the (values ON-KEY ON-POINTER DIRTY-P) contract — or NIL when that
   package is not loaded.  The frame-class sibling of this is WM-APP-ITEM; both exist so
   the menu can be written once and still only offer what the image actually has.

   BY NAME, deliberately.  glass must not depend on loom, warren or spool: they depend on
   glass.  Every first-party app is therefore discovered the same way an out-of-tree one
   would be, which also means a menu entry cannot be the thing that breaks a build."
  (let ((sym (and (find-package pkg) (find-symbol fn-name pkg))))
    (and sym (fboundp sym) (list* label :surface sym :title label args))))

(defun wm-sample-image ()
  "The path of glass's bundled sample image, if present."
  (let ((dir (ignore-errors (asdf:system-source-directory '#:mcclim-glass))))
    (and dir (let ((p (merge-pathnames "assets/sample.png" dir))) (and (probe-file p) (namestring p))))))

(defvar *extra-apps* '()
  "External-app root-menu items (LABEL . SPEC), contributed by other packages via
   REGISTER-APP and APPENDED to the default menu — so an out-of-tree glass app
   (e.g. a file browser) joins the workspace menu without editing WM-DEFAULT-MENU.
   Empty by default, so the stock menu is unchanged until something registers.")

(defun register-app (label spec)
  "Register an external app so it appears in the workspace root menu: push
   (LABEL . SPEC) onto *EXTRA-APPS* (which WM-DEFAULT-MENU appends).  SPEC is any
   window spec — typically (:surface MAKE-FN &key title width height) for a pure
   glass-surface app.  Re-registering the same LABEL replaces the prior entry, so
   this is idempotent.  Registrations made BEFORE run-wm show up automatically; to
   add one to an ALREADY-running desktop, also refresh that port's menu-items
   ((setf (glass-port-menu-items port) (wm-default-menu)))."
  (setf *extra-apps* (append (remove label *extra-apps* :key #'car :test #'equal)
                             (list (cons label spec))))
  label)

(defun wm-default-menu ()
  "The workspace root menu: the applications this project wrote, in one flat list, plus
   anything REGISTER-APP'd appended after.  Built at call time, so an entry appears only
   when the image actually has the code behind it.

   WHAT IS NOT HERE, AND WHY.  The menu used to lead with Inspect and Debug — Clouseau on
   (LIST-ALL-PACKAGES), and a menu item whose entire function was to signal an error and
   drop the desktop into a debugger.  Both are unreliable enough right now that offering
   them from the root menu of a desktop is a trap: the second one deliberately breaks the
   session that chose it, and a person clicking down a menu to see what a machine can do
   has not consented to that.  They are three lines to restore for anyone debugging glass
   itself, which is who they were ever for.

   The McCLIM demos went for a different reason: Calculator, Gadget Demo, Listener and
   Climacs are other people's applications, wired in when the menu needed contents to
   prove the launcher worked.  It has plenty now, and this is a desktop with a point of
   view rather than a McCLIM demo harness.  Nothing was deleted — WM-APP-ITEM still exists
   and each is one line — they are simply not what this machine leads with.

   ONE TERMINAL, the resizable one.  There were two, and the tabbed one has tabs but no
   RESIZE-FN at all: glass-term has no tabterm resize entry point to wire up, so a tabbed
   terminal is a window the WM cannot resize.  On a desktop whose whole screen now follows
   the viewer, a window that cannot change size is the worse offer, and :TABTERM is still
   there for anyone who wants tabs more than they want edges."
  (append
   (remove nil
           (list
            ;; The things you reach for, first and unnested.  A submenu was worth it when
            ;; half of these did not exist.
            '("Terminal"  :terminal)
            '("Browse"    :browse)                                  ; loom + weft
            (wm-surface-item "Files" '#:warren "DESKTOP-SURFACE"    ; warren, a glass surface
                             :width 1000 :height 640)
            (wm-app-item "Podcasts" '#:spool.app "PODCASTS"         ; spool: feeds, cache, playback
                         :width 980 :height 660 :title "Podcasts")
            ;; chord and stave, as windows.  Each says what it needs when it has no model
            ;; (GLASS_VOICE / GLASS_EARS), which is why they are offered unconditionally:
            ;; the app explaining the absence beats the menu hiding the capability.
            (wm-app-item "Speak" '#:glass-speak "SPEAK-BOX"
                         :width 560 :height 320 :title "Speak")
            (wm-app-item "Listen" '#:glass-listen "LISTEN-BOX"
                         :width 560 :height 400 :title "Listen")
            (let ((img (wm-sample-image))) (and img (list "Image Viewer" :image img)))))
   *extra-apps*))                                          ; external apps (empty unless registered)

(defvar *wm-tick-round* 0 "Monotonic compositing-round counter for the WM tick loop.")

(defun wm-tick (port)
  "One frame of the desktop, for EVERY seat.

   The shape is the one it always had — poll the surfaces, fold in the McCLIM repaints
   the mirrors accumulated, composite what changed — with the damage now expressed in
   each window's own coordinates and converted per seat, because one content change
   lands on a different rectangle of each screen.

   THE ROUND IS WHY THIS IS ONE FUNCTION AND NOT A LOOP OVER SEATS OF THE OLD ONE.  A
   surface reports its scroll translation consumingly, so if each seat asked in turn the
   first would take it and the rest would composite as though nothing moved — the second
   seat would lose the CopyRect, which on a window drag over a nested desktop is 24-57x
   the bytes.  So the tick takes each hint ONCE (*WM-COPY-ROUND*) and every seat judges
   the same one against its own stack, clip and screen; and it holds the scrolling
   surfaces' framebuffer locks across the whole round, so the pixels cannot move out from
   under the second seat's blit.  Normally that is one lock and one scrolling window."
  ;; Nobody holds the McCLIM token forever: a holder who has gone quiet loses it here, so
  ;; the next person's click is a silent take and not a contest.  One integer compare when
  ;; the token is free, which is what an idle desktop's every tick is.
  (clim-token-idle-sweep port)
  (let ((scrollers '()))
    (dolist (s (glass-port-surfaces port))
      ;; isolate each surface's poll: a signalling dirty-p is caught, the surface skipped
      ;; this frame (and culled if it keeps failing), and the compositor loop kept ALIVE —
      ;; one bad window can't take down the desktop.
      (handler-case
          (let ((dp (wm-surface-dirty-p s)))
            (cond ((null dp) (port-damage-all port))              ; unknown extent
                  ((funcall dp)
                   (port-damage-window port s)                      ; its own box, per seat
                   (when (wm-surface-copy-p s) (push s scrollers))))
            (setf (wm-surface-err-count s) 0))                      ; healthy poll -> reset
        (error (e) (wm-note-surface-error port s e))))
    ;; McCLIM repaints were accumulated per seat by PRESENT-MIRROR, already in each
    ;; seat's screen coordinates; drain them alongside the surface damage so a burst of
    ;; ~20 repaints still becomes ONE composite per seat.
    (let ((*wm-copy-round* (incf *wm-tick-round*)))
      (wm-with-scroll-locks
       scrollers
       (lambda ()
         (dolist (seat (glass-port-seats port))
           ;; Surface damage and McCLIM damage now land in the SAME per-seat
           ;; accumulator, so one drained box is the union the old loop built by hand,
           ;; and an idle desktop still composites exactly nothing.
           (let ((pend (seat-take-pending seat)))
             (when pend
               (ignore-errors
                (composite-seat seat (unless (eq pend :full) pend)))))))))))

;;; ---- running a session, and exposing a seat: two things ----------------------
;;;
;;; They were one call.  RUN-WM's own docstring said "Run a mini OPEN LOOK desktop OVER
;;; VNC", and it meant it literally: it built the session and opened a listener, so there
;;; was no way to have a session that serves nothing and no posture available other than
;;; "the session is the wire".  docs/seats-and-transports.md is the argument.
;;;
;;; The split is underneath, in three pieces that each do one thing, and RUN-WM is still
;;; the convenience it always was — a session, plus its home seat exposed — spelled out in
;;; the same order it always ran in, because the order is observable: the listener comes
;;; up BEFORE the initial windows are spawned, and those take the better part of a second
;;; each, so a client that connects during startup finds a desktop filling in rather than
;;; a refused connection.

(defun make-wm-session (&key (port 5900) (width 1000) (height 720) menu
                             background (background-mode :cover) (scale 1))
  "Build a window-manager session and return its GLASS-PORT: WM mode on, a screen of
   WIDTH x HEIGHT for the home seat, a root menu, and a wallpaper if asked for.

   NO LISTENER IS OPENED and no window is spawned.  PORT names the port instance and
   becomes the home seat's SEAT-PORT-NUM — the port it will serve on IF a transport is
   opened on it, which is a separate decision (OPEN-SEAT-TRANSPORT)."
  (let ((p (find-glass-port :port port)))
    (setf (glass-port-wm-p p) t
          (glass-port-screen-w p) width (glass-port-screen-h p) height
          (glass-port-fb p) (glass:make-framebuffer width height +wm-teal+)
          (glass-port-menu-items p) (or menu (wm-default-menu)))
    ;; THE DENSITY, BEFORE ANY WINDOW EXISTS.  A viewer only learns its display's scale when
    ;; it opens a window, by which time START-WM-SESSION has already spawned the session's
    ;; apps at whatever ppem they were handed — a terminal booted at 1x, sharp chrome around
    ;; small text, and no way to fix it short of re-rendering a window that already exists.
    ;; Passing it in here means the first seat is born knowing, so everything spawned after
    ;; is spawned at the right size.  GLASS-SDL:DISPLAY-SCALE is how a local viewer answers
    ;; this without a session to look at.
    (unless (eql scale 1)
      (dolist (seat (glass-port-seats p)) (setf (seat-scale seat) scale)))
    (when background (wm-set-background p background :mode background-mode))
    p))

(defun start-wm-session (p specs)
  "Start P's CLIM event loop and open SPECS as windows.  Returns P."
  (climi::restart-port p)                                   ; event-loop thread
  (dolist (spec specs p)
    (wm-spawn-spec p spec)
    (sleep 0.7)                                             ; stagger for distinct cascade slots
    (composite-all p)))

(defun run-wm-loop (p)
  "P's compositing loop.  Blocks forever — this is the desktop running."
  ;; Surface windows (terminals) render asynchronously in their own threads.
  ;; DAMAGE TRACKING: only recomposite when a surface actually changed (its
  ;; dirty-p reports so and clears) — an idle desktop does ZERO compositing, so
  ;; no wasted full-screen redraws.  A NIL dirty-p means "always redraw" (safe
  ;; default); a static surface (image) reports NIL forever.  WM operations
  ;; (move/resize/menu/...) recomposite directly, so they're not gated here.
  (loop (sleep 1/60) (wm-tick p)))

(defun run-session (specs &key (port 5900) (width 1000) (height 720) menu
                               background (background-mode :cover))
  "Run a mini OPEN LOOK desktop that SERVES NOTHING.  Same session RUN-WM runs — the same
   windows, the same compositing, the same seat — with no socket open onto it and no way
   in until a seat decides on one.  Blocks.

   That is the default the design note argues for and this is where it is spelled: a
   session is what runs, a seat is what you connect to, and a transport is what carries
   it.  To let somebody in, hold the port (RUN-SESSION blocks, so from another thread or
   over a control socket) and call (OPEN-SEAT-TRANSPORT (PORT-SEAT P)).

   PORT names the port instance and the home seat's would-be listening port; it opens
   nothing.  Everything else is RUN-WM's."
  (let ((p (make-wm-session :port port :width width :height height :menu menu
                            :background background :background-mode background-mode)))
    (start-wm-session p specs)
    (run-wm-loop p)))

(defun run-wm (specs &key (port 5900) (width 1000) (height 720) menu
                          background (background-mode :cover)
                          (address *seat-bind-address*)
                          (kind *seat-transport-kind*) path)
  "Run a mini OPEN LOOK desktop over VNC.  Each spec is a decorated window:
   (FRAME-CLASS &key WIDTH HEIGHT) for a McCLIM app, or (:terminal &key COLS ROWS
   PPEM) for a shell terminal.  Right-click the workspace for a root menu; pass
   MENU — a list of (LABEL . SPEC) labelled window specs (same vocabulary as
   SPECS), or (LABEL . THUNK) for an arbitrary action — to override its items.
   BACKGROUND is a desktop-wallpaper image path (any format pigment decodes, incl.
   SVG), placed per BACKGROUND-MODE (:cover/:fit/:stretch/:center/:tile).  Serves
   on PORT.

   RUN A SESSION, AND EXPOSE ITS HOME SEAT — the two things it has always done, now
   said in two calls (RUN-SESSION is the first of them on its own).  Nothing about it
   changed: same order, same port, same interface.

   ADDRESS is the interface the home seat's listener binds, defaulting to
   *SEAT-BIND-ADDRESS* — 0.0.0.0, every interface, exactly as before.  Passing
   \"127.0.0.1\" is the one-line opt-in to a desktop reachable only from this box.

   KIND :RFB-UNIX (or a PATH) is the next line of that same opt-in and the end of it: the
   home seat is then on a socket file only its owner can open, instead of a port every
   process on the box can reach.  ADDRESS stops meaning anything, and so does the port —
   which is the point.  The default is unchanged and deliberately so; see
   *SEAT-TRANSPORT-KIND*."
  (let ((p (make-wm-session :port port :width width :height height :menu menu
                            :background background :background-mode background-mode)))
    (open-seat-transport (port-seat p) :port-num port :address address :kind kind :path path)
    (start-wm-session p specs)
    (run-wm-loop p)))

(defun wm-seat-serve-address (asked)
  "The interface a NEW seat's listener binds, given what the caller ASKED for — a string
   they typed, or :DEFAULT for `you decide'.

   A string is obeyed, whatever it is: somebody who writes 0.0.0.0 has said the word, and
   OPEN-SEAT-TRANSPORT under this binds precisely what it is told.

   :DEFAULT is *SEAT-BIND-ADDRESS* when this desktop can actually demand a credential, and
   127.0.0.1 when it cannot.  That is SERVE-SEAT-VNC's rule (see its address parameter), and
   it is here for the same reason: ADD-WM-SEAT is typed at a live desktop — over a control
   socket, at a REPL — by somebody who named a port number and nothing else, and choosing an
   interface on their behalf is exactly what obliges us not to choose an exposing one.  A
   session-wide password with no DES verifier in the image is not a credential, it is a
   listener that rejects every client, so it counts as none: the question here is the same
   one the handshake will actually ask.

   The overrule is announced, because a seat you cannot reach from the machine you are
   sitting at is otherwise indistinguishable from a seat that failed to start."
  (if (stringp asked)
      asked
      (let ((credential (and (glass:vnc-auth-available-p)
                             (glass:effective-vnc-password :inherit))))
        (cond (credential *seat-bind-address*)
              ((loopback-address-p *seat-bind-address*) *seat-bind-address*)
              (t (format *error-output*
                         "~&glass: this session has no VNC credential, so the new seat is on ~
                          127.0.0.1 and not ~a — pass :ADDRESS to say otherwise.~%"
                         *seat-bind-address*)
                 (force-output *error-output*)
                 "127.0.0.1")))))

(defun add-wm-seat (port &key port-num (width 1000) (height 720) name background
                              (background-mode :cover) (audio t) (serve t)
                              (address :default) (scale 1)
                              (kind *seat-transport-kind*) path)
  "Attach a SECOND (third, …) person to a running desktop: a screen of their own at
   WIDTH x HEIGHT serving on PORT-NUM, with their own pointer, keyboard, focus, menu,
   clipboard, sound and arrangement of the SAME windows.

   Nothing about the session changes.  The new seat starts with no views, so it sees
   every window exactly where the others have it, and diverges one window at a time as
   it moves or raises them.  The tick loop picks it up on its next frame because it
   loops over PORT's seats.

   PORT-NUM is required and deliberately has no default: the obvious ones are ports
   somebody is already serving a desktop on, and a seat that quietly failed to listen
   is a seat nobody can find.  AUDIO t (the default) also gives them their own mix and
   their own microphone, on the ports beside it (5923 -> 5933 out, 5934 in) — so seat
   ports want a decade of room between them, and a seat with no sound is one :audio nil.

   SERVE t (the default) also opens the RFB listener — the same convenience RUN-WM is, and
   for the same reason: a second person asked for by port number wants the port.  :SERVE NIL
   makes the seat and leaves it unreachable, which is what a seat whose wire is something
   other than a VNC listener wants (OPEN-SEAT-TRANSPORT later, or a transport this file does
   not know about).

   AND IT OPENS IT ON LOOPBACK UNLESS THIS SESSION HAS A CREDENTIAL.  ADDRESS is a string
   you mean literally; unset, it is 127.0.0.1 while there is no password to go with the port
   and *SEAT-BIND-ADDRESS* once there is (WM-SEAT-SERVE-ADDRESS).  The opt-in-serving split
   moved the listener out of RUN-WM and did not come this far, so `add a second seat' went
   on binding 0.0.0.0 with no authentication — a whole desktop, on every interface, from a
   call whose only argument was a port number.  A seat is a person's place at the session,
   not a decision to publish it.

   Returns the seat."
  (check-type port-num (integer 1 65535))
  (let ((seat (add-seat port :scale scale
                             :name (or name (format nil "seat-~d" port-num))
                             :port-num port-num :width width :height height
                             :fb (glass:make-framebuffer width height +wm-teal+))))
    ;; A seat added to a session that already has a wallpaper gets that wallpaper, cut
    ;; for ITS screen: the picture is the session's taste and this is another pair of
    ;; eyes on it.  Recording the source (not just the pixels) is what lets this seat
    ;; re-cut it if its screen is later resized.
    (let ((path (or background (glass-port-bg-path port)))
          (mode (if background background-mode (glass-port-bg-mode port))))
      (when path
        (setf (seat-bg-path seat) path
              (seat-bg-mode seat) mode
              (seat-bg seat) (ignore-errors (wm-render-background seat path :mode mode)))))
    ;; the transport first: it is what fills SEAT-INJECTOR, which is the keyboard this
    ;; seat's dictation types on
    (when serve (open-seat-transport seat :port-num port-num
                                          :address (wm-seat-serve-address address)
                                          :kind kind :path path))
    (when audio (start-seat-audio port :seat seat))
    (composite-seat seat)
    seat))
