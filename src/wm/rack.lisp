;;;; src/wm/rack.lisp — the RACK: windows laid out like rack-mounted equipment, for a phone.
;;;;
;;;; A touch screen has no room for overlapping windows to be dragged about and found again, and a
;;;; phone turns.  So in the rack layout (WM-SET-LAYOUT WM :RACK) the windows are units in a rack:
;;;; one strip, in order, down the screen in portrait and across it in landscape.  The screen shows
;;;; a few UNITS of it -- two across in landscape; in portrait as many as are at least
;;;; *RACK-MIN-UNIT* tall, two or three -- and a window takes one or more (its SPAN: 1U, 2U ...),
;;;; first chosen from the size its app asks for.  More windows than fit is a longer rack, and it
;;;; SCROLLS, as niri's and PaperWM's strips of columns do.
;;;;
;;;; A UNIT IS ALL CONTENT: no title bar, no buttons.  The gestures are the window manager's:
;;;;   a tap, or a drag that starts at once    the app's, as always (held back only until the
;;;;                                           window manager knows it is not a long press)
;;;;   a LONG PRESS (*RACK-LONG-PRESS-MS*)     picks the unit up, outlined; then
;;;;     drag                                  moves it along the rack (the rack scrolls at its
;;;;                                           ends); letting go drops it ON THE SLOT under it
;;;;     let go where it was                   the unit's menu, at the finger: Taller / Shorter,
;;;;                                           Earlier / Later, Add..., Replace..., Close.  ADD
;;;;                                           drops a new unit where the finger was, as a drop
;;;;                                           does (inside a unit splits it; on its first slot
;;;;                                           steps it down); REPLACE puts another app in THIS
;;;;                                           unit's place -- same slot, same span, the units
;;;;                                           around it untouched -- and closes this one
;;;;   a drag the app does not take            scrolls the rack, with the finger (an app says which
;;;;                                           drags are its own with DRAG-P, its make-fn's ninth
;;;;                                           value; one with no pointer function takes none)
;;;;   the overflow bar (along the rack's edge, a finger wide to touch)   drag: scroll the rack;
;;;;                                           on release it settles on a whole unit
;;;;   an EMPTY SLOT                           a long press: the apps, to open one there; a drag
;;;;                                           scrolls; a tap does nothing.  There is no root menu:
;;;;                                           the rack fills the screen, an empty slot is filler
;;;;                                           like a unit's, and a long press is the one way in.
;;;;
;;;; THE MODEL: A UNIT HANGS FROM ITS SLOT.  A unit is a divider in the rack -- the slot it starts
;;;; at -- and it fills down to the next unit's, or to the bottom of its screen (a screen is
;;;; RACK-PAGE slots, the same both ways up -- landscape shows two of them at a time and scrolls;
;;;; nothing spans two screens).  So its span is not chosen, it follows: put on the first
;;;; slot of an empty screen a unit is the whole screen, on the last it is one slot.  Dropping a
;;;; unit puts its divider there:
;;;;   inside another unit     splits it: the upper keeps what is above the drop
;;;;   on another unit's top   two dividers cannot share a slot: that one steps down a slot (and
;;;;                           any it then lands on, in turn)
;;;;   an empty slot           it is there, and empty slots above it stay empty
;;;; Taking a unit out lets the one above it fill down again; closing one does too.  SHORTER caps a
;;;; unit's span, leaving empty slots below it; TALLER lifts the cap, or pushes the next divider
;;;; down a slot.  A new window splits the unit that has the keys (it takes that unit's last slot),
;;;; or, if that unit is one slot, starts the next screen.
;;;; A turn of the phone (WM-RESIZE-SCREEN) lays the rack out again the other way.  With the
;;;; on-screen keyboard up (WM-SET-OBSCURED) the rack fits what is left: a portrait phone shows two
;;;; slots above it instead of three, a unit is never taller than that, and the one being typed in
;;;; is scrolled into view; the arrangement is the same when it goes.
;;;;
;;;; Windows never overlap, so z-order does not matter.  The rack is the home seat's: another seat
;;;; (a desktop watching the phone's session) sees the windows where the rack put them.

(in-package #:glass.wm)

(defparameter *rack-units-landscape* 2 "Units across the screen in landscape.")
(defparameter *rack-min-unit* 250
  "A portrait unit's least height, in screen pixels: the units are as many as fit, two or three.")
(defparameter *rack-max-units* 3 "The most units a portrait screen is divided into.")
(defparameter *rack-title-scale* 0
  "The rack's title bars' density (*WM-TITLE-SCALE*): 0, none -- a unit is all content, and its
   window manager is reached by a long press (see the header).")
;; A unit's SURF-POS is its divider (the slot it starts at); its SURF-SPAN is NIL, or a cap SHORTER
;; set.  RACK-SPAN is what it actually spans.
(defparameter *rack-long-press-ms* 450 "How long a still press takes to pick a unit up.")
(defparameter *rack-bar-touch* 18 "How wide, in pixels, the overflow bar answers a finger.")

(defun rack-p (wm) (eq (wm-layout wm) :rack))
(defun rack-vertical-p (wm) (>= (screen-h wm) (screen-w wm)))
(defun rack-bar () "The overflow bar's thickness." (max 3 (round (* 3 *wm-scale*))))

(defun rack-page (wm)
  "Slots to a SCREEN of the rack -- what a unit fills down to: as many of *RACK-MIN-UNIT* as the
   screen's LONG side holds, two or three.  The same both ways up: turning the phone keeps the
   arrangement, and landscape shows fewer of its slots at a time (RACK-UNITS) and scrolls."
  (max 2 (min *rack-max-units* (floor (max (screen-w wm) (screen-h wm)) *rack-min-unit*))))

(defun rack-units (wm)
  "Slots on the screen at once: a screen's worth in portrait, *RACK-UNITS-LANDSCAPE* across."
  (if (rack-vertical-p wm) (rack-page wm) (min (rack-page wm) *rack-units-landscape*)))

(defun rack-along (wm)
  "The screen's length along the rack -- what the keyboard leaves of it, in portrait."
  (if (rack-vertical-p wm) (- (screen-h wm) (wm-obscured wm)) (screen-w wm)))
(defun rack-across (wm)
  "Across it, less the overflow bar -- and, in landscape, what the keyboard covers."
  (- (if (rack-vertical-p wm) (screen-w wm) (- (screen-h wm) (wm-obscured wm))) (rack-bar)))

(defun rack-visible-units (wm)
  "Slots on the screen now: RACK-UNITS -- or, with the keyboard up in portrait, as many of them as
   the rest of the screen holds (a three-slot phone above its keyboard is two), each a little
   smaller so they fill it."
  (if (and (rack-vertical-p wm) (plusp (wm-obscured wm)))
      (max 1 (min (rack-units wm) (round (rack-along wm) (floor (screen-h wm) (rack-units wm)))))
      (rack-units wm)))

(defun rack-unit (wm) (max 1 (floor (rack-along wm) (rack-visible-units wm))))

(defun wm-set-obscured (wm px)
  "The host's keyboard now hides PX pixels at the bottom of the screen (0: none).  The rack lays
   itself out in what is left, and shows the unit with the keys."
  (let ((px (max 0 (min (floor (screen-h wm) 2) px))))
    (unless (= px (wm-obscured wm))
      (setf (wm-obscured wm) px)
      (when (rack-p wm)
        (rack-layout wm :composite nil)
        (let ((f (wm-focus wm))) (when (and f (member f (wm-rack-order wm))) (rack-show wm f)))
        (rack-layout wm))))
  px)

(defun rack-page-end (wm pos)
  "The slot after the last of POS's screen."
  (* (rack-page wm) (1+ (floor pos (rack-page wm)))))

(defun rack-next-start (wm s)
  "The first divider below S's, or NIL."
  (let ((best nil))
    (dolist (w (wm-rack-order wm) best)
      (when (and (not (eq w s)) (surf-pos w) (> (surf-pos w) (surf-pos s))
                 (or (null best) (< (surf-pos w) best)))
        (setf best (surf-pos w))))))

(defun rack-span (wm s)
  "The slots S spans: down to the next unit or its screen's bottom, whichever is first, and no
   further than its cap (SHORTER)."
  (if (null (surf-pos s))
      1
      (let* ((next (rack-next-start wm s))
             (limit (- (min (or next most-positive-fixnum) (rack-page-end wm (surf-pos s))) (surf-pos s))))
        (max 1 (if (surf-span s) (min (surf-span s) limit) limit)))))

(defun rack-span-for (wm w h &optional fixed)
  "The span an app asking for a W x H window gets: the units its height (portrait) or width
   (landscape) fills, rounded -- or, FIXED (it cannot be resized), rounded UP, so all of it shows --
   at least one and at most a screen."
  (let ((need (if (rack-vertical-p wm) (+ h (wm-titleh) (* 2 (wm-border))) (+ w (* 2 (wm-border))))))
    (max 1 (min (rack-units wm) (if fixed (ceiling need (rack-unit wm)) (round need (rack-unit wm)))))))

(defun rack-end (wm s) (+ (surf-pos s) (rack-span wm s)))

(defun rack-sorted (wm)
  "The units in rack order: by position."
  (sort (copy-list (wm-rack-order wm)) #'< :key #'surf-pos))

(defun rack-slots (wm)
  "How many slots the rack has: whole screens, to the one its last unit is on -- and, while a unit
   is lifted, a screen more, so it can be dropped past the end."
  (let ((last (reduce #'max (wm-rack-order wm) :key (lambda (s) (or (surf-pos s) 0)) :initial-value -1)))
    (+ (if (minusp last) 0 (rack-page-end wm last))
       (if (wm-rack-lift wm) (rack-page wm) 0))))

(defun rack-length (wm)
  "The whole rack's length, in pixels."
  (* (rack-unit wm) (rack-slots wm)))

(defun rack-clamp-scroll (wm v)
  (max 0 (min v (max 0 (- (rack-length wm) (rack-along wm))))))

(defun rack-start (wm s)
  "Where S begins along the rack (unscrolled)."
  (* (rack-unit wm) (surf-pos s)))

(defun rack-pos-at (wm x y)
  "The slot under screen point (X,Y)."
  (max 0 (floor (+ (if (rack-vertical-p wm) y x) (wm-rack-scroll wm)) (rack-unit wm))))

(defun rack-settle (wm anchor)
  "ANCHOR's divider is where it was put; no two dividers may share a slot, so the ones at or below
   it that it now meets step down a slot each, in turn."
  (let ((cursor (surf-pos anchor)))
    (dolist (w (sort (remove anchor (copy-list (wm-rack-order wm))) #'< :key #'surf-pos))
      (when (>= (surf-pos w) (surf-pos anchor))
        (when (<= (surf-pos w) cursor) (setf (surf-pos w) (1+ cursor)))
        (setf cursor (surf-pos w))))))

(defun rack-next-screen (wm)
  "The first slot of the screen after the last unit's."
  (let ((last (reduce #'max (wm-rack-order wm) :key (lambda (s) (or (surf-pos s) -1)) :initial-value -1)))
    (if (minusp last) 0 (rack-page-end wm last))))

(defun rack-sync (wm)
  "The rack follows the windows: gone ones out; one it does not know starts the next screen."
  (setf (wm-rack-order wm) (remove-if-not (lambda (s) (member s (wm-surfaces wm))) (wm-rack-order wm)))
  (dolist (s (reverse (wm-surfaces wm)))
    (unless (member s (wm-rack-order wm))
      (setf (surf-pos s) (rack-next-screen wm) (surf-span s) nil)
      (setf (wm-rack-order wm) (append (wm-rack-order wm) (list s))))))

(defun rack-layout (wm &key (composite t))
  "Put every window in its rack slot at the current scroll, resizing those that can be to fill it
   (a window with no RESIZE-FN keeps its size, top-left in its slot).  Then repaint, unless
   COMPOSITE is NIL."
  (rack-sync wm)
  (setf (wm-rack-scroll wm) (rack-clamp-scroll wm (wm-rack-scroll wm)))
  (let* ((*seat* nil)
         (vert (rack-vertical-p wm)) (u (rack-unit wm)) (across (rack-across wm))
         (b (wm-border)) (th (wm-titleh)))
    (dolist (s (wm-rack-order wm))
      (let* ((len (min (* u (rack-span wm s)) (rack-along wm)))   ; never taller than the screen shows
             (p (- (rack-start wm s) (wm-rack-scroll wm)))
             (cx (if vert b (+ p b)))
             (cy (if vert (+ p th b) (+ th b)))
             (cw (max 32 (- (if vert across len) (* 2 b))))
             (ch (max 32 (- (if vert len across) th (* 2 b)))))
        (when (and (surf-resize-fn s) (not (and (= cw (surf-cw s)) (= ch (surf-ch s)))))
          (handler-case (funcall (surf-resize-fn s) cw ch)
            (error (e) (note-surface-error wm s e)))
          (forget-copy-bases wm s))
        ;; an app that keeps a size of its own is CENTRED in its slot both ways (one bigger than
        ;; the slot keeps its top-left, and is cut off at the slot's edges: RACK-DRAW-UNITS)
        (seat-move wm s (+ cx (max 0 (floor (- cw (surf-cw s)) 2)))
                        (+ cy (max 0 (floor (- ch (surf-ch s)) 2)))))))
  (when composite (wm-composite wm))
  t)

(defun wm-set-layout (wm layout)
  "LAYOUT :RACK lays WM's windows out in rack units (this file); NIL leaves them floating where
   they are.  Returns LAYOUT."
  (setf (wm-layout wm) layout (wm-rack-drag wm) nil (wm-rack-press wm) nil (wm-rack-lift wm) nil
        *wm-title-scale* (if (eq layout :rack) *rack-title-scale* nil))
  (dolist (s (wm-surfaces wm)) (setf (surf-deco-fb s) nil))       ; the bars at their new height
  (when (eq layout :rack)
    (setf (wm-rack-order wm) nil)
    (let ((i 0))
      (dolist (s (reverse (wm-surfaces wm)))           ; one slot each, oldest first
        (setf (surf-pos s) i (surf-span s) nil)
        (incf i)
        (setf (wm-rack-order wm) (append (wm-rack-order wm) (list s)))))
    (rack-layout wm))
  layout)

(defun rack-show (wm s)
  "Scroll so all of S is in view (as little as that takes)."
  (let* ((start (rack-start wm s)) (len (min (* (rack-unit wm) (rack-span wm s)) (rack-along wm)))
         (v (wm-rack-scroll wm)) (along (rack-along wm)))
    (setf (wm-rack-scroll wm)
          (rack-clamp-scroll wm (cond ((< start v) start)
                                      ((> (+ start len) (+ v along)) (- (+ start len) along))
                                      (t v))))))

;;; ---- the window manager's hooks (wm.lisp calls these when the layout is :RACK) -------------------

(defun rack-opened (wm s w h)
  "A new window S: dropped on the empty slot the root menu was opened on; else it splits the unit
   that had the keys (taking its last slot); else it starts the next screen.  Then in view."
  (declare (ignore w h))
  (let* ((want (wm-rack-want wm))
         (had (find-if (lambda (w) (and (not (eq w s)) (surf-pos w))) (rest (member s (wm-surfaces wm)))))
         (had (and had (member had (wm-rack-order wm)) had)))
    (setf (wm-rack-want wm) nil)
    ;; REPLACE: S takes OLD's divider and cap, OLD closes; nothing else moves
    (when (and (consp want) (eq (first want) :replace))
      (let* ((old (second want)) (pos (surf-pos old)) (cap (surf-span old)))
        (when (member old (wm-surfaces wm)) (wm-close wm old))
        (rack-insert wm s pos)
        (when cap (setf (surf-span s) cap) (rack-layout wm))
        (return-from rack-opened s)))
    (rack-insert wm s (cond (want want)
                            ((and had (> (rack-span wm had) 1)) (+ (surf-pos had) (rack-span wm had) -1))
                            (t (rack-next-screen wm))))))

(defun rack-insert (wm s pos)
  "Put S's divider at POS (see the header's model), and show it."
  (setf (wm-rack-order wm) (append (remove s (wm-rack-order wm)) (list s))
        (surf-pos s) (max 0 pos) (surf-span s) nil)
  (rack-settle wm s)
  (rack-layout wm :composite nil)
  (rack-show wm s)
  (rack-layout wm))

(defun rack-closed (wm)
  "A unit closed: the one above it fills down again."
  (rack-layout wm))

(defun rack-taller (wm s)
  "One slot more: lift S's cap, else push the next divider down -- not past S's screen."
  (let ((span (rack-span wm s)) (next (rack-next-start wm s)) (end (rack-page-end wm (surf-pos s))))
    (when (< (+ (surf-pos s) span) end)
      (cond ((and (surf-span s) (or (null next) (> next (+ (surf-pos s) span))))
             (setf (surf-span s) (1+ span)))
            (next
             (setf (surf-span s) nil)
             (let ((w (find next (wm-rack-order wm) :key #'surf-pos)))
               (incf (surf-pos w))
               (rack-settle wm w))))
      (rack-layout wm))))

(defun rack-shorter (wm s)
  "One slot less: a cap, leaving the slot below it empty."
  (when (> (rack-span wm s) 1)
    (setf (surf-span s) (1- (rack-span wm s)))
    (rack-layout wm)))

(defun rack-move (wm s delta)
  "S trades places with the unit before (DELTA -1) or after (+1) it."
  (let* ((sorted (rack-sorted wm)) (i (position s sorted))
         (o (and i (<= 0 (+ i delta) (1- (length sorted))) (nth (+ i delta) sorted))))
    (when o
      (rotatef (surf-pos s) (surf-pos o))
      (rotatef (surf-span s) (surf-span o))
      (rack-layout wm :composite nil)
      (rack-show wm s)
      (rack-layout wm))))

(defvar *rack-menu-slot* nil "The slot the unit menu being built was opened on (RACK-OPEN-MENU).")

(defun rack-menu-items (wm s)
  "The unit menu's rack entries: Taller / Shorter where they can, trading places, and the two ways
   to put an app here."
  (let ((span (rack-span wm s)) (slot (or *rack-menu-slot* (surf-pos s))))
    (append
     (when (< (+ (surf-pos s) span) (rack-page-end wm (surf-pos s)))
       (list (cons "Taller" (lambda () (rack-taller wm s)))))
     (when (> span 1)
       (list (cons "Shorter" (lambda () (rack-shorter wm s)))))
     (list (cons "Earlier" (lambda () (rack-move wm s -1)))
           (cons "Later" (lambda () (rack-move wm s 1)))
           ;; ADD...: a menu of its own, opened when this item is chosen -- i.e. after the finger has
           ;; lifted -- in this menu's place.  (A submenu opened under the finger, and the lift
           ;; that chose Open chose whatever app was drawn under it.)
           (cons "Add..." (lambda () (rack-open-apps-menu wm (seat-px (cur-seat wm)) (seat-py (cur-seat wm))
                                                           :want slot)))
           (cons "Replace..." (lambda () (rack-open-apps-menu wm (seat-px (cur-seat wm)) (seat-py (cur-seat wm))
                                                              :want (list :replace s))))))))

(defun rack-unit-at (wm x y)
  "The unit whose slot holds screen point (X,Y), or NIL past the last one."
  (find-if (lambda (s) (destructuring-bind (sx0 sy0 w h) (rack-slot-box wm s)
                         (and (<= sx0 x (+ sx0 w -1)) (<= sy0 y (+ sy0 h -1)))))
           (wm-rack-order wm)))

(defun rack-in-content-p (s x y)
  (and (<= (sx s) x (+ (sx s) (surf-cw s) -1)) (<= (sy s) y (+ (sy s) (surf-ch s) -1))))

;;; ---- gestures ------------------------------------------------------------------------------------

(defun rack-along-of (wm x y) (if (rack-vertical-p wm) y x))
(defun rack-slop () (max 8 (round (* 8 *wm-scale*))))
(defun rack-now-ms () (floor (* 1000 (get-internal-real-time)) internal-time-units-per-second))

(defun rack-begin-press (wm s mask x y)
  "A finger down in unit S (NIL: an empty slot): hold it back until it is a tap, a drag or a long
   press."
  (setf (wm-rack-press wm) (list s x y (rack-now-ms) mask)))

(defun rack-press-pointer (wm mask x y down)
  "The press not yet decided: moved past a tap's slop, it was the app's drag all along -- hand it
   the press and this; lifted, it was the app's tap.  Still and held, RACK-TICK makes it a lift."
  (destructuring-bind (s x0 y0 t0 mask0) (wm-rack-press wm)
    (declare (ignore t0))
    (when (or (not down) (> (max (abs (- x x0)) (abs (- y y0))) (rack-slop)))
      (setf (wm-rack-press wm) nil)
      (if s (%focus wm s :force (surf-text-p s)) (%focus wm nil))
      (cond
        ;; A DRAG THE APP DOES NOT TAKE SCROLLS THE RACK: one on the infill round a smaller app,
        ;; on an app with no pointer, or where the app's DRAG-P says it is not its own (asked
        ;; BEFORE the app sees the press: an app whose buttons act on a press must not act)
        ((and down (not (rack-app-drag-p s x0 y0)))
         (setf (wm-rack-drag wm) (list :content (rack-along-of wm x0 y0) (wm-rack-scroll wm) t))
         (rack-drag-to wm x y t))
        ;; a press on the INFILL round a smaller app is the unit's: a tap there only focuses it
        ((and s (rack-in-content-p s x0 y0))
         (when down (setf (wm-grab wm) s))
         (%to-app s mask0 x0 y0)
         (%to-app s mask x y))))))

(defun rack-app-drag-p (s x y)
  "Is a drag starting at screen (X,Y) in S the app's?  Not on the infill, not without a pointer
   function; else its DRAG-P's answer, and an app that has none takes every drag (as it always did)."
  (and s (surf-on-pointer s) (rack-in-content-p s x y)
       (or (null (surf-drag-p s))
           (ignore-errors (funcall (surf-drag-p s) (- x (sx s)) (- y (sy s)))))))

(defun rack-tick (wm)
  "Each tick: a press held still long enough picks its unit up."
  (let ((p (wm-rack-press wm)))
    (when (and p (>= (- (rack-now-ms) (fourth p)) *rack-long-press-ms*))
      (destructuring-bind (s x y t0 mask) p
        (declare (ignore t0 mask))
        (setf (wm-rack-press wm) nil)
        (if (null s)
            ;; an empty slot: what to open there -- shown NOW, while the finger is still down, and
            ;; armed only once it lifts (WM-MENU-ARMED NIL, its origin the press), so the lift
            ;; chooses nothing
            (rack-open-apps-menu wm x y :want (rack-pos-at wm x y) :armed nil)
            (progn
              (setf (wm-rack-lift wm) (list s (- (rack-along-of wm x y) (rack-screen-start wm s)) nil))
              (wm-raise wm s)
              (wm-composite wm)))))))

(defun rack-open-apps-menu (wm x y &key want (armed t))
  "The applications, as a menu at (X,Y): what is chosen is dropped on slot WANT, or takes the place
   of the unit in WANT = (:REPLACE unit), or (WANT NIL) goes after the unit with the keys.  ARMED NIL: the finger that opened it is still down, and its lift where
   it pressed only arms the menu."
  (setf (wm-rack-want wm) want
        (wm-menu wm) (wm-place-menu (make-wm-menu :title (if (consp want) "Replace with" "Add")
                                                  :items (mapcar (lambda (a) (cons (first a) (list :app (first a))))
                                                                 (wm-apps wm)))
                                    (screen-w wm) (screen-h wm) x y (if armed 0 1))
        (wm-menu-armed (wm-menu wm)) armed
        (wm-menu-origin (wm-menu wm)) (cons x y))
  (wm-composite wm (menu-chain-box wm)))

(defun rack-screen-start (wm s)
  "Where S's slot begins on the screen, along the rack."
  (- (rack-start wm s) (wm-rack-scroll wm)))

(defun rack-place-lifted (wm s screen-start)
  "Put the lifted S with its slot's start at SCREEN-START along the rack (the rest stay put)."
  (let ((b (wm-border)))
    (if (rack-vertical-p wm)
        (seat-move wm s b (+ screen-start b))
        (seat-move wm s (+ screen-start b) b))))

(defun rack-lift-pointer (wm x y down)
  "The unit picked up: follow the finger along the rack, scrolling when it nears an end; let go,
   drop it where it is -- or, if it never moved, open its menu there."
  (destructuring-bind (s grab moved) (wm-rack-lift wm)
    (let* ((a (rack-along-of wm x y))
           (moved (or moved (> (abs (- a (+ grab (rack-screen-start wm s)))) (rack-slop)))))
      (setf (third (wm-rack-lift wm)) moved)
      (cond
        ((and down moved)
         (let* ((along (rack-along wm)) (edge (floor (rack-unit wm) 4)) (step (max 4 (floor edge 3))))
           (cond ((< a edge) (setf (wm-rack-scroll wm) (rack-clamp-scroll wm (- (wm-rack-scroll wm) step))))
                 ((> a (- along edge)) (setf (wm-rack-scroll wm) (rack-clamp-scroll wm (+ (wm-rack-scroll wm) step)))))
           (rack-layout wm :composite nil)
           (rack-place-lifted wm s (- a grab))
           (wm-composite wm)))
        (down nil)
        (moved
         (setf (wm-rack-lift wm) nil)
         (rack-drop wm s (+ (- a grab) (wm-rack-scroll wm))))
        (t
         (setf (wm-rack-lift wm) nil)
         (rack-layout wm)
         (rack-open-menu wm s x y))))))

(defun rack-drop (wm s start)
  "Drop S, whose top is now at START along the (unscrolled) rack, on the nearest slot (the
   header's model: inside a unit splits it, on a unit's top steps that one down)."
  (rack-insert wm s (round start (rack-unit wm))))

(defun rack-open-menu (wm s x y)
  "S's menu at the finger, already armed: the lift's release was the press that opened it."
  (%focus wm s :keyboard nil)
  (setf (wm-rack-want wm) nil)
  (setf (wm-menu wm) (wm-place-menu (make-wm-menu :title (surf-title s)
                                                  :items (let ((*rack-menu-slot* (rack-pos-at wm x y)))
                                                           (window-menu-items wm s)))
                                    (screen-w wm) (screen-h wm) x y 0)
        (wm-menu-armed (wm-menu wm)) t)
  (wm-composite wm (menu-chain-box wm)))

(defun rack-on-bar-p (wm x y)
  (if (rack-vertical-p wm)
      (>= x (- (screen-w wm) (max (rack-bar) *rack-bar-touch*)))
      (<= (- (screen-h wm) (wm-obscured wm) (max (rack-bar) *rack-bar-touch*)) y (- (screen-h wm) (wm-obscured wm)))))

(defun rack-press-bar (wm x y)
  (setf (wm-rack-drag wm) (list :bar (rack-along-of wm x y) (wm-rack-scroll wm) nil)))

(defun rack-drag-to (wm x y down)
  "The rack scrolled by a finger: on the overflow bar (:BAR) the thumb follows it, the whole rack
   scaled to the screen; on a unit (:CONTENT) the rack moves with it, as a page does.  On the lift
   the rack settles on a whole unit."
  (destructuring-bind (mode a0 v0 moved) (wm-rack-drag wm)
    (declare (ignore moved))
    (let* ((along (rack-along wm)) (total (max along (rack-length wm))) (d (- (rack-along-of wm x y) a0))
           (v (rack-clamp-scroll wm (if (eq mode :bar) (+ v0 (floor (* d total) along)) (- v0 d)))))
      (unless (= v (wm-rack-scroll wm))
        (setf (wm-rack-scroll wm) v)
        (rack-layout wm))
      (unless down
        (setf (wm-rack-drag wm) nil)
        (let ((u (rack-unit wm)))
          (setf (wm-rack-scroll wm) (rack-clamp-scroll wm (* u (round (wm-rack-scroll wm) u))))
          (rack-layout wm))))))

(defun rack-draw-lift (wm fb)
  "The lifted unit's outline: white over black, so it reads on any content."
  (let ((l (wm-rack-lift wm)))
    (when l
      (destructuring-bind (x y w h) (window-box (first l))
        (let ((t2 (max 3 (round (* 3 *wm-scale*)))))
          (glass:fb-frame fb x y w h glass:+white+ t2)
          (glass:fb-frame fb (+ x t2) (+ y t2) (max 0 (- w (* 2 t2))) (max 0 (- h (* 2 t2))) glass:+black+ 1))))))

;;; ---- drawing the units -------------------------------------------------------------------------

(defparameter *rack-panel* (glass:rgb 30 37 48)
  "A slot's colour behind its window: what shows round an app that keeps a size of its own.")

(defparameter *rack-seam* (glass:rgb 70 82 100) "The line between two units.")

(defun rack-slot-box (wm s)
  "S's slot on the screen: (x y w h)."
  (let ((start (rack-screen-start wm s)) (len (* (rack-unit wm) (rack-span wm s))) (across (rack-across wm)))
    (if (rack-vertical-p wm) (list 0 start across len) (list start 0 len across))))

(defun rack-draw-units (wm fb)
  "Each unit: its slot's panel, then its window clipped to the slot -- an app that keeps a size of
   its own (no RESIZE-FN) sits on the panel, top left, and one bigger than its slot is cut off
   there rather than drawn over its neighbour.  A lifted unit is drawn last, unclipped, over the rest."
  (let ((lifted (first (wm-rack-lift wm))))
    (dolist (s (wm-rack-order wm))
      (unless (eq s lifted)
        (destructuring-bind (x y w h) (rack-slot-box wm s)
          (glass:with-fb-clip (fb x y w h)
            (glass:fb-rect fb x y w h *rack-panel*)
            (handler-case (draw-surface wm s fb)
              (error (e) (note-surface-error wm s e)))
            ;; the seam to the next unit, so two panels side by side read as two
            (if (rack-vertical-p wm)
                (glass:fb-rect fb x (+ y h -1) w 1 *rack-seam*)
                (glass:fb-rect fb (+ x w -1) y 1 h *rack-seam*))))))
    ;; the empty slots' seams, so filler reads as slots too
    (let* ((u (rack-unit wm)) (v (wm-rack-scroll wm)) (vert (rack-vertical-p wm)) (across (rack-across wm)))
      (loop for i from (floor v u) to (floor (+ v (rack-along wm)) u)
            unless (find-if (lambda (w) (and (<= (surf-pos w) i) (< i (rack-end wm w)))) (wm-rack-order wm))
              do (let ((e (- (* (1+ i) u) v 1)))
                   (if vert
                       (glass:fb-rect fb 0 e across 1 *rack-seam*)
                       (glass:fb-rect fb e 0 1 across *rack-seam*)))))
    (when lifted
      (handler-case (draw-surface wm lifted fb)
        (error (e) (note-surface-error wm lifted e))))))

;;; ---- the overflow bar ----------------------------------------------------------------------------

(defun rack-draw-bar (wm fb)
  "Along the rack's edge (the right in portrait, the bottom in landscape): the whole rack as a dim
   track, the part on screen as a bright thumb.  A rack that fits shows the track alone."
  (let* ((vert (rack-vertical-p wm)) (t1 (rack-bar)) (along (rack-along wm))
         (total (max along (rack-length wm)))
         (start (floor (* along (wm-rack-scroll wm)) total))
         (len (max (* 4 t1) (floor (* along along) total)))
         (track (glass:rgb 30 37 48)) (thumb (glass:rgb 200 206 214)))
    (if vert
        (let ((x (- (screen-w wm) t1)))
          (glass:fb-rect fb x 0 t1 along track)
          (when (> total along) (glass:fb-rect fb x start t1 (min len (- along start)) thumb)))
        (let ((y (- (screen-h wm) (wm-obscured wm) t1)))
          (glass:fb-rect fb 0 y along t1 track)
          (when (> total along) (glass:fb-rect fb start y (min len (- along start)) t1 thumb))))))
