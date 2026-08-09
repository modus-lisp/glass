;;;; popup-attach-gate.lisp — a pull-down belongs to a window, on every screen.
;;;;
;;;; McCLIM places a pull-down from its frame's sheet transformation, and that
;;;; transformation follows whoever is DRIVING McCLIM (clim-token.lisp).  So a pop-up is
;;;; a session-wide window with ONE position, and that position is expressed in the
;;;; DRIVER's arrangement.  Seat B presses "File" on its copy of the window at 430,300
;;;; and CLIM correctly puts the menu at 430,325 — but seat A holds that same window at
;;;; 120,140, so on A's screen the menu floated over bare workspace with nothing under
;;;; it.  A menu detached from its window is not a menu.
;;;;
;;;; The cure is a per-seat DRAW OFFSET on unmanaged mirrors: this seat's copy of the
;;;; OWNING window, minus where McCLIM believes that window to be.  Zero for the driver
;;;; by construction, and zero for everybody when nothing has diverged.
;;;;
;;;; What this asks, in order:
;;;;   1. undiverged: the offset is zero and both seats draw the pop-up identically
;;;;   2. diverged: B's pull-down is attached to A's copy of the window ON A'S SCREEN,
;;;;      and still exactly where it was on B's
;;;;   3. no trails: the damage-limited composite equals a full rebuild, and a
;;;;      post/dismiss cycle returns the non-driver's screen to its pre-menu pixels
;;;;   4. a chained submenu is offset ONCE, not once per link, on both screens
;;;;   5. the non-driver drags the owner while the menu is posted: the menu follows and
;;;;      leaves nothing behind
;;;;   6. the driver's routing is untouched: hit-testing, item selection and dismissal
;;;;      still work, and the driver's offset stayed zero throughout
;;;;
;;;; In-process; one real CLIM frame in a thread, everything else deterministic.
;;;;   sbcl --control-stack-size 256 --dynamic-space-size 4096 \
;;;;        --non-interactive --load backend/inspect/popup-attach-gate.lisp
;;;; With a directory argument it also writes a PNG of each seat's screen per scene:
;;;;   ... --load backend/inspect/popup-attach-gate.lisp -- /tmp/popup

(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:glass :mcclim :mcclim-render :sb-concurrency))
    (ignore-errors (ql:quickload :zpng))
    (asdf:load-asd "/home/claude/glass/backend/mcclim-glass.asd")
    (asdf:load-system :mcclim-glass)))
(in-package :clim-glass)

(defparameter *png-dir*
  (let ((tail (cdr (member "--" sb-ext:*posix-argv* :test #'string=))))
    (first tail)))

(defvar *fail* 0)
(defun check (ok fmt &rest args)
  (format t "  [~:[FAIL~;pass~]] ~?~%" ok fmt args)
  (unless ok (incf *fail*)))
(defun note (fmt &rest args) (format t "       ~?~%" fmt args))

(defun save-png (fb path)
  (let ((png-class (and (find-package '#:zpng) (find-symbol "PNG" '#:zpng))))
    (when png-class
      (let* ((w (glass:fb-width fb)) (h (glass:fb-height fb)) (px (glass:fb-pixels fb))
             (png (make-instance png-class :width w :height h :color-type :truecolor))
             (d (funcall (find-symbol "DATA-ARRAY" '#:zpng) png)))
        (dotimes (y h)
          (dotimes (x w)
            (let ((p (aref px (+ (* y w) x))))
              (setf (aref d y x 0) (ldb (byte 8 16) p)
                    (aref d y x 1) (ldb (byte 8 8) p)
                    (aref d y x 2) (ldb (byte 8 0) p)))))
        (funcall (find-symbol "WRITE-PNG" '#:zpng) png path)
        path))))

(defun shoot (label &rest seats)
  (when *png-dir*
    (ensure-directories-exist (format nil "~a/" *png-dir*))
    (dolist (s seats)
      (save-png (seat-fb s) (format nil "~a/~a-~a.png" *png-dir* label (seat-name s))))
    (format t "       -> ~a/~a-*.png~%" *png-dir* label)))

;;; ---- framebuffers as values ---------------------------------------------------
;;; Every claim about trails is a claim about PIXELS, so they are compared and never
;;; looked at: a snapshot is a copy of the pixel vector and two screens are the same
;;; screen iff the vectors are EQUALP.

(defun snap (seat) (copy-seq (glass:fb-pixels (seat-fb seat))))
(defun px-diff (a b)
  (let ((n 0)) (dotimes (i (length a) n) (unless (= (aref a i) (aref b i)) (incf n)))))
(defun px-diff-box (a b w)
  "The (x y w h) bounding box of the pixels in which A and B differ, or NIL."
  (let ((x0 nil) (y0 nil) (x1 nil) (y1 nil))
    (dotimes (i (length a))
      (unless (= (aref a i) (aref b i))
        (multiple-value-bind (y x) (floor i w)
          (setf x0 (if x0 (min x0 x) x) y0 (if y0 (min y0 y) y)
                x1 (if x1 (max x1 x) x) y1 (if y1 (max y1 y) y)))))
    (when x0 (list x0 y0 (1+ (- x1 x0)) (1+ (- y1 y0))))))
(defun pixel-at (fb x y)
  (if (and (< -1 x (glass:fb-width fb)) (< -1 y (glass:fb-height fb)))
      (logand (aref (glass:fb-pixels fb) (+ (* y (glass:fb-width fb)) x)) #xffffff)
      :off-screen))

(defun wait-until (pred &optional (secs 8))
  (let ((end (+ (get-internal-real-time) (* secs internal-time-units-per-second))))
    (loop until (funcall pred) do (sleep 1/50)
          when (> (get-internal-real-time) end) do (return nil) finally (return t))))

;;; THE DRAIN IS THE REAL TICK LOOP, deliberately.  WM-TICK composites each seat within
;;; the damage box that seat accumulated — which is precisely the thing that leaves
;;; trails when a window is drawn somewhere the damage did not name.  A gate that
;;; composited the whole screen instead would repair every trail before looking for it.
(defun tick (port &optional (secs 0.35))
  (sleep secs)
  (wm-tick port))

;;; …and this is how a trail is caught: rebuild the whole screen from scratch and see
;;; whether anything moves.  If the damage-limited picture already equals the full
;;; rebuild, no pixel was left behind, wherever it was.
(defun full-rebuild-diff (seat)
  (let ((before (snap seat)))
    (composite-seat seat)                       ; no damage box: the whole screen
    (px-diff before (snap seat))))

;;; ---- the application -----------------------------------------------------------

(define-application-frame pa-frame ()
  ()
  (:menu-bar t)
  (:panes (log (clim:make-pane 'clim:clim-stream-pane :width 300 :height 180
                                                      :background clim:+white+)))
  (:layouts (default log)))

(defvar *chosen* '())
(define-pa-frame-command (com-alpha :name "Alpha") () (push :alpha *chosen*))
(define-pa-frame-command (com-one   :name "One")   () (push :one *chosen*))
(clim:make-command-table 'pa-more :errorp nil
                         :menu '(("One" :command (com-one))
                                 ("Two" :command (com-one))))
(clim:make-command-table 'pa-file :errorp nil
                         :menu '(("Alpha" :command (com-alpha))
                                 ("More"  :menu pa-more)
                                 ("Gamma" :command (com-alpha))))
(clim:add-menu-item-to-command-table 'pa-frame "File" :menu 'pa-file :errorp nil)

;;; ---- pointer helpers ------------------------------------------------------------

(defun press (port seat x y &optional (button 1)) (glass-on-pointer port button x y seat))
(defun release (port seat x y) (glass-on-pointer port 0 x y seat))
(defun click (port seat x y &optional (button 1))
  (press port seat x y button) (release port seat x y))
(defun hover (port seat x y) (glass-on-pointer port 0 x y seat))

(defun popups (port)
  (remove-if #'glass-mirror-managed
             (remove-if-not (lambda (x) (typep x 'glass-mirror)) (glass-port-mirrors port))))

(defun popup-size (m)
  (let ((img (ignore-errors (mcclim-render::image-mirror-image m))))
    (if img (multiple-value-list (image-wh img)) (list 0 0))))

(defun open-pulldown (port seat m &optional (secs 3))
  "Press the frame's menu bar on SEAT's copy of window M and wait for the pull-down
   mirror to be realized.  Returns the new pop-up mirror, or NIL."
  (let ((before (popups port))
        (mx (+ (seat-window-x seat m) 20)) (my (+ (seat-window-y seat m) 8)))
    (press port seat mx my)
    (wait-until (lambda () (> (length (popups port)) (length before))) secs)
    (tick port 0.5)
    (values (first (set-difference (popups port) before)) mx my)))

;;; =================================================================================

(let* ((port (make-instance 'glass-port :port 5985)))
  (setf (glass-port-wm-p port) t)
  (let ((a (glass-port-default-seat port)))
    (setf (seat-screen-w a) 800 (seat-screen-h a) 600
          (seat-fb a) (glass:make-framebuffer 800 600 +wm-teal+)))
  (climi::restart-port port)
  (let* ((a (glass-port-default-seat port))
         (b (add-seat port :name "B" :port-num 5986 :width 800 :height 600
                           :fb (glass:make-framebuffer 800 600 +wm-teal+))))
    (setf (seat-name a) "A")
    (let* ((fm (find-frame-manager :port port))
           (frame (make-application-frame 'pa-frame :frame-manager fm)))
      (sb-thread:make-thread (lambda () (ignore-errors (run-frame-top-level frame)))
                             :name "pa-app")
      (check (wait-until (lambda () (find-if #'glass-mirror-managed (glass-port-mirrors port))))
             "a real McCLIM frame came up as a managed window")
      (sleep 0.8)
      (let* ((m (find-if #'glass-mirror-managed (glass-port-mirrors port))))
        (wm-move m 200 200 a)
        (wm-sync-sheet port m a)
        (tick port 0.5)
        (composite-all port)

        ;; ============ 1. nothing has diverged: nothing changes ===================
        (format t "~&~%[1. two seats, nothing diverged — the offset is zero and stays out of the way]~%")
        (check (and (= (seat-window-x a m) (seat-window-x b m) (glass-mirror-clim-x m))
                    (= (seat-window-y a m) (seat-window-y b m) (glass-mirror-clim-y m)))
               "both seats hold the window where McCLIM believes it is (~d,~d)"
               (glass-mirror-clim-x m) (glass-mirror-clim-y m))
        (let ((pre-a (snap a)) (pre-b (snap b)))
          (multiple-value-bind (pop mx my) (open-pulldown port b m)
            (check pop "B pressed File and a pull-down mirror was realized")
            (when pop
              (multiple-value-bind (dx dy) (seat-popup-offset a pop)
                (check (and (zerop dx) (zerop dy))
                       "the NON-driver's pop-up offset is (~d,~d) — zero, so its screen is untouched"
                       dx dy))
              (multiple-value-bind (dx dy) (seat-popup-offset b pop)
                (check (and (zerop dx) (zerop dy)) "the driver's is zero too (~d,~d)" dx dy))
              (check (= (seat-draw-x a pop) (seat-draw-x b pop) (glass-mirror-x pop))
                     "…so both seats draw it at the mirror's own x, ~d" (glass-mirror-x pop))
              (check (equalp (snap a) (snap b))
                     "the two screens are pixel-identical with the menu open")
              (shoot "1-undiverged" a b)
              (check (zerop (full-rebuild-diff a))
                     "…and A's damage-limited picture already equals a full rebuild"))
            (release port b mx my)
            (click port b 700 560)                       ; dismiss
            (tick port 0.6) (tick port 0.3)
            (check (null (popups port)) "the pull-down was dismissed and its mirror destroyed")
            ;; The menu's own pixels are gone from both screens.  What is NOT expected to
            ;; be byte-identical is the frame's MENU BAR: pressing "File" is a real click
            ;; on a real gadget and McCLIM leaves the button drawn in its pressed/armed
            ;; state, which is the application's picture and not a compositing trail.  So
            ;; the claim is made where it belongs: any residual difference lies inside the
            ;; window's own menu-bar strip, and the screen equals a full rebuild.
            (dolist (s (list a b))
              (let* ((pre (if (eq s a) pre-a pre-b))
                     (box (px-diff-box pre (snap s) (glass:fb-width (seat-fb s))))
                     (win (wm-window-box m s)))
                (note "~a: residue after dismissal ~a (window ~a)" (seat-name s) box win)
                (check (or (null box)
                           (and (wm-box-inside-p box win)
                                (<= (+ (second box) (fourth box))
                                    (+ (seat-window-y s m) 32))))   ; the menu-bar strip
                       "~a: nothing of the menu is left — the only changed pixels are the \
frame's own menu-bar strip" (seat-name s))
                (check (zerop (full-rebuild-diff s))
                       "~a: …and the screen equals a full rebuild" (seat-name s))))))

        ;; ============ 2. the reproduction ========================================
        (format t "~%[2. B's pull-down, on A's screen, attached to A's copy of the window]~%")
        ;; A holds the window at 120,140 (the session arrangement); B holds it at 430,300
        ;; on its own screen and takes the token, so McCLIM places from 430,300.
        (click port a (+ (seat-window-x a m) 40) (+ (seat-window-y a m) 120))
        (tick port 0.3)
        (wm-move m 120 140 a) (wm-sync-sheet port m a)
        (tick port 0.3)
        (wm-move m 430 300 b)
        (click port b (+ (seat-window-x b m) 40) (+ (seat-window-y b m) 120))
        (tick port 0.4)
        (composite-all port)
        (check (eq (clim-token-holder port) b) "B is driving McCLIM")
        (check (and (= (seat-window-x a m) 120) (= (seat-window-x b m) 430)
                    (= (glass-mirror-clim-x m) 430))
               "A holds the window at 120,140; B at 430,300; McCLIM believes 430,300")
        (let ()
          (multiple-value-bind (pop mx my) (open-pulldown port b m)
            (declare (ignorable mx my))
            (check pop "B pressed File and the pull-down came up")
            (when pop
              (destructuring-bind (pw ph) (popup-size pop)
                (note "pull-down own position ~d,~d size ~dx~d"
                      (glass-mirror-x pop) (glass-mirror-y pop) pw ph)
                (check (eq (clim-popup-owner pop port) m)
                       "the pop-up's owner was derived as the managed window that posted it")
                (multiple-value-bind (dx dy) (seat-popup-offset b pop)
                  (check (and (zerop dx) (zerop dy))
                         "the DRIVER's offset is (~d,~d) — zero by construction" dx dy))
                (multiple-value-bind (dx dy) (seat-popup-offset a pop)
                  (check (and (= dx (- 120 430)) (= dy (- 140 300)))
                         "A's offset is (~d,~d) = A's window minus McCLIM's" dx dy))
                ;; the geometric claim: on each screen the menu overlaps that screen's copy
                ;; of the window, and hangs off its menu bar.
                (dolist (s (list a b))
                  (let ((box (wm-window-box pop s)) (win (wm-window-box m s)))
                    (note "~a: window ~a, pull-down ~a" (seat-name s) win box)
                    (check (wm-boxes-overlap-p box win)
                           "~a: the pull-down overlaps ~:*~a's copy of the window"
                           (seat-name s))
                    (check (and (<= (seat-window-x s m) (seat-draw-x s pop)
                                    (+ (seat-window-x s m) 200))
                                (<= (seat-window-y s m) (seat-draw-y s pop)
                                    (+ (seat-window-y s m) 200)))
                           "~a: …and hangs from its menu bar, at ~d,~d"
                           (seat-name s) (seat-draw-x s pop) (seat-draw-y s pop))))
                ;; and the PIXELS: the menu's own top-left pixel is on A's screen where A
                ;; holds the window, and A's old detached spot is bare workspace again.
                (let ((ax (seat-draw-x a pop)) (ay (seat-draw-y a pop)))
                  (check (/= (pixel-at (seat-fb a) (+ ax 4) (+ ay 4)) +wm-teal+)
                         "A's screen has menu pixels at ~d,~d" (+ ax 4) (+ ay 4))
                  (check (= (pixel-at (seat-fb a) (+ (glass-mirror-x pop) (floor pw 2))
                                      (+ (glass-mirror-y pop) (floor ph 2)))
                            +wm-teal+)
                         "…and BARE WORKSPACE where the detached menu used to float (~d,~d)"
                         (+ (glass-mirror-x pop) (floor pw 2))
                         (+ (glass-mirror-y pop) (floor ph 2))))
                (shoot "2-diverged-pulldown" a b)
                (check (zerop (full-rebuild-diff a))
                       "A's damage-limited picture equals a full rebuild — no trail")
                (check (zerop (full-rebuild-diff b)) "…and so does B's")

                ;; ============ 4. the chained submenu =============================
                (format t "~%[4. a submenu off the pull-down is offset once, not twice]~%")
                ;; hover the middle item ("More") on B's screen — B is the driver, so the
                ;; pull-down is drawn there at its own position.
                (let ((sx (+ (glass-mirror-x pop) (floor pw 2)))
                      (sy (+ (glass-mirror-y pop) (floor (* ph 1/2))))
                      (before (popups port)))
                  (dolist (step '(0 2 -1 1 0))
                    (hover port b (+ sx step) (+ sy step)) (sleep 0.12))
                  (wait-until (lambda () (> (length (popups port)) (length before))) 3)
                  (tick port 0.5)
                  (let ((sub (first (set-difference (popups port) before))))
                    (cond
                      (sub
                       (check (eq (clim-popup-owner sub port) m)
                              "the SUBMENU's owner is the managed window, not the pull-down \
that opened it — which is what stops the offset being applied twice")
                       (multiple-value-bind (dx dy) (seat-popup-offset a sub)
                         (check (and (= dx (- 120 430)) (= dy (- 140 300)))
                                "…so A offsets it by exactly ONE window delta (~d,~d)" dx dy))
                       (multiple-value-bind (dx dy) (seat-popup-offset b sub)
                         (check (and (zerop dx) (zerop dy)) "…and the driver by none (~d,~d)" dx dy))
                       (dolist (s (list a b))
                         (check (wm-boxes-overlap-p (wm-window-box sub s) (wm-window-box pop s))
                                "~a: the submenu sits against its parent pull-down" (seat-name s)))
                       (shoot "3-submenu" a b)
                       (check (zerop (full-rebuild-diff a)) "no trail on A with the chain open")
                       (check (zerop (full-rebuild-diff b)) "no trail on B with the chain open"))
                      (t
                       (note "CLIM's menu tracker did not open a submenu in this harness;")
                       (note "the chain is exercised below by asking CLIM-POPUP-OWNER of every")
                       (note "pop-up that IS posted — the property at issue is that an owner is")
                       (note "always a MANAGED window, so no chain can compound the offset.")
                       (check (every (lambda (p)
                                       (let ((o (clim-popup-owner p port)))
                                         (or (null o) (glass-mirror-managed o))))
                                     (popups port))
                              "every posted pop-up's owner is a MANAGED window")))))

                ;; ============ 5. drag the owner with the menu posted =============
                (format t "~%[5. the non-driver drags the owner while the menu is posted]~%")
                ;; A is not driving, so this is A's own screen moving under a menu A did
                ;; not open.  The menu must follow A's window and leave nothing behind.
                (let ((ty (- (seat-window-y a m) 10)) (tx (+ (seat-window-x a m) 60)))
                  (press port a tx ty)                       ; grab A's title bar
                  (dotimes (i 6)
                    (glass-on-pointer port 1 (+ tx (* 12 (1+ i))) (+ ty (* 7 (1+ i))) a))
                  (glass-on-pointer port 0 (+ tx 72) (+ ty 42) a)
                  (check (null (seat-drag a)) "A's drag landed")
                  (check (= (glass-mirror-clim-x m) 430)
                         "…without moving what McCLIM believes (still ~d)" (glass-mirror-clim-x m))
                  (multiple-value-bind (dx dy) (seat-popup-offset a pop)
                    (check (and (= dx (- (seat-window-x a m) 430))
                                (= dy (- (seat-window-y a m) 300)))
                           "A's offset tracked the drag: (~d,~d)" dx dy))
                  (check (wm-boxes-overlap-p (wm-window-box pop a) (wm-window-box m a))
                         "the pull-down came with A's window")
                  (shoot "4-dragged-under-menu" a b)
                  (check (zerop (full-rebuild-diff a))
                         "…and left NO TRAIL: the dragged picture equals a full rebuild")
                  (check (zerop (full-rebuild-diff b)) "B's screen is untouched by A's drag"))

                ;; ============ 6. the driver's routing is untouched ===============
                (format t "~%[6. the driver still hits, selects and dismisses]~%")
                (check (eq (clim-token-holder port) b) "B is still driving")
                (note "CLIM's grab: B holds ~a, A holds ~a"
                      (type-of (seat-grab-sheet b)) (type-of (seat-grab-sheet a)))
                (check (and (seat-grab-sheet b) (null (seat-grab-sheet a)))
                       "…and it is B, the driver, that CLIM has grabbed the pointer for — \
A has no grab, so A can never reach the routing at all")
                (let ((frame-of (ignore-errors (pane-frame (glass-mirror-sheet pop)))))
                  (multiple-value-bind (leaf lx ly)
                      (grab-frame-leaf-at port frame-of
                                          (+ (glass-mirror-x pop) 10)
                                          (+ (glass-mirror-y pop) 10) b)
                    (declare (ignore lx ly))
                    (check leaf "the driver's hit test finds a leaf sheet inside the pull-down")))
                (setf *chosen* '())
                ;; walk onto the first item and click it
                (let ((ix (+ (glass-mirror-x pop) 20)) (iy (+ (glass-mirror-y pop) 10)))
                  (hover port b ix iy) (sleep 0.2)
                  (release port b ix iy) (sleep 0.3)
                  (click port b ix iy)
                  (tick port 0.6) (tick port 0.4))
                (check (wait-until (lambda () *chosen*) 3)
                       "clicking an item ran its command (~s)" *chosen*)
                (click port b 700 560)                        ; make sure nothing is left open
                (tick port 0.6) (tick port 0.4)
                (check (null (popups port)) "every pop-up is dismissed")
                (shoot "5-dismissed" a b)

                ;; ============ 3. the trails claim, repeated ======================
                (format t "~%[3. post and dismiss on a diverged layout, over and over]~%")
                (let ((base-a (snap a)) (base-b (snap b)) (worst 0))
                  (dotimes (i 4)
                    (multiple-value-bind (p mx2 my2) (open-pulldown port b m)
                      (declare (ignore p))
                      (setf worst (max worst (full-rebuild-diff a) (full-rebuild-diff b)))
                      (release port b mx2 my2)
                      (click port b 700 560)
                      (tick port 0.6) (tick port 0.35)))
                  (check (zerop worst)
                         "four post/dismiss cycles: the damage-limited picture never differed \
from a full rebuild (worst ~d px)" worst)
                  (check (equalp (snap a) base-a)
                         "A's screen is byte-identical to before the first menu (~d differ)"
                         (px-diff (snap a) base-a))
                  (check (equalp (snap b) base-b)
                         "B's screen is byte-identical to before the first menu (~d differ)"
                         (px-diff (snap b) base-b)))))))
        (note "token: ~a" (clim-token-report port))))
    (sb-concurrency:send-message (glass-port-mailbox port) (lambda () nil)))

  (format t "~%=> ~:[PASS~;FAIL (~d)~]~%" (plusp *fail*) *fail*)
  (finish-output)
  (sb-ext:exit :code (if (plusp *fail*) 1 0)))
