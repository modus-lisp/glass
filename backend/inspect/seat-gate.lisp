;;;; seat-gate.lisp — two people, one session.
;;;;
;;;; A SEAT is who is watching: a screen, a pointer, a keyboard, a focus, an open menu,
;;;; a clipboard, and an arrangement of the session's windows (see backend/seat.lisp).
;;;; The applications and their content framebuffers are shared; a window's SIZE is
;;;; shared, because size is what an application lays its content out to.  Position and
;;;; stacking are not.
;;;;
;;;; This holds ONE desktop with TWO seats and asks the questions that separate a real
;;;; second seat from a second view of the first one.  It reads PIXELS wherever a pixel
;;;; can answer, because two seats agreeing about a data structure and disagreeing about
;;;; what is on the screen is precisely the bug this design could have.
;;;;
;;;; In-process, no VNC, no sockets, fully deterministic.
;;;;   sbcl --control-stack-size 256 --dynamic-space-size 4096 \
;;;;        --non-interactive --load backend/inspect/seat-gate.lisp
;;;; With a directory argument it also writes a PNG of each seat's screen per scene:
;;;;   ... --load backend/inspect/seat-gate.lisp -- /tmp/seats

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

(defun pixel-at (fb x y)
  (if (and (< -1 x (glass:fb-width fb)) (< -1 y (glass:fb-height fb)))
      (logand (aref (glass:fb-pixels fb) (+ (* y (glass:fb-width fb)) x)) #xffffff)
      :off-screen))

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

(defvar *scene* 0)
(defun shoot (label a b)
  "Write both seats' screens for one scene, side by side in the filenames."
  (when *png-dir*
    (ensure-directories-exist (format nil "~a/" *png-dir*))
    (incf *scene*)
    (save-png (seat-fb a) (format nil "~a/~2,'0d-~a-A.png" *png-dir* *scene* label))
    (save-png (seat-fb b) (format nil "~a/~2,'0d-~a-B.png" *png-dir* *scene* label))
    (format t "    -> ~a/~2,'0d-~a-{A,B}.png~%" *png-dir* *scene* label)))

;;; ---- a surface whose content is a flat colour, so a pixel names the window ----

(defun add-block (port x y w h colour title)
  (let ((surf (add-surface port (lambda (fb) (glass:fb-fill fb colour)
                                  (values nil nil (constantly nil)))
                           :title title :width w :height h)))
    (setf (wm-surface-x surf) x (wm-surface-y surf) y)
    surf))

;;; A surface that records what was typed into it, so "who has the keyboard" is a
;;; question with a written answer rather than an inference.
(defun add-typist (port x y w h colour title)
  (let* ((typed (make-array 0 :element-type 'character :adjustable t :fill-pointer t))
         (surf (add-surface port (lambda (fb) (glass:fb-fill fb colour)
                                   (values (lambda (down k)
                                             (when (and down (<= 32 k 126))
                                               (vector-push-extend (code-char k) typed)))
                                           nil (constantly nil)))
                            :title title :width w :height h)))
    (setf (wm-surface-x surf) x (wm-surface-y surf) y)
    (values surf (lambda () (coerce typed 'string)))))

;;; NB: not +RED+/+GREEN+/+BLUE+ — CLIM exports those as NAMED-COLOR objects and this
;;; file is in a package that uses CLIM.  These are raw glass pixels.
(defconstant +ink-red+   #xcc2222)
(defconstant +ink-green+ #x22aa44)
(defconstant +ink-blue+  #x2244cc)

(let* ((port (make-instance 'glass-port :port 5991)))
  (setf (glass-port-wm-p port) t)
  ;; seat A: the primary one, made by INITIALIZE-INSTANCE; give it a screen.
  (let ((a (glass-port-default-seat port)))
    (setf (seat-screen-w a) 800 (seat-screen-h a) 600
          (seat-fb a) (glass:make-framebuffer 800 600 +wm-teal+)))
  (let* ((a (glass-port-default-seat port))
         ;; seat B: a DIFFERENT SCREEN SIZE, which is the point of screen size being
         ;; per-seat — a phone and a desktop watching one session.
         (b (add-seat port :name "seat-B" :port-num 5992 :width 640 :height 480
                           :fb (glass:make-framebuffer 640 480 +wm-teal+))))

    (format t "~&[multi-seat: two people, one session]~%")
    (check (= (length (glass-port-seats port)) 2) "the session has two seats")
    (check (and (seat-primary-p a) (not (seat-primary-p b)))
           "one is primary (it owns the position McCLIM believes in)")
    (check (not (eq (seat-fb a) (seat-fb b))) "each seat composites into its OWN screen")
    (check (and (= (glass:fb-width (seat-fb a)) 800) (= (glass:fb-width (seat-fb b)) 640))
           "the two screens are different sizes (800x600 and 640x480)")

    ;; ---- SHARED: the windows themselves -------------------------------------
    (format t "~%[what runs is shared]~%")
    (let ((r (add-block port  80 100 220 160 +ink-red+   "red"))
          (g (add-block port 160 160 220 160 +ink-green+ "green"))
          (bl (add-block port 100 260 200 120 +ink-blue+  "blue")))
      (composite-all port)
      (check (= (length (glass-port-surfaces port)) 3) "three windows in ONE session")
      (check (and (eq (wm-surface-fb r) (wm-surface-fb r)))
             "each window has ONE content framebuffer, not one per seat")
      ;; A fresh seat has diverged from nothing, so it sees the session arrangement.
      (check (zerop (hash-table-count (seat-views b)))
             "a new seat holds NO view records — it sees the desktop as it stands")
      (check (and (= (seat-window-x a r) (seat-window-x b r))
                  (= (seat-window-y a r) (seat-window-y b r)))
             "…so both seats agree where the red window is, to begin with")
      (check (= (pixel-at (seat-fb a) 100 120) (pixel-at (seat-fb b) 100 120) +ink-red+)
             "and both screens show it there")
      (shoot "shared" a b)

      ;; ---- PER-SEAT: position ------------------------------------------------
      (format t "~%[where a window sits is per-seat]~%")
      (wm-move r 420 60 b)                       ; seat B moves the red window; A must not
      (composite-all port)
      (check (and (= (seat-window-x a r) 80) (= (seat-window-x b r) 420))
             "seat B moved the red window; seat A still holds it at 80")
      (check (= (pixel-at (seat-fb a) 100 120) +ink-red+) "A's screen: red still at 100,120")
      (check (/= (pixel-at (seat-fb b) 100 120) +ink-red+) "B's screen: nothing red there any more")
      (check (= (pixel-at (seat-fb b) 440 80) +ink-red+)  "B's screen: red is at 440,80 instead")
      (check (= (pixel-at (seat-fb a) 440 80) +wm-teal+) "A's screen: bare workspace at 440,80")
      (check (= (hash-table-count (seat-views b)) 1)
             "B materialised exactly ONE view — copy-on-write, not a whole second desktop")
      (shoot "positions" a b)

      ;; ---- PER-SEAT: stacking ------------------------------------------------
      (format t "~%[what is in front is per-seat]~%")
      ;; red is out of the way on B; use the green/blue pair, which overlaps on both.
      (let ((ox 180) (oy 280))                   ; a point inside BOTH green and blue
        (wm-raise port g a)                      ; A brings green forward
        (wm-raise port bl b)                     ; B brings blue forward
        (composite-all port)
        (check (= (pixel-at (seat-fb a) ox oy) +ink-green+) "A sees GREEN in front at the overlap")
        (check (= (pixel-at (seat-fb b) ox oy) +ink-blue+)  "B sees BLUE in front at the same point")
        (check (and (eq (wm-topmost port a) g) (eq (wm-topmost port b) bl))
               "the two stacking orders disagree, and each is internally consistent")
        (check (and (eq (wm-hit port ox oy a) g) (eq (wm-hit port ox oy b) bl))
               "a click at that point hits a DIFFERENT window on each seat")
        (check (and (wm-obstructed-p* port bl (list (list ox oy 1 1)) a)
                    (not (wm-obstructed-p* port bl (list (list ox oy 1 1)) b)))
               "the occlusion guard follows each seat's own stack")
        (shoot "stacking" a b))

      ;; ---- PER-SEAT: pointer --------------------------------------------------
      ;; Driven through GLASS-ON-POINTER, the RFB callback itself, not the WM router
      ;; below it: the pointer POSITION is recorded at the callback (that is where a
      ;; seat's hand enters the system), so a test that called the router would be
      ;; testing something no client can reach.
      (format t "~%[each seat has its own pointer]~%")
      (glass-on-pointer port 0 700 500 a)
      (glass-on-pointer port 0 120  90 b)
      (check (and (= (seat-px a) 700) (= (seat-py a) 500)) "A's pointer is at 700,500")
      (check (and (= (seat-px b) 120) (= (seat-py b)  90)) "B's pointer is at 120,90")
      (check (/= (seat-px a) (seat-px b)) "…and moving one did not move the other")
      (format t "    (the CURSOR needs no code: glass sends the shape once as an RFB~%~
                  ~a pseudo-encoding and each viewer draws it at its own pointer)~%" "     ")

      ;; ---- PER-SEAT: keyboard focus, typing at the same time ------------------
      (format t "~%[two keyboards, two focused windows, at once]~%")
      (multiple-value-bind (ta reada) (add-typist port 420 380 160 100 #x884400 "typist-A")
        (multiple-value-bind (tb readb) (add-typist port 600 380 160 100 #x008888 "typist-B")
          (composite-all port)
          ;; each seat clicks into a DIFFERENT window's content
          (glass-on-pointer port 1 (+ (seat-window-x a ta) 20) (+ (seat-window-y a ta) 20) a)
          (glass-on-pointer port 0 (+ (seat-window-x a ta) 20) (+ (seat-window-y a ta) 20) a)
          (glass-on-pointer port 1 (+ (seat-window-x b tb) 20) (+ (seat-window-y b tb) 20) b)
          (glass-on-pointer port 0 (+ (seat-window-x b tb) 20) (+ (seat-window-y b tb) 20) b)
          (check (and (eq (seat-focus-surface a) ta) (eq (seat-focus-surface b) tb))
                 "A focused typist-A, B focused typist-B")
          (check (not (eq (seat-focus-surface a) (seat-focus-surface b)))
                 "…so focus is genuinely per-seat, not one focus with two owners")
          ;; interleave the two keyboards, as two people typing really would
          (loop for ca across "hello" for cb across "world" do
            (glass-on-key port t (char-code ca) a)
            (glass-on-key port t (char-code cb) b))
          (check (string= (funcall reada) "hello") "A typed \"hello\" into its own window")
          (check (string= (funcall readb) "world") "B typed \"world\" into its own window")
          (check (and (string= (funcall reada) "hello") (string= (funcall readb) "world"))
                 "neither keyboard leaked a single character into the other's window")
          ;; and A's focus survived B's click entirely
          (check (eq (seat-focus-surface a) ta) "B clicking elsewhere did not steal A's focus")))

      ;; ---- PER-SEAT: the open menu -------------------------------------------
      (format t "~%[a menu opens for the person who opened it]~%")
      (setf (glass-port-menu-items port)
            (list (cons "Alpha" (lambda () nil)) (cons "Beta" (lambda () nil))))
      (glass-on-pointer port 4 600 200 a)           ; right-press the bare workspace on A
      (composite-all port)
      (check (and (seat-menu a) (null (seat-menu b)))
             "A has an open root menu; B has none")
      (check (/= (pixel-at (seat-fb a) 606 206) +wm-teal+)
             "A's screen has menu pixels where the menu is")
      (check (= (pixel-at (seat-fb b) 400 200) +wm-teal+)
             "B's screen is still bare workspace")
      (shoot "menu" a b)
      (glass-on-pointer port 1 40 560 a)            ; dismiss on A
      (check (null (seat-menu a)) "A dismissed its own menu")

      ;; ---- PER-SEAT: the clipboard -------------------------------------------
      (format t "~%[two people copying do not clobber each other]~%")
      (check (not (eq (seat-clipboard a) (seat-clipboard b))) "each seat has its own selection")
      (glass:clipboard-own (seat-clipboard a) :test-a :text "A's paragraph")
      (glass:clipboard-own (seat-clipboard b) :test-b :text "B's filename")
      (check (string= (glass:clipboard-text (seat-clipboard a)) "A's paragraph")
             "A's clipboard still holds A's text")
      (check (string= (glass:clipboard-text (seat-clipboard b)) "B's filename")
             "…and B's holds B's, after both copied")

      ;; ---- the CopyRect hint reaches BOTH seats -------------------------------
      ;; A surface reports its translation consumingly, so the naive fan-out gives the
      ;; hint to whichever seat composites first and silently costs the other one the
      ;; CopyRect.  The round (see WM-SURFACE-ROUND-HINT) is what stops that.
      (format t "~%[one scroll, two CopyRects]~%")
      (let* ((pending nil)
             (sc (add-surface port (lambda (fb) (glass:fb-fill fb #x333355)
                                     (values nil nil (constantly t)))
                              :title "scroller" :width 240 :height 180)))
        (setf (wm-surface-x sc) 40 (wm-surface-y sc) 40
              (wm-surface-copy-p sc) (lambda () (prog1 pending (setf pending nil))))
        ;; put it somewhere unobstructed on BOTH seats, and give each screen a whole
        ;; composite so the copy-base is established for both
        (wm-raise port sc a) (wm-raise port sc b)
        (composite-all port)
        (glass:fb-take-frame (seat-fb a)) (glass:fb-take-frame (seat-fb b))
        (setf pending (list 0 20 0 0 240 160))       ; "my content moved up by 20"
        (wm-tick port)                                ; ONE round, both seats
        (multiple-value-bind (fa da ca) (glass:fb-take-frame (seat-fb a))
          (declare (ignore fa da))
          (multiple-value-bind (fb* db cb) (glass:fb-take-frame (seat-fb b))
            (declare (ignore fb* db))
            (check (and ca cb) "BOTH seats got a CopyRect hint from the one scroll")
            (check (and ca cb (equal (subseq ca 4) (subseq cb 4)))
                   "…the same moved block on each (~a / ~a)" ca cb)))
        ;; and with the seats holding it at different places, the hints differ in
        ;; screen coordinates while describing the same content move
        (wm-move sc 300 220 b)
        (composite-all port)
        (glass:fb-take-frame (seat-fb a)) (glass:fb-take-frame (seat-fb b))
        (setf pending (list 0 20 0 0 240 160))
        (wm-tick port)
        (multiple-value-bind (fa da ca) (glass:fb-take-frame (seat-fb a))
          (declare (ignore fa da))
          (multiple-value-bind (fb* db cb) (glass:fb-take-frame (seat-fb b))
            (declare (ignore fb* db))
            (check (and ca cb (not (equal ca cb)))
                   "with the window at different places, each seat's hint is in ITS OWN "
                   "screen coordinates")
            (format t "    A: ~a~%    B: ~a~%" ca cb))))

      ;; ---- closing a window closes it for everybody ---------------------------
      (format t "~%[what runs is still shared]~%")
      (wm-close port g)
      (check (not (member g (glass-port-surfaces port))) "closing green removed it from the session")
      (check (and (null (seat-view a g)) (null (seat-view b g)))
             "…and no seat kept a view of a window that is gone")
      (composite-all port)
      (check (and (/= (pixel-at (seat-fb a) 180 280) +ink-green+)
                  (/= (pixel-at (seat-fb b) 180 280) +ink-green+))
             "it is off BOTH screens")
      (shoot "final" a b)))

  (format t "~%=> ~:[PASS~;FAIL (~d)~]~%" (plusp *fail*) *fail*)
  (finish-output)
  (sb-ext:exit :code (if (plusp *fail*) 1 0)))
