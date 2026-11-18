;;;; test/wm-seats-test.lisp — glass.wm's cooperative SEATS: one session of windows, a screen, hands
;;;; and arrangement per person watching.
;;;;   1. one seat: no per-seat record at all, and the stack is the session's own list (the same path)
;;;;   2. two seats in process: whose pointer, focus, drag, raise, menu, screen size; content damage
;;;;      reaching both; the home seat's moves inherited; a closed window gone for both; a seat
;;;;      nobody watches is not painted -- every composite equal to a full repaint, per seat
;;;;   3. two seats over two real RFB connections to START-WM (WM-SEAT-PORT), pixel-checked, and a
;;;;      reconnect by the same key getting the same seat back
;;;;   4. a rough cost check: composite time with 1 vs 2 seats, idle and during a drag
;;;;   sbcl --script test/wm-seats-test.lisp   (with glass/wm findable by ASDF; exits 0 iff all pass)

(require :asdf)
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (asdf:load-system :glass/wm)
    (asdf:load-system :glass/client)))
(require :sb-bsd-sockets)

(defpackage #:glass-wm-seats-test (:use #:cl))
(in-package #:glass-wm-seats-test)

(defvar *fails* 0)
(defun check (name ok &optional detail)
  (format t "~&SEATS ~:[FAIL~;ok  ~] ~a~@[   ~a~]~%" ok name detail)
  (unless ok (incf *fails*)))

(defconstant +title-grey+ #xcccccc)
(defparameter +teal+ (glass:rgb 61 122 138))

(defun pixels (fb) (copy-seq (glass:fb-pixels fb)))

(defun same-as-full-p (wm seat)
  "Does SEAT's screen equal what a full repaint of it draws?"
  (let* ((glass.wm:*seat* seat)
         (fb (glass.wm:seat-fb seat)) (before (pixels fb)))
    (glass.wm::wm-composite wm nil)
    (equalp before (glass:fb-pixels fb))))

(defmacro as-seat ((seat) &body body) `(let ((glass.wm:*seat* ,seat)) ,@body))

(defun ptr (wm mask x y &optional seat) (glass.wm:wm-pointer wm mask x y seat) (glass.wm:wm-tick wm))

(defun title-point (s &optional seat)
  "A point on S's title bar, left of centre, clear of the wedge, on SEAT's screen."
  (as-seat (seat) (values (+ (glass.wm::sx s) 60) (- (glass.wm::sy s) 10))))

;; A window whose content the test repaints at will: CONTENT damage, the session's.
(defvar *blink* nil)
(defun blink-app (fb)
  (glass:fb-fill fb #x202020)
  (values nil nil (lambda () (when *blink* (glass:fb-fill fb *blink*) (setf *blink* nil) t))))

(defun register (wm)
  (dolist (a glass.desk:*standard-apps*)
    (destructuring-bind (label fn &key width height) a
      (glass.wm:wm-register-app wm label fn :width width :height height)))
  (glass.wm:wm-register-app wm "Blink" 'blink-app :width 120 :height 80))

;;; ---- 1. one seat ------------------------------------------------------------------------------

(let* ((screen (glass:make-framebuffer 800 600))
       (wm (glass.wm:make-wm screen))
       (home (glass.wm:wm-home wm)))
  (register wm)
  (let ((paint (glass.wm:wm-open wm "Paint"))
        (notes (glass.wm:wm-open wm "Notes")))
    (glass.wm:wm-tick wm)
    (multiple-value-bind (x y) (title-point paint)
      (ptr wm 1 x y) (ptr wm 1 (+ x 100) (+ y 120)) (ptr wm 0 (+ x 100) (+ y 120)))
    (ptr wm 1 (+ (glass.wm::surf-x notes) 30) (+ (glass.wm::surf-y notes) 30))
    (ptr wm 0 (+ (glass.wm::surf-x notes) 30) (+ (glass.wm::surf-y notes) 30))
    (glass.wm::wm-lower wm notes) (glass.wm::wm-raise wm notes)
    (glass.wm:wm-resize-screen wm 640 480)
    (ptr wm 4 630 470) (ptr wm 0 630 470) (ptr wm 1 5 470) (ptr wm 0 5 470)
    (check "one seat: the session has exactly one seat, the home seat"
           (and (equal (glass.wm:wm-seats wm) (list home)) (glass.wm:seat-home-p home)))
    (check "one seat: after drag, focus, raise, lower, screen resize and a menu -- zero view records"
           (and (zerop (glass.wm:seat-view-count home)) (null (glass.wm::seat-views home))
                (null (glass.wm::seat-copy-bases home))))
    (check "one seat: the stack it walks IS the session's list (EQ: no copy, no sort)"
           (eq (glass.wm::stack wm) (glass.wm:wm-surfaces wm)))
    (check "one seat: the home seat's clipboard is the session clipboard"
           (eq (glass.wm:seat-clipboard home) (glass:session-clipboard)))
    (check "one seat: screen = full repaint" (same-as-full-p wm home))))

;;; ---- 2. two seats, in process -----------------------------------------------------------------

(let* ((screen (glass:make-framebuffer 800 600))
       (wm (glass.wm:make-wm screen))
       (a (glass.wm:wm-home wm)))
  (register wm)
  (let* ((paint (glass.wm:wm-open wm "Paint"))     ; 68x? cascade: Paint 40,62
         (notes (glass.wm:wm-open wm "Notes"))     ; Notes 68,90, on top
         (b (glass.wm:wm-add-seat wm 1 (glass:make-framebuffer 800 600) :key "B"
                                  :clipboard (glass:make-clipboard))))
    (glass.wm::wm-seat-viewers wm b 1)
    (glass.wm:wm-tick wm)
    (check "a new seat sees the session as it stands (B's screen = A's), holding no record"
           (and (equalp (glass:fb-pixels screen) (glass:fb-pixels (glass.wm:seat-fb b)))
                (zerop (glass.wm:seat-view-count b))))
    ;; A focuses Notes; B presses Paint's title and drags it
    (ptr wm 1 (+ (glass.wm::surf-x notes) 30) (+ (glass.wm::surf-y notes) 30))
    (ptr wm 0 (+ (glass.wm::surf-x notes) 30) (+ (glass.wm::surf-y notes) 30))
    (let ((a-px (pixels screen)) (a-x (glass.wm:seat-px a)) (a-y (glass.wm:seat-py a))
          (own (list (glass.wm::surf-x paint) (glass.wm::surf-y paint))))
      (multiple-value-bind (x y) (title-point paint b)
        (let ((glass.wm:*wireframe-lag-ms* 1000000) (glass.wm:*wireframe-queue-kb* 1d9))
          (ptr wm 1 x y b) (ptr wm 1 (+ x 300) (+ y 200) b) (ptr wm 0 (+ x 300) (+ y 200) b)))
      (check "B's press and drag left A's pointer, buttons and focus alone"
             (and (= a-x (glass.wm:seat-px a)) (= a-y (glass.wm:seat-py a))
                  (zerop (glass.wm:seat-last-mask a)) (eq notes (glass.wm:seat-focus a))))
      (check "B's press focused Paint for B" (eq paint (glass.wm:seat-focus b)))
      (check "B's drag moved Paint on B's screen only (a view), not the window's own position"
             (and (equal own (list (glass.wm::surf-x paint) (glass.wm::surf-y paint)))
                  (as-seat (b) (and (= (glass.wm::sx paint) (+ (first own) 300))
                                    (= (glass.wm::sy paint) (+ (second own) 200))))
                  (= 1 (glass.wm:seat-view-count b))))
      (check "A's screen is pixel-for-pixel unchanged by B's drag" (equalp a-px (glass:fb-pixels screen)))
      (check "B's raise is B's: Paint tops B's stack, Notes still tops A's"
             (and (eq paint (first (as-seat (b) (glass.wm::stack wm))))
                  (eq notes (first (glass.wm::stack wm)))))
      (check "B's screen = full repaint after its drag" (same-as-full-p wm b))
      (check "A's screen = full repaint" (same-as-full-p wm a)))
    ;; the home seat moves Notes, which B never moved: B inherits it
    (multiple-value-bind (x y) (title-point notes)
      (let ((glass.wm:*wireframe-lag-ms* 1000000) (glass.wm:*wireframe-queue-kb* 1d9))
        (ptr wm 1 x y) (ptr wm 1 (+ x 40) (+ y 30)) (ptr wm 0 (+ x 40) (+ y 30))))
    (check "A's move of a window B never moved is B's too (it reads the window's own slots)"
           (and (as-seat (b) (= (glass.wm::sx notes) (glass.wm::surf-x notes)))
                (= 1 (glass.wm:seat-view-count b))))
    (check "and B's screen was repainted for it (= full repaint)" (same-as-full-p wm b))
    ;; CONTENT damage reaches both, each where it draws the window
    (let ((blink (glass.wm:wm-open wm "Blink" )))
      (glass.wm:wm-tick wm)
      (multiple-value-bind (x y) (title-point blink b)
        (ptr wm 1 x y b) (ptr wm 1 (+ x 200) (+ y 100) b) (ptr wm 0 (+ x 200) (+ y 100) b))
      (setf *blink* #x00ff00)
      (glass.wm:wm-tick wm)
      (flet ((center (seat) (as-seat (seat) (list (+ (glass.wm::sx blink) 60) (+ (glass.wm::sy blink) 40)))))
        (check "a window repainting itself reaches both seats, each at its own place for it"
               (and (/= (first (center a)) (first (center b)))
                    (= #x00ff00 (apply #'glass:fb-get screen (center a)))
                    (= #x00ff00 (apply #'glass:fb-get (glass.wm:seat-fb b) (center b))))))
      (check "both screens = full repaint after the content change"
             (and (same-as-full-p wm a) (same-as-full-p wm b)))
      ;; a seat nobody watches is not painted; looked at again, it is repainted whole
      (glass.wm::wm-seat-viewers wm b 0)
      (let ((b-px (pixels (glass.wm:seat-fb b))))
        (setf *blink* #xff0000)
        (glass.wm:wm-tick wm)
        (check "a seat nobody watches is not composited" (equalp b-px (glass:fb-pixels (glass.wm:seat-fb b)))))
      (glass.wm::wm-seat-viewers wm b 1)
      (glass.wm:wm-tick wm)
      (check "watched again, it is repainted whole (= full repaint)" (same-as-full-p wm b))
      ;; B closes Blink: it is gone for everybody
      (multiple-value-bind (kx ky ks) (glass.wm::wm-close-button-box (glass.wm::surf-cw blink))
        (as-seat (b)
          (let ((x (+ (glass.wm::sx blink) kx (floor ks 2)))
                (y (+ (- (glass.wm::sy blink) (glass.wm::wm-titleh)) ky (floor ks 2))))
            (ptr wm 1 x y b) (ptr wm 0 x y b))))
      (glass.wm:wm-tick wm)
      (check "a window B closes is gone for A too, and from B's views"
             (and (not (member blink (glass.wm:wm-surfaces wm))) (= 1 (glass.wm:seat-view-count b))))
      (check "both screens = full repaint after the close" (and (same-as-full-p wm a) (same-as-full-p wm b))))
    ;; B's menu
    (let ((a-px (pixels screen)))
      (ptr wm 4 780 580 b)
      (check "B's root menu opens on B only" (and (glass.wm:seat-menu b) (null (glass.wm:seat-menu a))
                                                 (equalp a-px (glass:fb-pixels screen))))
      (ptr wm 0 780 580 b) (ptr wm 1 5 5 b) (ptr wm 0 5 5 b)
      (check "and B dismisses it" (null (glass.wm:seat-menu b))))
    ;; B's screen size
    (let ((sizes (mapcar (lambda (s) (list (glass.wm::surf-cw s) (glass.wm::surf-ch s))) (glass.wm:wm-surfaces wm))))
      (check "B resizing its screen says it changed" (glass.wm:wm-resize-screen wm 400 700 b))
      (check "B's screen is 400x700, A's still 800x600"
             (and (= 400 (glass:fb-width (glass.wm:seat-fb b))) (= 700 (glass:fb-height (glass.wm:seat-fb b)))
                  (= 800 (glass:fb-width screen)) (= 600 (glass:fb-height screen))))
      (check "a seat other than home does not resize the session's windows"
             (equal sizes (mapcar (lambda (s) (list (glass.wm::surf-cw s) (glass.wm::surf-ch s))) (glass.wm:wm-surfaces wm))))
      (glass.wm:wm-tick wm)
      (check "both screens = full repaint after B's resize" (and (same-as-full-p wm a) (same-as-full-p wm b))))
    ;; keys go to each seat's own focus
    (let ((got '()))
      (setf (glass.wm::surf-on-key notes) (lambda (d k) (when d (push (list :notes k) got)))
            (glass.wm::surf-on-key paint) (lambda (d k) (when d (push (list :paint k) got))))
      ;; (both foci went with Blink -- A opened it, B closed it): A presses into Notes, B into Paint
      (ptr wm 1 (+ (glass.wm::sx notes) 20) (+ (glass.wm::sy notes) 20))
      (ptr wm 0 (+ (glass.wm::sx notes) 20) (+ (glass.wm::sy notes) 20))
      (as-seat (b) (ptr wm 1 (+ (glass.wm::sx paint) 20) (+ (glass.wm::sy paint) 20) b)
                   (ptr wm 0 (+ (glass.wm::sx paint) 20) (+ (glass.wm::sy paint) 20) b))
      (glass.wm:wm-key wm t 97) (glass.wm:wm-key wm t 98 b)
      (check "a key goes to its own seat's focus" (equal got '((:paint 98) (:notes 97)))
             (format nil "~s" got)))))

;;; ---- 3. two seats over real RFB connections ---------------------------------------------------

(defun free-port ()
  (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (sb-bsd-sockets:socket-bind s #(127 0 0 1) 0)
    (prog1 (nth-value 1 (sb-bsd-sockets:socket-name s)) (sb-bsd-sockets:socket-close s))))

(defun wait-until (fn &optional (timeout 5))
  (let ((deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
    (loop (when (funcall fn) (return t))
          (when (> (get-internal-real-time) deadline) (return nil))
          (sleep 0.02))))

(defun settle () (sleep 0.4))

(defun view (port)
  (let ((r (glass-client:connect-remote "127.0.0.1" port :width 64 :height 64)))
    (wait-until (lambda () (and (glass-client:remote-connected-p r) (> (glass-client:remote-width r) 64))))
    r))

(defun at (r x y) (glass:fb-get (glass-client:remote-fb r) x y))

(defun click (r mask x y) (glass-client:remote-pointer r mask x y) (sleep 0.05))

(defun drag (r x0 y0 x1 y1)
  (click r 1 x0 y0)
  (loop for i from 1 to 10
        do (click r 1 (+ x0 (round (* i (- x1 x0)) 10)) (+ y0 (round (* i (- y1 y0)) 10))))
  (click r 0 x1 y1)
  (settle))

(defun region= (r1 r2 x0 y0 x1 y1)
  "Do R1 and R2 show the same pixels in [X0,X1) x [Y0,Y1)?"
  (loop for y from y0 below y1 always
        (loop for x from x0 below x1 always (= (at r1 x y) (at r2 x y)))))

(defun region (r x0 y0 x1 y1)
  (let ((out '()))
    (loop for y from y0 below y1 do (loop for x from x0 below x1 do (push (at r x y) out)))
    out))

(defun raw-send (port bytes)
  "A bare RFB client on PORT: handshake, send BYTES, wait a moment, close."
  (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (sb-bsd-sockets:socket-connect sock #(127 0 0 1) port)
    (let ((s (sb-bsd-sockets:socket-make-stream sock :input t :output t
                                                     :element-type '(unsigned-byte 8) :buffering :full)))
      (labels ((rd (n) (let ((b (make-array n :element-type '(unsigned-byte 8)))) (read-sequence b s) b))
               (wr (v) (write-sequence (coerce v '(vector (unsigned-byte 8))) s) (finish-output s)))
        (rd 12) (wr (map 'list #'char-code (format nil "RFB 003.008~c" (code-char 10))))
        (let ((n (aref (rd 1) 0))) (rd n) (wr (list 1)) (rd 4))
        (wr (list 1))
        (let ((init (rd 24))) (rd (+ (ash (aref init 20) 24) (ash (aref init 21) 16) (ash (aref init 22) 8) (aref init 23))))
        (wr bytes)
        (sleep 0.5)
        (ignore-errors (close s))))))

(defun be16 (n) (list (ldb (byte 8 8) n) (ldb (byte 8 0) n)))
(defun be32 (n) (list (ldb (byte 8 24) n) (ldb (byte 8 16) n) (ldb (byte 8 8) n) (ldb (byte 8 0) n)))
(defun set-desktop-size (w h) (append (list 251 0) (be16 w) (be16 h) (list 1 0) (list 0 0 0 1 0 0 0 0) (be16 w) (be16 h) (list 0 0 0 0)))
(defun cut-text (text) (append (list 6 0 0 0) (be32 (length text)) (map 'list #'char-code text)))

(let* ((port (free-port))
       (session (nth-value 3 (let ((*trace-output* (make-broadcast-stream)) (*error-output* (make-broadcast-stream)))
                               (glass.wm:start-wm port :width 800 :height 600
                                                       :open '("Clock" "Paint" "Notes"))))))
  (check "START-WM's fourth value is the session; its port designates it too"
         (and session (eq session (glass.wm:wm-session port))))
  (check "WM-SEAT-PORT with no key is the home port" (eql port (glass.wm:wm-seat-port port nil)))
  (let ((a (view port)))
    ;; the home seat arranges the session: Clock off to the right, clear of the others.  After:
    ;; Clock 520,62 (260x110); Paint 68,90 (320x240); Notes 96,118 (420x300) on top.
    (settle)
    (drag a 150 51 630 51)
    (check "(A put the Clock at the right)" (= #x10141a (at a 600 160)))
    (let* ((bport (glass.wm:wm-seat-port port "phone-B"))
           (b (view bport)))
      (check "WM-SEAT-PORT gives a key a port of its own" (and (integerp bport) (/= bport port)))
      (check "the same key asked again gets the same port" (eql bport (glass.wm:wm-seat-port session "phone-B")))
      (settle)
      (check "B sees the session as A arranged it (screens equal outside the ticking Clock)"
             (and (= 800 (glass-client:remote-width b))
                  (region= a b 0 0 519 600) (region= a b 519 175 800 600)))
      ;; A focuses Notes; B presses into Paint's visible strip (raising and focusing it for B)
      (click a 1 300 300) (click a 0 300 300) (settle)
      (let ((notes-before (region a 110 130 500 200)))
        (click b 1 75 200) (click b 0 75 200) (settle)
        (check "B's press raised Paint on B's screen: Paint's paper where Notes' title bar is on A"
               (and (= +title-grey+ (at a 200 105)) (= #xf4f1ea (at b 200 105))))
        (dolist (k '(104 105)) (glass-client:remote-key a t k) (glass-client:remote-key a nil k))
        (settle)
        (check "after B's press A's keys still reach A's Notes (B took neither A's pointer nor focus)"
               (not (equal notes-before (region a 110 130 500 200)))))
      ;; B drags Notes down by its visible title (right of Paint)
      (drag b 450 105 450 255)
      (check "B's drag: on B's screen Notes' title bar is lower and its old place is workspace"
             (and (= +title-grey+ (at b 450 255)) (= +teal+ (at b 450 105))))
      (check "B's drag: A's screen still has Notes where it was (pixel-checked)"
             (and (= +title-grey+ (at a 450 105)) (/= +title-grey+ (at a 450 255))))
      ;; the Clock ticks on both
      (let ((ca (region a 530 70 700 120)) (cb (region b 530 70 700 120)))
        (check "a window's content change (the Clock ticking) reaches both seats"
               (wait-until (lambda () (and (not (equal ca (region a 530 70 700 120)))
                                           (not (equal cb (region b 530 70 700 120)))))
                           4)))
      ;; B's menu: right press on bare workspace
      (click b 4 700 450) (click b 0 700 450) (settle)
      (check "B's root menu opens on B's screen only"
             (and (/= +teal+ (at b 710 460)) (= +teal+ (at a 710 460))))
      (click b 1 5 590) (click b 0 5 590) (settle)
      (check "and B can dismiss it" (= +teal+ (at b 710 460)))
      ;; clipboards
      (glass:clipboard-set (glass:session-clipboard) "home text")
      (raw-send bport (cut-text "from B"))
      (check "B's ClientCutText lands in B's clipboard"
             (equal "from B" (glass:clipboard-text (glass.wm:wm-session-seat-clipboard port "phone-B"))))
      (check "and not in A's, the session clipboard"
             (equal "home text" (glass:clipboard-text (glass:session-clipboard))))
      (raw-send port (cut-text "from A"))
      (check "A's ClientCutText is the session clipboard's, and B's is untouched"
             (and (equal "from A" (glass:clipboard-text (glass:session-clipboard)))
                  (equal "from B" (glass:clipboard-text (glass.wm:wm-session-seat-clipboard port "phone-B")))))
      ;; reconnect by key
      (glass-client:remote-stop b)
      (sleep 0.5)
      (let* ((bport2 (glass.wm:wm-seat-port port "phone-B"))
             (b2 (view bport2)))
        (settle)
        (check "reconnecting with the same key returns the same seat's port" (eql bport bport2))
        (check "and that seat still has B's arrangement (Notes moved, Paint raised)"
               (and (= +title-grey+ (at b2 450 255)) (= +teal+ (at b2 450 105)) (= #xf4f1ea (at b2 200 105))))
        (let* ((cport (glass.wm:wm-seat-port port "phone-C")) (c (view cport)))
          (settle)
          (check "a different key gets a different seat, which inherits A's arrangement, not B's"
                 (and (/= cport bport) (= +title-grey+ (at c 450 105)) (= +title-grey+ (at c 200 105))))
          (glass-client:remote-stop c))
        ;; B's screen size
        (let ((a-px (region a 0 175 519 600)))
          (raw-send bport (set-desktop-size 400 700))
          (check "B's SetDesktopSize: B's viewer is told 400x700"
                 (wait-until (lambda () (and (= 400 (glass-client:remote-width b2))
                                             (= 700 (glass-client:remote-height b2))))))
          (settle)
          (check "A's screen is still 800x600 and its picture unchanged"
                 (and (= 800 (glass-client:remote-width a)) (= 600 (glass-client:remote-height a))
                      (equal a-px (region a 0 175 519 600)))))
        (glass-client:remote-stop b2)))
    (glass-client:remote-stop a)))

;;; ---- 4. a rough cost check --------------------------------------------------------------------

(defun ms-per (n thunk)
  (let ((t0 (get-internal-real-time)))
    (dotimes (i n) (funcall thunk))
    (/ (* 1000.0 (- (get-internal-real-time) t0)) internal-time-units-per-second n)))

(defun cost (nseats)
  "(values IDLE-MS DRAG-MS FULL-MS): a tick of an idle desktop (the Clock ticking once a second
   is all that changes), a drag step and its tick, and a full repaint -- with NSEATS seats, every
   one watched."
  (let* ((wm (glass.wm:make-wm (glass:make-framebuffer 1024 768))))
    (register wm)
    (dolist (l '("Clock" "Paint" "Notes" "Media")) (glass.wm:wm-open wm l))
    (loop for i from 1 below nseats
          do (glass.wm::wm-seat-viewers wm (glass.wm:wm-add-seat wm i (glass:make-framebuffer 1024 768)) 1))
    (glass.wm:wm-tick wm)
    (let* ((idle (let ((spent 0))           ; 1.25 s of 60 Hz ticks: the Clock repaints once or twice
                   (dotimes (k 75 (/ (* 1000.0 spent) internal-time-units-per-second 75))
                     (let ((t0 (get-internal-real-time)))
                       (glass.wm:wm-tick wm)
                       (incf spent (- (get-internal-real-time) t0)))
                     (sleep 1/60))))
           (s (first (last (glass.wm:wm-surfaces wm))))
           (x0 (+ (glass.wm::surf-x s) 60)) (y0 (- (glass.wm::surf-y s) 10)) (i 0)
           (drag (let ((glass.wm:*wireframe-lag-ms* 1000000) (glass.wm:*wireframe-queue-kb* 1d9))
                   (glass.wm:wm-pointer wm 1 x0 y0)
                   (prog1 (ms-per 200 (lambda ()
                                        (incf i)
                                        (glass.wm:wm-pointer wm 1 (+ x0 (mod i 200)) (+ y0 (floor (mod i 200) 2)))
                                        (glass.wm:wm-tick wm)))
                     (glass.wm:wm-pointer wm 0 x0 y0))))
           (full (ms-per 50 (lambda () (dolist (seat (glass.wm:wm-seats wm)) (glass.wm::seat-damage seat nil))
                              (glass.wm:wm-tick wm)))))
      (values idle drag full (glass.wm::wm-opaque-moves wm)))))

(multiple-value-bind (i1 d1 f1 m1) (cost 1)
  (multiple-value-bind (i2 d2 f2 m2) (cost 2)
    (check "cost: the drag being timed really moved the window, opaquely, every step"
           (and (>= m1 199) (>= m2 199)) (format nil "~d and ~d moves" m1 m2))
    (format t "~&SEATS cost (1024x768, 4 windows; ms per): idle tick 1 seat ~,3f / 2 seats ~,3f;  ~
               drag step 1 seat ~,3f / 2 seats ~,3f;  full repaint 1 seat ~,3f / 2 seats ~,3f~%"
            i1 i2 d1 d2 f1 f2)
    (check "cost: a second watched seat keeps a drag step under 3x + 1 ms (it composites a second screen)"
           (< d2 (+ (* 3 d1) 1.0)) (format nil "~,3f vs ~,3f ms" d2 d1))))

(format t "~&SEATS ~d failure~:p~%" *fails*)
(finish-output)
(sb-ext:exit :code (if (zerop *fails*) 0 1) :abort t)
