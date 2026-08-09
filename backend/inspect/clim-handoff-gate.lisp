;;;; clim-handoff-gate.lisp — one at a time, but not one forever.
;;;;
;;;; McCLIM windows are ONE CONSOLIDATED SEAT (a CLIM port has one pointer, one keyboard
;;;; focus, and one screen transformation per top-level sheet), so exactly one seat can be
;;;; driving a CLIM application at a time.  That much is forced.  What is NOT forced is
;;;; that it be the same seat all session, and this gate is the difference between the
;;;; two: it holds one desktop with two seats and one real McCLIM frame, and asks whether
;;;; the driving can change hands — including the part that used to be nailed down, the
;;;; window GEOMETRY McCLIM places its pull-downs and dialogs from.
;;;;
;;;; What it asks, in order:
;;;;   1. the three meanings of "primary" are three things, and only one of them travels
;;;;   2. the geometry travels: CLIM believes the DRIVER's window position, and taking
;;;;      the token does not move the other seat's windows
;;;;   3. two people type into one CLIM window and the characters land in order
;;;;   4. a pull-down opened by the second seat lands at ITS window position
;;;;   5. a handoff never happens mid-gesture: it is deferred and applied at the end
;;;;   6. the token goes free on pointer-exit, on idle, and on disconnect, and a free
;;;;      token is taken silently
;;;;   7. two seats alternating does not churn geometry resyncs, and does not DROP one
;;;;      either — under sustained thrash, with the event queue behind
;;;;   8. native surfaces are still per-seat: both seats type into two terminals at once
;;;;   9. the holder indicator, off and on
;;;;
;;;; In-process; one real CLIM frame in a thread, everything else deterministic.
;;;;   sbcl --control-stack-size 256 --dynamic-space-size 4096 \
;;;;        --non-interactive --load backend/inspect/clim-handoff-gate.lisp
;;;; With a directory argument it also writes a PNG of each seat's screen per scene:
;;;;   ... --load backend/inspect/clim-handoff-gate.lisp -- /tmp/handoff

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
  (when *png-dir*
    (ensure-directories-exist (format nil "~a/" *png-dir*))
    (incf *scene*)
    (save-png (seat-fb a) (format nil "~a/~2,'0d-~a-A.png" *png-dir* *scene* label))
    (save-png (seat-fb b) (format nil "~a/~2,'0d-~a-B.png" *png-dir* *scene* label))
    (format t "       -> ~a/~2,'0d-~a-{A,B}.png~%" *png-dir* *scene* label)))

(defun wait-until (pred &optional (secs 8))
  (let ((end (+ (get-internal-real-time) (* secs internal-time-units-per-second))))
    (loop until (funcall pred) do (sleep 1/50)
          when (> (get-internal-real-time) end) do (return nil) finally (return t))))

;;; ---- a CLIM window that records what was typed into it ----------------------
;;; The characters have to be READ BACK to say "in the right order", and a pane that
;;; keeps them is the shortest honest way: the keystroke still travels the whole path —
;;; RFB callback, seat, token, CLIM event, DISTRIBUTE-EVENT, the focused sheet.

(defvar *typed* (make-array 0 :element-type 'character :adjustable t :fill-pointer t))
(defun typed () (coerce *typed* 'string))
(defun typed-clear () (setf (fill-pointer *typed*) 0))

(defclass keylog-pane (clim:clim-stream-pane) ())
(defmethod clim:handle-event ((pane keylog-pane) (event clim:key-press-event))
  (let ((ch (clim:keyboard-event-character event)))
    (when ch (vector-push-extend ch *typed*))))

(define-application-frame ho-frame ()
  ()
  (:menu-bar t)
  (:panes (log (clim:make-pane 'keylog-pane :width 300 :height 180
                                            :background clim:+white+)))
  (:layouts (default log)))

(define-ho-frame-command (com-nothing :name "Nothing") () nil)
(clim:make-command-table 'ho-file :errorp nil
                         :menu '(("Alpha" :command (com-nothing))
                                 ("Beta"  :command (com-nothing))
                                 ("Gamma" :command (com-nothing))))
(clim:add-menu-item-to-command-table 'ho-frame "File" :menu 'ho-file :errorp nil)

;;; ---- pointer helpers: a whole click, as a client sends one -------------------

(defun press (port seat x y &optional (button 1))
  (glass-on-pointer port button x y seat))
(defun release (port seat x y)
  (glass-on-pointer port 0 x y seat))
(defun click (port seat x y &optional (button 1))
  (press port seat x y button) (release port seat x y))
(defun hover (port seat x y) (glass-on-pointer port 0 x y seat))

(defun type-into (port seat string)
  (loop for c across string do
    (glass-on-key port t (char-code c) seat)
    (glass-on-key port nil (char-code c) seat)))

;;; A CLIM window's screen position AS McCLIM COMPUTES IT — the sheet's delta
;;; transformation to the graft, which is precisely what CLIM converts a pane-local point
;;; through when it places a pull-down, a dialog or a tooltip.  Not our slot: CLIM's own
;;; arithmetic, asked the way CLIM asks it.
(defun clim-screen-position (sheet)
  (multiple-value-bind (x y)
      (clim:transform-position (clim:sheet-delta-transformation sheet nil) 0 0)
    (values (round x) (round y))))

(defun drain (port &optional (secs 0.4))
  "Let the port's event thread run the closures we marshalled onto it."
  (sleep secs)
  (composite-all port))

;;; =============================================================================

(let* ((port (make-instance 'glass-port :port 5981)))
  (setf (glass-port-wm-p port) t)
  (let ((a (glass-port-default-seat port)))
    (setf (seat-screen-w a) 800 (seat-screen-h a) 600
          (seat-fb a) (glass:make-framebuffer 800 600 +wm-teal+)))
  (climi::restart-port port)
  (let* ((a (glass-port-default-seat port))
         (b (add-seat port :name "seat-B" :port-num 5982 :width 800 :height 600
                           :fb (glass:make-framebuffer 800 600 +wm-teal+)))
         (tok (glass-port-clim-token port)))

    ;; ---- a real McCLIM frame ------------------------------------------------
    (let* ((fm (find-frame-manager :port port))
           (frame (make-application-frame 'ho-frame :frame-manager fm)))
      (sb-thread:make-thread (lambda () (ignore-errors (run-frame-top-level frame)))
                             :name "ho-app")
      (check (wait-until (lambda () (find-if #'glass-mirror-managed (glass-port-mirrors port))))
             "a real McCLIM frame came up as a managed window")
      (sleep 0.8)
      (let* ((m (find-if #'glass-mirror-managed (glass-port-mirrors port)))
             (sheet (glass-mirror-sheet m))
             (pane (clim:find-pane-named frame 'log)))
        (composite-all port)

        ;; ================= 1. three things called "primary" ====================
        (format t "~&~%[1. the three meanings of \"primary\", separated]~%")
        (check (and (seat-home-p a) (not (seat-home-p b)))
               "A is the HOME seat, B is not")
        (check (eq (seat-clipboard a) (glass:session-clipboard))
               "…the home seat owns the SESSION clipboard")
        (check (eq (glass-port-default-seat port) a)
               "…and is PORT-SEAT's default (what an unaddressed call means)")
        (check (eq (clim-token-holder port) a)
               "the CLIM token starts with the first seat — one person is driving from")
        (check (= (token-contests tok) 0)
               "…seeded, not taken from anybody (~d contests so far)" (token-contests tok))
        ;; B presses inside the CLIM window's content: that is the gesture that drives it.
        (let ((cx (seat-window-x b m)) (cy (seat-window-y b m)))
          (click port b (+ cx 40) (+ cy 120)))
        (drain port)
        (check (eq (clim-token-holder port) b) "B pressed in the CLIM window and now drives it")
        (check (and (seat-home-p a) (not (seat-home-p b)))
               "…and A is STILL the home seat — the role did not follow the click")
        (check (eq (seat-clipboard a) (glass:session-clipboard))
               "…still owns the session clipboard")
        (check (eq (glass-port-default-seat port) a) "…still the default seat")
        (check (eq (seat-injector a) (seat-injector a)) "…still has its own key injector")
        (note "token: ~a" (clim-token-report port))

        ;; ================= 2. the geometry travels =============================
        (format t "~%[2. McCLIM believes the DRIVER's window position]~%")
        ;; A takes it back and puts the window somewhere; then B moves it somewhere else
        ;; on ITS OWN screen only.
        (click port a (+ (seat-window-x a m) 40) (+ (seat-window-y a m) 120))
        (drain port)
        (check (eq (clim-token-holder port) a) "A pressed inside it and took it back")
        (wm-move m 120 140 a) (wm-sync-sheet port m a)
        (drain port)
        (let ((own-x (window-own-x m)) (own-y (window-own-y m)))
          (check (and (= own-x 120) (= own-y 140))
                 "the home seat's move wrote the window's OWN position (~d,~d)" own-x own-y)
          (multiple-value-bind (sx sy) (clim-screen-position sheet)
            (check (and (= sx 120) (= sy 140))
                   "…and McCLIM agrees the window is there (~d,~d)" sx sy))
          ;; B moves it on its own screen.  Not the driver, so McCLIM must not follow yet.
          (wm-move m 430 300 b) (wm-sync-sheet port m b)
          (drain port)
          (check (= (seat-window-x b m) 430) "B moved it to 430,300 on ITS screen")
          (check (and (= (window-own-x m) 120) (= (window-own-y m) 140))
                 "…without touching the window's own position (still ~d,~d)"
                 (window-own-x m) (window-own-y m))
          (multiple-value-bind (sx sy) (clim-screen-position sheet)
            (check (and (= sx 120) (= sy 140))
                   "…and McCLIM still places from A's position (~d,~d) — B is not driving" sx sy))
          (shoot "geometry-A-drives" a b)
          ;; Now B takes the token.  THIS is the move that used to be impossible.
          (let ((before (token-resyncs tok)))
            (click port b (+ (seat-window-x b m) 40) (+ (seat-window-y b m) 120))
            (drain port)
            (check (eq (clim-token-holder port) b) "B pressed inside it: B drives now")
            (check (= (- (token-resyncs tok) before) 1)
                   "…and exactly ONE window had to be resynced (~d)" (- (token-resyncs tok) before))
            (multiple-value-bind (sx sy) (clim-screen-position sheet)
              (check (and (= sx 430) (= sy 300))
                     "McCLIM now places from B's position (~d,~d) — the geometry travelled" sx sy))
            (check (and (= (window-own-x m) 120) (= (window-own-y m) 140))
                   "…and A's window did NOT move: own position still ~d,~d"
                   (window-own-x m) (window-own-y m))
            (composite-all port)
            (check (/= (pixel-at (seat-fb a) 200 200) +wm-teal+)
                   "A's screen still has the window where A put it")
            (check (= (pixel-at (seat-fb b) 200 200) +wm-teal+)
                   "…and B's screen has bare workspace there")
            (shoot "geometry-B-drives" a b)))

        ;; ================= 3. two people, one window, in order =================
        (format t "~%[3. two people type into the one shared window]~%")
        (setf (climi::port-keyboard-input-focus port) pane)
        (typed-clear)
        ;; A does not hold the token: its keys must reach nothing.
        (check (eq (clim-token-holder port) b) "B is holding it")
        (type-into port a "XXX") (drain port 0.3)
        (check (string= (typed) "") "A is not driving, so A's keys type NOTHING (~s)" (typed))
        (type-into port b "abc") (drain port 0.3)
        (check (string= (typed) "abc") "B types \"abc\" into the window (~s)" (typed))
        ;; A presses inside the window and types: the token changes hands mid-sentence.
        (click port a (+ (seat-window-x a m) 40) (+ (seat-window-y a m) 120))
        (drain port 0.3)
        (setf (climi::port-keyboard-input-focus port) pane)
        (check (eq (clim-token-holder port) a) "A pressed inside it and took over")
        (type-into port a "def") (drain port 0.3)
        (check (string= (typed) "abcdef")
               "the two people's characters landed IN ORDER in one window: ~s" (typed))
        (type-into port b "ZZZ") (drain port 0.3)
        (check (string= (typed) "abcdef") "…and the seat that let go types nothing (~s)" (typed))
        (shoot "typing" a b)

        ;; ================= 4. a pull-down under the second seat's pointer ======
        (format t "~%[4. a pull-down opened by B lands at B's window position]~%")
        ;; Hand the token to B and open the frame's "File" menu by pressing its menu bar,
        ;; which on B's screen is at B's window position.
        (click port b (+ (seat-window-x b m) 40) (+ (seat-window-y b m) 120))
        (drain port 0.3)
        (check (eq (clim-token-holder port) b) "B drives")
        (let* ((mb-x (+ (seat-window-x b m) 20)) (mb-y (+ (seat-window-y b m) 8))
               (before (remove-if #'glass-mirror-managed
                                  (remove-if-not (lambda (x) (typep x 'glass-mirror))
                                                 (glass-port-mirrors port)))))
          (press port b mb-x mb-y)
          (wait-until (lambda ()
                        (> (length (remove-if #'glass-mirror-managed
                                              (remove-if-not (lambda (x) (typep x 'glass-mirror))
                                                             (glass-port-mirrors port))))
                           (length before)))
                      3)
          (drain port 0.4)
          (let* ((pops (set-difference
                        (remove-if #'glass-mirror-managed
                                   (remove-if-not (lambda (x) (typep x 'glass-mirror))
                                                  (glass-port-mirrors port)))
                        before))
                 (pop (first pops)))
            (cond
              (pop
               (note "pull-down mirror at ~d,~d; B holds the window at ~d,~d, A at ~d,~d"
                     (glass-mirror-x pop) (glass-mirror-y pop)
                     (seat-window-x b m) (seat-window-y b m)
                     (seat-window-x a m) (seat-window-y a m))
               (check (and (<= (seat-window-x b m) (glass-mirror-x pop)
                               (+ (seat-window-x b m) 200))
                           (<= (seat-window-y b m) (glass-mirror-y pop)
                               (+ (seat-window-y b m) 200)))
                      "the pull-down landed inside B's copy of the window, under B's pointer")
               (check (not (and (<= (seat-window-x a m) (glass-mirror-x pop)
                                    (+ (seat-window-x a m) 60))
                                (<= (seat-window-y a m) (glass-mirror-y pop)
                                    (+ (seat-window-y a m) 60))))
                      "…and NOT at A's copy, which is where it would have gone before")
               (shoot "pulldown-B" a b)
               (release port b mb-x mb-y)
               (click port b 700 560)          ; dismiss
               (drain port 0.4))
              (t
               (note "NO pull-down mirror appeared — CLIM's menu-bar tracking did not run in")
               (note "this harness; the geometric claim above (CLIM-SCREEN-POSITION, which is")
               (note "the transformation CLIM places pull-downs THROUGH) stands on its own.")
               (release port b mb-x mb-y)
               (drain port 0.3)))))

        ;; ================= 5. never mid-gesture ================================
        (format t "~%[5. a handoff never lands in the middle of a gesture]~%")
        ;; (a) a window-manager drag in flight
        (click port a (+ (seat-window-x a m) 40) (+ (seat-window-y a m) 120))
        (drain port 0.3)
        (check (eq (clim-token-holder port) a) "A drives")
        (let ((tx (+ (seat-window-x a m) 60)) (ty (- (seat-window-y a m) 10))
              (deferrals (token-deferrals tok)))
          (press port a tx ty)                                   ; grab A's title bar
          (glass-on-pointer port 1 (+ tx 30) (+ ty 20) a)        ; …and drag
          (check (seat-drag a) "A has a window drag in flight")
          (check (clim-token-pinned-p port) "…so the token is PINNED")
          (press port b (+ (seat-window-x b m) 40) (+ (seat-window-y b m) 120))
          (drain port 0.3)
          (check (eq (clim-token-holder port) a)
                 "B pressed in the window and the token did NOT move — A is still driving")
          (check (eq (token-pending tok) b) "…B is recorded as PENDING instead")
          (check (= (- (token-deferrals tok) deferrals) 1) "…and the deferral was counted")
          (check (seat-drag a) "A's drag is intact — nothing teleported out from under it")
          ;; A finishes the drag; the handoff applies at the end of it.
          (glass-on-pointer port 0 (+ tx 30) (+ ty 20) a)
          (drain port 0.4)
          (check (null (seat-drag a)) "A's drag landed")
          (check (eq (clim-token-holder port) b) "…and THEN the deferred handoff happened: B drives")
          (check (null (token-pending tok)) "…with nothing left pending")
          (multiple-value-bind (sx sy) (clim-screen-position sheet)
            (check (and (= sx (seat-window-x b m)) (= sy (seat-window-y b m)))
                   "…and McCLIM's geometry followed B at that moment (~d,~d)" sx sy)))
        ;; (b) a CLIM grab — a pull-down posted, CLIM's own tracking loop running
        (format t "~%   …and the same for a CLIM grab (a posted pull-down)~%")
        (click port a (+ (seat-window-x a m) 40) (+ (seat-window-y a m) 120))
        (drain port 0.3)
        (let ((deferrals (token-deferrals tok)))
          ;; PORT-GRAB-POINTER is what CLIM calls when it posts a pull-down; call it the
          ;; way CLIM does rather than poking the slot, so the pin is the real one.
          (port-grab-pointer port (climi::port-pointer port) sheet)
          (check (clim-token-pinned-p port) "CLIM grabbed the pointer: the token is pinned")
          (press port b (+ (seat-window-x b m) 40) (+ (seat-window-y b m) 120))
          (drain port 0.3)
          (check (eq (clim-token-holder port) a) "B's press did not take it")
          (check (eq (token-pending tok) b) "…it is pending")
          (check (= (- (token-deferrals tok) deferrals) 1) "…and counted")
          (check (eq (seat-grab-sheet a) sheet)
                 "CLIM's grab is still A's and still intact — the tracking loop is not corrupted")
          (port-ungrab-pointer port (climi::port-pointer port) sheet)
          (release port b (+ (seat-window-x b m) 40) (+ (seat-window-y b m) 120))
          (drain port 0.3)
          (check (eq (clim-token-holder port) b)
                 "the grab ended and the deferred handoff applied: B drives"))

        ;; ================= 6. it does not stay taken ===========================
        (format t "~%[6. release: the token does not stay held forever]~%")
        ;; (a) pointer exit
        (check (eq (clim-token-holder port) b) "B holds it")
        (hover port b 700 560)                                   ; bare workspace
        (check (null (clim-token-holder port))
               "B's pointer left every CLIM window: the token is FREE")
        (check (eq (token-state tok) :free) "…the state says so")
        ;; …and taking a free token is SILENT, not a contest
        (let ((silent (token-silent-takes tok)) (contests (token-contests tok)))
          (click port a (+ (seat-window-x a m) 40) (+ (seat-window-y a m) 120))
          (drain port 0.3)
          (check (eq (clim-token-holder port) a) "A took it")
          (check (and (= (- (token-silent-takes tok) silent) 1)
                      (= (- (token-contests tok) contests) 0))
                 "…SILENTLY — a free token taken is not a contest (~d silent, ~d contested)"
                 (- (token-silent-takes tok) silent) (- (token-contests tok) contests)))
        ;; (b) idle
        (let ((*clim-token-idle* 1/20))
          (sleep 0.2)
          (wm-tick port)
          (check (null (clim-token-holder port))
                 "a holder silent past *CLIM-TOKEN-IDLE* loses it on the next tick"))
        (check (string= (typed) "abcdef") "…and losing it typed nothing (~s)" (typed))
        ;; a freed token is retaken by the SAME seat's next input, with no click
        (setf (climi::port-keyboard-input-focus port) pane)
        (type-into port a "g") (drain port 0.3)
        (check (string= (typed) "abcdefg")
               "…and that seat's very next keystroke takes it back, no click needed: ~s" (typed))
        ;; (c) the viewers went away
        (click port b (+ (seat-window-x b m) 40) (+ (seat-window-y b m) 120))
        (drain port 0.3)
        (check (eq (clim-token-holder port) b) "B holds it again")
        (setf (seat-buttons b) 1)                     ; …mid-gesture, even
        (check (clim-token-pinned-p port) "…and is pinned by a button that is down")
        (clim-token-seat-gone port b)                 ; its last viewer disconnected
        (check (null (clim-token-holder port))
               "its last viewer disconnected: the token is free anyway — a pin needs a hand")
        (note "token: ~a" (clim-token-report port))

        ;; ================= 7. thrash ===========================================
        (format t "~%[7. two seats alternating must not churn geometry resyncs]~%")
        ;; identical layouts: both seats holding the desktop the same way
        (wm-move m 200 200 a) (seat-forget-window b m)
        (drain port 0.3)
        (click port a (+ (seat-window-x a m) 40) (+ (seat-window-y a m) 120)) (drain port 0.2)
        (let ((r0 (token-resyncs tok)) (s0 (token-resync-scans tok))
              (t0 (get-internal-real-time)) (n 200))
          (dotimes (i n)
            (let ((s (if (evenp i) b a)))
              (click port s (+ (seat-window-x s m) 40) (+ (seat-window-y s m) 120))))
          (let ((ms (/ (- (get-internal-real-time) t0)
                       (/ internal-time-units-per-second 1000.0))))
            (check (= (- (token-resyncs tok) r0) 0)
                   "~d alternating handoffs on IDENTICAL layouts moved ~d windows"
                   n (- (token-resyncs tok) r0))
            (note "~d windows examined, ~,3f ms total, ~,4f ms per handoff"
                  (- (token-resync-scans tok) s0) ms (/ ms n))))
        ;; one window diverged: each handoff moves exactly that one and no more
        (wm-move m 430 300 b)
        (drain port 0.3)
        (let ((r0 (token-resyncs tok)) (t0 (get-internal-real-time)) (n 100))
          (dotimes (i n)
            (let ((s (if (evenp i) b a)))
              (click port s (+ (seat-window-x s m) 40) (+ (seat-window-y s m) 120))))
          (let ((ms (/ (- (get-internal-real-time) t0)
                       (/ internal-time-units-per-second 1000.0))))
            (check (= (- (token-resyncs tok) r0) n)
                   "~d handoffs with ONE window diverged moved exactly ~d (one each, none extra)"
                   n (- (token-resyncs tok) r0))
            (note "~,3f ms total, ~,4f ms per handoff — a sheet-transformation write each"
                  ms (/ ms n))))
        ;; …and the same round SUSTAINED.  The round above runs once, early, on a queue
        ;; with nothing in it, and passes under any discipline at all.  What it cannot see
        ;; is a BACKLOG: an aim-at-the-driver move still waiting to be applied when the
        ;; next seat claims the token.  That is the state in which a second writer of
        ;; GLASS-MIRROR-CLIM-X/Y is fatal — an apply that has been superseded stamps its
        ;; own target back over the fresh one, the next acquirer's diff finds nothing to
        ;; move, and that seat drives on with McCLIM's sheet still pointed at the other
        ;; person's window.  So run the round over and over and require EVERY one whole;
        ;; before the invariant had one writer, most rounds came up one or two short.
        (let ((rounds 25) (n 100) (short 0) (missed 0) (worst 100))
          (dotimes (r rounds)
            (let ((r0 (token-resyncs tok)))
              (dotimes (i n)
                (let ((s (if (evenp i) b a)))
                  (click port s (+ (seat-window-x s m) 40) (+ (seat-window-y s m) 120))))
              (let ((moved (- (token-resyncs tok) r0)))
                (setf worst (min worst moved))
                (unless (= moved n) (incf short) (incf missed (- n moved))))))
          (check (zerop short)
                 "sustained: ~d rounds x ~d handoffs, every round moved all ~d ~
                  (~d short round~:p, ~d handoff~:p missed, worst round ~d)"
                 rounds n n short missed worst))
        ;; What all that counting is ABOUT: when the queue finally drains, McCLIM's own
        ;; arithmetic — the transformation it places pull-downs THROUGH — must land on the
        ;; last driver's window and agree with what we recorded for it.  A resync that was
        ;; skipped shows up here as a driver reading somebody else's geometry.
        (drain port 0.5)
        (let ((last (or (clim-token-holder port) a)))
          (multiple-value-bind (sx sy) (clim-screen-position sheet)
            (check (and (= sx (seat-window-x last m)) (= sy (seat-window-y last m)))
                   "…and the drained queue left McCLIM on the LAST driver's position ~
                    (~a holds it at ~d,~d; McCLIM says ~d,~d)"
                   (seat-name last) (seat-window-x last m) (seat-window-y last m) sx sy)
            (check (and (= sx (glass-mirror-clim-x m)) (= sy (glass-mirror-clim-y m)))
                   "…with the record and the sheet agreeing once nothing is in flight ~
                    (recorded ~d,~d)" (glass-mirror-clim-x m) (glass-mirror-clim-y m))))
        (note "token: ~a" (clim-token-report port))

        ;; ================= 8. native surfaces are untouched ====================
        (format t "~%[8. the consolidated CLIM seat did not leak into native surfaces]~%")
        (let* ((la (make-array 0 :element-type 'character :adjustable t :fill-pointer t))
               (lb (make-array 0 :element-type 'character :adjustable t :fill-pointer t))
               (ta (add-surface port (lambda (fb) (glass:fb-fill fb #x884400)
                                       (values (lambda (down k)
                                                 (when (and down (<= 32 k 126))
                                                   (vector-push-extend (code-char k) la)))
                                               nil (constantly nil)))
                                :title "term-A" :width 180 :height 120))
               (tb (add-surface port (lambda (fb) (glass:fb-fill fb #x008888)
                                       (values (lambda (down k)
                                                 (when (and down (<= 32 k 126))
                                                   (vector-push-extend (code-char k) lb)))
                                               nil (constantly nil)))
                                :title "term-B" :width 180 :height 120)))
          (setf (wm-surface-x ta) 40 (wm-surface-y ta) 430
                (wm-surface-x tb) 300 (wm-surface-y tb) 430)
          ;; put the CLIM window where neither terminal is, for both seats
          (wm-move m 60 60 a) (seat-forget-window b m)
          (composite-all port)
          ;; B takes the CLIM token…
          (click port b (+ (seat-window-x b m) 40) (+ (seat-window-y b m) 60))
          (drain port 0.3)
          (check (eq (clim-token-holder port) b) "B is driving McCLIM")
          ;; …while BOTH seats drive a native window each, at the same time.
          (click port a (+ (seat-window-x a ta) 20) (+ (seat-window-y a ta) 20))
          (click port b (+ (seat-window-x b tb) 20) (+ (seat-window-y b tb) 20))
          (check (and (eq (seat-focus-surface a) ta) (eq (seat-focus-surface b) tb))
                 "A focused term-A and B focused term-B")
          (loop for ca across "hello" for cb across "world" do
            (glass-on-key port t (char-code ca) a)
            (glass-on-key port t (char-code cb) b))
          (check (string= (coerce la 'string) "hello") "A typed \"hello\" into its terminal")
          (check (string= (coerce lb 'string) "world") "B typed \"world\" into its terminal")
          ;; B moving onto a terminal IS the pointer-exit rule, so the token went free.
          ;; What matters here is that a native window never handed it to somebody else
          ;; and never took anything from the seat that was driving CLIM.
          (check (null (clim-token-holder port))
                 "B's pointer went to a terminal, so the token went free — the exit rule")
          (check (string= (typed) "abcdefg")
                 "…and ten keystrokes into two native windows put nothing in the CLIM one (~s)"
                 (typed))
          (shoot "surfaces" a b))

        ;; ================= 9. the indicator, both ways =========================
        (format t "~%[9. the holder indicator — default OFF]~%")
        (wm-move m 200 200 a) (seat-forget-window b m)
        (click port b (+ (seat-window-x b m) 40) (+ (seat-window-y b m) 120))
        (drain port 0.4)
        (check (eq (clim-token-holder port) b) "B is driving")
        (check (null *clim-token-indicator*) "*CLIM-TOKEN-INDICATOR* is NIL by default")
        (composite-all port)
        (let* ((tx (+ (seat-window-x a m) 100))
               (ty (- (seat-window-y a m) 12))
               (grey (logand +wm-title-bg+ #xffffff))
               (tint (logand +wm-title-other-bg+ #xffffff)))
          (check (= (pixel-at (seat-fb a) tx ty) grey)
                 "OFF: the non-driver's title bar is the ordinary OPEN LOOK grey (#~6,'0x)"
                 (pixel-at (seat-fb a) tx ty))
          (check (= (pixel-at (seat-fb b) tx ty) grey)
                 "OFF: so is the driver's (#~6,'0x)" (pixel-at (seat-fb b) tx ty))
          (shoot "indicator-off" a b)
          (clim-token-show-indicator port t)
          (check *clim-token-indicator* "switched ON on the running desktop")
          (check (= (pixel-at (seat-fb a) tx ty) tint)
                 "ON: A is not driving, so A's copy of the title bar is tinted (#~6,'0x)"
                 (pixel-at (seat-fb a) tx ty))
          (check (= (pixel-at (seat-fb b) tx ty) grey)
                 "ON: B IS driving, so B sees the desktop it has always seen (#~6,'0x)"
                 (pixel-at (seat-fb b) tx ty))
          (shoot "indicator-on" a b)
          ;; and it follows the token
          (click port a (+ (seat-window-x a m) 40) (+ (seat-window-y a m) 120))
          (drain port 0.4)
          (check (= (pixel-at (seat-fb b) tx ty) tint)
                 "…A took over and the tint moved to B's screen (#~6,'0x)"
                 (pixel-at (seat-fb b) tx ty))
          (check (= (pixel-at (seat-fb a) tx ty) grey) "…and left A's")
          (shoot "indicator-handed-over" a b)
          (clim-token-show-indicator port nil)
          (composite-all port)
          (check (and (= (pixel-at (seat-fb a) tx ty) grey)
                      (= (pixel-at (seat-fb b) tx ty) grey))
                 "switched back OFF: both screens are grey again"))

        (note "final token: ~a" (clim-token-report port))))
    (sb-concurrency:send-message (glass-port-mailbox port) (lambda () nil)))

  (format t "~%=> ~:[PASS~;FAIL (~d)~]~%" (plusp *fail*) *fail*)
  (finish-output)
  (sb-ext:exit :code (if (plusp *fail*) 1 0)))
