;;;; compositor-proof.lisp — prove the REAL compositor-actor multiplexes isolated
;;;; app-actors on the shared WM display.  A WM-mode glass-port (the shared screen)
;;;; is wrapped in a compositor-actor; an unmodified CLIM frame is spawned as an
;;;; app-actor (its own thread + message-port) drawing to it over messages.  We
;;;; assert: (1) the create-window handshake registered a managed WM window; (2)
;;;; the app's pixels reached the DECORATED screen framebuffer (app -> compositor
;;;; -> shared display); (3) a WM resize (the same path a corner-drag uses) routed
;;;; :resize to the app and its relaid-out window came back bigger (the full
;;;; compositor -> app -> compositor loop); (4) a content click through the WM
;;;; input path focuses the actor window.  In-process, no VNC — host-load-immune.
;;;;   sbcl --control-stack-size 256 --dynamic-space-size 4096 --load backend/inspect/compositor-proof.lisp
(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:glass :mcclim :mcclim-render :sb-concurrency :clim-examples))
    (asdf:load-asd "/home/claude/glass/backend/mcclim-glass.asd")
    (asdf:load-system :mcclim-glass)))
(in-package :clim-glass)

(defparameter *calculator*
  (find-symbol "CALCULATOR-APP" (find-package '#:clim-demo.calculator))
  "The stock McCLIM calculator frame class — a REAL app, spawned as an actor.")

;;; an ordinary CLIM app — NOT modified for actors
(define-application-frame cproof-frame ()
  ()
  (:panes (canvas :application :display-function 'draw-cproof :scroll-bars nil))
  (:layouts (default canvas)))
(defun draw-cproof (frame pane)
  (declare (ignore frame))
  (draw-rectangle* pane 3 3 40 30 :ink +red+))       ; ARGB 0xFFFF0000

(defun screen-has-red-p (port)
  (let ((px (glass:fb-pixels (glass-port-fb port))))
    (dotimes (i (length px) nil) (when (= (logand (aref px i) #xffffff) #xff0000) (return t)))))

(defun wait-until (pred &optional (secs 10))
  (let ((end (+ (get-internal-real-time) (* secs internal-time-units-per-second))))
    (loop until (funcall pred) do (sleep 1/50)
          when (> (get-internal-real-time) end) do (return nil)
          finally (return t))))

(let* ((port (make-instance 'glass-port :port 5955))
       (fail 0))
  (setf (glass-port-wm-p port) t
        (glass-port-screen-w port) 800 (glass-port-screen-h port) 600
        (glass-port-fb port) (glass:make-framebuffer 800 600 +wm-teal+))
  (flet ((check (ok fmt &rest args) (format t "  [~:[FAIL~;pass~]] ~?~%" ok fmt args) (unless ok (incf fail))))
    (let* ((comp (start-compositor port))
           (_ (spawn-app-actor comp 'cproof-frame :width 200 :height 150 :name "actor-cproof")))
      (declare (ignore _))
      (format t "~&[compositor-actor: a desktop of isolated app-actors]~%")
      ;; (1) create-window handshake -> a managed WM window
      (check (wait-until (lambda () (plusp (compositor-next comp))))
             "create-window handshake -> ~d managed window(s)" (compositor-next comp))
      (check (= (length (glass-port-surfaces port)) 1) "the app-actor is a WM surface on the screen")
      ;; (2) the app's pixels reached the DECORATED shared screen
      (check (wait-until (lambda () (screen-has-red-p port)))
             "the app's rectangle is on the shared (decorated) screen fb")
      (let* ((surf (first (glass-port-surfaces port)))
             (w0 (glass:fb-width (wm-surface-fb surf))))
        ;; (3) a WM resize (== a corner-drag) round-trips through the actor
        (wm-resize port surf 360 260)
        (check (wait-until (lambda () (> (glass:fb-width (wm-surface-fb surf)) w0)))
               "WM resize -> :resize routed to app -> relaid-out window came back bigger (~d -> ~d)"
               w0 (glass:fb-width (wm-surface-fb surf)))
        ;; (4) a content click through the WM input path focuses the actor window
        (multiple-value-bind (cx cy) (values (+ (wm-surface-x surf) 10) (+ (wm-surface-y surf) 10))
          (wm-on-pointer port 1 cx cy)                 ; left-press on content
          (check (eq (glass-port-focus-surface port) surf) "content click focuses the actor window")
          (wm-on-pointer port 0 cx cy)))               ; release
      ;; (5) a SECOND, real app-actor (the stock McCLIM calculator) on the same
      ;;     compositor — two isolated actors sharing one display.
      (when *calculator*
        (spawn-app-actor comp *calculator* :width 320 :height 300 :name "actor-calculator")
        (check (wait-until (lambda () (= (compositor-next comp) 2)))
               "the stock McCLIM calculator joined as a 2nd isolated app-actor")
        (check (wait-until (lambda () (= (length (glass-port-surfaces port)) 2)))
               "two app-actors are multiplexed on one shared display"))
      (sb-concurrency:send-message (compositor-mbox comp) '(:stop))))
  (format t "~%=> ~:[PASS~;FAIL (~d)~]~%" (plusp fail) fail)
  (finish-output) (sb-ext:exit :code (if (plusp fail) 1 0)))
