;;;; adaptive-drag-proof.lisp — a drag stays OPAQUE while the link keeps up, and
;;;; switches to WIREFRAME (window stays put, only an outline moves) once the send
;;;; backlog crosses the threshold; on release the window lands at the final spot.
;;;; Drives wm-on-pointer directly with a forced glass:*send-queue*.  In-process.
;;;;   sbcl --control-stack-size 256 --dynamic-space-size 4096 --non-interactive --load backend/inspect/adaptive-drag-proof.lisp
(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:glass :mcclim :mcclim-render :sb-concurrency))
    (asdf:load-asd "/home/claude/glass/backend/mcclim-glass.asd")
    (asdf:load-system :mcclim-glass)))
(in-package :clim-glass)

(let* ((port (make-instance 'glass-port :port 5972)) (fail 0)
       (sfb (glass:make-framebuffer 200 150 (glass:rgb 40 40 40))))
  (setf (glass-port-wm-p port) t (glass-port-screen-w port) 1280 (glass-port-screen-h port) 800
        (glass-port-fb port) (glass:make-framebuffer 1280 800 +wm-teal+))
  (let ((surf (make-wm-surface :fb sfb :x 100 :y 100 :title "t" :dirty-p (constantly nil))))
    (push surf (glass-port-surfaces port))
    (setf (glass-port-drag port) (list surf :move 10 10))     ; a move in progress, grab offset (10,10)
    (flet ((check (ok fmt &rest args) (format t "  [~:[FAIL~;pass~]] ~?~%" ok fmt args) (unless ok (incf fail))))
      (format t "~&[adaptive drag: opaque when the link keeps up, wireframe when it lags]~%")
      ;; (1) low backlog -> opaque: the real window moves, no wireframe
      (setf glass:*send-queue* 0d0)
      (wm-on-pointer port 1 150 150)                          ; button down, content -> (140,140)
      (check (and (= (wm-surface-x surf) 140) (not (glass-port-drag-wire port)))
             "low backlog -> OPAQUE move (surf x=~d, wire=~a)" (wm-surface-x surf) (glass-port-drag-wire port))
      ;; (2) high backlog -> switch to wireframe: window stays put, outline appears
      (setf glass:*send-queue* 500d0)
      (wm-on-pointer port 1 200 200)                          ; content -> (190,190)
      (check (glass-port-drag-wire port) "high backlog -> switched to WIREFRAME")
      (check (= (wm-surface-x surf) 140) "the real window did NOT move in wireframe (still x=~d)" (wm-surface-x surf))
      (check (consp (glass-port-drag-wire-box port)) "a moving outline box exists: ~a" (glass-port-drag-wire-box port))
      ;; (3) stays wireframe even if backlog recovers (no flapping mid-drag)
      (setf glass:*send-queue* 0d0)
      (wm-on-pointer port 1 220 220)
      (check (and (glass-port-drag-wire port) (= (wm-surface-x surf) 140))
             "stays wireframe for the rest of the drag (no flapping)")
      ;; (4) release -> the real window lands at the final position, wireframe cleared
      (wm-on-pointer port 0 260 260)                          ; button up, content -> (250,250)
      (check (and (= (wm-surface-x surf) 250) (not (glass-port-drag-wire port)) (null (glass-port-drag port)))
             "release -> window landed at final pos (x=~d, wire cleared)" (wm-surface-x surf))))
  (format t "~%=> ~:[PASS~;FAIL (~d)~]~%" (plusp fail) fail)
  (finish-output) (sb-ext:exit :code (if (plusp fail) 1 0)))
