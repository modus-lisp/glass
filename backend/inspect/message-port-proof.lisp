;;;; message-port-proof.lisp — prove an UNMODIFIED CLIM app renders + takes input
;;;; across an actor boundary.  A mock compositor-actor owns a screen fb and a
;;;; mailbox; a proof-frame runs on a message-port whose display ships :damage to
;;;; that mailbox.  We assert: (1) the frame's pixels crossed the boundary (the
;;;; red rectangle it drew appears in the compositor's fb), and (2) a neutral
;;;; :resize input tuple sent the other way drove the app to relayout (a later
;;;; :damage arrives at the new size).  In-process, so host load can't skew it.
;;;;   sbcl --non-interactive --load backend/inspect/message-port-proof.lisp
(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:glass :mcclim :mcclim-render :sb-concurrency))
    (asdf:load-asd "/home/claude/glass/backend/mcclim-glass.asd")
    (asdf:load-system :mcclim-glass)))
(in-package :clim-glass)

;;; ---- a mock compositor-actor ----------------------------------------------
(defstruct mockcomp
  (mbox (sb-concurrency:make-mailbox))
  (fb (glass:make-framebuffer 400 300))
  (surfaces (make-hash-table)) (next 0)
  (damages '()))                          ; (id x y w h) of each damage received, newest-first

(defun blit-argb (fb arr ox oy w h)
  (let ((dpx (glass:fb-pixels fb)) (fw (glass:fb-width fb)) (fh (glass:fb-height fb)))
    (dotimes (iy h)
      (let ((fy (+ oy iy)))
        (when (< -1 fy fh)
          (let ((frow (* fy fw)))
            (dotimes (ix w)
              (let ((fx (+ ox ix)))
                (when (< -1 fx fw)
                  (setf (aref dpx (+ frow fx)) (logand (aref arr iy ix) #xffffff)))))))))))

(defun mock-run (mc)
  (loop
    (let ((msg (sb-concurrency:receive-message (mockcomp-mbox mc))))
      (case (car msg)
        (:create-window
         (destructuring-bind (w h title reply owner) (cdr msg) (declare (ignore w h title owner))
           (let ((id (incf (mockcomp-next mc))))
             (setf (gethash id (mockcomp-surfaces mc)) t)
             (sb-concurrency:send-message reply id))))
        (:damage
         (destructuring-bind (id x y w h arr) (cdr msg)
           (blit-argb (mockcomp-fb mc) arr x y w h)
           (push (list id x y w h) (mockcomp-damages mc))))
        (:destroy nil)
        (:stop (return))))))

;;; ---- an ordinary CLIM app (NOT modified for actors in any way) -------------
(define-application-frame proof-frame ()
  ()
  (:panes (canvas :application :display-function 'draw-proof
                  :scroll-bars nil :width 120 :height 80))
  (:layouts (default canvas)))
(defun draw-proof (frame pane)
  (declare (ignore frame))
  (draw-rectangle* pane 5 5 60 40 :ink +red+))       ; ARGB 0xFFFF0000

;;; ---- run it across the boundary --------------------------------------------
(defun has-red-p (fb)
  (let ((px (glass:fb-pixels fb)))
    (dotimes (i (length px) nil) (when (= (logand (aref px i) #xffffff) #xff0000) (return t)))))

(defun wait-until (pred &optional (secs 8))
  (let ((end (+ (get-internal-real-time) (* secs internal-time-units-per-second))))
    (loop until (funcall pred) do (sleep 1/50)
          when (> (get-internal-real-time) end) do (return nil)
          finally (return t))))

(let* ((mc (make-mockcomp))
       (comp-thread (sb-thread:make-thread (lambda () (mock-run mc)) :name "mock-compositor"))
       (port (make-message-port (mockcomp-mbox mc)))
       (fm (find-frame-manager :port port))
       (fail 0))
  (flet ((check (ok fmt &rest args) (format t "  [~:[FAIL~;pass~]] ~?~%" ok fmt args) (unless ok (incf fail))))
    (climi::restart-port port)                        ; start the port's event loop
    (let* ((frame (make-application-frame 'proof-frame :frame-manager fm))
           (app-thread (sb-thread:make-thread
                        (lambda () (handler-case (run-frame-top-level frame)
                                     (error (e) (format *trace-output* "~&app: ~a~%" e))))
                        :name "proof-app")))
      (format t "~&[message-port: unmodified CLIM app across an actor boundary]~%")
      ;; (1) render crossed the boundary
      (check (wait-until (lambda () (mockcomp-damages mc))) "app painted -> :damage crossed the boundary")
      (check (plusp (mockcomp-next mc)) ":create-window handshake (~d window(s))" (mockcomp-next mc))
      (check (has-red-p (mockcomp-fb mc)) "the red rectangle it drew is in the COMPOSITOR's fb")
      (let ((before (first (last (mockcomp-damages mc)))))   ; first damage = initial size
        ;; (2) neutral input tuple the other way drives the unmodified app
        (setf (mockcomp-damages mc) nil)
        (sb-concurrency:send-message (glass-port-mailbox port) (list :resize 300 200))
        (check (wait-until (lambda () (mockcomp-damages mc)))
               ":resize input tuple crossed -> app repainted")
        (let ((after (first (mockcomp-damages mc))))
          (check (and before after (not (equal (cdddr before) (cdddr after))))
                 "app RELAID OUT to the new size (~a -> ~a)"
                 (and before (list (fourth before) (fifth before)))
                 (and after (list (fourth after) (fifth after))))))
      (ignore-errors (sb-thread:terminate-thread app-thread))
      (sb-concurrency:send-message (mockcomp-mbox mc) '(:stop))
      (ignore-errors (sb-thread:join-thread comp-thread :timeout 2))))
  (format t "~%=> ~:[PASS~;FAIL (~d)~]~%" (plusp fail) fail)
  (finish-output) (sb-ext:exit :code (if (plusp fail) 1 0)))
