;;;; chrome-scale-gate.lisp — the compositor and the hit test must agree about chrome.
;;;;
;;;; THE BUG THIS EXISTS FOR is one that has not happened, because this gate is what keeps
;;;; it from happening.  Window chrome is consumed twice: the compositor DRAWS a title bar,
;;;; and WM-HIT decides a click landed on one.  Once the height depends on a seat's density,
;;;; those two can disagree — and a 1x hit test against a 2x drawing does not fail loudly.
;;;; It puts every title bar somewhere other than where it is shown, so windows drag from a
;;;; strip of empty desktop and the bar you can see does nothing.  There is no error, no
;;;; backtrace, and nothing in a log.
;;;;
;;;; So the test is deliberately end to end: make a real window at a real density, ask the
;;;; compositor where it put the frame, and click the middle of the title bar it drew.
;;;; Anything other than agreement shows up as a miss.
;;;;
;;;; 3/2 is in the list on purpose.  Integer scales can agree by accident when both sides
;;;; round the same way; a fractional one is where a second rounding site would show.
;;;;
;;;;   sbcl --script inspect/chrome-scale-gate.lisp

(require :asdf)
(unless (find-package :quicklisp)
  (let ((setup (find-if #'probe-file
                        (remove nil (list (let ((e (sb-ext:posix-getenv "QUICKLISP_SETUP")))
                                            (and e (pathname e)))
                                          #p"/opt/quicklisp/setup.lisp"
                                          (merge-pathnames "quicklisp/setup.lisp"
                                                           (user-homedir-pathname)))))))
    (unless setup
      (format *error-output* "~&chrome-scale-gate: no Quicklisp — McCLIM comes from there.~%")
      (sb-ext:exit :code 1))
    (load setup)))
(let ((here (or *load-truename* *default-pathname-defaults*)))
  (asdf:initialize-source-registry
   `(:source-registry (:tree ,(merge-pathnames "../../" here))
                      (:exclude "vendor") (:exclude "deps") :inherit-configuration))
  (handler-bind ((warning #'muffle-warning))
    (let ((*standard-output* (make-broadcast-stream)))
      (ignore-errors (asdf:load-system :pigment))
      (asdf:load-asd (merge-pathnames "../backend/mcclim-glass.asd" here)))))

(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream))) (asdf:load-system :mcclim-glass)))
(defvar *pass* 0) (defvar *fail* 0)
(defun ok (n g &optional d)
  (if g (progn (incf *pass*) (format t "  [pass] ~a~@[ — ~a~]~%" n d))
      (progn (incf *fail*) (format t "  [FAIL] ~a~@[ — ~a~]~%" n d))))

(format t "~&== the metric follows the bound scale ==~%")
(ok "unbound is 1x" (= 22 (clim-glass:wm-titleh)) (format nil "~a" (clim-glass:wm-titleh)))
(let ((clim-glass:*wm-scale* 2))
  (ok "2x doubles the title bar" (= 44 (clim-glass:wm-titleh)))
  (ok "...and the border" (= 2 (clim-glass:wm-border))))
(let ((clim-glass:*wm-scale* 3/2))
  (ok "3/2 rounds once" (= 33 (clim-glass:wm-titleh)) (format nil "~a" (clim-glass:wm-titleh))))

(format t "~&== drawing and hit-testing agree — the thing that breaks dragging ==~%")
(let ((p (clim-glass:make-wm-session :width 1200 :height 800)))
  (clim-glass:start-wm-session p '())
  (sb-thread:make-thread (lambda () (clim-glass:run-wm-loop p)) :name "c")
  (sleep 1.5)
  (let ((seat (clim-glass:port-seat p)))
    (dolist (scale '(1 2 3/2))
      (setf (clim-glass:seat-scale seat) scale)
      (clim-glass::resize-seat-screen seat (* 1200 (if (integerp scale) scale 1))
                                           (* 800 (if (integerp scale) scale 1)))
      (sleep 0.4)
      (let ((surf (clim-glass::wm-add-terminal p :cols 40 :rows 10)))
        (sleep 0.8)
        ;; where the compositor DREW this window's title bar, at this seat's density
        (let* ((box (clim-glass::with-seat-scale (seat) (clim-glass::wm-window-box surf seat)))
               (bx (first box)) (by (second box))
               ;; a point solidly inside the drawn title bar
               (tx (+ bx 60)) (ty (+ by (floor (clim-glass::with-seat-scale (seat)
                                                 (clim-glass:wm-titleh)) 2))))
          (multiple-value-bind (what obj) (clim-glass::wm-hit p tx ty seat)
            (declare (ignore obj))
            (ok (format nil "at ~a x, a click on the drawn title bar hits chrome" scale)
                (and what (not (eq what :desktop)))
                (format nil "box ~a, probe (~a,~a) -> ~s" box tx ty what))))
        (ignore-errors (clim-glass::wm-close-window p surf seat))
        (sleep 0.3)))))
(format t "~&~d passed, ~d failed~%=> ~:[FAIL~;PASS~]~%" *pass* *fail* (zerop *fail*))
(finish-output)
(sb-ext:exit :code (if (plusp *fail*) 1 0))
