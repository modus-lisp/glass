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

(format t "~&== pop-up menus scale, and a click finds the row it highlighted ==~%")
;; The menu is the case that showed why binding at CALL SITES is not enough.  It rendered
;; through paths that bound the density and was INDEXED through one that did not, so at 2x
;; the middle of row 1 came back as row 4: the highlight follows the pointer and the click
;; opens something else.  A menu now carries the scale its framebuffer was drawn at, so the
;; pixels and the arithmetic that reads them cannot come apart.
(defvar *pass* 0) (defvar *fail* 0)
(defun ok (n g &optional d)
  (if g (progn (incf *pass*) (format t "  [pass] ~a~@[ — ~a~]~%" n d))
      (progn (incf *fail*) (format t "  [FAIL] ~a~@[ — ~a~]~%" n d))))
(let ((p (clim-glass:make-wm-session :width 1400 :height 900)))
  (clim-glass:start-wm-session p '())
  (sb-thread:make-thread (lambda () (clim-glass:run-wm-loop p)) :name "m")
  (sleep 1.5)
  (let ((seat (clim-glass:port-seat p)) (sizes '()))
    (dolist (sc '(1 3/2 2))
      (setf (clim-glass:seat-scale seat) sc)
      ;; open the root menu the way a click does
      (clim-glass::wm-open-menu p 200 200 seat)
      (sleep 0.5)
      (let* ((menu (clim-glass::seat-menu seat))
             (fb (and menu (clim-glass::wm-menu-fb menu))))
        (if (null fb)
            (ok (format nil "menu opens at ~a x" sc) nil "no menu framebuffer")
            (let ((w (glass:fb-width fb)) (h (glass:fb-height fb)))
              (push (list sc w h) sizes)
              (ok (format nil "menu opens at ~a x" sc) t (format nil "~ax~a" w h))
              ;; hit-testing must find the row the render drew
              (let* ((ih (clim-glass::with-seat-scale (seat) (clim-glass::menu-itemh)))
                     (th (clim-glass::with-seat-scale (seat) (clim-glass::menu-titleh)))
                     (mx (clim-glass::wm-menu-x menu)) (my (clim-glass::wm-menu-y menu))
                     ;; the middle of item 1
                     (probe-y (+ my th ih (floor ih 2)))
                     (idx (clim-glass::wm-menu-index menu (+ mx 20) probe-y)))
                (ok (format nil "...and row 1 hit-tests as row 1 at ~a x" sc)
                    (eql idx 1) (format nil "index ~s" idx))))))
      (setf (clim-glass::seat-menu seat) nil)
      (sleep 0.3))
    (let ((s1 (assoc 1 sizes)) (s2 (assoc 2 sizes)))
      (ok "a 2x menu is about twice a 1x menu"
          (and s1 s2 (< 1.7 (/ (third s2) (float (third s1))) 2.3))
          (format nil "1x ~ax~a vs 2x ~ax~a" (second s1) (third s1) (second s2) (third s2))))))


(format t "~&== the window-menu wedge is clickable where it is drawn ==~%")
;; The bug this is for, reported as \"the click region is the top left corner of where it
;; should be\": the button was DRAWN inset by 4*scale and sized from the bar, and HIT-TESTED
;; at a fixed 4..18.  Those agree at 1x by construction and nowhere else, so at 2x the live
;; quarter of the button was its top-left corner — which feels like a bad mouse rather than
;; a bug, and is why it survived a screenshot.  One function now answers both, and the test
;; probes the CENTRE of what the renderer drew, plus a point just past its edge so that
;; passing cannot mean \"the region got bigger than the button\".
(let ((p (clim-glass:make-wm-session :width 1400 :height 900)))
  (clim-glass:start-wm-session p '())
  (sb-thread:make-thread (lambda () (clim-glass:run-wm-loop p)) :name "w")
  (sleep 1.5)
  (let ((seat (clim-glass:port-seat p)))
    (dolist (sc '(1 3/2 2))
      (setf (clim-glass:seat-scale seat) sc)
      (let ((surf (clim-glass::wm-add-terminal p :cols 40 :rows 10)))
        (sleep 0.8)
        (clim-glass::with-seat-scale (seat)
          (multiple-value-bind (bx by bs) (clim-glass::wm-menu-button-box)
            (let* ((cx (clim-glass::seat-window-x seat surf))
                   (cy (clim-glass::seat-window-y seat surf))
                   (ty (- cy (clim-glass:wm-titleh)))
                   ;; the CENTRE of the button as the renderer draws it
                   (px (+ cx bx (floor bs 2)))
                   (py (+ ty by (floor bs 2))))
              (multiple-value-bind (obj what) (clim-glass::wm-hit p px py seat)
                (declare (ignore obj))
                (ok (format nil "at ~a x, the centre of the drawn wedge is :winmenu" sc)
                    (eq what :winmenu)
                    (format nil "button ~a,~a size ~a -> ~s" bx by bs what)))
              ;; and a point just outside it must NOT be
              (multiple-value-bind (obj what) (clim-glass::wm-hit p (+ cx bx bs 3) py seat)
                (declare (ignore obj))
                (ok (format nil "...and just past its right edge is not" sc)
                    (not (eq what :winmenu)) (format nil "~s" what))))))
        (ignore-errors (clim-glass::wm-close-window p surf seat))
        (sleep 0.3)))))

(format t "~&~d passed, ~d failed~%=> ~:[FAIL~;PASS~]~%" *pass* *fail* (zerop *fail*))
(finish-output)
(sb-ext:exit :code (if (plusp *fail*) 1 0))
