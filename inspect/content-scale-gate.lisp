;;;; inspect/content-scale-gate.lisp — apps that do not know about density are magnified.
;;;;
;;;; THE PROBLEM: on a 2x seat the browser and the file manager came out tiny.  Nothing was
;;;; wrong in either of them.  An application that draws at a fixed number of pixels per
;;;; character is drawing for ONE density, and on a screen with twice as many pixels its
;;;; output is half the size — and there is nothing it can do about that without being told,
;;;; which nearly nothing is.
;;;;
;;;; So the WM magnifies what such an app drew.  That is what every desktop does for a program
;;;; that does not ask, and it is the only thing that CAN be done without the program's
;;;; cooperation.  The default is therefore to assume a program is unaware, because nearly all
;;;; of them are; a caller that really does draw in device pixels says so and is left alone.
;;;;
;;;; A terminal is that caller, and is in this gate for the contrast: it sizes its glyphs by
;;;; SEAT-PPEM, so it is already sharp at any density and magnifying it would blur text that
;;;; was right.  The distinction is exactly "does this thing take a size in pixels and mean
;;;; it".
;;;;
;;;; NEAREST NEIGHBOUR on purpose: at an integer scale every source pixel becomes a block and
;;;; nothing is invented, which is what text drawn as pixels wants.  Interpolation would blur
;;;; glyph edges that were already sharp.
;;;;
;;;;   sbcl --script inspect/content-scale-gate.lisp

(require :asdf)
(unless (find-package :quicklisp)
  (let ((setup (find-if #'probe-file
                        (remove nil (list (let ((e (sb-ext:posix-getenv "QUICKLISP_SETUP")))
                                            (and e (pathname e)))
                                          #p"/opt/quicklisp/setup.lisp"
                                          (merge-pathnames "quicklisp/setup.lisp"
                                                           (user-homedir-pathname)))))))
    (unless setup
      (format *error-output* "~&content-scale-gate: no Quicklisp.~%") (sb-ext:exit :code 1))
    (load setup)))
(let* ((here (or *load-truename* *default-pathname-defaults*))
       (root (truename (make-pathname :name nil :type nil
                                      :defaults (merge-pathnames "../../" here)))))
  (asdf:initialize-source-registry
   `(:source-registry (:tree ,root) (:exclude "vendor") (:exclude "deps") :inherit-configuration))
  (handler-bind ((warning #'muffle-warning))
    (let ((*standard-output* (make-broadcast-stream)))
      (ignore-errors (asdf:load-system :pigment))
      (asdf:load-asd (merge-pathnames "../backend/mcclim-glass.asd" here))
      (asdf:load-system :mcclim-glass))))

(defvar *fail* 0)
(defun ok (n g &optional d)
  (if g (format t "  [pass] ~a~@[ — ~a~]~%" n d)
      (progn (incf *fail*) (format t "  [FAIL] ~a~@[ — ~a~]~%" n d))))
;; the magnifier itself, before any desktop
(let ((src (glass:make-framebuffer 2 2 0)))
  (glass:fb-put src 0 0 #xFF0000) (glass:fb-put src 1 0 #x00FF00)
  (glass:fb-put src 0 1 #x0000FF) (glass:fb-put src 1 1 #xFFFFFF)
  (let ((dst (glass:make-framebuffer 4 4 0)))
    (glass:fb-blit-scaled dst src 0 0 2)
    (ok "an integer scale turns each pixel into a block"
        (and (= #xFF0000 (glass:fb-get dst 0 0)) (= #xFF0000 (glass:fb-get dst 1 1))
             (= #x00FF00 (glass:fb-get dst 2 0)) (= #xFFFFFF (glass:fb-get dst 3 3))))
    (ok "...and every destination pixel is written"
        (notany (lambda (p) (= p 0))
                (loop for y below 4 append (loop for x below 4 collect (glass:fb-get dst x y)))))))
(let ((p (clim-glass:make-wm-session :width 1400 :height 900)))
  (clim-glass:start-wm-session p '())
  (sb-thread:make-thread (lambda () (clim-glass:run-wm-loop p)) :name "m")
  (sleep 1.5)
  (let ((seat (clim-glass:port-seat p)))
    (setf (clim-glass:seat-scale seat) 2)
    ;; a DPI-unaware app: asks for 400x300 of its own pixels
    (let ((surf (clim-glass::wm-spawn-spec
                 p (list :surface (lambda (fb) (declare (ignore fb))
                                    (values (lambda (d k) (declare (ignore d k)))
                                            (lambda (m x y) (declare (ignore m x y)))
                                            (lambda () nil)))
                         :title "unaware" :width 400 :height 300))))
      (declare (ignore surf))
      (sleep 0.6)
      (let ((s (find "unaware" (clim-glass::glass-port-surfaces p)
                     :key #'clim-glass:wm-surface-title :test #'equal)))
        (ok "the app got the framebuffer it asked for"
            (and s (= 400 (glass:fb-width (clim-glass::wm-surface-fb s))))
            (format nil "~ax~a" (glass:fb-width (clim-glass::wm-surface-fb s))
                    (glass:fb-height (clim-glass::wm-surface-fb s))))
        (ok "...and the WM magnifies it to the seat's density"
            (and s (= 2 (clim-glass::wm-surface-content-scale s))))
        ;; WM-BORDER read inside the seat's density, like the box itself was: outside one it
        ;; answers for 1x and the arithmetic silently uses the wrong border.
        (clim-glass::with-seat-scale (seat)
          (let ((box (clim-glass::wm-window-box s seat)))
            (ok "...so the WINDOW is twice the size on screen"
                (and box (= 800 (- (third box) (* 2 (clim-glass:wm-border)))))
                (format nil "content 400 -> window ~a (box ~a)"
                        (- (third box) (* 2 (clim-glass:wm-border))) box))))))
    ;; a terminal sizes itself in device pixels and must be left alone
    (let ((term (clim-glass::wm-add-terminal p :cols 20 :rows 6)))
      (sleep 0.6)
      (ok "a terminal is not magnified" (= 1 (clim-glass::wm-surface-content-scale term))
          "it makes its glyphs at SEAT-PPEM, so it is already sharp"))))
(format t "~&=> ~:[FAIL~;PASS~]~%" (zerop *fail*))

(finish-output)
(sb-ext:exit :code (if (plusp *fail*) 1 0))
