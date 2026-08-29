;;;; density-gate.lisp — a seat has a density, and a pixel has a colour space.
;;;;
;;;; Two early steps toward HiDPI, gated here because both are the kind of change whose
;;;; whole value is that it does NOT alter behaviour today.  A scale that quietly moved
;;;; something at 1x would be worse than not having one.
;;;;
;;;; What is asserted, and why each:
;;;;
;;;;   the colour space is STATED.  sRGB was true and unwritten; a space you have not
;;;;   named cannot be converted from, which is the whole reason HDR is not merely
;;;;   unimplemented but undescribable.  See docs/density-and-colour.md.
;;;;
;;;;   scale 1 is the IDENTITY, everywhere.  This is the important one: every existing
;;;;   desktop keeps the pixels it had.
;;;;
;;;;   a fractional scale survives.  3/2 is a scale real displays use, and the slot is
;;;;   rational so that rounding happens once, at the point of use, rather than at the
;;;;   door where it could not be undone.
;;;;
;;;;   the SCREEN is untouched.  SEAT-SCREEN-W stays device pixels.  A framebuffer is a
;;;;   rectangle of real pixels and does not acquire a second kind; the scale says only
;;;;   what a LAYOUT number is multiplied by before it becomes one.
;;;;
;;;;   sbcl --script inspect/density-gate.lisp

(require :asdf)
(unless (find-package :quicklisp)
  (let ((setup (find-if #'probe-file
                        (remove nil (list (let ((e (sb-ext:posix-getenv "QUICKLISP_SETUP")))
                                            (and e (pathname e)))
                                          #p"/opt/quicklisp/setup.lisp"
                                          (merge-pathnames "quicklisp/setup.lisp"
                                                           (user-homedir-pathname)))))))
    (unless setup
      (format *error-output* "~&density-gate: no Quicklisp — McCLIM comes from there.~%")
      (sb-ext:exit :code 1))
    (load setup)))
(let ((here (or *load-truename* *default-pathname-defaults*)))
  (asdf:initialize-source-registry
   `(:source-registry (:tree ,(merge-pathnames "../../" here))
                      (:exclude "vendor") (:exclude "deps") :inherit-configuration))
  (handler-bind ((warning #'muffle-warning))
    (let ((*standard-output* (make-broadcast-stream)))
      (asdf:load-asd (merge-pathnames "../backend/mcclim-glass.asd" here)))))

(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream))) (asdf:load-system :mcclim-glass)))
(defvar *pass* 0) (defvar *fail* 0)
(defun ok (name got &optional d)
  (if got (progn (incf *pass*) (format t "  [pass] ~a~@[ — ~a~]~%" name d))
      (progn (incf *fail*) (format t "  [FAIL] ~a~@[ — ~a~]~%" name d))))

(format t "~&== the colour space is stated ==~%")
(ok "glass:+pixel-colour-space+ is :SRGB" (eq glass:+pixel-colour-space+ :srgb)
    (format nil "~s" glass:+pixel-colour-space+))
(ok "rgb still packs X8R8G8B8" (= (glass:rgb 61 122 138) #x3D7A8A)
    (format nil "#x~6,'0X" (glass:rgb 61 122 138)))

(format t "~&== a seat has a density, and 1 changes nothing ==~%")
(let ((p (clim-glass:make-wm-session :width 800 :height 600)))
  (clim-glass:start-wm-session p '())
  (sb-thread:make-thread (lambda () (clim-glass:run-wm-loop p)) :name "d")
  (sleep 1)
  (let ((s (clim-glass:port-seat p)))
    (ok "a seat defaults to scale 1" (eql 1 (clim-glass:seat-scale s)))
    (ok "ppem at 1x is the identity" (= 14 (clim-glass:seat-ppem s 14))
        (format nil "14 -> ~a" (clim-glass:seat-ppem s 14)))
    (ok "a metric at 1x is the identity" (= 24 (clim-glass:seat-metric s 24)))
    (setf (clim-glass:seat-scale s) 2)
    (ok "at 2x, ppem doubles" (= 28 (clim-glass:seat-ppem s 14))
        (format nil "14 -> ~a" (clim-glass:seat-ppem s 14)))
    (ok "at 2x, a metric doubles" (= 48 (clim-glass:seat-metric s 24)))
    (setf (clim-glass:seat-scale s) 3/2)
    (ok "a fractional scale survives" (= 21 (clim-glass:seat-ppem s 14))
        (format nil "3/2 of 14 -> ~a" (clim-glass:seat-ppem s 14)))
    (ok "...and rounds rather than truncating" (= 24 (clim-glass:seat-metric s 16))
        (format nil "3/2 of 16 -> ~a" (clim-glass:seat-metric s 16)))
    (setf (clim-glass:seat-scale s) 1)
    (ok "NIL seat is answered, not refused" (= 14 (clim-glass:seat-ppem nil 14)))
    (ok "the SCREEN is untouched by scale — device pixels stay device pixels"
        (and (= 800 (clim-glass::seat-screen-w s)) (= 600 (clim-glass::seat-screen-h s)))
        (format nil "~ax~a" (clim-glass::seat-screen-w s) (clim-glass::seat-screen-h s)))))
(format t "~&~d passed, ~d failed~%=> ~:[FAIL~;PASS~]~%" *pass* *fail* (zerop *fail*))
(finish-output)
(sb-ext:exit :code (if (plusp *fail*) 1 0))
