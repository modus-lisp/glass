;;;; wm-menu-repro.lisp — reproduce McCLIM menu rendering in WM (desktop) mode,
;;;; which is what serve-desktop runs and where menus look wrong.  IN-PROCESS: a
;;;; gadget-test app is hosted as a decorated managed window; we read the managed
;;;; mirror's actual screen position, click its menu bar THERE, and screenshot.
;;;; If McCLIM positions the dropdown from its own (WM-overridden) idea of where
;;;; the window is, the menu lands detached from the window.
;;;;   sbcl --dynamic-space-size 4096 --disable-debugger --load backend/inspect/wm-menu-repro.lisp
(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:mcclim :mcclim-render :glass :zpng :clim-examples))
    (require :sb-concurrency)
    (asdf:load-asd "/home/claude/glass/backend/mcclim-glass.asd")
    (asdf:load-system :mcclim-glass)))

(defpackage #:glass-wm-menu-repro (:use #:cl))
(in-package #:glass-wm-menu-repro)

(defun save-fb (port path)
  (let ((fb (clim-glass::glass-port-fb port)))
    (glass:with-fb-locked (fb)
      (let* ((w (glass:fb-width fb)) (h (glass:fb-height fb)) (px (glass:fb-pixels fb))
             (png (make-instance 'zpng:png :width w :height h :color-type :truecolor))
             (d (zpng:data-array png)))
        (dotimes (y h) (dotimes (x w)
          (let ((p (aref px (+ (* y w) x))))
            (setf (aref d y x 0) (ldb (byte 8 16) p) (aref d y x 1) (ldb (byte 8 8) p) (aref d y x 2) (ldb (byte 8 0) p)))))
        (zpng:write-png png path))))
  (format t "~&wrote ~a~%" path) (finish-output))

(defun ptr (port mask x y) (clim-glass::glass-on-pointer port mask x y))
(defun move (port x y) (ptr port 0 x y))
(defun click (port x y) (ptr port 0 x y) (ptr port 1 x y) (ptr port 0 x y))

(defun managed-mirror (port)
  (find-if (lambda (m) (and (typep m 'clim-glass::glass-mirror) (clim-glass::glass-mirror-managed m)))
           (clim-glass::glass-port-mirrors port)))

(let ((port-num 5944) (sw 900) (sh 640))
  (sb-thread:make-thread
   (lambda () (handler-case
                  (clim-glass:run-wm '((clim-demo::gadget-test :width 420 :height 320))
                                     :port port-num :width sw :height sh)
                (error (e) (format t "~&WM ERROR ~a~%" e) (finish-output))))
   :name "wm")
  (let ((port nil) (mir nil))
    (loop repeat 400
          until (and (setf port (clim-glass::find-glass-port :port port-num))
                     (clim-glass::glass-port-fb port)
                     (setf mir (managed-mirror port)))
          do (sleep 0.1))
    (unless mir (format t "~&NO MANAGED WINDOW~%") (finish-output) (sb-ext:exit :code 1))
    (sleep 2.0)
    (clim-glass::composite-all port)
    (let* ((mx (clim-glass::glass-mirror-x mir)) (my (clim-glass::glass-mirror-y mir)))
      (format t "~&managed window CONTENT at screen (~d,~d); its menu bar is the top ~~18px~%" mx my)
      (finish-output)
      (save-fb port "/tmp/wmmenu-00-initial.png")
      ;; click the "Lisp" menu on the app's menu bar, at the window's ACTUAL position
      (click port (+ mx 20) (+ my 8)) (sleep 0.9)
      (save-fb port "/tmp/wmmenu-01-lisp-open.png")
      (format t "~&clicked app menu bar at screen (~d,~d)~%" (+ mx 20) (+ my 8)) (finish-output)
      (click port (+ mx 380) (+ my 300)) (sleep 0.6)   ; dismiss: click window content away from the dropdown
      ;; --- DRAG the window by its title bar +220px right, then re-open the menu ---
      (let ((ty (- my clim-glass::+wm-titleh+)))         ; a point on the title bar
        (ptr port 1 (+ mx 120) (+ ty 6))                ; press on title bar
        (ptr port 1 (+ mx 250) (+ ty 6))               ; drag right
        (ptr port 1 (+ mx 340) (+ ty 6))
        (ptr port 0 (+ mx 340) (+ ty 6))               ; release
        (sleep 1.0))
      (let* ((nx (clim-glass::glass-mirror-x mir)) (ny (clim-glass::glass-mirror-y mir)))
        (format t "~&after drag, window CONTENT at (~d,~d)~%" nx ny) (finish-output)
        (save-fb port "/tmp/wmmenu-02-after-drag.png")
        (click port (+ nx 20) (+ ny 8)) (sleep 0.9)     ; re-open Lisp menu at the NEW spot
        (save-fb port "/tmp/wmmenu-03-lisp-after-drag.png"))
      (format t "DONE~%") (finish-output))))
(sb-ext:exit)
