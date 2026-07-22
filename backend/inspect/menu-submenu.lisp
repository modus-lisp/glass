;;;; menu-submenu.lisp — verify a nested submenu (the "Heir >" fly-out) opens on
;;;; HOVER over its parent item.  IN-PROCESS (reads fb, injects pointer events).
;;;;   sbcl --dynamic-space-size 4096 --disable-debugger --load backend/inspect/menu-submenu.lisp
(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:mcclim :mcclim-render :glass :zpng :clim-examples))
    (require :sb-concurrency)
    (asdf:load-asd "/home/claude/glass/backend/mcclim-glass.asd")
    (asdf:load-system :mcclim-glass)))
(defpackage #:glass-menu-submenu (:use #:cl)) (in-package #:glass-menu-submenu)
(defun save-fb (port path)
  (let ((fb (clim-glass::glass-port-fb port)))
    (glass:with-fb-locked (fb)
      (let* ((w (glass:fb-width fb)) (h (glass:fb-height fb)) (px (glass:fb-pixels fb))
             (png (make-instance 'zpng:png :width w :height h :color-type :truecolor)) (d (zpng:data-array png)))
        (dotimes (y h) (dotimes (x w)
          (let ((p (aref px (+ (* y w) x))))
            (setf (aref d y x 0) (ldb (byte 8 16) p) (aref d y x 1) (ldb (byte 8 8) p) (aref d y x 2) (ldb (byte 8 0) p)))))
        (zpng:write-png png path)))) (format t "~&wrote ~a~%" path) (finish-output))
(defun ptr (port mask x y) (clim-glass::glass-on-pointer port mask x y))
(defun move (port x y) (ptr port 0 x y))
(defun click (port x y) (ptr port 0 x y) (ptr port 1 x y) (ptr port 0 x y))

(let ((port-num 5947) (w 600) (h 420))
  (sb-thread:make-thread
   (lambda () (handler-case (clim-glass:run-frame 'clim-demo::gadget-test :port port-num :width w :height h)
                (error (e) (format t "~&FRAME ERROR ~a~%" e))))
   :name "f")
  (let ((port nil))
    (loop repeat 300 until (and (setf port (clim-glass::find-glass-port :port port-num))
                                (clim-glass::glass-port-fb port)) do (sleep 0.1))
    (sleep 2.0)
    (click port 20 8) (sleep 0.8) (save-fb port "/tmp/submenu-0-lisp-open.png")
    ;; walk the pointer down into the "Heir" item (a few motion events so the tracker
    ;; sees the move and arms the submenu button); a final jiggle triggers a frame
    ;; flush (as continuous mouse movement would), NO explicit composite-all.
    (move port 30 30) (sleep 0.15) (move port 38 36) (sleep 0.15)
    (move port 42 38) (sleep 0.3) (move port 44 39) (sleep 0.5)
    (move port 45 40) (sleep 0.2) (move port 44 39) (sleep 0.8)
    (save-fb port "/tmp/submenu-1-heir-hover.png")
    (labels ((find-submenu-btn (s)
               (if (typep s 'climi::menu-button-submenu-pane) s
                   (some #'find-submenu-btn (ignore-errors (clim:sheet-children s))))))
      (let ((btn (some (lambda (m) (and (typep m 'clim-glass::glass-mirror)
                                        (find-submenu-btn (clim-glass::glass-mirror-sheet m))))
                       (clim-glass::glass-port-mirrors port))))
        (format t "~&hovered Heir; grab-sheet=~a  mirrors=~d~%"
                (type-of (clim-glass::glass-port-grab-sheet port))
                (length (clim-glass::glass-port-mirrors port)))
        (when btn
          (format t "  Heir button: armed=~a active=~a~%"
                  (ignore-errors (climi::gadget-armed-p btn)) (ignore-errors (climi::gadget-active-p btn))))
        (dolist (m (clim-glass::glass-port-mirrors port))
          (when (typep m 'clim-glass::glass-mirror)
            (let ((img (ignore-errors (mcclim-render::image-mirror-image m))))
              (format t "  mirror ~a @(~a,~a) size=~a~%"
                      (type-of (clim-glass::glass-mirror-sheet m))
                      (clim-glass::glass-mirror-x m) (clim-glass::glass-mirror-y m)
                      (and img (ignore-errors (multiple-value-list (clim-glass::image-wh img))))))))
        (finish-output)))
    (format t "DONE~%") (finish-output)))
(sb-ext:exit)
