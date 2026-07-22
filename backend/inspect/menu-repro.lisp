;;;; menu-repro.lisp — reproduce + screenshot McCLIM menu rendering over the glass
;;;; backend so the exact behaviour can be evaluated from PNGs.  IN-PROCESS (no VNC
;;;; socket — the shared host is too loaded for socket round-trips): pointer events
;;;; are injected straight through the backend's RFB entry point
;;;; (clim-glass::glass-on-pointer, which just ENQUEUEs CLIM events exactly like a
;;;; real client), and the framebuffer is read directly (glass:fb-pixels).
;;;;
;;;; Runs clim-demo::gadget-test — menu bar: Lisp / Edit / View / Search; the Lisp
;;;; menu has a nested "Heir" submenu.  Dumps /tmp/menu-*.png for each state.
;;;;   sbcl --dynamic-space-size 4096 --disable-debugger --load backend/inspect/menu-repro.lisp
(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:mcclim :mcclim-render :glass :zpng :clim-examples))
    (require :sb-concurrency)
    (asdf:load-asd "/home/claude/glass/backend/mcclim-glass.asd")
    (asdf:load-system :mcclim-glass)))

(defpackage #:glass-menu-repro (:use #:cl))
(in-package #:glass-menu-repro)

(defun save-fb (port path)
  "Dump the port's framebuffer to PATH (locked, so no tearing vs the compositor)."
  (let ((fb (clim-glass::glass-port-fb port)))
    (glass:with-fb-locked (fb)
      (let* ((w (glass:fb-width fb)) (h (glass:fb-height fb)) (px (glass:fb-pixels fb))
             (png (make-instance 'zpng:png :width w :height h :color-type :truecolor))
             (d (zpng:data-array png)))
        (dotimes (y h)
          (dotimes (x w)
            (let ((p (aref px (+ (* y w) x))))
              (setf (aref d y x 0) (ldb (byte 8 16) p)
                    (aref d y x 1) (ldb (byte 8 8) p)
                    (aref d y x 2) (ldb (byte 8 0) p)))))
        (zpng:write-png png path))))
  (format t "~&wrote ~a~%" path) (finish-output))

;; inject a pointer state (mask = button bits) at screen (x,y) — same path as RFB
(defun ptr (port mask x y) (clim-glass::glass-on-pointer port mask x y))
(defun move (port x y) (ptr port 0 x y))
(defun click (port x y) (ptr port 0 x y) (ptr port 1 x y) (ptr port 0 x y))  ; opens a sticky menu

(let ((port-num 5943) (w 600) (h 420))
  (sb-thread:make-thread
   (lambda () (handler-case (clim-glass:run-frame 'clim-demo::gadget-test :port port-num :width w :height h)
                (error (e) (format t "~&FRAME ERROR ~a~%" e) (finish-output))))
   :name "gadget-test-frame")
  ;; wait for the port + its framebuffer to come up
  (let ((port nil))
    (loop repeat 300
          until (and (setf port (clim-glass::find-glass-port :port port-num))
                     (clim-glass::glass-port-fb port))
          do (sleep 0.1))
    (unless (and port (clim-glass::glass-port-fb port)) (format t "~&NO FB~%") (finish-output) (sb-ext:exit :code 1))
    (sleep 2.0)                                   ; let the frame paint
    (flet ((snap (name) (save-fb port (format nil "/tmp/menu-~a.png" name)))
           (dismiss () (click port 420 8) (sleep 0.5)))   ; empty menu-bar area -> cancels tracking
      (snap "00-initial")
      ;; --- Lisp menu (leftmost); then hover "Heir" to expand its nested submenu ---
      (click port 20 8) (sleep 0.8) (snap "01-lisp-open")
      (move port 40 40)  (sleep 0.8) (snap "02-lisp-heir-submenu")
      (dismiss)          (sleep 0.3) (snap "03-after-dismiss")
      ;; --- each remaining menu bar item (x-positioning under the clicked item) ---
      (click port 62 8)  (sleep 0.8) (snap "04-edit-open")   (dismiss)
      (click port 105 8) (sleep 0.8) (snap "05-view-open")   (dismiss)
      (click port 160 8) (sleep 0.8) (snap "06-search-open") (dismiss)
      (format t "~&DONE~%") (finish-output))))
(sb-ext:exit)
