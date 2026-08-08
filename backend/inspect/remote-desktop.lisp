;;;; remote-desktop.lisp — a glass desktop whose root menu can open ANOTHER glass
;;;; desktop as a window, and a control socket to drive it from outside.
;;;;
;;;; The rig behind inspect/nested-copyrect.lisp and the one to point a VNC client
;;;; at when looking at a remote desktop by hand.  It is an ordinary desktop —
;;;; wallpaper, root menu, terminal — plus mcclim-glass/remote, so "Remote desktop"
;;;; is in the menu and OPEN-REMOTE opens one without a pointer.
;;;;
;;;;   sbcl --control-stack-size 256 --dynamic-space-size 4096 \
;;;;        --load backend/inspect/remote-desktop.lisp -- 5921 5931 4021
;;;;        (outer VNC port, inner desktop's VNC port, control socket)

(require :asdf)
(load "~/quicklisp/setup.lisp")

(defparameter *outer* 5921)
(defparameter *inner* 5901)
(defparameter *control* 4021)

(let ((tail (cdr (member "--" sb-ext:*posix-argv* :test #'string=))))
  (when (first tail)  (setf *outer*   (parse-integer (first tail))))
  (when (second tail) (setf *inner*   (parse-integer (second tail))))
  (when (third tail)  (setf *control* (parse-integer (third tail)))))

(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (asdf:load-asd "/home/claude/glass/glass.asd")
    (ql:quickload '(:glass :glass/client :mcclim :mcclim-render :sb-concurrency :pigment))
    (asdf:load-asd "/home/claude/glass/backend/mcclim-glass.asd")
    (asdf:load-system :mcclim-glass)
    (asdf:load-system :mcclim-glass/remote)))

(in-package :clim-glass)

(setf glass:*desktop-name*
      (format nil "modus-lisp :: glass desktop (:~d)" (- (symbol-value 'cl-user::*outer*) 5900)))
(setf glass-remote:*remote-port* (symbol-value 'cl-user::*inner*))

(glass-remote:register)

(defun open-remote (&key (port (symbol-value 'cl-user::*outer*)))
  "Open the remote-desktop window without touching the menu."
  (let ((p (find-glass-port :port port)))
    (prog1 (wm-spawn-spec p (cdr (assoc "Remote desktop" (wm-default-menu) :test #'equal)))
      (composite-all p))))

(defun start-control-socket (port)
  (sb-thread:make-thread
   (lambda ()
     (let ((listen (glass:tcp-listen port :address "127.0.0.1")))
       (loop
         (handler-case
             (let ((s (sb-bsd-sockets:socket-make-stream
                       (sb-bsd-sockets:socket-accept listen)
                       :input t :output t :element-type 'character :buffering :full)))
               (unwind-protect
                    (let* ((*package* (find-package :clim-glass))
                           (form (read s nil nil)))
                      (when form
                        (write-string (handler-case (princ-to-string (eval form))
                                        (error (e) (format nil "ERROR: ~a" e)))
                                      s)
                        (terpri s) (force-output s)))
                 (ignore-errors (close s))))
           (error () nil)))))
   :name (format nil "glass-control-~d" port)))

(start-control-socket (symbol-value 'cl-user::*control*))

(let ((wp (namestring (merge-pathnames "assets/wallpaper.svg"
                                       (asdf:system-source-directory :mcclim-glass)))))
  (format *error-output* "~&@@ outer desktop on :~d (remote = :~d), control ~d~%"
          (symbol-value 'cl-user::*outer*) (symbol-value 'cl-user::*inner*)
          (symbol-value 'cl-user::*control*))
  (finish-output *error-output*)
  (run-wm '((:terminal :cols 60 :rows 16 :ppem 13))
          :port (symbol-value 'cl-user::*outer*) :width 1600 :height 1000
          :background wp :background-mode :cover))
