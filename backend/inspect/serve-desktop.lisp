;;;; serve-desktop.lisp — a PERSISTENT OPEN LOOK glass desktop over VNC (blocks).
;;;; SVG wallpaper + the full Apps menu (Calculator, Browser, Inspector, Debugger,
;;;; Image Viewer, Listener, ...).  Point any VNC client at <host>:5901.
;;;;   sbcl --control-stack-size 256 --dynamic-space-size 4096 --load backend/inspect/serve-desktop.lisp
(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:glass :mcclim :mcclim-render :sb-concurrency
                    :weft/render :clim-examples :clim-listener))
    (ignore-errors (asdf:load-system :loom/glass))            ; the browser (optional)
    (asdf:load-asd "/home/claude/glass/backend/mcclim-glass.asd")
    (asdf:load-system :mcclim-glass)))

(setf glass:*desktop-name* "modus-lisp :: glass desktop")

;;; Bare-TCP control/eval socket on 127.0.0.1:4009 — read one form, eval it in the
;;; clim-glass package, write the printed result.  Lets us read live perf and poke
;;; at the RUNNING desktop with no restart:
;;;   echo '(glass:perf-reset)'  | nc -q1 127.0.0.1 4009
;;;   echo '(glass:perf-report)' | nc -q1 127.0.0.1 4009
(defun start-control-socket (&optional (port 4009))
  (sb-thread:make-thread
   (lambda ()
     (let ((listen (glass:tcp-listen port :address "127.0.0.1")))
       (loop
         (handler-case
             (let ((s (sb-bsd-sockets:socket-make-stream
                       (sb-bsd-sockets:socket-accept listen)
                       :input t :output t :element-type 'character :buffering :full)))
               (unwind-protect
                    (let ((*package* (find-package :clim-glass))
                          (form (read s nil nil)))
                      (when form
                        (write-string
                         (handler-case (princ-to-string (eval form))
                           (error (e) (format nil "ERROR: ~a" e)))
                         s)
                        (terpri s) (force-output s)))
                 (ignore-errors (close s))))
           (error () nil)))))
   :name "glass-control"))

;; VNC password: if ~/.glass-vnc-pass exists (create it yourself, mode 600 — it is
;; NOT in the repo), require + verify it (secures the 0.0.0.0 bind, and macOS saves
;; it to Keychain so it stops prompting).  Absent -> the open any-password posture.
(let ((pwfile (merge-pathnames ".glass-vnc-pass" (user-homedir-pathname))))
  (when (probe-file pwfile)
    (let ((pw (string-trim '(#\Space #\Tab #\Newline #\Return)
                           (with-open-file (in pwfile) (or (read-line in nil "") "")))))
      (when (plusp (length pw)) (setf glass:*vnc-password* pw)))))

(let ((wp (namestring (merge-pathnames "assets/wallpaper.svg"
                                       (asdf:system-source-directory :mcclim-glass)))))
  (start-control-socket 4009)
  (format *error-output* "~&@@ glass desktop serving on 0.0.0.0:5901 (~a)~%" wp)
  (format *error-output* "@@ control socket on 127.0.0.1:4009~%")
  (format *error-output* "@@ VNC auth: ~:[OPEN — any password accepted~;REQUIRED — ~:*~d-char password loaded~]~%"
          (and glass:*vnc-password* (length glass:*vnc-password*)))
  (finish-output *error-output*)
  (clim-glass:run-wm '((:terminal :cols 80 :rows 24 :ppem 14))
                     :port 5901 :width 1280 :height 800
                     :background wp :background-mode :cover))
