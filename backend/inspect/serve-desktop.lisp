;;;; serve-desktop.lisp — a PERSISTENT OPEN LOOK glass desktop over VNC (blocks).
;;;; SVG wallpaper + the full Apps menu (Calculator, Browser, Inspector, Debugger,
;;;; Image Viewer, Listener, ...).  Point any VNC client at <host>:5901.
;;;;   sbcl --control-stack-size 256 --dynamic-space-size 4096 --load backend/inspect/serve-desktop.lisp
;;;;
;;;; GLASS_DISPLAY picks the display number (default 1), X-style: every port this desktop owns
;;;; is derived from it, so a second desktop is one environment variable and not a fork of this
;;;; file.  Display N: VNC on 5900+N, the session's audio on 5910+N, the control socket on
;;;; 4008+N — which leaves display 1 on exactly the ports it has always used.
(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:glass :glass/vncauth :mcclim :mcclim-render :sb-concurrency
                    :pigment :clim-examples :clim-listener))   ; vncauth = DES via seal
    (ignore-errors (asdf:load-system :loom/glass))            ; the browser (optional)
    (ignore-errors (asdf:load-system :warren))                ; the file browser (optional)
    (ignore-errors (asdf:load-system :glass/audio-stream))    ; the session's sound (optional)
    (ignore-errors (asdf:load-system :glass/speech))          ; and its voice, via quill (optional)
    (asdf:load-asd "/home/claude/glass/backend/mcclim-glass.asd")
    (asdf:load-system :mcclim-glass)
    (ignore-errors (asdf:load-system :mcclim-glass/speak))))  ; type-and-say window (optional)

(defparameter *display*
  (or (ignore-errors (parse-integer (or (sb-ext:posix-getenv "GLASS_DISPLAY") "1"))) 1))
(defparameter *vnc-port* (+ 5900 *display*))
(defparameter *audio-port* (+ 5910 *display*))
(defparameter *control-port* (+ 4008 *display*))

(setf glass:*desktop-name* (format nil "modus-lisp :: glass desktop :~d" *display*))

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
  (start-control-socket *control-port*)
  ;; The session's sound, on its own port beside the screen (see src/audio-stream.lisp): one
  ;; mixer in the process the applications run in, and any number of listeners in OTHER
  ;; processes subscribing to the same mix.  Found by name, so a build without
  ;; :glass/audio-stream still starts a desktop — silence is a working desktop, no desktop is not.
  (let ((start (find-symbol "START-SESSION-AUDIO" :glass)))
    (when (and start (fboundp start)) (funcall start :port *audio-port* :address "127.0.0.1")))
  ;; The voice (see src/speech.lisp), if :glass/speech loaded and a voice is actually on this
  ;; box.  GLASS_VOICE still wins; this only fills in the one that lives here, and leaves the
  ;; variable alone — so SPEAK's complaint stays accurate — when the file is missing.
  (let ((var (find-symbol "*SPEECH-VOICE*" :glass))
        (here "/mnt/lisp/quill/export/en_US-lessac-medium.graph"))
    (when (and var (boundp var) (null (symbol-value var)) (probe-file here))
      (setf (symbol-value var) here)))
  (format *error-output* "~&@@ glass desktop :~d serving on 0.0.0.0:~d (~a)~%" *display* *vnc-port* wp)
  (format *error-output* "@@ control socket on 127.0.0.1:~d, session audio on 127.0.0.1:~d~%"
          *control-port* *audio-port*)
  (format *error-output* "@@ voice: ~:[none — set GLASS_VOICE~;~:*~a~]~%"
          (let ((var (find-symbol "*SPEECH-VOICE*" :glass)))
            (and var (boundp var) (symbol-value var))))
  (format *error-output* "@@ VNC auth: ~:[OPEN — any password accepted~;REQUIRED — ~:*~d-char password loaded~]~%"
          (and glass:*vnc-password* (length glass:*vnc-password*)))
  (finish-output *error-output*)
  ;; If warren (the pure-CL file browser) loaded, register it in the root menu as a
  ;; generic :surface app — found by name so this is a no-op when warren is absent
  ;; (mirrors the loom/glass optional dependency; read-time-safe: no warren symbol
  ;; is referenced literally, only resolved at run time).
  (when (find-package :warren)
    (ignore-errors
     (clim-glass:register-app "File Browser"
       (list :surface (symbol-function (find-symbol "DESKTOP-SURFACE" :warren))
             :title "Files" :width 1000 :height 640))))
  ;; And the window that types into the desktop's voice, which registers itself
  ;; (GLASS-SPEAK:REGISTER looks CLIM-GLASS up by name, so the system also loads in
  ;; an image with no desktop).  Absent when glass/speech didn't load — the desktop
  ;; is still a desktop, just a mute one.
  (when (find-package :glass-speak)
    (ignore-errors (funcall (find-symbol "REGISTER" :glass-speak))))
  (clim-glass:run-wm '((:terminal :cols 80 :rows 24 :ppem 14))
                     :port *vnc-port* :width 1280 :height 800
                     :background wp :background-mode :cover))
