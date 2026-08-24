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
;; Quicklisp, wherever it is -- and not at all when it is already here.  A
;; hardcoded ~/quicklisp assumes the process has a home it owns and that
;; Quicklisp is under it; an image that runs unprivileged has neither, and a core
;; with the systems already in it needs none of this.  (RUNNING.md's rough edge
;; #1, for this file at least.)
(let ((ql (find-if #'probe-file
                   (list #p"/opt/quicklisp/setup.lisp"
                         (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))))
  (when (and ql (not (find-package '#:quicklisp))) (load ql)))
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:glass :glass/vncauth :mcclim :mcclim-render :sb-concurrency
                    :pigment :clim-examples :clim-listener))   ; vncauth = DES via seal
    (ignore-errors (asdf:load-system :loom/glass))            ; the browser (optional)
    (ignore-errors (asdf:load-system :warren))                ; the file browser (optional)
    (ignore-errors (asdf:load-system :glass/audio-stream))    ; the session's sound (optional)
    (ignore-errors (asdf:load-system :glass/speech))          ; and its voice, via chord (optional)
    (ignore-errors (asdf:load-system :glass/nostr))           ; admission + enrolment (optional)
    (ignore-errors (asdf:load-system :glass/mic-stream))      ; a peer's microphone, inbound (optional)
    (asdf:load-asd (merge-pathnames "../mcclim-glass.asd" *load-truename*))
    (asdf:load-system :mcclim-glass)
    (ignore-errors (asdf:load-system :mcclim-glass/speak))))  ; type-and-say window (optional)

(defparameter *display*
  (or (ignore-errors (parse-integer (or (sb-ext:posix-getenv "GLASS_DISPLAY") "1"))) 1))
(defparameter *vnc-port* (+ 5900 *display*))
(defparameter *audio-port* (+ 5910 *display*))
(defparameter *control-port* (+ 4008 *display*))

;; ONLY IF NOBODY HAS NAMED IT.  A launcher that gave this session an identity also
;; gave it a name (kiln does: three words read off its own pubkey), and this file
;; loading afterwards must not paint over it with a description.  "glass" is the
;; placeholder GLASS:*DESKTOP-NAME* starts as, so anything else was somebody's choice.
(when (or (null glass:*desktop-name*)
          (string= glass:*desktop-name* "glass"))
  (setf glass:*desktop-name* (format nil "modus-lisp :: glass desktop :~d" *display*)))

;;; Windows opened at startup.  Empty by default: the desktop comes up as a bare
;;; workspace, and every app — terminal included — is one right-click away on the
;;; root menu.  Opening a shell nobody asked for is a decision, not a default, and
;;; on a desktop reachable over VNC it is a shell sitting there for whoever
;;; connects first.
;;;
;;; GLASS_APPS is a comma-separated list that puts windows back: "terminal" for a
;;; shell, or any McCLIM frame-class name (in the CLIM-GLASS package unless the
;;; name is package-qualified).  GLASS_APPS=terminal is exactly what this file
;;; used to do unconditionally.
(defun %split-commas (string)
  (loop with len = (length string)
        for start = 0 then (1+ end)
        for end = (or (position #\, string :start start) len)
        for piece = (string-trim '(#\Space #\Tab) (subseq string start end))
        unless (zerop (length piece)) collect piece
        while (< end len)))

(defparameter *startup-apps*
  (loop for name in (%split-commas (or (sb-ext:posix-getenv "GLASS_APPS") ""))
        collect (if (string-equal name "terminal")
                    '(:terminal :cols 80 :rows 24 :ppem 14)
                    ;; A frame class: read it in CLIM-GLASS so an unqualified
                    ;; name resolves where the desktop's frames actually live.
                    (list (let ((*package* (find-package :clim-glass)))
                            (read-from-string name))))))

;;; Bare-TCP control/eval socket on 127.0.0.1:4009 — read one form, eval it in the
;;; clim-glass package, write the printed result.  Lets us read live perf and poke
;;; at the RUNNING desktop with no restart:
;;;   echo '(glass:perf-reset)'  | nc -q1 127.0.0.1 4009
;;;   echo '(glass:perf-report)' | nc -q1 127.0.0.1 4009
;;;
;;; CLIM-GLASS:START-CONTROL-SOCKET now, and not a copy of it here: this file's copy
;;; swallowed reader errors (a symbol in the wrong package closed the connection with no
;;; output at all, which reads like an answer), and it had three siblings with the same
;;; bug.  backend/control.lisp is the one that answers.

;; VNC password: if ~/.glass-vnc-pass exists (create it yourself, mode 600 — it is
;; NOT in the repo), require + verify it (secures the 0.0.0.0 bind, and macOS saves
;; it to Keychain so it stops prompting).  Absent -> the open any-password posture.
;; WHERE THE SCREEN LISTENS.  Loopback unless GLASS_BIND says otherwise — reaching this
;; desktop from another machine is a thing to ask for, not a thing to discover.  A socket
;; file (run-wm's :KIND :RFB-UNIX) is better still where the viewer is local.
(defvar *bind-address* (or (sb-ext:posix-getenv "GLASS_BIND") "127.0.0.1"))

;;; ...and a SOCKET FILE instead of a port, which is better wherever the client is local:
;;; nothing to publish, no interface to get wrong, and the file's own 0600 mode is the
;;; access control rather than a password sitting in front of an open port.  GLASS_RFB_SOCKET
;;; =auto puts it in the runtime directory under this seat's name; an absolute path puts it
;;; exactly there.  The three sibling sockets — audio, microphone, admission — are DERIVED
;;; from this one (SOCKET-SIBLING), so naming one names all four and they cannot drift.
(defvar *rfb-socket*
  (let ((v (sb-ext:posix-getenv "GLASS_RFB_SOCKET")))
    (cond ((or (null v) (string= v "") (string= v "0")) nil)
          ((or (string= v "auto") (string= v "1"))
           (glass:socket-path (format nil "seat-~d.rfb" *display*)))
          (t v))))

(let ((pwfile (merge-pathnames ".glass-vnc-pass" (user-homedir-pathname))))
  (when (probe-file pwfile)
    (let ((pw (string-trim '(#\Space #\Tab #\Newline #\Return)
                           (with-open-file (in pwfile) (or (read-line in nil "") "")))))
      (when (plusp (length pw)) (setf glass:*vnc-password* pw)))))

(let ((wp (namestring (merge-pathnames "assets/wallpaper.svg"
                                       (asdf:system-source-directory :mcclim-glass)))))
  (clim-glass:start-control-socket :port *control-port*)
  ;; The session's sound, on its own port beside the screen (see src/audio-stream.lisp): one
  ;; mixer in the process the applications run in, and any number of listeners in OTHER
  ;; processes subscribing to the same mix.  Found by name, so a build without
  ;; :glass/audio-stream still starts a desktop — silence is a working desktop, no desktop is not.
  ;; ...and it follows the screen: a socket screen means a socket mixer beside it, by the
  ;; same SOCKET-SIBLING rule the gateway uses to find it.  Four endpoints, one name.
  (let ((start (find-symbol "START-SESSION-AUDIO" :glass)))
    (when (and start (fboundp start))
      (if *rfb-socket*
          (funcall start :path (glass:socket-sibling *rfb-socket* "audio"))
          (funcall start :port *audio-port* :address "127.0.0.1"))))
  ;; ...and the way back IN: the peer's microphone, on its own sibling socket.  Nothing
  ;; started this, so a gateway would connect a browser's mic to a socket nobody was
  ;; listening on and say so -- "no ear at …seat-1.mic, the microphone goes nowhere" --
  ;; which is a desktop that can speak and cannot hear.  Same optional treatment as the
  ;; mixer: found by name, and a build without :glass/mic-stream still starts a desktop.
  (let ((start (find-symbol "START-SESSION-MIC" :glass)))
    (when (and start (fboundp start))
      (handler-case
          (if *rfb-socket*
              (funcall start :path (glass:socket-sibling *rfb-socket* "mic"))
              (funcall start :port (glass:seat-mic-port *vnc-port*) :address "127.0.0.1"))
        (serious-condition (e)
          (format *error-output* "~&@@ microphone not listening: ~a~%" e)))))
  ;; The voice (see src/speech.lisp), if :glass/speech loaded and a voice is actually on this
  ;; box.  GLASS_VOICE still wins; this only fills in the one that lives here, and leaves the
  ;; variable alone — so SPEAK's complaint stays accurate — when the file is missing.
  (let ((var (find-symbol "*SPEECH-VOICE*" :glass))
        (here "/mnt/lisp/chord/export/en_US-lessac-medium.graph"))
    (when (and var (boundp var) (null (symbol-value var)) (probe-file here))
      (setf (symbol-value var) here)))
  ;; THE ADDRESS IT ACTUALLY BOUND.  This said "0.0.0.0" unconditionally — a literal in a
  ;; log line, correct only by coincidence and wrong the moment the default moved.  A
  ;; startup banner that states a posture rather than reporting one is worse than none.
  (if *rfb-socket*
      (format *error-output* "~&@@ glass desktop :~d serving on ~a (~a)~%"
              *display* *rfb-socket* wp)
      (format *error-output* "~&@@ glass desktop :~d serving on ~a:~d (~a)~%"
              *display* *bind-address* *vnc-port* wp))
  (when (and (null *rfb-socket*) (string= *bind-address* "0.0.0.0"))
    (format *error-output* "@@ WARNING: bound to every interface (GLASS_BIND) — ~
                            ~:[NO PASSWORD IS SET~;a password is set~]~%"
            (and (boundp 'glass:*vnc-password*) glass:*vnc-password*)))
  (if *rfb-socket*
      (format *error-output* "@@ control socket on 127.0.0.1:~d, session audio on ~a~%"
              *control-port* (glass:socket-sibling *rfb-socket* "audio"))
      (format *error-output* "@@ control socket on 127.0.0.1:~d, session audio on 127.0.0.1:~d~%"
              *control-port* *audio-port*))
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
  ;; ADMISSION, when this desktop has an identity of its own.  The allowlist, the enrolment
  ;; store and the login tokens live HERE now rather than in whatever gateway is in front:
  ;; a gateway is a pipe, and "may this person open the desktop" is the desktop's question
  ;; to answer.  Gated on the key because a desktop without one cannot answer it — and it is
  ;; the SAME key the gateway runs on, or every issued link and every enrolled device stops
  ;; verifying.  Found by name, so a build without :glass/nostr still starts a desktop.
  (let ((start (find-symbol "START-SESSION-NOSTR" :glass))
        (secvar (find-symbol "*BOX-SECRET*" :glass))
        (key (or (sb-ext:posix-getenv "GLASS_NOSTR_SEC") (sb-ext:posix-getenv "NOSTR_SEC"))))
    (when (and start (fboundp start) key (plusp (length key)))
      ;; FILL THE SECRET IN AFTER LOAD, which is what *BOX-SECRET* documents itself as
      ;; wanting: it is a DEFVAR read from the environment when :glass/nostr loads.  In a
      ;; saved core that load happened at IMAGE-BUILD time, where there was no identity to
      ;; read, and a defvar keeps that NIL through every later load -- so a box with a
      ;; perfectly good key in its environment still says "this desktop has no identity"
      ;; and refuses everyone.  The launcher is the one that knows, so the launcher sets it.
      (when secvar (setf (symbol-value secvar) key))
      (handler-case
          (funcall start :path (and *rfb-socket* (glass:socket-sibling *rfb-socket* "admit")))
        (serious-condition (e)
          ;; A desktop that cannot answer admission is still a desktop; it just refuses
          ;; everyone, which the gateway reports as "admission service unreachable".
          (format *error-output* "~&@@ admission failed to start: ~a~%" e)
          (finish-output *error-output*)))))
  (apply #'clim-glass:run-wm *startup-apps*
         :width 1280 :height 800 :background wp :background-mode :cover
         (if *rfb-socket*
             (list :kind :rfb-unix :path *rfb-socket*)
             (list :port *vnc-port* :address *bind-address*))))
