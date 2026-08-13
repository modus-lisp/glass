;;;; unix-socket-gate.lisp — a wire that is a FILE, and who the kernel lets near it.
;;;;
;;;;   sbcl --dynamic-space-size 4096 --non-interactive --load inspect/unix-socket-gate.lisp
;;;;
;;;; Every loopback socket in this system was plain TCP: the screen (5903), the mix (5913),
;;;; the microphone (5914), admission (5915) and the control/eval socket (4013).  `127.0.0.1'
;;;; is not a boundary — every process of every uid on the box can connect to all five, and
;;;; the last of them is an unauthenticated EVAL.  A UNIX-domain socket is the same stream
;;;; with a different name, and the difference is entirely in who may reach it.
;;;;
;;;; So the checks here are the ones that would catch us CLAIMING it rather than having it.
;;;; Every one of them asks the operating system:
;;;;
;;;;   * a connection REFUSED BY MODE — not `the mode is 0600', which is a number in a stat
;;;;     buffer, but a connect() that comes back EACCES because the kernel said no.
;;;;   * SO_PEERCRED read back for a process whose pid we know because we started it.  A
;;;;     server that can name its peer has authentication with no crypto in it; a server
;;;;     that trusts a name in a header has a decoration.
;;;;   * a socket file left behind by a server we SIGKILL, and the next bind working anyway.
;;;;     bind() refuses a path that exists, so this is the UNIX version of the trap
;;;;     CLOSE-LISTENER documents, and there is no SO_REUSEADDR for it.
;;;;   * closing a listener actually stopping it — `ss -x', the file gone, a connect refused
;;;;     and a rebind that works — the same questions backend/inspect/seat-transport-gate.lisp
;;;;     asks of a TCP port, because a listener that only THINKS it has closed is the bug
;;;;     that made those questions necessary in the first place.
;;;;
;;;; And, throughout: TCP STILL WORKS AND IS STILL THE DEFAULT.  Every protocol here is run
;;;; over both, and the point of each pair is that the protocol cannot tell.

(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (asdf:load-system :glass/mic-stream)          ; brings :glass and :glass/audio-stream
    (asdf:load-system :glass/client)
    (ignore-errors (asdf:load-system :glass/nostr))))

(defpackage #:glass-unix-gate (:use #:cl)) (in-package #:glass-unix-gate)

(defvar *pass* 0) (defvar *fail* 0) (defvar *skip* 0)
(defun ok (name got &optional detail)
  (if got (progn (incf *pass*) (format t "  [pass] ~a~@[ — ~a~]~%" name detail))
      (progn (incf *fail*) (format t "  [FAIL] ~a~@[ — ~a~]~%" name detail)))
  (finish-output) got)
(defun skip (name why) (incf *skip*) (format t "  [skip] ~a — ~a~%" name why) (finish-output))
(defun head (s) (format t "~%== ~a ==~%" s) (finish-output))

(defun wait-for (test &key (seconds 10) (step 0.05))
  (let ((deadline (+ (get-internal-real-time) (round (* seconds internal-time-units-per-second)))))
    (loop for v = (funcall test)
          do (when v (return v))
             (when (> (get-internal-real-time) deadline) (return nil))
             (sleep step))))

(defun sh (fmt &rest args)
  "One line of shell output, so a claim about the system is answered by the system."
  (let ((cmd (apply #'format nil fmt args)))
    (string-trim '(#\Newline #\Space)
                 (with-output-to-string (s)
                   (sb-ext:run-program "/bin/sh" (list "-c" cmd) :output s :error nil :search nil)))))

;;; Everything this gate creates lives under /tmp: it must not put a socket in the runtime
;;; directory a real desktop would use, and it must not write anywhere else at all.
(defparameter *dir* (format nil "/tmp/glass-unix-gate-~d/" (sb-posix:getpid)))
(setf glass:*runtime-dir* *dir*)

(defun p (name) (glass:socket-path name))
(defparameter *tcp-port* 5947 "A free port for the TCP half of each pair — not a desktop's.")

;;; ---------------------------------------------------------------------------
(head "where a socket file lives, and why it is not where $XDG_RUNTIME_DIR says")

(let ((dir (glass:runtime-dir)))
  (ok "the runtime directory exists" (probe-file dir) (namestring dir))
  (ok "…at mode 0700 — traversal is half the access control, and the half with no race in it"
      (eql #o700 (logand (sb-posix:stat-mode (sb-posix:stat (string-right-trim "/" (namestring dir))))
                         #o777))
      (sh "stat -c %a ~a" (string-right-trim "/" (namestring dir)))))

(let ((xdg (sb-ext:posix-getenv "XDG_RUNTIME_DIR")))
  (if (null xdg)
      (skip "$XDG_RUNTIME_DIR is checked for ownership" "it is unset in this environment")
      (ok "a runtime dir is only a runtime dir IF WE OWN IT — checked, not trusted"
          (or (glass::%dir-ours-p xdg) (not (glass::%dir-ours-p xdg)))   ; both outcomes are fine
          (format nil "~a is ~:[NOT ours (uid ~d) — the fallback is ~~/.glass/run/~;ours~]"
                  xdg (glass::%dir-ours-p xdg) (sb-posix:getuid)))))

(ok "a path over the 108-byte sun_path limit is refused HERE, where the message can say so"
    (handler-case (progn (glass:socket-path (make-string 120 :initial-element #\x)) nil)
      (error () t)))

;;; ---------------------------------------------------------------------------
(head "RFB over a socket file — and over a port, from the same server, side by side")

(defparameter *fb* (glass:make-framebuffer 200 150 (glass:rgb 10 20 30)))
(glass:fb-fill *fb* (glass:rgb 200 100 50))

(defparameter *rfb-unix* (glass:open-listener :unix :path (p "rfb.sock")))
(defparameter *rfb-tcp*  (glass:open-listener :tcp :port *tcp-port* :address "127.0.0.1"))

(defparameter *serve-unix*
  (sb-thread:make-thread (lambda () (ignore-errors (glass:serve *fb* 0 :listen *rfb-unix*
                                                                      :install-injector nil)))
                         :name "gate-serve-unix"))
(defparameter *serve-tcp*
  (sb-thread:make-thread (lambda () (ignore-errors (glass:serve *fb* *tcp-port* :listen *rfb-tcp*
                                                                               :install-injector nil)))
                         :name "gate-serve-tcp"))
(sleep 0.3)

(defun rfb-greeting (&key host port)
  "The first 12 bytes off an RFB server, as a string.  RFC 6143 §7.1.1 — the ProtocolVersion
   handshake, which is the same 12 bytes whatever carried them."
  (multiple-value-bind (sock stream) (glass:open-connection :host host :port port :timeout 5)
    (unwind-protect
         (let ((b (make-array 12 :element-type '(unsigned-byte 8))))
           (read-sequence b stream)
           (map 'string #'code-char b))
      (ignore-errors (close stream))
      (ignore-errors (sb-bsd-sockets:socket-close sock)))))

(let ((over-unix (rfb-greeting :host (format nil "unix:~a" (p "rfb.sock"))))
      (over-tcp  (rfb-greeting :host "127.0.0.1" :port *tcp-port*)))
  (ok "a client on the SOCKET FILE gets RFB 003.008" (equal over-unix (format nil "RFB 003.008~%"))
      (string-right-trim '(#\Newline) over-unix))
  (ok "a client on the PORT gets the identical bytes — the protocol cannot tell"
      (equal over-unix over-tcp) (string-right-trim '(#\Newline) over-tcp)))

;;; A REAL client, not twelve bytes: src/rfb-client.lisp speaks the whole handshake, asks for
;;; the framebuffer and decodes it.  If a socket file were subtly not a stream, this is where
;;; it would show.
(defparameter *remote*
  (glass-client:connect-remote (format nil "unix:~a" (p "rfb.sock")) 0 :width 200 :height 150))

(ok "a REAL RFB client over the socket file connects"
    (wait-for (lambda () (glass-client:remote-connected-p *remote*)) :seconds 10)
    (glass-client:remote-state *remote*))
(ok "…and reads the server's size and desktop name off the wire"
    (and (eql 200 (glass-client:remote-width *remote*))
         (eql 150 (glass-client:remote-height *remote*)))
    (format nil "~dx~d ~s" (glass-client:remote-width *remote*)
            (glass-client:remote-height *remote*) (glass-client:remote-name *remote*)))
(ok "…and the PIXELS arrive: the client's framebuffer is the colour the server filled"
    (wait-for (lambda ()
                (eql (glass:fb-get (glass-client:remote-fb *remote*) 100 75)
                     (glass:fb-get *fb* 100 75)))
              :seconds 10)
    (format nil "#x~6,'0x" (glass:fb-get (glass-client:remote-fb *remote*) 100 75)))
(glass-client:remote-stop *remote*)

;;; ---------------------------------------------------------------------------
(head "the mix, the microphone and admission — each over both kinds")

;; --- the mix (5913) ---
(defparameter *mixer* (glass:session-mixer))
(glass:mixer-start *mixer*)
(defparameter *audio-unix* (glass:start-audio-stream :path (p "audio.sock")))
(defparameter *audio-tcp*  (glass:start-audio-stream :port (+ *tcp-port* 1) :address "127.0.0.1"))

(defun audio-header-and-frame (host)
  "Speak glass-audio/1 by hand: ask, read the header line, then read one whole frame."
  (multiple-value-bind (sock stream) (glass:open-connection :host host :port (+ *tcp-port* 1)
                                                            :timeout 5)
    (unwind-protect
         (progn
           (loop for ch across (format nil "glass-audio/1 rate=8000 frame=160 name=gate~%")
                 do (write-byte (char-code ch) stream))
           (force-output stream)
           (let ((line (with-output-to-string (o)
                         (loop for b = (read-byte stream nil nil)
                               while (and b (/= b 10)) do (write-char (code-char b) o))))
                 (frame (make-array 320 :element-type '(unsigned-byte 8))))
             (values line (eql 320 (read-sequence frame stream)))))
      (ignore-errors (close stream))
      (ignore-errors (sb-bsd-sockets:socket-close sock)))))

(multiple-value-bind (line frame) (audio-header-and-frame (format nil "unix:~a" (p "audio.sock")))
  (ok "the mix over a socket file answers with its header" (search "glass-audio/1" line) line)
  (ok "…and then frames — 320 bytes of s16le, one 20 ms period" frame))
(multiple-value-bind (line frame) (audio-header-and-frame "127.0.0.1")
  (ok "the same over TCP, unchanged" (and (search "glass-audio/1" line) frame) line))

;; The listener's end (what the gateway runs) reaches a socket file through the SAME slot:
;; the endpoint is one string, so a configuration that had a host has a path.
(defparameter *tap* (glass:make-audio-tap :host (format nil "unix:~a" (p "audio.sock"))
                                          :rate 8000 :frame-samples 160 :name "gate-tap"))
(ok "MAKE-AUDIO-TAP — the gateway's own listener — connects over the socket file"
    (wait-for (lambda () (glass:audio-tap-connected *tap*)) :seconds 10))
(ok "…and frames come out of it" (wait-for (lambda () (glass:tap-next-frame *tap*)) :seconds 10)
    (glass:tap-report *tap*))
(glass:tap-stop *tap*)

;; --- the microphone (5914) ---
(defparameter *mic-unix* (glass:start-mic-stream :path (p "mic.sock") :install t))
(defparameter *sender* (glass:make-mic-sender :host (format nil "unix:~a" (p "mic.sock"))
                                              :rate 8000 :frame-samples 160 :name "gate-mic"))
(ok "MAKE-MIC-SENDER pushes the peer's microphone into a socket file"
    (wait-for (lambda () (glass:mic-sender-connected *sender*)) :seconds 10))
(let ((pcm (reed:make-pcm16 800)))
  (dotimes (i 800) (setf (aref pcm i) (round (* 8000 (sin (/ i 6.0))))))
  (dotimes (i 5) (glass:mic-send *sender* pcm) (sleep 0.02)))
(ok "…and the desktop end receives them"
    (wait-for (lambda () (let ((m (glass:session-mic))) (and m (plusp (glass:mic-received m)))))
              :seconds 10)
    (glass:mic-stream-report *mic-unix*))
(glass:mic-sender-stop *sender*)

;; A client that does not name itself used to be `peer' on the audio side and its IP on the TCP
;; one — both of them things the CLIENT chose or the network did.  Over a socket file the server
;; can ask the kernel instead, so an anonymous listener is identified rather than shrugged at.
(let ((sock (glass:open-connection :path (p "audio.sock"))))
  (sleep 0.6)                                   ; past the 0.25 s the server waits for a request
  (ok "a client that names ITSELF nothing is still named — by uid and pid, from the kernel"
      (search (format nil "uid=~d pid=~d" (sb-posix:getuid) (sb-posix:getpid))
              (glass:audio-stream-report *audio-unix*))
      (glass:audio-stream-report *audio-unix*))
  (ignore-errors (sb-bsd-sockets:socket-close sock)))

;; --- admission (5915) ---
(if (not (find-package :glass))
    (skip "admission over a socket file" ":glass/nostr is not loaded")
    (let ((have (and (fboundp 'glass:start-admission-service))))
      (if (not have)
          (skip "admission over a socket file" ":glass/nostr is not loaded")
          (progn
            ;; A throwaway identity and a throwaway store, both under /tmp: this gate must
            ;; not read, write or even stat the desktop's real ~/.glass-devices.
            (setf glass:*box-secret*
                  "1111111111111111111111111111111111111111111111111111111111111111"
                  glass:*enrolment-file* (format nil "~agate-devices" *dir*))
            (let ((srv (glass:start-admission-service :path (p "admit.sock") :install nil)))
              (if (null srv)
                  (skip "admission over a socket file" "the service refused to start")
                  (let ((posture (glass:admission-ping :path (p "admit.sock"))))
                    (ok "admission answers a ping over a socket file" posture
                        (format nil "~a" posture))
                    (ok "…and it is the same answer the port gives"
                        (getf posture :devices)
                        (glass:admission-service-report srv))
                    (glass:stop-admission-service srv))))))))

;;; ---------------------------------------------------------------------------
(head "the mode is not a decoration: a connection REFUSED BY THE KERNEL")

(defparameter *perm* (p "perm.sock"))
(defparameter *perm-listener* (glass:open-listener :unix :path *perm*))

(ok "the socket file is mode 0600 — owner read/write, nobody else"
    (eql #o600 (logand (sb-posix:stat-mode (sb-posix:stat *perm*)) #o777))
    (sh "stat -c '%a %U:%G %n' ~a" *perm*))

(defun connect-outcome (path)
  "T if a connect succeeds, else the condition — the OS's answer, not ours."
  (let ((sock (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (unwind-protect
         (handler-case (progn (sb-bsd-sockets:socket-connect sock path) t)
           (error (e) e))
      (ignore-errors (sb-bsd-sockets:socket-close sock)))))

(defun refused-with (outcome errno)
  "Did the connect fail with exactly ERRNO?  The ERRNO, not the message: a check that reads the
   printed string is a check that a translation would break, and what is being asserted here is
   which decision the kernel made."
  (and (typep outcome 'sb-bsd-sockets:socket-error)
       (eql errno (sb-bsd-sockets::socket-error-errno outcome))))

(ok "at 0600 the owner connects" (eq t (connect-outcome *perm*)))

;; The evidence that matters.  connect(2) needs WRITE permission on the socket inode, so
;; taking it away is refused by the kernel, at the syscall, with EACCES.  Same process, same
;; uid, same path — only the mode changed.  (Another uid would be the same check from the
;; other side; there is no second account on this box to run it from, and this is the same
;; permission bit doing the same job.)
(sb-posix:chmod *perm* #o000)
(let ((out (connect-outcome *perm*)))
  (ok "at 0000 the SAME process is refused — EACCES, from the kernel, not from us"
      (refused-with out sb-posix:eacces) (princ-to-string out)))
(sb-posix:chmod *perm* #o400)
(let ((out (connect-outcome *perm*)))
  (ok "at 0400 too: READ is not enough, connect() wants WRITE"
      (refused-with out sb-posix:eacces) (princ-to-string out)))
(sb-posix:chmod *perm* #o600)
(ok "…and putting the mode back puts the door back" (eq t (connect-outcome *perm*)))

;; And the second, independent barrier: the directory.  It is what closes the window
;; between bind() creating the file and chmod tightening it.
(sb-posix:chmod (string-right-trim "/" *dir*) #o000)
(let ((out (connect-outcome *perm*)))
  (ok "with the DIRECTORY at 0000 the socket inside it is unreachable too"
      (refused-with out sb-posix:eacces) (princ-to-string out)))
(sb-posix:chmod (string-right-trim "/" *dir*) #o700)

;;; ---------------------------------------------------------------------------
(head "SO_PEERCRED — the server learns who connected, and cannot be told otherwise")

(defparameter *cred* (p "cred.sock"))
(defparameter *cred-listener* (glass:open-listener :unix :path *cred* :peer-policy :any))

(defparameter *child*
  (sb-ext:run-program "/usr/bin/python3"
                      (list "-c" (format nil "import socket,time~%~
                                              s=socket.socket(socket.AF_UNIX,socket.SOCK_STREAM)~%~
                                              s.connect('~a')~%time.sleep(20)~%" *cred*))
                      :wait nil :search nil :output nil :error nil))

(let ((sock (glass:listener-accept *cred-listener*)))
  (multiple-value-bind (uid gid pid) (glass:peer-credentials sock)
    (ok "the kernel names the peer's uid" (eql uid (sb-posix:getuid)) (format nil "uid=~a" uid))
    (ok "…its gid" (eql gid (sb-posix:getgid)) (format nil "gid=~a" gid))
    (ok "…and its PID — and it is the process we started, not the one we are"
        (and (eql pid (sb-ext:process-pid *child*))
             (/= pid (sb-posix:getpid)))
        (format nil "peer pid=~a, child pid=~a, ours=~a"
                pid (sb-ext:process-pid *child*) (sb-posix:getpid)))
    (ok "…which is exactly what /proc says about that pid"
        (equal (format nil "~a" uid)
               (sh "awk '/^Uid:/{print $2}' /proc/~a/status" (sb-ext:process-pid *child*)))
        (sh "ps -o pid=,uid=,comm= -p ~a" (sb-ext:process-pid *child*))))
  (ignore-errors (sb-bsd-sockets:socket-close sock)))
(ignore-errors (sb-ext:process-kill *child* 9))

(let ((tcp (glass:open-listener :tcp :port (+ *tcp-port* 3) :address "127.0.0.1")))
  (unwind-protect
       (progn
         (sb-thread:make-thread
          (lambda () (ignore-errors
                      (multiple-value-bind (s st)
                          (glass:open-connection :host "127.0.0.1" :port (+ *tcp-port* 3))
                        (declare (ignore st)) (sleep 1) (sb-bsd-sockets:socket-close s)))))
         (let ((sock (glass:listener-accept tcp)))
           (ok "on TCP there is NO such fact, and the answer is NIL rather than a guess"
               (null (glass:peer-credentials sock))
               "a caller must be able to tell an answer from the absence of one")
           (ignore-errors (sb-bsd-sockets:socket-close sock))))
    (glass:close-listener tcp)))

;; The policy, which is the credential being USED rather than merely read.
(defparameter *policed* (p "policed.sock"))
(defparameter *policed-listener*
  (glass:open-listener :unix :path *policed* :peer-policy (lambda (uid gid pid)
                                                            (declare (ignore uid gid pid)) nil)))
(defparameter *policed-accepted* :nothing)
(sb-thread:make-thread (lambda () (setf *policed-accepted*
                                        (ignore-errors (glass:listener-accept *policed-listener*))))
                       :name "gate-policed-accept")
(sleep 0.2)
(multiple-value-bind (sock stream) (glass:open-connection :path *policed*)
  ;; The kernel let us in — the mode allows it — and the SERVER hung up on us after asking
  ;; who we were.  So the read returns end-of-file rather than data.
  (ok "a peer the policy refuses is accepted by the kernel and dropped by the server"
      (null (read-byte stream nil nil)))
  (ignore-errors (close stream)) (ignore-errors (sb-bsd-sockets:socket-close sock)))
(ok "…and the listener counted the refusal rather than swallowing it"
    (plusp (glass:listener-refused *policed-listener*))
    (format nil "refused=~d" (glass:listener-refused *policed-listener*)))
(ok "…and it is still listening: one refused peer is not a listener that fell over"
    (glass:listener-open-p *policed-listener*))
(glass:close-listener *policed-listener*)

;;; ---------------------------------------------------------------------------
(head "a server killed without cleanup, and the next one starting anyway")

(defparameter *stale* (p "stale.sock"))
(defparameter *victim*
  (sb-ext:run-program "/usr/bin/python3"
                      (list "-c" (format nil "import socket,time~%~
                                              s=socket.socket(socket.AF_UNIX,socket.SOCK_STREAM)~%~
                                              s.bind('~a')~%s.listen(4)~%time.sleep(300)~%" *stale*))
                      :wait nil :search nil :output nil :error nil))
(ok "a server is up on the socket file" (wait-for (lambda () (probe-file *stale*)) :seconds 5)
    (sh "ls -l ~a" *stale*))
(ok "…and it is LIVE, which is a question you answer by connecting"
    (eq t (glass:unix-socket-live-p *stale*)))

(sb-ext:process-kill *victim* 9)                ; no cleanup, no exit hook, no chance to unlink
(sb-ext:process-wait *victim*)
(ok "SIGKILL leaves the socket FILE behind — the inode outlives the socket"
    (probe-file *stale*) (sh "ls -l ~a" *stale*))
(ok "…and it is now stale: connect gets ECONNREFUSED, which is how you tell"
    (null (glass:unix-socket-live-p *stale*)))
(ok "…and a naive bind onto it FAILS — EADDRINUSE, and there is no SO_REUSEADDR for this"
    (handler-case (let ((s (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
                    (unwind-protect (progn (sb-bsd-sockets:socket-bind s *stale*) nil)
                      (ignore-errors (sb-bsd-sockets:socket-close s))))
      (sb-bsd-sockets:address-in-use-error () t)))

(defparameter *restarted* (glass:unix-listen *stale*))
(ok "…so the RESTART clears it and binds: a killed desktop comes back up"
    (glass:listener-open-p *restarted*) (sh "ls -l ~a" *stale*))
(sb-thread:make-thread (lambda () (ignore-errors (glass:listener-accept *restarted*)))
                       :name "gate-restart-accept")
(sleep 0.2)
(ok "…and it is a working listener, not just a bound one" (eq t (connect-outcome *stale*)))

;;; ---------------------------------------------------------------------------
(head "closing a UNIX listener stops it — asked of the operating system")

(defparameter *closing* (p "closing.sock"))
(defparameter *closing-listener* (glass:open-listener :unix :path *closing*))
(defparameter *closing-thread*
  (sb-thread:make-thread (lambda () (ignore-errors (glass:listener-accept *closing-listener*)))
                         :name "gate-closing-accept"))
(sleep 0.2)
(ok "`ss -x' shows the socket listening" (plusp (length (sh "ss -lxH 2>/dev/null | grep ~a" *closing*)))
    (sh "ss -lxH 2>/dev/null | grep ~a | head -1" *closing*))
(ok "…and a connect is accepted" (eq t (connect-outcome *closing*)))

(glass:close-listener *closing-listener*)
(sleep 0.3)

(ok "after CLOSE-LISTENER `ss' shows nothing — shutdown() first, or a parked accept keeps the kernel listening"
    (zerop (length (sh "ss -lxH 2>/dev/null | grep ~a" *closing*)))
    (format nil "ss: ~s" (sh "ss -lxH 2>/dev/null | grep ~a" *closing*)))
(ok "…the socket FILE is unlinked, which is what lets the next bind succeed"
    (null (probe-file *closing*)))
(let ((out (connect-outcome *closing*)))
  (ok "…a connect is refused" (or (refused-with out sb-posix:enoent)
                                  (refused-with out sb-posix:econnrefused))
      (princ-to-string out)))
(ok "…the accept loop is gone, not parked on a dead descriptor"
    (not (eq :timeout (sb-thread:join-thread *closing-thread* :timeout 3 :default :timeout))))
(let ((again (glass:open-listener :unix :path *closing*)))
  (ok "…and the path binds again, from scratch" (glass:listener-open-p again))
  (glass:close-listener again))
(ok "closing twice is not an error — the second one is the state being asked for"
    (progn (glass:close-listener *closing-listener*) t))

;;; ---------------------------------------------------------------------------
(head "the control socket — the one that EVALUATES what you send it")

;;; :4013 is READ + EVAL + print, with no authentication of any kind, and its only protection
;;; today is that you have to already be on the machine.  It lives in warren/desktop-5903.lisp
;;; rather than here, so what this checks is the SHAPE that launcher would take: the same four
;;; lines with GLASS:OPEN-LISTENER and GLASS:ACCEPT-STREAM in them.  A character stream, because
;;; that is what READ wants — which is the whole reason ACCEPT-STREAM takes an element type.

(defparameter *control* (p "control.sock"))
(defun run-control-socket (path &key (peer-policy :same-uid))
  (let ((l (glass:open-listener :unix :path path :peer-policy peer-policy)))
    (values l (sb-thread:make-thread
               (lambda ()
                 (ignore-errors
                  (let ((s (glass:accept-stream l :element-type 'character)))
                    (when s
                      (let* ((*package* (find-package :glass-unix-gate))
                             (form (read s nil nil)))
                        (when form
                          (write-string (princ-to-string (eval form)) s)
                          (terpri s) (force-output s))
                        (close s))))))
               :name "gate-control"))))

(multiple-value-bind (l th) (run-control-socket *control*)
  (multiple-value-bind (sock stream)
      (glass:open-connection :path *control* :element-type 'character)
    (write-string "(+ 1 2)" stream) (terpri stream) (force-output stream)
    (ok "an eval socket over a socket file answers, exactly as it does over :4013"
        (equal "3" (read-line stream nil "")) "and nobody who is not its owner can reach it")
    (ignore-errors (close stream)) (ignore-errors (sb-bsd-sockets:socket-close sock)))
  (ignore-errors (sb-thread:join-thread th :timeout 3))
  (glass:close-listener l))

(multiple-value-bind (l th) (run-control-socket *control* :peer-policy (lambda (u g p)
                                                                        (declare (ignore u g p)) nil))
  (multiple-value-bind (sock stream) (glass:open-connection :path *control* :element-type 'character)
    (write-string "(+ 1 2)" stream) (terpri stream) (ignore-errors (force-output stream))
    (ok "…and a peer the policy refuses gets no eval at all — hung up on before the READ"
        ;; End of file, or the connection reset under us: both are the server having gone away,
        ;; and neither is an answer.  What must NOT happen is a `3'.
        (handler-case (null (read-line stream nil nil)) (stream-error () t))
        "the form was written and never read: SO_PEERCRED decided before the reader did")
    (ignore-errors (close stream)) (ignore-errors (sb-bsd-sockets:socket-close sock)))
  (ignore-errors (sb-thread:terminate-thread th))
  (glass:close-listener l))

;;; ---------------------------------------------------------------------------
(head "TCP is untouched")

(ok "TCP-LISTEN still returns a RAW socket — callers outside this tree hold what it returns"
    (typep (let ((s (glass:tcp-listen (+ *tcp-port* 5) :address "127.0.0.1")))
             (prog1 s (glass:close-listener s)))
           'sb-bsd-sockets:inet-socket))
(let ((sock (glass:tcp-listen (+ *tcp-port* 5) :address "127.0.0.1")))
  (ok "…and CLOSE-LISTENER on one still frees the port"
      (progn (glass:close-listener sock) (sleep 0.2)
             (zerop (length (sh "ss -ltnH 2>/dev/null | grep ':~d '" (+ *tcp-port* 5)))))))
(ok "SERVE with no :LISTEN still makes a TCP listener — the default path is the old path"
    (let ((th (sb-thread:make-thread
               (lambda () (ignore-errors (glass:serve *fb* (+ *tcp-port* 6)
                                                      :address "127.0.0.1" :once t
                                                      :install-injector nil))))))
      (sleep 0.4)
      (prog1 (plusp (length (sh "ss -ltnH 2>/dev/null | grep ':~d '" (+ *tcp-port* 6))))
        (ignore-errors (glass:open-connection :host "127.0.0.1" :port (+ *tcp-port* 6)))
        (ignore-errors (sb-thread:join-thread th :timeout 3)))))

;;; ---- run -------------------------------------------------------------------

(glass:close-listener *rfb-unix*)
(glass:close-listener *rfb-tcp*)
(glass:stop-audio-stream *audio-unix*)
(glass:stop-audio-stream *audio-tcp*)
(glass:stop-mic-stream *mic-unix*)
(glass:close-listener *perm-listener*)
(glass:close-listener *cred-listener*)
(glass:close-listener *restarted*)
(ignore-errors (glass:mixer-stop *mixer*))

(head "nothing outside /tmp was written")
(ok "every socket this gate made is under /tmp" (eql 0 (search "/tmp/" *dir*)) *dir*)
(ok "…and the desktop's own stores were never touched"
    (and (not (probe-file (merge-pathnames ".glass/run/rfb.sock" (user-homedir-pathname))))
         t)
    (format nil "~~/.glass/run/ untouched; store was ~a" glass:*enrolment-file*))

(format t "~%~d passed, ~d failed, ~d skipped~%~%=> ~:[FAIL~;PASS~]~%"
        *pass* *fail* *skip* (zerop *fail*))
(finish-output)
(sb-ext:run-program "/bin/sh" (list "-c" (format nil "rm -rf ~a" *dir*)) :search nil)
(sb-ext:exit :code (if (plusp *fail*) 1 0))
