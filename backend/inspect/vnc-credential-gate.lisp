;;;; vnc-credential-gate.lisp — a credential belongs to ONE WIRE, and a seat can open
;;;; and close a VNC port from its own root menu.
;;;;
;;;;   sbcl --control-stack-size 256 --dynamic-space-size 4096 \
;;;;        --non-interactive --load backend/inspect/vnc-credential-gate.lisp
;;;;
;;;; THE CLAIM THAT MATTERS IS THE ONE THAT WAS IMPOSSIBLE BEFORE.  GLASS:*VNC-PASSWORD*
;;;; was a global read inside the handshake, so it applied to every listener at once, and
;;;; that is why turning it on broke video: the VP8 capture is an RFB client of the same
;;;; desktop, it speaks security type None, and a session-wide password locked it out
;;;; while the browser's bridged RFB kept working.  The SECURE configuration was the
;;;; broken one.  So this gate does not test a password in isolation — it stands up ONE
;;;; desktop with TWO wires onto ONE seat, demands a password on the TCP one, and then
;;;; runs THE REAL CAPTURE CODE (webrtc-data's glass-capture.lisp, the gateway's own
;;;; source, unmodified) over the UNIX one and encodes a REAL VP8 keyframe out of what it
;;;; captured, at the same moment, in the same process.
;;;;
;;;; And where it asks whether a port is open or shut it asks the OPERATING SYSTEM —
;;;; `ss -ltn', a bind that succeeds because the port is free, a connect that is refused —
;;;; because CLOSE-LISTENER exists precisely because a slot saying "closed" and a kernel
;;;; still listening are a combination this code has already produced once.
;;;;
;;;; NOTHING HERE WRITES OUTSIDE /tmp.  The runtime dir is under /tmp (never the live
;;;; desktop's ~/.glass/run), the ports are ones nothing on this box uses, and the live
;;;; stores' mtimes are noted and checked at the end.

(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:glass :glass/vncauth :glass/client :mcclim :mcclim-render :sb-concurrency))
    (asdf:load-asd "/home/claude/glass/backend/mcclim-glass.asd")
    (asdf:load-system :mcclim-glass)
    ;; The gateway's video path, exactly as the gateway loads it.  Optional only in the
    ;; sense that this file says so if it is missing; the evidence needs it.
    (ignore-errors
     (ql:quickload '(:webrtc-data :webrtc-media))
     ;; ASKED FOR BY NAME.  This used to load an absolute path under /home/claude —
     ;; another machine's home directory — so on every other machine the form failed
     ;; inside its own IGNORE-ERRORS and the gate carried on without the video path
     ;; it says it needs.  A check that runs on one laptop is not a check.
     (asdf:load-system "glass-webrtc"))))
(in-package :clim-glass)

(defvar *fail* 0)
(defun check (ok fmt &rest args)
  (format t "  [~:[FAIL~;pass~]] ~?~%" ok fmt args)
  (finish-output)
  (unless ok (incf *fail*)))
(defun banner (s) (format t "~&~%== ~a ==~%" s))

;;; ---- our own runtime dir, and the live one left alone ------------------------

(defparameter *rundir* (format nil "/tmp/glass-vnc-cred-gate-~d/" (sb-posix:getpid)))
(setf glass:*runtime-dir* *rundir*)
(defparameter *live-run* (namestring (merge-pathnames ".glass/run/" (user-homedir-pathname))))
(defparameter *live-pass* (namestring (merge-pathnames ".glass-vnc-pass" (user-homedir-pathname))))
(defparameter *live-before*
  (list (and (probe-file *live-run*) (file-write-date *live-run*))
        (and (probe-file *live-pass*) (file-write-date *live-pass*))))
;; The password file is an OVERRIDE this gate must not be at the mercy of: point the
;; parameter at a path under /tmp that does not exist, so "generate one" is what is
;; being tested whether or not the operator of this box keeps a password of their own.
(defparameter *pass-fixture* (format nil "~ano-such-password" *rundir*))
(setf *vnc-password-file* *pass-fixture*)

;;; ---- asking the operating system ---------------------------------------------

(defparameter *sh-out* (format nil "/tmp/glass-vnc-cred-gate-~d.out" (sb-posix:getpid)))

(defun sh (cmd)
  "Run CMD and return its output.

   THROUGH A FILE AND NOT A PIPE, which is not fussiness: RUN-PROGRAM with :OUTPUT to a
   Lisp stream makes a pipe and drains it with SERVE-EVENT, and in an image with this
   many threads and sockets that read has been observed parked forever on a child that
   had ALREADY EXITED (`#<PROCESS :EXITED 0>' at the bottom of a poll) — a descriptor for
   the write end survived somewhere.  A gate that asks the operating system questions
   must not be the thing that hangs; :OUTPUT to a pathname hands the child the file
   directly and there is no pipe to be left open."
  (sb-ext:run-program "/bin/sh" (list "-c" cmd)
                      :output *sh-out* :if-output-exists :supersede
                      :search nil :error nil :wait t)
  (with-open-file (in *sh-out* :if-does-not-exist nil)
    (if (null in) ""
        (with-output-to-string (o)
          (loop for line = (read-line in nil) while line do (write-line line o))))))

(defun os-listening-p (port)
  (plusp (length (string-trim '(#\Space #\Newline)
                              (sh (format nil "ss -ltn 2>/dev/null | grep -E '[:.]~d ' || true" port))))))

(defun os-listen-line (port)
  (string-trim '(#\Space #\Newline)
               (sh (format nil "ss -ltn 2>/dev/null | grep -E '[:.]~d ' || true" port))))

(defun port-bindable-p (port)
  "Can WE bind PORT?  Succeeds only if nobody is LISTENING on it."
  (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (unwind-protect
         (handler-case
             (progn (setf (sb-bsd-sockets:sockopt-reuse-address sock) t)
                    (sb-bsd-sockets:socket-bind sock (sb-bsd-sockets:make-inet-address "0.0.0.0") port)
                    (sb-bsd-sockets:socket-listen sock 1)
                    t)
           (error () nil))
      (ignore-errors (sb-bsd-sockets:socket-shutdown sock :direction :io))
      (ignore-errors (sb-bsd-sockets:socket-close sock)))))

;;; ---- a client's first bytes, by hand -----------------------------------------
;;;
;;; By hand rather than through GLASS-CLIENT because what is being measured here is
;;; WHICH SECURITY TYPES ARE OFFERED — the one thing a client that just connects would
;;; hide by succeeding.

(defun r8 (s) (read-byte s))
(defun r32 (s) (let ((v 0)) (dotimes (k 4 v) (setf v (logior (ash v 8) (r8 s))))))
(defun rn (s n) (let ((b (make-array n :element-type '(unsigned-byte 8)))) (read-sequence b s) b))

(defun rfb-stream (&key host port path)
  (handler-case
      (multiple-value-bind (sock s)
          (if path (glass:open-connection :path path :timeout 3)
              (glass:open-connection :host (or host "127.0.0.1") :port port :timeout 3))
        (values s sock))
    (error () (values nil nil))))

(defun security-types (&key host port path)
  "(values VERSION-STRING TYPES) — the RFB greeting and the security types the server
   offers a 3.8 client, or NIL if nothing would take the connection."
  (multiple-value-bind (s sock) (rfb-stream :host host :port port :path path)
    (when s
      (unwind-protect
           (handler-case
               (let ((ver (map 'string #'code-char (rn s 12))))
                 (write-sequence (map '(vector (unsigned-byte 8)) #'char-code
                                      (format nil "RFB 003.008~c" #\Newline)) s)
                 (force-output s)
                 (let ((n (r8 s)))
                   (values ver (coerce (rn s n) 'list))))
             (error () nil))
        (ignore-errors (close s))
        (ignore-errors (sb-bsd-sockets:socket-close sock))))))

(defun try-password (password &key host port path)
  "Run the client half of a VNC-auth handshake with PASSWORD.  Returns the u32
   SecurityResult (0 = admitted, 1 = rejected), or NIL if it never got that far."
  (multiple-value-bind (s sock) (rfb-stream :host host :port port :path path)
    (when s
      (unwind-protect
           (handler-case
               (progn
                 (rn s 12)
                 (write-sequence (map '(vector (unsigned-byte 8)) #'char-code
                                      (format nil "RFB 003.008~c" #\Newline)) s)
                 (force-output s)
                 (let* ((n (r8 s)) (types (coerce (rn s n) 'list)))
                   (unless (member 2 types) (return-from try-password :no-auth-offered))
                   (write-sequence (vector 2) s) (force-output s)
                   (let ((challenge (rn s 16)))
                     (write-sequence (glass:vnc-auth-response password challenge) s)
                     (force-output s)
                     (r32 s))))
             (error () nil))
        (ignore-errors (close s))
        (ignore-errors (sb-bsd-sockets:socket-close sock))))))

;;; ==============================================================================
(banner "0. a session that serves nothing, and the ports it is not on")
;;; ==============================================================================

(defparameter *rfb* 5977)          ; free on this box: 5901/5903/5910 are the live desktops'
(check (port-bindable-p *rfb*) "before anything starts, :~d is free" *rfb*)

(defparameter *session*
  (sb-thread:make-thread
   (lambda () (handler-case (run-session '() :port *rfb* :width 320 :height 240)
                (error (e) (format t "~&session died: ~a~%" e))))
   :name "vnc-cred-gate-session"))
(sleep 2.5)

(defparameter *p* (find-glass-port :port *rfb*))
(defparameter *seat* (port-seat *p*))
(check (and *p* (glass-port-wm-p *p*)) "the session is up and in window-manager mode")
(check (not (seat-serving-p *seat*)) "…and its seat is serving nothing")
(check (not (os-listening-p *rfb*)) "`ss -ltn' shows nothing on :~d" *rfb*)

;;; ==============================================================================
(banner "1. one seat, two wires: a password on the port, none on the socket file")
;;; ==============================================================================

(defparameter *utr* (open-seat-transport *seat* :kind :rfb-unix))
(multiple-value-bind (tr credential note) (serve-seat-vnc *seat* :port-num *rfb*)
  (defparameter *ttr* tr)
  (defparameter *pw* credential)
  (defparameter *note* note))

(check (stringp *pw*) "SERVE-SEAT-VNC minted a credential: ~s (~d chars)" *pw* (length *pw*))
(check (= 8 (length *pw*))
       "…eight characters, because the VNC-auth DES key IS eight bytes — more would be theatre")
(check (every (lambda (c) (find c glass:+vnc-credential-alphabet+)) *pw*)
       "…from the unambiguous alphabet, because somebody has to read it off a screen")
(check (null *note*) "…and nothing had to be decided differently (~a)" *note*)
(check (not (equal *pw* (glass:make-vnc-credential)))
       "two credentials in a row are not the same string")

(check (equal (transport-credential *ttr*) *pw*)
       "the TCP transport carries THAT credential and not a global")
(check (transport-authenticated-p *ttr*) "…so it demands a password")
(check (null (transport-credential *utr*)) "the UNIX transport carries NONE")
(check (not (transport-authenticated-p *utr*)) "…so it demands nothing")
(check (null glass:*vnc-password*)
       "…and GLASS:*VNC-PASSWORD*, the old session-wide global, is still NIL: nothing was ~
        set session-wide to achieve any of this")

(format t "~&   -- what a client is actually offered, on each wire --~%")
(multiple-value-bind (ver types) (security-types :port *rfb*)
  (check (equal ver (format nil "RFB 003.008~%")) "TCP :~d greets with ~s" *rfb* ver)
  (check (equal types '(2)) "…and offers security type 2 (VNC auth) ONLY: ~a" types))
(multiple-value-bind (ver types) (security-types :path (transport-path *utr*))
  (check (equal ver (format nil "RFB 003.008~%")) "the socket file greets identically: ~s" ver)
  (check (equal types '(1))
         "…and offers type 1 (None) — no password, in the SAME session, at the SAME moment: ~a"
         types))

(format t "~&   -- and the password is enforced, not advertised --~%")
(check (eql 1 (try-password "wrongpass" :port *rfb*))
       "a WRONG password gets SecurityResult 1 (rejected)")
(check (eql 0 (try-password *pw* :port *rfb*))
       "the RIGHT password gets SecurityResult 0 (admitted)")

;;; ---- real pixels, through glass's own client ---------------------------------

(banner "1b. …and the right password gets a real screen, not just a 0")

(defparameter *view*
  (glass-client:connect-remote "127.0.0.1" *rfb* :width 320 :height 240 :password *pw*))
(loop repeat 200
      until (and (glass-client:remote-connected-p *view*)
                 (plusp (getf (glass-client:remote-stats *view*) :frames)))
      do (sleep 0.05))
(check (glass-client:remote-connected-p *view*)
       "GLASS-CLIENT is connected over the authenticated port: ~a"
       (glass-client:remote-name *view*))
(check (plusp (getf (glass-client:remote-stats *view*) :frames))
       "…and has taken ~d framebuffer update(s)"
       (getf (glass-client:remote-stats *view*) :frames))
(let* ((fb (glass-client:remote-fb *view*))
       (px (glass:fb-pixels fb))
       (teal (count +wm-teal+ px)))
  (check (and (= (glass:fb-width fb) 320) (= (glass:fb-height fb) 240))
         "…the screen it got is the seat's own ~dx~d"
         (glass:fb-width fb) (glass:fb-height fb))
  (check (> teal (floor (length px) 2))
         "…and REAL PIXELS: ~d of ~d are the workspace's teal" teal (length px)))

(defparameter *wrong-view*
  (glass-client:connect-remote "127.0.0.1" *rfb* :width 320 :height 240 :password "notitmate"))
(sleep 1.5)
(check (not (glass-client:remote-connected-p *wrong-view*))
       "…while the same client with the WRONG password never comes up: ~a"
       (getf (glass-client:remote-stats *wrong-view*) :last-error))
(glass-client:remote-stop *wrong-view*)

;;; ==============================================================================
(banner "2. THE ONE THAT WAS IMPOSSIBLE: authenticated VNC and live VP8 capture, at once")
;;; ==============================================================================
;;; The capture is not a re-implementation for the test — it is webrtc-data's
;;; glass-capture.lisp, the gateway's own file, loaded above and called the way
;;; gateway-nostr.lisp calls it.  It speaks security type None and has no VNC-auth
;;; implementation at all, which is exactly why a session-wide password used to kill it.

(if (not (find-package "WEBRTC-DATA"))
    (check nil "webrtc-data / the gateway's glass-capture.lisp did not load — ~
                the claim that matters CANNOT BE CHECKED in this image")
    (let* ((start (find-symbol "CAPTURE-START" "WEBRTC-DATA"))
           (take (find-symbol "CAPTURE-TAKE" "WEBRTC-DATA"))
           (stop (find-symbol "CAPTURE-STOP" "WEBRTC-DATA"))
           (stats (find-symbol "CAPTURE-STATS" "WEBRTC-DATA"))
           (encode (find-symbol "ENCODE-GRAY-FRAME" "WEBRTC-MEDIA.VP8"))
           (parse (find-symbol "PARSE-KEYFRAME" "WEBRTC-MEDIA.VP8"))
           (cap (funcall start (format nil "unix:~a" (transport-path *utr*)) 0)))
      (check cap "the gateway's CAPTURE-START came up over the socket file while :~d ~
                  demands a password" *rfb*)
      (sleep 1.5)                            ; a frame to arrive and be converted
      (multiple-value-bind (y u v w h) (funcall take cap)
        (check (and y u v) "…and it has a YUV mirror of the desktop")
        (when (and y u v)
          (check (and (= w 320) (= h 240)) "…at the seat's size, ~dx~d" w h)
          (check (> (count-if (lambda (b) (/= b 16)) y) (floor (length y) 2))
                 "…holding real luma and not an empty plane")
          ;; and the whole point: that mirror becomes a VP8 keyframe
          (multiple-value-bind (frame recon) (funcall encode y w h :qi 30 :u u :v v)
            (declare (ignore recon))
            (check (and frame (> (length frame) 100))
                   "…which the REAL VP8 encoder turns into a ~d-byte keyframe" (length frame))
            (let ((info (ignore-errors (funcall parse frame))))
              (check info "…that VP8's own parser reads back: ~a" info)))))
      (format t "~&   -- with the port STILL demanding a password, while the capture runs --~%")
      (check (eql 1 (try-password "wrongpass" :port *rfb*))
             "a wrong password on :~d is still refused" *rfb*)
      (check (eql 0 (try-password *pw* :port *rfb*)) "…and the right one still admitted")
      (check (equal '(1) (nth-value 1 (security-types :path (transport-path *utr*))))
             "…and the socket file is still offering None to the capture")
      (let ((s (funcall stats cap)))
        (check (zerop (getf s :reconnects))
               "…and the capture never had to reconnect: ~a" s))
      (funcall stop cap)))

(glass-client:remote-stop *view*)

;;; ==============================================================================
(banner "3. the root menu: a seat's own item, and it names the port it opened")
;;; ==============================================================================
;;; The items are built by WM-VNC-MENU-ITEMS and appended by WM-OPEN-MENU, closed over
;;; the seat whose menu it is — so this opens the seat's real root menu and runs the real
;;; item, and asks the kernel what happened.

(close-seat-transports *seat*)
(sleep 0.3)
(check (not (os-listening-p *rfb*)) "starting from nothing on :~d" *rfb*)

(defun menu-labels (seat)
  (mapcar #'car (wm-menu-items (seat-menu seat))))
(defun menu-item (seat prefix)
  (find-if (lambda (it) (eql 0 (search prefix (car it)))) (wm-menu-items (seat-menu seat))))

(wm-open-menu *p* 20 20 *seat*)
(check (seat-menu *seat*) "the seat's root menu opened: ~a" (menu-labels *seat*))
(defparameter *start-item* (menu-item *seat* "Serve this seat over VNC"))
(check *start-item* "…and it offers to START serving: ~s" (and *start-item* (car *start-item*)))
(check (null (menu-item *seat* "Stop serving VNC"))
       "…and does NOT offer to stop something that is not happening")

(wm-menu-run *p* (cdr *start-item*) *seat*)
(setf (seat-menu *seat*) nil)
(sleep 0.5)

(check (os-listening-p *rfb*) "picking it opened the port — `ss -ltn' says: ~a" (os-listen-line *rfb*))
(check (not (port-bindable-p *rfb*)) "…and we can no longer bind it")
(defparameter *mtr* (seat-vnc-transport *seat*))
(check *mtr* "…the seat holds a TCP transport: ~a" *mtr*)
(defparameter *mpw* (transport-credential *mtr*))
(check (stringp *mpw*) "…with a credential of its own")
(check (eql 0 (try-password *mpw* :port *rfb*)) "…which really is the one the wire wants")
(check (equal *mpw* (glass:clipboard-text (seat-clipboard *seat*)))
       "…and it is on THIS SEAT's clipboard, ready to paste into a viewer")
(check (find-if (lambda (s) (equal (wm-surface-title s) "VNC")) (glass-port-surfaces *p*))
       "…and a window came up to show it: ~a"
       (mapcar #'wm-surface-title (glass-port-surfaces *p*)))

(wm-open-menu *p* 20 20 *seat*)
(defparameter *stop-item* (menu-item *seat* "Stop serving VNC"))
(check *stop-item* "the menu now offers to STOP, and NAMES THE PORT: ~s"
       (and *stop-item* (car *stop-item*)))
(check (and *stop-item* (search (princ-to-string *rfb*) (car *stop-item*)))
       "…the port number is in the label, so `am I exposed right now' is one glance")
(check (and *stop-item* (not (search "NO PASSWORD" (car *stop-item*))))
       "…and it does not say NO PASSWORD, because there is one")
(check (menu-item *seat* "Show the VNC password")
       "…and the password can be seen again without minting a different one")
(check (null (menu-item *seat* "Serve this seat over VNC"))
       "…while `serve' is gone: one toggle, not two contradicting items")

(wm-menu-run *p* (cdr *stop-item*) *seat*)
(setf (seat-menu *seat*) nil)
(sleep 0.5)

(format t "~&   -- and the operating system agrees it is shut --~%")
(check (not (os-listening-p *rfb*)) "`ss -ltn' shows nothing on :~d" *rfb*)
(check (port-bindable-p *rfb*)
       "…THE PORT IS REBINDABLE.  A bare SOCKET-CLOSE would have left the kernel listening")
(check (null (security-types :port *rfb*)) "…and a connection is refused")
(check (not (transport-open-p *mtr*)) "…the transport says closed too, which is the easy half")
(wm-open-menu *p* 20 20 *seat*)
(check (menu-item *seat* "Serve this seat over VNC") "…and the item offers to start again")
(setf (seat-menu *seat*) nil)

;;; ==============================================================================
(banner "4. a wire with no credential does not get to face the network")
;;; ==============================================================================

(multiple-value-bind (tr credential note) (serve-seat-vnc *seat* :port-num *rfb*
                                                                :address "0.0.0.0"
                                                                :password nil)
  (check (null credential) "asked for NO password…")
  (check (equal (transport-address tr) "127.0.0.1")
         "…and the bind address was overruled to ~a, though 0.0.0.0 was asked for"
         (transport-address tr))
  (check (and note (search "loopback" note)) "…and it says so: ~a" note)
  (check (search "127.0.0.1" (os-listen-line *rfb*))
         "…the kernel agrees: ~a" (os-listen-line *rfb*))
  (check (equal '(1) (nth-value 1 (security-types :port *rfb*)))
         "…it really is open (None) — which is why it may only be local")
  (close-seat-transport tr))
(sleep 0.3)
(check (port-bindable-p *rfb*) ":~d is free again" *rfb*)

;;; ---- and an image that CANNOT verify a password ------------------------------
;;;
;;; In a CHILD PROCESS, because the claim is about an absence: the DES lives in the
;;; optional :glass/vncauth system, and THIS image has it.  A desktop without it that
;;; set a password would not become secure, it would become UNREACHABLE — the handshake
;;; fails closed and rejects everybody — so the menu item must not mint one there.

(banner "…and an image with no DES at all does not pretend to have a credential")

(defparameter *child-src* (format nil "/tmp/glass-vnc-noverify-~d.lisp" (sb-posix:getpid)))
(with-open-file (s *child-src* :direction :output :if-exists :supersede)
  (format s "
(require :asdf)
(load \"~~/quicklisp/setup.lisp\")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:glass :mcclim :mcclim-render :sb-concurrency))   ; NO :glass/vncauth
    (asdf:load-asd \"/home/claude/glass/backend/mcclim-glass.asd\")
    (asdf:load-system :mcclim-glass)))
(in-package :clim-glass)
(setf *vnc-password-file* \"/tmp/glass-no-such-vnc-pass-~d\")
(let* ((p (make-instance 'glass-port :port 5978))
       (a (glass-port-default-seat p)))
  (setf (seat-fb a) (glass:make-framebuffer 64 48))
  (multiple-value-bind (tr cred note) (serve-seat-vnc a :port-num 5978 :address \"0.0.0.0\")
    (format t \"~~&CHILD seal=~~a verifier=~~a addr=~~a cred=~~a note=~~a~~%\"
            (and (find-package \"SEAL\") t) (glass:vnc-auth-available-p)
            (transport-address tr) cred note)
    (close-seat-transport tr)))
(finish-output)
(sb-ext:exit :code 0)
" (sb-posix:getpid)))

(defparameter *child*
  (string-trim '(#\Space #\Newline)
               (sh (format nil "sbcl --non-interactive --disable-debugger --load ~a 2>/dev/null | grep '^CHILD ' || true"
                           *child-src*))))
(format t "    ~a~%" *child*)
(check (search "seal=NIL" *child*)
       "the child image has no seal in it — core glass is still crypto-free")
(check (search "verifier=NIL" *child*) "…so it cannot verify a VNC password")
(check (search "cred=NIL" *child*)
       "…and SERVE-SEAT-VNC did NOT mint one: a password there rejects everybody")
(check (search "addr=127.0.0.1" *child*)
       "…so it bound LOOPBACK, though 0.0.0.0 was asked for")
(check (search "no DES verifier" *child*) "…and said exactly why")
(check (not (find #\~ *child*)) "…in a sentence with no stray format directives in it")
(ignore-errors (delete-file *child-src*))

(banner "…and *SEAT-VNC-ADDRESS* is 0.0.0.0, which is the point of the feature")
(check (equal *seat-vnc-address* "0.0.0.0")
       "*SEAT-VNC-ADDRESS* — every interface, WITH a credential: a loopback-only VNC ~
        listener would be a worse copy of the socket file already sitting there")

;;; ==============================================================================
(banner "5. nothing moved for a desktop that never touches the menu")
;;; ==============================================================================

(check (null glass:*vnc-password*) "GLASS:*VNC-PASSWORD* is still NIL — nothing set it")
(check (equal *seat-bind-address* "0.0.0.0") "*SEAT-BIND-ADDRESS* is unchanged")
(check (eq *seat-transport-kind* :rfb) "*SEAT-TRANSPORT-KIND* is unchanged")

(let ((tr (open-seat-transport *seat* :port-num *rfb*)))
  (check (eq (transport-password tr) :inherit)
         "a transport opened the old way inherits: its password setting is ~s"
         (transport-password tr))
  (check (equal '(1) (nth-value 1 (security-types :port *rfb*)))
         "…so with no session password it offers None, exactly as glass always has")
  ;; The live-settable global still reaches a listener that is ALREADY RUNNING, which is
  ;; what its docstring has always promised and what a per-call capture would have broken.
  (setf glass:*vnc-password* "livesetp")
  (check (equal '(2) (nth-value 1 (security-types :port *rfb*)))
         "…and (setf glass:*vnc-password* …) on a RUNNING listener still takes effect")
  (check (eql 0 (try-password "livesetp" :port *rfb*)) "…and is verified")
  (check (equal (transport-credential tr) "livesetp")
         "…which is what TRANSPORT-CREDENTIAL reports: what a client would MEET")
  ;; …and even then the socket file stays open, because a UNIX transport takes no
  ;; credential at all.  This is the regression that broke video, made impossible.
  (let ((u (open-seat-transport *seat* :kind :rfb-unix)))
    (check (equal '(1) (nth-value 1 (security-types :path (transport-path u))))
           "…while the socket file STILL offers None with a session password set — ~
            the capture cannot be locked out by somebody securing the port")
    (close-seat-transport u))
  (setf glass:*vnc-password* nil)
  (close-seat-transport tr))
(sleep 0.3)

(check (sb-thread:thread-alive-p *session*) "the session ran through all of it")

;;; ==============================================================================
(banner "nothing outside /tmp was written")
;;; ==============================================================================

(close-seat-transports *seat*)
(check (equal *live-before*
              (list (and (probe-file *live-run*) (file-write-date *live-run*))
                    (and (probe-file *live-pass*) (file-write-date *live-pass*))))
       "~~/.glass/run and ~~/.glass-vnc-pass are exactly as they were (~a)" *live-before*)
(check (eql 0 (search "/tmp/" (namestring (glass:runtime-dir))))
       "our runtime dir was under /tmp: ~a" (namestring (glass:runtime-dir)))
(sh (format nil "rm -rf ~a" *rundir*))
(ignore-errors (delete-file *sh-out*))

(format t "~%=> ~:[PASS~;FAIL (~d)~]~%" (plusp *fail*) *fail*)
(finish-output)
(sb-ext:exit :code (if (plusp *fail*) 1 0) :abort t)
