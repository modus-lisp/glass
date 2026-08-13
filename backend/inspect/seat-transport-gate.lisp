;;;; seat-transport-gate.lisp — a seat has a name of its own, and decides what carries it.
;;;;
;;;;   sbcl --control-stack-size 256 --dynamic-space-size 4096 \
;;;;        --non-interactive --load backend/inspect/seat-transport-gate.lisp
;;;;
;;;; docs/seats-and-transports.md makes two claims that this file is here to hold to:
;;;;
;;;;   1. A SEAT HAS AN IDENTITY, persisted, and it is NOT the person's.  Core glass must
;;;;      not have grown a crypto dependency to get it — cl-nostr and ironclad are
;;;;      :glass/nostr's alone — so the check that matters is made IN ANOTHER PROCESS that
;;;;      never loads that system: core loads, seats exist, nothing crypto is present, and
;;;;      the desktop works.  A claim about what an image does NOT contain cannot be made
;;;;      from inside an image that contains it.
;;;;
;;;;   2. SERVING IS A SEAT'S DECISION.  A session can run with nothing listening at all,
;;;;      a seat can open a wire later and close it again, and "nothing was listening"
;;;;      is asked of the OPERATING SYSTEM — `ss -ltn', a bind that succeeds because the
;;;;      port is free, and a connect that is refused — rather than of a slot in a struct.
;;;;      A test that asserted our own intent would have passed against the old code.
;;;;
;;;; NOTHING HERE WRITES OUTSIDE /tmp.  The seat key store is a fixture under /tmp, bound
;;;; before the first seat exists, and both real stores' mtimes are noted and checked at
;;;; the end — a suite that minted into ~/.glass-seats would give the live desktop's seats
;;;; keys they never asked for.

(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:glass :glass/nostr :mcclim :mcclim-render :sb-concurrency))
    (asdf:load-asd "/home/claude/glass/backend/mcclim-glass.asd")
    (asdf:load-system :mcclim-glass)))
(in-package :clim-glass)

(defvar *fail* 0)
(defun check (ok fmt &rest args)
  (format t "  [~:[FAIL~;pass~]] ~?~%" ok fmt args)
  (finish-output)
  (unless ok (incf *fail*)))
(defun banner (s) (format t "~&~%== ~a ==~%" s))

;;; ---- the fixture, and the real stores that must not be touched ---------------

(defparameter *fixture* "/tmp/glass-seat-keys-gate")
(defparameter *live-seats* (namestring (merge-pathnames ".glass-seats" (user-homedir-pathname))))
(defparameter *live-devices* (namestring (merge-pathnames ".glass-devices" (user-homedir-pathname))))
(defparameter *live-before*
  (list (and (probe-file *live-seats*) (file-write-date *live-seats*))
        (and (probe-file *live-devices*) (file-write-date *live-devices*))))

(ignore-errors (delete-file *fixture*))
(setf glass:*seat-key-file* *fixture*)
(clrhash glass:*seat-keys*)

;;; ---- asking the operating system, not ourselves ------------------------------

(defun sh (cmd)
  (with-output-to-string (o)
    (sb-ext:run-program "/bin/sh" (list "-c" cmd) :output o :search nil :error nil)))

(defun os-listening-p (port)
  "Does `ss -ltn' show anything listening on PORT?  The question a person would ask."
  (plusp (length (string-trim '(#\Space #\Newline)
                              (sh (format nil "ss -ltn 2>/dev/null | grep -E '[:.]~d ' || true" port))))))

(defun port-bindable-p (port)
  "Can WE bind PORT?  Succeeds only if nobody is LISTENING on it — SO_REUSEADDR lets a
   bind step over the TIME_WAIT a just-closed connection leaves behind, and does NOT let
   a second listener share a bound port (that is SO_REUSEPORT, which we do not set).  So
   this is `the port is free' asked of the kernel and not of our own bookkeeping.

   The option is not a fudge to make the check pass: without it this would answer `taken'
   for a minute after any client disconnected, which is a fact about a closed connection
   and not about whether anybody can still get in."
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

(defun rfb-greeting (port &optional (host "127.0.0.1"))
  "Connect to PORT and read the RFB version string the server opens with, or NIL if
   nothing would take the connection.  A real client's first twelve bytes."
  (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (handler-case
        (progn
          (sb-bsd-sockets:socket-connect sock (sb-bsd-sockets:make-inet-address host) port)
          (let* ((s (sb-bsd-sockets:socket-make-stream sock :input t :output t
                                                            :element-type '(unsigned-byte 8)
                                                            :buffering :full :timeout 3))
                 (b (make-array 12 :element-type '(unsigned-byte 8))))
            (read-sequence b s)
            (map 'string #'code-char b)))
      (error () nil)
      (:no-error (v) v))))

;;; ==============================================================================
(banner "the fixture is in /tmp, and the live stores are noted")
;;; ==============================================================================

(check (eql 0 (search "/tmp/" glass:*seat-key-file*)) "the seat key store is under /tmp")
(check (not (equal glass:*seat-key-file* *live-seats*)) "…and it is not the real one")

;;; ==============================================================================
(banner "1a. core glass has no crypto — asked of an image that never loaded :glass/nostr")
;;; ==============================================================================
;;; In a CHILD PROCESS, because the claim is about absence and this image has cl-nostr in
;;; it.  The child loads :glass and the McCLIM backend and nothing else, makes a session
;;; and a second seat, and reports what it found.

(defparameter *child-src* "/tmp/glass-core-crypto-free.lisp")
(with-open-file (s *child-src* :direction :output :if-exists :supersede)
  (write-string "
(require :asdf)
(load \"~/quicklisp/setup.lisp\")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:glass :mcclim :mcclim-render :sb-concurrency))
    (asdf:load-asd \"/home/claude/glass/backend/mcclim-glass.asd\")
    (asdf:load-system :mcclim-glass)))
(in-package :clim-glass)
;; a whole desktop session, in an image with no crypto in it at all
(let* ((p (make-instance 'glass-port :port 5969))
       (a (glass-port-default-seat p))
       (b (add-seat p :name \"child-B\" :port-num 5970 :width 320 :height 240)))
  (setf (seat-fb a) (glass:make-framebuffer 320 240))
  (format t \"~&CHILD nostr-keys=~a ironclad=~a secp=~a seats=~d identity-a=~a npub-a=~a serving=~a~%\"
          (and (find-package \"CL-NOSTR.KEYS\") t)
          (and (find-package \"IRONCLAD\") t)
          (and (find-package \"SECP256K1-FAST\") t)
          (length (glass-port-seats p))
          (seat-identity a) (seat-npub b) (seat-serving-p a)))
(finish-output)
(sb-ext:exit :code 0)
" s))

(defparameter *child*
  (string-trim '(#\Space #\Newline)
               (sh (format nil "sbcl --non-interactive --disable-debugger --load ~a 2>/dev/null | grep '^CHILD ' || true"
                           *child-src*))))
(format t "    ~a~%" *child*)
(check (search "nostr-keys=NIL" *child*)
       "CL-NOSTR is not in an image that loaded core glass + the McCLIM backend")
(check (search "ironclad=NIL" *child*) "…nor is IRONCLAD")
(check (search "secp=NIL" *child*) "…nor is any secp256k1 implementation")
(check (search "seats=2" *child*) "…and that image still makes a session with two seats")
(check (search "identity-a=NIL" *child*)
       "a seat there has NO identity — which is an ordinary desktop, not a broken one")
(check (search "npub-a=NIL" *child*) "…and asking for its npub answers NIL rather than signalling")
(check (search "serving=NIL" *child*) "…and it is serving nothing, because nobody asked it to")

;;; ==============================================================================
(banner "1b. two seats, two identities — and the same seat keeps its name")
;;; ==============================================================================

(defparameter *port* (make-instance 'glass-port :port 5964))
(setf (glass-port-wm-p *port*) t)
(defparameter *a* (glass-port-default-seat *port*))
(setf (seat-fb *a*) (glass:make-framebuffer 320 240 +wm-teal+)
      (seat-screen-w *a*) 320 (seat-screen-h *a*) 240)
(defparameter *b* (add-seat *port* :name "seat-desk" :port-num 5965 :width 320 :height 240
                                   :fb (glass:make-framebuffer 320 240 +wm-teal+)))

(check (and (seat-identity *a*) (seat-identity *b*)) "both seats have an identity")
(check (let ((n (seat-npub *a*))) (and (stringp n) (eql 0 (search "npub1" n))))
       "…and it is an npub: ~a" (seat-npub *a*))
(check (not (equal (seat-npub *a*) (seat-npub *b*)))
       "TWO SEATS HAVE TWO IDENTITIES~%         A: ~a~%         B: ~a"
       (seat-npub *a*) (seat-npub *b*))
(check (not (equal (glass:seat-identity-secret (seat-identity *a*))
                   (glass:seat-identity-secret (seat-identity *b*))))
       "…two keys, not one key with two names")
(check (not (search (glass:seat-identity-secret (seat-identity *a*))
                    (princ-to-string (seat-identity *a*))))
       "a seat identity does not print its secret")

;; PERSISTENCE.  Wipe every trace of the key from this process, then make the seat again:
;; if the npub comes back it came from the FILE, which is the claim.
(defparameter *b-npub* (seat-npub *b*))
(defparameter *b-pub* (glass:seat-identity-pubkey (seat-identity *b*)))
(check (with-open-file (s *fixture*)
         (loop for line = (read-line s nil) while line
               thereis (eql 0 (search "seat-desk " line))))
       "the key is on disk, one `<seat-name> <secret-hex>' line")
(clrhash glass:*seat-keys*)
(defparameter *port2* (make-instance 'glass-port :port 5966))
(defparameter *b2* (add-seat *port2* :name "seat-desk" :port-num 5967 :width 320 :height 240))
(check (equal *b-npub* (seat-npub *b2*))
       "A SEAT RECREATED FROM NOTHING IN MEMORY IS THE SAME SEAT: ~a" *b-npub*)
(check (equal *b-pub* (glass:seat-identity-pubkey (seat-identity *b2*)))
       "…down to the pubkey, so it is one key and not two that agree about a prefix")
(check (not (equal *b-npub* (seat-npub (glass-port-default-seat *port2*))))
       "…while a DIFFERENTLY named seat in the same store is still somebody else")

;; the distinction the note says is load-bearing
(check (not (equal glass:*seat-key-file* glass:*enrolment-file*))
       "seat keys and person enrolments are different stores — a place is not a person")
(check (null (glass:seat-identity-known "nobody-has-ever-sat-here"))
       "asking whether a seat has a key does not mint one")

;;; ==============================================================================
(banner "2. a session that serves nothing")
;;; ==============================================================================
;;; RUN-SESSION is RUN-WM without the transport.  It blocks (it IS the desktop), so it
;;; runs in a thread and everything below is done to it from outside, which is also how a
;;; control socket would reach a live desktop.

(defparameter *rfb* 5968)          ; free on this box; nothing of ours or anybody's is here
(check (port-bindable-p *rfb*) "before anything starts, :~d is free" *rfb*)

(defparameter *session*
  (sb-thread:make-thread
   (lambda () (handler-case (run-session '() :port *rfb* :width 320 :height 240
                                             :menu (list (cons "Nothing" (lambda () nil))))
                (error (e) (format t "~&session died: ~a~%" e))))
   :name "gate-session"))
(sleep 2.5)                        ; the CLIM event loop and the first composite

(defparameter *p* (find-glass-port :port *rfb*))
(defparameter *seat* (port-seat *p*))
(check (and *p* (glass-port-wm-p *p*)) "the session is up and in window-manager mode")
(check (glass:framebuffer-p (seat-fb *seat*)) "…its home seat has a screen")
(check (null (seat-transports *seat*)) "…and holds no transport")
(check (not (seat-serving-p *seat*)) "…so it is serving nothing")

(format t "~&   -- and the operating system agrees --~%")
(check (not (os-listening-p *rfb*)) "`ss -ltn' shows nothing on :~d" *rfb*)
(check (port-bindable-p *rfb*) "…the port is free: WE can bind it while the session runs")
(check (null (rfb-greeting *rfb*)) "…and a connection to it is refused")

;;; ---- the seat opens one ------------------------------------------------------

(banner "…the seat opens a transport")
(defparameter *tr* (open-seat-transport *seat* :port-num *rfb*))
(check (typep *tr* 'seat-transport) "OPEN-SEAT-TRANSPORT returned a transport: ~a" *tr*)
(check (transport-open-p *tr*) "…it is open")
(check (seat-serving-p *seat*) "…and the seat is now serving")
(check (os-listening-p *rfb*) "`ss -ltn' shows :~d listening" *rfb*)
(check (not (port-bindable-p *rfb*)) "…the port is taken: we can no longer bind it")
(let ((g (rfb-greeting *rfb*)))
  (check (and g (eql 0 (search "RFB " g)))
         "…and a real client connecting gets glass's RFB greeting: ~s" g))
(check (eq (seat-server *seat*) (transport-thread *tr*))
       "GLASS-PORT-SERVER still means `the thread serving this screen'")
(check (eq *tr* (open-seat-transport *seat* :port-num *rfb*))
       "asking twice for the same wire returns the same one rather than fighting for the port")

;;; ---- and closes it again -----------------------------------------------------

(banner "…and closes it again")
(check (close-seat-transport *tr*) "CLOSE-SEAT-TRANSPORT reports it closed one")
(check (not (transport-open-p *tr*)) "…the transport is closed")
(check (not (seat-serving-p *seat*)) "…the seat is serving nothing again")
(check (null (seat-transports *seat*)) "…and holds no transport")
(check (null (seat-server *seat*)) "…and GLASS-PORT-SERVER is nil, as it was at the start")

(format t "~&   -- and the operating system agrees again --~%")
(check (not (os-listening-p *rfb*)) "`ss -ltn' shows nothing on :~d" *rfb*)
(check (port-bindable-p *rfb*)
       "…THE PORT IS FREE AGAIN.  A bare SOCKET-CLOSE would have left it listening here")
(check (null (rfb-greeting *rfb*)) "…and a connection is refused")
(check (not (sb-thread:thread-alive-p (transport-thread *tr*)))
       "…and the accept loop is gone, not parked on a dead descriptor")

;;; ---- the session never noticed -----------------------------------------------

(banner "…and the session ran through all of it")
(check (sb-thread:thread-alive-p *session*) "the session is still running")
(setf (glass-port-menu-items *p*) (list (cons "Still here" (lambda () nil))))
(glass-on-pointer *p* 4 40 40 *seat*)                 ; right-press the workspace
(sleep 0.2)
(check (seat-menu *seat*) "…and still answers its seat's hands: a root menu opened")
(glass-on-pointer *p* 0 40 40 *seat*)
(setf (seat-menu *seat*) nil)

;; a second wire, on a different port, to the same seat — and shut down cleanly
(defparameter *tr2* (open-seat-transport *seat* :port-num 5971))
(check (and (os-listening-p 5971) (not (os-listening-p *rfb*)))
       "the same seat re-exposed on a DIFFERENT port: :5971 up, :~d still down" *rfb*)
(check (= 1 (close-seat-transports *seat*)) "CLOSE-SEAT-TRANSPORTS closed the one it had")
(check (and (not (os-listening-p 5971)) (port-bindable-p 5971)) ":5971 is free again")

;;; ---- the bind address is a parameter, and its default is unchanged ------------

(banner "the bind address is explicit — and defaults to exactly what it was")
(check (equal *seat-bind-address* "0.0.0.0")
       "*SEAT-BIND-ADDRESS* is 0.0.0.0 — every interface, as glass has always bound")
(let ((tr (open-seat-transport *seat* :port-num 5971 :address "127.0.0.1")))
  (check (equal (transport-address tr) "127.0.0.1") "a transport can be asked for loopback only")
  (check (plusp (length (sh "ss -ltn 2>/dev/null | grep '127.0.0.1:5971' || true")))
         "…and `ss' shows it bound to 127.0.0.1 and not to 0.0.0.0")
  (close-seat-transport tr))

;;; ---- and the same seat on a wire that is a FILE -------------------------------
;;;
;;; A UNIX-domain transport is a SIBLING of the TCP one — the same RFB, the same seat, the
;;; same hands — and the whole of the difference is who the kernel lets near it.  A port on
;;; 127.0.0.1 is reachable by every process of every uid on this box; a socket file at mode
;;; 0600 is reachable by its owner, decided by the kernel on connect().  So the checks are
;;; the same ones asked of the port, plus the one a port cannot answer at all: WHO.

(banner "a seat can be carried by a socket file instead — and by both at once")

(defparameter *sock-dir* (format nil "/tmp/glass-seat-sockets-~d/" (sb-posix:getpid)))
(setf glass:*runtime-dir* *sock-dir*)          ; under /tmp: never a real desktop's runtime dir

(defun unix-listening-p (path)
  (plusp (length (string-trim '(#\Space #\Newline)
                              (sh (format nil "ss -lxH 2>/dev/null | grep ~a || true" path))))))

(defun unix-rfb-greeting (path)
  "The twelve bytes an RFB server opens with, off a SOCKET FILE.  Same client, same read."
  (handler-case
      (multiple-value-bind (sock stream) (glass:open-connection :path path :timeout 3)
        (unwind-protect
             (let ((b (make-array 12 :element-type '(unsigned-byte 8))))
               (read-sequence b stream)
               (map 'string #'code-char b))
          (ignore-errors (close stream))
          (ignore-errors (sb-bsd-sockets:socket-close sock))))
    (error () nil)))

(defparameter *utr* (open-seat-transport *seat* :kind :rfb-unix))
(check (eq (transport-kind *utr*) :rfb-unix) "the transport says what it is: ~a" (transport-kind *utr*))
(check (equal (transport-path *utr*) (seat-socket-path *seat*))
       "…and where: ~a — named after the SEAT, because a socket file's name is ours to choose"
       (transport-path *utr*))
(check (eql #o600 (logand (sb-posix:stat-mode (sb-posix:stat (transport-path *utr*))) #o777))
       "…at mode 0600: owner-only, checked by the kernel on connect(), not by us")
(check (unix-listening-p (transport-path *utr*)) "`ss -x' shows it listening")
(check (equal (unix-rfb-greeting (transport-path *utr*)) (format nil "RFB 003.008~%"))
       "…and a real client on it gets RFB 003.008 — the seat, on a wire nobody else can open")

;; Both at once: same seat, same screen, same pair of hands, two doors.
(defparameter *ttr* (open-seat-transport *seat* :port-num 5971 :address "127.0.0.1"))
(check (and (os-listening-p 5971) (unix-listening-p (transport-path *utr*)))
       "the seat holds BOTH: a port and a socket file, side by side")
(check (equal (rfb-greeting 5971) (unix-rfb-greeting (transport-path *utr*)))
       "…and the two wires answer identically — RFB cannot tell what is carrying it")
(check (= 2 (length (seat-transports *seat*))) "…and the seat knows it has two")

(let ((path (transport-path *utr*)))
  (check (= 2 (close-seat-transports *seat*)) "CLOSE-SEAT-TRANSPORTS closes both")
  (check (not (unix-listening-p path)) "…`ss -x' shows nothing")
  (check (null (probe-file path))
         "…the socket FILE is unlinked, or the next bind would fail with EADDRINUSE")
  (check (null (unix-rfb-greeting path)) "…and a connection is refused")
  (check (not (os-listening-p 5971)) "…and the port is down too"))

(banner "…and the default did not move")
(check (eq *seat-transport-kind* :rfb)
       "*SEAT-TRANSPORT-KIND* is :RFB — a TCP port, exactly as before")
(let ((tr (open-seat-transport *seat* :port-num 5971)))
  (check (and (eq (transport-kind tr) :rfb) (os-listening-p 5971))
         "…so a seat asked for a transport with nothing said still gets a port")
  (close-seat-transport tr))
(sh (format nil "rm -rf ~a" *sock-dir*))

;;; ==============================================================================
(banner "nothing outside /tmp was written")
;;; ==============================================================================

(check (equal *live-before*
              (list (and (probe-file *live-seats*) (file-write-date *live-seats*))
                    (and (probe-file *live-devices*) (file-write-date *live-devices*))))
       "~~/.glass-seats and ~~/.glass-devices are exactly as they were (~a)" *live-before*)
(ignore-errors (delete-file *fixture*))
(ignore-errors (delete-file *child-src*))

(format t "~%=> ~:[PASS~;FAIL (~d)~]~%" (plusp *fail*) *fail*)
(finish-output)
(sb-ext:exit :code (if (plusp *fail*) 1 0) :abort t)
