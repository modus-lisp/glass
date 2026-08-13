;;;; socket.lisp — what carries a wire: a port anybody on the box can reach, or a file
;;;; only its owner can open.
;;;;
;;;; Every loopback socket in this system was plain TCP, which means the access control on
;;;; the desktop's screen, its microphone, its mix, its admission service and its EVAL
;;;; socket was "you must already be on this machine".  On a box with one user that reads
;;;; like a boundary.  It is not one: `127.0.0.1' is reachable by every process of every
;;;; uid on the host, so any account — a compromised service, a shared build user, a
;;;; container sharing the netns — could connect to :4013 and evaluate a form as us.
;;;;
;;;; A UNIX-DOMAIN socket is the same stream with a different name.  What changes is who
;;;; the kernel lets near it:
;;;;
;;;;   1. IT IS A FILE.  connect(2) requires WRITE permission on the socket inode, checked
;;;;      by the kernel like any other open — so mode 0600 in a mode 0700 directory is
;;;;      owner-only, ENFORCED, not hoped for.  (Root bypasses it, as root bypasses
;;;;      everything; a root that wants in can ptrace the process instead.)
;;;;   2. SO_PEERCRED.  The server can ask the kernel who connected — uid, gid and pid of
;;;;      the peer process, filled in at connect() time and unforgeable by the peer.  That
;;;;      is peer authentication with no crypto, no handshake and no shared secret, and it
;;;;      is the thing a TCP loopback socket can never have.
;;;;   3. There is no network stack to bind, scan, or expose by editing an address.
;;;;
;;;; TCP IS NOT GOING ANYWHERE AND IS STILL THE DEFAULT.  A transport kind is a sibling,
;;;; not a replacement: LISTENER has two subclasses and everything above it — RFB, the
;;;; audio mix, the microphone, admission — is a stream protocol that cannot tell which it
;;;; got.  Nothing in this file changes what an existing caller does.

(in-package #:glass)

;;; ---- where socket files live -------------------------------------------------
;;;
;;; A path, not a port, so the question "where" is now ours to answer.  In order:
;;;
;;;   $GLASS_RUNTIME_DIR   — explicit, and what a test sets so it writes only under /tmp.
;;;   $XDG_RUNTIME_DIR/glass/  — the right answer WHEN IT IS REALLY OURS.  It is a
;;;       tmpfs the login session owns at mode 0700, cleaned up when the last session of
;;;       that user ends, so a socket there cannot outlive a reboot and become a stale
;;;       file somebody has to reason about.
;;;   ~/.glass/run/        — the fallback, and on this box it is the one that gets used.
;;;
;;; THE OWNERSHIP CHECK IS NOT DEFENSIVE PARANOIA, it is this machine: $XDG_RUNTIME_DIR
;;; here is `/run/user/0' — ROOT's — inherited from whatever started the session, while
;;; /run/user/1001 does not exist at all.  Trusting the variable would put the desktop's
;;; sockets in root's directory (in practice: EACCES, and a "why won't it start" hunt).
;;; A runtime dir is only a runtime dir if we own it.
;;;
;;; Why not /tmp: it is world-writable and shared, so the parent directory is somebody
;;; else's to race, and a tmp-cleaner that removes an idle socket file takes the listener
;;; with it.  ~ is the same place ~/.glass-devices and ~/.glass-seats already live.

(defparameter *runtime-dir* nil
  "Directory for socket files, or NIL to decide per RUNTIME-DIR.  A string or pathname.")

(defparameter *socket-file-mode* #o600
  "Mode a UNIX listener's socket file is created with: owner read/write, nobody else.
   connect(2) needs write permission on it, so this is the access control.")

(defparameter *socket-dir-mode* #o700
  "Mode the socket directory is created with.  It matters INDEPENDENTLY of the file's:
   traversal is checked too, so a wrong-moded socket in a 0700 directory is still
   unreachable, which is what closes the window between bind() and chmod().")

(defun %as-directory (s)
  (if (and (plusp (length s)) (char= (char s (1- (length s))) #\/)) s (concatenate 'string s "/")))

(defun %dir-ours-p (dir)
  "Does DIR exist, as a directory, owned by this uid?  NIL for anything else — including
   the case that actually happens, a runtime dir belonging to another user."
  (handler-case
      (let ((st (sb-posix:stat (string-right-trim "/" (namestring dir)))))
        (and (sb-posix:s-isdir (sb-posix:stat-mode st))
             (eql (sb-posix:stat-uid st) (sb-posix:getuid))))
    (error () nil)))

(defun runtime-dir ()
  "The directory socket files go in, as a pathname, CREATED (mode 0700) if it is missing.

   The directory's mode is half the access control and the half with no race in it: bind()
   creates the socket file with the process umask, so there is a moment before the chmod
   in which the file could be too open.  There is no moment in which its DIRECTORY is."
  (let* ((explicit (or *runtime-dir* (sb-ext:posix-getenv "GLASS_RUNTIME_DIR")))
         (xdg (sb-ext:posix-getenv "XDG_RUNTIME_DIR"))
         (dir (cond (explicit (pathname (%as-directory (namestring explicit))))
                    ((and xdg (plusp (length xdg)) (%dir-ours-p xdg))
                     (merge-pathnames "glass/" (pathname (%as-directory xdg))))
                    (t (merge-pathnames ".glass/run/" (user-homedir-pathname))))))
    (ensure-directories-exist dir)
    (ignore-errors (sb-posix:chmod (string-right-trim "/" (namestring dir)) *socket-dir-mode*))
    dir))

(defun socket-sibling (path type)
  "The socket file beside PATH, named for TYPE: `…/seat-0.rfb' -> `…/seat-0.audio'.

   A DESKTOP AND THE PROCESS THAT CONNECTS TO IT MUST AGREE ON FOUR PATHS, and there are
   only two honest ways to make them: type all four into both, or derive three from one.
   Typed-in copies drift, which is the whole reason both ends of glass-audio/1 and
   glass-mic/1 live in this repository rather than one each side of the wire.  So: derive,
   from the one path a launcher and a gateway both already have to name.

   It is the socket-file reading of a convention that already exists.  A seat's audio port
   is its screen port + 10 and its microphone + 11 (*AUDIO-PORT-OFFSET*), read as arithmetic
   rather than as a number typed into a startup script.  Arithmetic on a path is a name in
   the same directory, and this is it."
  (namestring (make-pathname :type (string-downcase (string type)) :defaults (pathname path))))

(defconstant +sun-path-max+ 107
  "sun_path is 108 bytes including the terminator — a hard kernel limit, not a convention.
   A path over it is silently truncated by some libcs and rejected by others, so we reject
   it ourselves, where the message can say which path and how long.")

(defun socket-path (name &optional (dir (runtime-dir)))
  "The path of the socket file called NAME — `(socket-path \"rfb\")' is the whole of what a
   caller needs to say to get a private, owner-only wire in the right place."
  (let ((path (namestring (merge-pathnames name dir))))
    (when (> (length path) +sun-path-max+)
      (error "socket path is ~d bytes, over the ~d-byte sun_path limit: ~a"
             (length path) +sun-path-max+ path))
    path))

;;; ---- who is on the other end -------------------------------------------------
;;;
;;; SO_PEERCRED, and it is the prize.  sb-bsd-sockets does not expose it — it has
;;; SOCKOPT-PASS-CREDENTIALS (SO_PASSCRED, the ancillary-message half) and nothing for
;;; SO_PEERCRED — so this is a raw getsockopt through sb-alien, which is the same seam
;;; SOCKET-UNSENT-BYTES already uses for SIOCOUTQ.
;;;
;;; `struct ucred' is { pid_t pid; uid_t uid; gid_t gid; } — three 32-bit ints on every
;;; Linux ABI we run on, in THAT order (pid first, which is easy to get backwards).  The
;;; kernel fills it at connect() time from the connecting process's credentials; the peer
;;; cannot choose what it says, which is exactly why this is authentication and a
;;; self-reported name in a protocol header is not.
;;;
;;; It is a UNIX-socket property.  On TCP it returns NIL — there is no answer, and a
;;; caller must be able to tell "the peer is uid 1001" from "there is no such thing here".

(defconstant +sol-socket+ 1)
(defconstant +so-peercred+ 17 "Linux SO_PEERCRED (x86-64/arm64/riscv; alpha, mips, parisc and sparc differ).")

(defun socket-fd (thing)
  "The file descriptor behind a socket, a stream made from one, or a number."
  (typecase thing
    (integer thing)
    (sb-bsd-sockets:socket (sb-bsd-sockets:socket-file-descriptor thing))
    (sb-sys:fd-stream (sb-sys:fd-stream-fd thing))
    (t nil)))

(defun peer-credentials (thing)
  "(values UID GID PID) of the process at the other end, or NIL if there is no such fact.

   NIL means the question does not apply (a TCP socket, a closed one, a kernel without
   SO_PEERCRED) — never `unknown but probably fine'.  A caller deciding whether to talk to
   somebody must be able to tell an answer from the absence of one."
  (let ((fd (socket-fd thing)))
    (when fd
      (handler-case
          (sb-alien:with-alien ((buf (sb-alien:array sb-alien:int 3))
                                (len sb-alien:int 12))
            (if (zerop (sb-alien:alien-funcall
                        (sb-alien:extern-alien "getsockopt"
                          (function sb-alien:int sb-alien:int sb-alien:int sb-alien:int
                                    (* t) (* sb-alien:int)))
                        fd +sol-socket+ +so-peercred+
                        (sb-alien:cast (sb-alien:addr buf) (* t)) (sb-alien:addr len)))
                ;; struct ucred: pid, uid, gid — returned uid-first because that is the
                ;; order a caller asks in (`who is this, and may they?').
                ;;
                ;; ON A TCP SOCKET THIS SYSCALL SUCCEEDS AND ANSWERS NOTHING: Linux returns
                ;; pid=0, uid=-1, gid=-1 rather than failing.  A caller that trusted the
                ;; return code would get a credential of uid 4294967295 and treat it as a
                ;; fact.  pid 0 is never a process that connected to anything, so it is the
                ;; tell, and NIL is what it turns into.
                (let ((pid (sb-alien:deref buf 0)))
                  (when (plusp pid)
                    (values (sb-alien:deref buf 1) (sb-alien:deref buf 2) pid)))
                nil))
        (error () nil)))))

(defparameter *peer-policy* :same-uid
  "Who may connect to a UNIX listener, checked after accept():

     :SAME-UID — this uid, or root.  The default.
     :ANY      — anybody the file mode already let through.
     a function of (UID GID PID) — a caller's own rule.

   Root is admitted because refusing it buys nothing: a root that wants this process's
   memory has ptrace, /proc/mem and the socket file's mode bits anyway, and refusing it
   only breaks an operator who reached for sudo.

   This is BELT AND BRACES over the file mode, and it is not redundant: the mode is a
   property of a file somebody could chmod, and this is asked of the kernel per
   connection, of the peer that actually turned up.")

(defun peer-allowed-p (policy uid gid pid)
  (cond ((null uid) t)                            ; no credentials to judge — not a UNIX peer
        ((eq policy :any) t)
        ((functionp policy) (and (funcall policy uid gid pid) t))
        ((eq policy :same-uid) (or (eql uid (sb-posix:getuid)) (eql uid 0)))
        (t t)))

(defun peer-name (sock)
  "A short name for whoever connected, for a log line: `1.2.3.4:5555' on TCP, and on a UNIX
   socket the thing that is actually true and actually useful — WHICH PROCESS."
  (or (multiple-value-bind (uid gid pid) (peer-credentials sock)
        (declare (ignore gid))
        (and uid (format nil "uid=~d pid=~d" uid pid)))
      (ignore-errors
       (multiple-value-bind (addr port) (sb-bsd-sockets:socket-peername sock)
         (format nil "~{~d~^.~}:~d" (coerce addr 'list) port)))
      "peer"))

;;; ---- listeners ---------------------------------------------------------------
;;;
;;; One abstraction, two kinds, and the protocols above it cannot tell which they are on.
;;; A class rather than a struct for the reason everything long-lived here is one: a
;;; desktop runs for weeks and a redefined DEFSTRUCT strands the instances already made.

(defclass listener ()
  ((socket  :initarg :socket :accessor listener-socket
            :documentation "The listening socket, held HERE and not only inside the parked
             accept, because a listener you cannot name is a listener you cannot close.")
   (backlog :initarg :backlog :initform 8 :reader listener-backlog))
  (:documentation "Something accepting connections: a TCP port, or a socket file."))

(defclass tcp-listener (listener)
  ((address :initarg :address :initform "0.0.0.0" :reader listener-address)
   (port    :initarg :port    :initform 0         :reader listener-port))
  (:documentation "A port.  Reachable by anything that can route to ADDRESS — on
   127.0.0.1, that is every process of every user on this box."))

(defclass unix-listener (listener)
  ((path  :initarg :path  :reader listener-path)
   (inode :initarg :inode :initform nil :reader listener-inode
          :documentation "st_ino of the socket file WE created.  Unlinking on close is
           checked against it, so a listener that closes late cannot delete the file a
           successor has already bound at the same path.")
   (mode  :initarg :mode  :initform #o600 :reader listener-mode)
   (peer-policy :initarg :peer-policy :initform :same-uid :accessor listener-peer-policy)
   (refused :initform 0 :accessor listener-refused))
  (:documentation "A socket file.  Reachable by whoever the kernel says may write to it,
   and its peers are identifiable — see PEER-CREDENTIALS."))

(defgeneric listener-kind (l)
  (:method ((l tcp-listener)) :tcp)
  (:method ((l unix-listener)) :unix)
  (:method ((l sb-bsd-sockets:socket)) :tcp))

(defgeneric listener-endpoint (l)
  (:documentation "What this listener is, in one phrase, for a log line.")
  (:method ((l tcp-listener)) (format nil "port ~d" (listener-port l)))
  (:method ((l unix-listener)) (listener-path l))
  (:method ((l sb-bsd-sockets:socket))
    (or (ignore-errors (multiple-value-bind (addr port) (sb-bsd-sockets:socket-name l)
                         (declare (ignore addr))
                         (format nil "port ~d" port)))
        "a socket")))

(defgeneric listener-open-p (l)
  (:method ((l listener)) (and (listener-socket l) t))
  (:method ((l sb-bsd-sockets:socket)) t)
  (:method ((l null)) nil))

(defmethod print-object ((l listener) stream)
  (print-unreadable-object (l stream :type t)
    (format stream "~a~:[ closed~;~]" (listener-endpoint l) (listener-open-p l))))

;;; ---- opening one -------------------------------------------------------------

(defun tcp-listen (port &key (backlog 8) (address "0.0.0.0"))
  "A listening TCP socket bound to PORT.

   ADDRESS defaults to 0.0.0.0 — every interface — which is what it has always been and
   what every caller in this tree gets unless it says otherwise.  It is a PARAMETER and
   not a constant so that a caller who wants \"127.0.0.1\" can say so in one place; see
   CLIM-GLASS:OPEN-SEAT-TRANSPORT, which passes it through for exactly that reason.

   Returns the RAW SOCKET, not a LISTENER, and goes on doing so on purpose: callers
   outside this tree hold what it returns and call SOCKET-ACCEPT on it themselves.
   OPEN-LISTENER is the one that gives you the object."
  (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address sock) t)
    (sb-bsd-sockets:socket-bind sock (sb-bsd-sockets:make-inet-address address) port)
    (sb-bsd-sockets:socket-listen sock backlog)
    sock))

(defvar *unix-listeners* '() "Every open UNIX listener, so an exiting image can unlink them.")
(defvar *unix-listeners-lock* (sb-thread:make-mutex :name "glass-unix-listeners"))

(defun %socket-file-p (path)
  (handler-case (sb-posix:s-issock (sb-posix:stat-mode (sb-posix:stat path)))
    (error () nil)))

(defun unix-socket-live-p (path)
  "Is somebody ACCEPTING on PATH right now?  The only honest way to ask: connect to it.

   A socket file left behind by a process that died tells you nothing by existing — it is
   the same inode whether the server is alive or was SIGKILLed a week ago.  connect()
   distinguishes them: ECONNREFUSED means the file has no listener behind it and is stale;
   a successful connect means somebody is there and this path is taken."
  (let ((sock (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (unwind-protect
         (handler-case (progn (sb-bsd-sockets:socket-connect sock path) t)
           (sb-bsd-sockets:connection-refused-error () nil)
           (error () :unknown))              ; EACCES, ENOENT, … — not ours to clear
      (ignore-errors (sb-bsd-sockets:socket-close sock)))))

(defun clear-stale-socket (path)
  "Unlink PATH if it is a socket file with nobody behind it.  Returns T if it removed one.

   THIS IS THE UNIX-SOCKET VERSION OF THE TRAP CLOSE-LISTENER DOCUMENTS.  bind() will not
   reuse a path that exists — EADDRINUSE, whether or not anything is listening — so a
   server killed with -9, or an image that died, leaves a file that makes the NEXT start
   fail.  There is no SO_REUSEADDR for this; the only fix is to remove it, and the only
   safe way to decide is to try to connect first."
  (and (probe-file path)
       (%socket-file-p path)
       (null (unix-socket-live-p path))
       (progn (ignore-errors (sb-posix:unlink path)) t)))

(defun unix-listen (path &key (backlog 8) (mode *socket-file-mode*) (peer-policy *peer-policy*))
  "A listening UNIX socket at PATH, owner-only.  Returns a UNIX-LISTENER.

   THE ORDER HERE IS THE SECURITY.  bind() creates the socket file with 0777 &~ umask —
   which on a default umask is 0755, readable and CONNECTABLE by everyone — so there is a
   window between creating it and tightening it.  chmod BEFORE listen() closes the window
   completely rather than narrowing it: a bound socket that is not yet listening refuses
   every connect with ECONNREFUSED, so during the only moment the mode is wrong there is
   nothing to connect to.  (The 0700 directory would cover it anyway; two independent
   reasons, because this is the property the whole change exists for.)"
  (ensure-directories-exist (directory-namestring path))
  (clear-stale-socket path)
  (let ((sock (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (handler-bind ((error (lambda (e) (declare (ignore e))
                            (ignore-errors (sb-bsd-sockets:socket-close sock)))))
      (sb-bsd-sockets:socket-bind sock path)
      (sb-posix:chmod path mode)                       ; before listen: see above
      (sb-bsd-sockets:socket-listen sock backlog))
    (let ((l (make-instance 'unix-listener :socket sock :path path :backlog backlog
                                           :mode mode :peer-policy peer-policy
                                           :inode (ignore-errors (sb-posix:stat-ino (sb-posix:stat path))))))
      (sb-thread:with-mutex (*unix-listeners-lock*) (push l *unix-listeners*))
      l)))

(defun open-listener (kind &key port (address "0.0.0.0") path name (backlog 8)
                                (mode *socket-file-mode*) (peer-policy *peer-policy*))
  "Open a listener of KIND — :TCP on PORT at ADDRESS, or :UNIX at PATH (or NAME, resolved
   in the runtime directory).  Returns a LISTENER either way.

   The two are SIBLINGS, not a case and a special case: everything above this — RFB, the
   mix, the microphone, admission, the control socket — is a stream protocol that is
   handed a stream and never asks how it was made."
  (ecase kind
    (:tcp (make-instance 'tcp-listener :socket (tcp-listen port :backlog backlog :address address)
                                       :address address :port port :backlog backlog))
    ((:unix :local)
     (unix-listen (or path (socket-path (or name (error "a :unix listener needs a PATH or a NAME"))))
                  :backlog backlog :mode mode :peer-policy peer-policy))))

;;; ---- accepting on one --------------------------------------------------------

(defgeneric listener-accept (l)
  (:documentation "Block until somebody connects; return the accepted SOCKET.

   A UNIX listener applies its peer policy HERE and loops, so a refused peer is invisible
   to the protocol above — it never sees a stream it must then decide about, and an accept
   loop that has no idea unix sockets exist does not have to grow a rejection path."))

(defmethod listener-accept ((l tcp-listener))
  (let ((sock (sb-bsd-sockets:socket-accept (listener-socket l))))
    ;; TCP_NODELAY: see ACCEPT-STREAM.  Set at accept so every consumer of a listener gets
    ;; it, not only the ones that remembered.
    (when sock (ignore-errors (setf (sb-bsd-sockets:sockopt-tcp-nodelay sock) t)))
    sock))

(defmethod listener-accept ((l sb-bsd-sockets:socket))
  (let ((sock (sb-bsd-sockets:socket-accept l)))
    (when sock (ignore-errors (setf (sb-bsd-sockets:sockopt-tcp-nodelay sock) t)))
    sock))

(defmethod listener-accept ((l unix-listener))
  (loop
    (let ((sock (sb-bsd-sockets:socket-accept (listener-socket l))))
      (when (null sock) (return nil))
      (multiple-value-bind (uid gid pid) (peer-credentials sock)
        (if (peer-allowed-p (listener-peer-policy l) uid gid pid)
            (return sock)
            (progn
              (incf (listener-refused l))
              (ignore-errors
               (format *error-output* "~&glass: ~a refused uid=~a gid=~a pid=~a (policy ~a)~%"
                       (listener-path l) uid gid pid (listener-peer-policy l))
               (force-output *error-output*))
              (ignore-errors (sb-bsd-sockets:socket-close sock))))))))

(defgeneric accept-stream (l &key element-type buffering timeout)
  (:documentation "Accept one connection and return it as a stream."))

(defmethod accept-stream ((l listener) &key (element-type '(unsigned-byte 8))
                                            (buffering :full) timeout)
  (let ((sock (listener-accept l)))
    (and sock (apply #'sb-bsd-sockets:socket-make-stream sock
                     :input t :output t :element-type element-type :buffering buffering
                     (when timeout (list :timeout timeout))))))

(defmethod accept-stream ((l sb-bsd-sockets:socket) &key (element-type '(unsigned-byte 8))
                                                         (buffering :full) timeout)
  ;; The legacy path, unchanged byte for byte: TCP_NODELAY, then a binary stream.
  ;; TCP_NODELAY matters because an interactive frame is small (a drag is ~1.6 KB), and
  ;; with Nagle on, TCP holds each one up to ~40 ms waiting on the peer's ACK — which caps
  ;; interactive updates at ~17-25 fps regardless of how fast we encode.  VNC is
  ;; request/response with tiny payloads, exactly Nagle's worst case, so every real VNC
  ;; server disables it.
  (let ((sock (listener-accept l)))
    (and sock (apply #'sb-bsd-sockets:socket-make-stream sock
                     :input t :output t :element-type element-type :buffering buffering
                     (when timeout (list :timeout timeout))))))

;;; ---- closing one, and meaning it ---------------------------------------------

(defgeneric close-listener (l)
  (:documentation
   "Stop a listener, and MEAN IT.  T if there was one.

    SOCKET-CLOSE ON ITS OWN DOES NOT STOP LISTENING.  A thread parked in SOCKET-ACCEPT
    still holds the open file description, so close() drops this process's descriptor and
    the kernel goes on accepting: the port stays bound, `ss -ltn' goes on showing it, and a
    client can still connect.  That is not a subtlety anybody should have to rediscover —
    it is the difference between a seat that has closed its transport and one that only
    thinks it has.

    SHUTDOWN is what the parked accept notices (it returns EINVAL on Linux and the accept
    loop unwinds), so it comes first and the close comes after.  Errors from either are
    ignored on purpose: a socket already shut down, or already closed, is the state being
    asked for, and a listener that refuses to be closed twice is a worse object than one
    that does nothing the second time.

    A UNIX listener has one more step, and it is the same class of trap: the SOCKET FILE
    outlives the socket, and bind() refuses a path that exists.  So it is unlinked here —
    and only if its inode is still the one we created, or a listener closing late would
    delete the file its successor has already bound."))

(defmethod close-listener ((sock sb-bsd-sockets:socket))
  (ignore-errors (sb-bsd-sockets:socket-shutdown sock :direction :io))
  (ignore-errors (sb-bsd-sockets:socket-close sock))
  t)

(defmethod close-listener ((l null)) nil)

(defmethod close-listener ((l listener))
  (let ((sock (listener-socket l)))
    (setf (listener-socket l) nil)
    (when sock
      (ignore-errors (sb-bsd-sockets:socket-shutdown sock :direction :io))
      (ignore-errors (sb-bsd-sockets:socket-close sock))
      t)))

(defmethod close-listener ((l unix-listener))
  (let ((live (call-next-method)))
    (sb-thread:with-mutex (*unix-listeners-lock*)
      (setf *unix-listeners* (remove l *unix-listeners*)))
    (let ((path (listener-path l)) (ino (listener-inode l)))
      (when (and path (or (null ino)
                          (eql ino (ignore-errors (sb-posix:stat-ino (sb-posix:stat path))))))
        (ignore-errors (sb-posix:unlink path))))
    live))

(defvar *unix-exit-hook-installed* nil)
(unless *unix-exit-hook-installed*
  (setf *unix-exit-hook-installed* t)
  (push (lambda ()
          ;; A CLEAN exit tidies up after itself; a kill -9 cannot, which is why
          ;; CLEAR-STALE-SOCKET exists and why this hook is a courtesy and not the
          ;; mechanism.  Never rely on it: the next bind() is what actually handles a
          ;; leftover file, because that is the case an exit hook by definition misses.
          (dolist (l (sb-thread:with-mutex (*unix-listeners-lock*) (copy-list *unix-listeners*)))
            (ignore-errors (close-listener l))))
        sb-ext:*exit-hooks*))

;;; ---- the other end: connecting ----------------------------------------------
;;;
;;; A client says WHERE in one string, because that is what a client already has: an env
;;; var, a config line, a --host flag.  `unix:/run/.../rfb.sock' (or a bare absolute path)
;;; means a socket file; anything else is a hostname and the port beside it means what it
;;; always meant.  One syntax, no second variable to keep in step with the first, and
;;; every existing value keeps working because no existing value starts with `/'.

(defun parse-endpoint (host &optional port)
  "(values KIND HOST PORT PATH) for an endpoint written as a string.

   \"127.0.0.1\" + 5903   -> :tcp
   \"unix:/x/y.sock\"     -> :unix
   \"/x/y.sock\"          -> :unix"
  (let ((host (and host (string-trim " " host))))
    (cond ((null host) (values :tcp nil port nil))
          ((and (> (length host) 5) (string-equal "unix:" (subseq host 0 5)))
           (values :unix nil nil (subseq host 5)))
          ((and (plusp (length host)) (char= (char host 0) #\/))
           (values :unix nil nil host))
          (t (values :tcp host port nil)))))

(defun open-connection (&key host port path (element-type '(unsigned-byte 8))
                             (buffering :full) timeout)
  "Connect to an endpoint and return (values SOCKET STREAM).

   An explicit PATH wins; otherwise HOST is parsed, so \"unix:/x/y.sock\" in a config or an
   env var reaches the same place with no second variable to keep in step.  A TCP
   connection is made exactly as it always was, TCP_NODELAY and all.

   The STREAM is what the protocol above talks, and it is the same object either way."
  (multiple-value-bind (kind h p sock-path) (parse-endpoint host port)
    (let* ((path (or path (and (eq kind :unix) sock-path)))
           (sock (if path
                     (make-instance 'sb-bsd-sockets:local-socket :type :stream)
                     (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))))
      (handler-bind ((error (lambda (e) (declare (ignore e))
                              (ignore-errors (sb-bsd-sockets:socket-close sock)))))
        (if path
            (sb-bsd-sockets:socket-connect sock path)
            (sb-bsd-sockets:socket-connect
             sock (sb-bsd-sockets:host-ent-address
                   (sb-bsd-sockets:get-host-by-name (or h "127.0.0.1")))
             (or p (error "a TCP connection needs a port")))))
      ;; ENOPROTOOPT on a UNIX socket, and harmless: there is no Nagle to disable because
      ;; there is no TCP.  Ignored rather than branched on, the way every caller here
      ;; already ignores it.
      (ignore-errors (setf (sb-bsd-sockets:sockopt-tcp-nodelay sock) t))
      (values sock
              (apply #'sb-bsd-sockets:socket-make-stream sock
                     :input t :output t :element-type element-type :buffering buffering
                     (when timeout (list :timeout timeout)))))))

(defun endpoint-string (&key host port path)
  "How an endpoint reads in a log line — a path, or host:port, whichever it is."
  (multiple-value-bind (kind h p sock-path) (parse-endpoint host port)
    (let ((path (or path (and (eq kind :unix) sock-path))))
      (if path
          (princ-to-string path)
          (format nil "~a:~d" (or h "127.0.0.1") (or p 0))))))

;;; ---- how full a send queue is ------------------------------------------------

(defconstant +siocoutq+ #x5411 "Linux SIOCOUTQ: bytes in a socket's send queue not yet sent to the peer.")
(defun socket-unsent-bytes (fd)
  "Bytes queued in FD's send buffer the peer hasn't drained yet — the real backlog when the
   sender out-produces a slow client.  0 on any error / non-Linux.

   SIOCOUTQ is a TCP ioctl; on a UNIX socket it returns 0 or fails, and 0 is the right
   answer there anyway: a UNIX socket's backlog is bounded by its buffer, the writer blocks
   instead of queueing without limit, and the slow-client-drop this drives cannot trigger."
  (handler-case
      (sb-alien:with-alien ((n sb-alien:int 0))
        (if (zerop (sb-alien:alien-funcall
                    (sb-alien:extern-alien "ioctl"
                      (function sb-alien:int sb-alien:int sb-alien:unsigned-long (* sb-alien:int)))
                    fd +siocoutq+ (sb-alien:addr n)))
            n 0))
    (error () 0)))
