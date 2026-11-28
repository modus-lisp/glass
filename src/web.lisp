;;;; src/web.lisp -- the desk in a web browser: HTTP for noVNC's files, RFB over a WebSocket.
;;;;
;;;; A web endpoint is an RFB-ENDPOINT (rfb.lisp) with an OPENER: each connection it accepts is
;;;; first an HTTP request, read on the network thread once the browser has sent it.
;;;;
;;;;   GET /                  the desk's page (src/web/desk.html): noVNC, pointed at /websockify
;;;;   GET /core/..., /vendor/...   noVNC 1.7.0's own modules, from glass-webrtc/novnc/
;;;;   GET /websockify        with Upgrade: websocket -- RFC 6455's handshake, and then the
;;;;                          connection is a WS-STREAM and goes through the SAME RFB handshake
;;;;                          and peer path as a TCP viewer.  RFB itself is untouched.
;;;;
;;;; Every other request gets its status and the connection is closed (Connection: close; one
;;;; request per connection, so nothing about HTTP lives past the answer).
;;;;
;;;; A WS-STREAM is a binary Gray stream: reading decodes the client's masked frames (data,
;;;; continuation, ping -> pong, close -> close), writing collects bytes, and FORCE-OUTPUT /
;;;; FINISH-OUTPUT send them as ONE unmasked binary frame -- an RFB update is one frame.  Its
;;;; LISTEN reads the frames that have begun to arrive (each to its end) and waits for no new
;;;; one, so "has this peer sent anything?" is answered by decoded bytes and not by a readable
;;;; socket that may hold only a ping (the peer is made with :POLL NIL).
;;;;
;;;; WHO MAY CONNECT.  RFB here asks for no password, so the web endpoint must not let a page on
;;;; some other site drive the desk through the visitor's browser.  A WebSocket is not bound by
;;;; the same-origin policy, so the server checks: a request whose Origin is not this server
;;;; (http://HOST of its own Host header) is refused; and a listener bound to loopback answers only
;;;; a Host naming loopback (127.0.0.1, localhost, [::1]), which is what stops DNS rebinding --
;;;; a hostile name resolving to 127.0.0.1 would otherwise be "same origin" with itself.
;;;;
;;;; THE WEBSOCKET FRAMING is a server's, written here: seal/src/websocket.lisp is the org's
;;;; client (it masks what it sends and would bring TLS with it).  SHA-1 and base64 for the
;;;; accept key are natrium's.

(defpackage #:glass.web
  (:use #:cl)
  (:export #:make-web-opener #:web-root #:web-index #:ws-stream #:*novnc-root*))

(in-package #:glass.web)

;;; ---- where the files are -------------------------------------------------------------------

(defvar *novnc-root* nil
  "The directory holding noVNC's core/ and vendor/, or NIL: glass-webrtc/novnc/ beside glass.")

(defun %glass-file (rel)
  "REL under glass's source directory (ASDF knows where that is; this file's own truename may be
   a cached FASL's)."
  (let ((f (and (find-package "ASDF") (find-symbol "SYSTEM-RELATIVE-PATHNAME" "ASDF"))))
    (and f (funcall f "glass" rel))))

(defun web-root ()
  "The noVNC directory the web endpoint serves: *NOVNC-ROOT*, else the vendored copy in the
   glass-webrtc repository next to glass."
  (or *novnc-root*
      (let ((ssd (and (find-package "ASDF") (find-symbol "SYSTEM-SOURCE-DIRECTORY" "ASDF"))))
        (ignore-errors (merge-pathnames "novnc/" (funcall ssd "glass-webrtc"))))
      (%glass-file "../glass-webrtc/novnc/")))

(defun web-index ()
  "The desk's page: src/web/desk.html, beside this file."
  (%glass-file "src/web/desk.html"))

(defun %read-octets (path)
  (with-open-file (f path :element-type '(unsigned-byte 8))
    (let* ((n (file-length f))
           (v (make-array n :element-type '(unsigned-byte 8))))
      (let ((got (read-sequence v f)))
        (if (= got n) v (subseq v 0 got))))))

(defun %ascii (string)
  (let ((v (make-array (length string) :element-type '(unsigned-byte 8))))
    (dotimes (i (length string) v) (setf (aref v i) (logand #xff (char-code (char string i)))))))

;;; ---- the WebSocket stream ------------------------------------------------------------------

(defconstant +ws-max-frame+ (* 16 1024 1024)
  "The largest frame a client may send.  RFB's client messages are a few bytes to a few KB.")

(defclass ws-stream (sb-gray:fundamental-binary-input-stream
                     sb-gray:fundamental-binary-output-stream)
  ((inner :initarg :inner :reader ws-inner)              ; the socket stream, octets
   (fd    :initarg :fd :initform nil :reader ws-fd)      ; its descriptor (readiness), or NIL
   (in    :initform (make-array 4096 :element-type '(unsigned-byte 8)) :accessor ws-in)
   (in-pos :initform 0 :accessor ws-in-pos)
   (in-end :initform 0 :accessor ws-in-end)
   (eof   :initform nil :accessor ws-eof)                ; the peer closed, or broke the protocol
   (sent-close :initform nil :accessor ws-sent-close)
   (out   :initform (make-array 65536 :element-type '(unsigned-byte 8)) :accessor ws-out)
   (out-fill :initform 0 :accessor ws-out-fill)
   (hdr   :initform (make-array 14 :element-type '(unsigned-byte 8)) :reader ws-hdr))
  (:documentation "RFB over RFC 6455: masked client frames in, unmasked binary frames out."))

(defun %inner-ready-p (s)
  (let ((inner (ws-inner s)) (fd (ws-fd s)))
    (handler-case (or (listen inner) (and fd (sb-sys:wait-until-fd-usable fd :input 0)))
      (error () t))))

(defun %send-frame (s opcode payload start end)
  "One unmasked frame of OPCODE carrying PAYLOAD[START,END), written and pushed out."
  (let* ((inner (ws-inner s)) (h (ws-hdr s)) (n (- end start)) (k 2))
    (setf (aref h 0) (logior #x80 opcode))
    (cond ((< n 126) (setf (aref h 1) n))
          ((< n 65536) (setf (aref h 1) 126 (aref h 2) (ldb (byte 8 8) n) (aref h 3) (ldb (byte 8 0) n)
                             k 4))
          (t (setf (aref h 1) 127 k 10)
             (loop for i from 0 below 8
                   do (setf (aref h (+ 2 i)) (ldb (byte 8 (* 8 (- 7 i))) n)))))
    (write-sequence h inner :end k)
    (when (plusp n) (write-sequence payload inner :start start :end end))
    (force-output inner)))

(defun %send-close (s code)
  (unless (ws-sent-close s)
    (setf (ws-sent-close s) t)
    (ignore-errors
     (let ((p (make-array 2 :element-type '(unsigned-byte 8))))
       (setf (aref p 0) (ldb (byte 8 8) code) (aref p 1) (ldb (byte 8 0) code))
       (%send-frame s #x8 p 0 2)))))

(defun %read-exactly (s v start end)
  "Fill V[START,END) from the socket; NIL if it ended first."
  (= (read-sequence v (ws-inner s) :start start :end end) end))

(defun %room (s n)
  "Make room for N more decoded bytes at the end of the input buffer."
  (let ((pos (ws-in-pos s)) (end (ws-in-end s)) (in (ws-in s)))
    (when (= pos end) (setf pos 0 end 0 (ws-in-pos s) 0 (ws-in-end s) 0))
    (when (> (+ end n) (length in))
      (let ((live (- end pos)))
        (if (<= (+ live n) (length in))
            (replace in in :start2 pos :end2 end)
            (let ((new (make-array (max (* 2 (length in)) (+ live n)) :element-type '(unsigned-byte 8))))
              (replace new in :start2 pos :end2 end)
              (setf (ws-in s) new)))
        (setf (ws-in-pos s) 0 (ws-in-end s) live)))))

(defun %read-frame (s)
  "Read and act on one frame from the client (blocking until it has all arrived).  Data and
   continuation frames add their payload to the input; a ping is answered; a close is answered
   and ends the input, as does anything RFC 6455 says a server must not accept."
  (let ((h (ws-hdr s)))
    (unless (%read-exactly s h 0 2) (setf (ws-eof s) t) (return-from %read-frame))
    (let* ((b0 (aref h 0)) (b1 (aref h 1))
           (opcode (logand b0 #x0f))
           (masked (logbitp 7 b1))
           (n (logand b1 #x7f)))
      (cond ((= n 126)
             (unless (%read-exactly s h 2 4) (setf (ws-eof s) t) (return-from %read-frame))
             (setf n (logior (ash (aref h 2) 8) (aref h 3))))
            ((= n 127)
             (unless (%read-exactly s h 2 10) (setf (ws-eof s) t) (return-from %read-frame))
             (setf n 0)
             (loop for i from 2 below 10 do (setf n (logior (ash n 8) (aref h i))))))
      (flet ((fail (code) (%send-close s code) (setf (ws-eof s) t) (return-from %read-frame)))
        ;; a client MUST mask (RFC 6455 section 5.1); reserved bits must be clear
        (unless masked (fail 1002))
        (unless (zerop (logand b0 #x70)) (fail 1002))
        (when (> n +ws-max-frame+) (fail 1009))
        (when (and (>= opcode 8) (or (> n 125) (not (logbitp 7 b0)))) (fail 1002))
        (unless (%read-exactly s h 10 14) (setf (ws-eof s) t) (return-from %read-frame))
        (case opcode
          ((0 1 2)
           (%room s n)
           (let* ((in (ws-in s)) (at (ws-in-end s)) (end (+ at n)))
             (unless (%read-exactly s in at end) (setf (ws-eof s) t) (return-from %read-frame))
             (loop for i from at below end for j of-type fixnum from 0
                   do (setf (aref in i) (logxor (aref in i) (aref h (+ 10 (logand j 3))))))
             (setf (ws-in-end s) end)))
          ((8 9 10)
           (let ((p (make-array n :element-type '(unsigned-byte 8))))
             (unless (%read-exactly s p 0 n) (setf (ws-eof s) t) (return-from %read-frame))
             (dotimes (j n) (setf (aref p j) (logxor (aref p j) (aref h (+ 10 (logand j 3))))))
             (case opcode
               (8 (%send-close s (if (>= n 2) (logior (ash (aref p 0) 8) (aref p 1)) 1000))
                  (setf (ws-eof s) t))
               (9 (ignore-errors (%send-frame s #xa p 0 n)))
               (10 nil))))
          (t (fail 1002)))))))

(defun %have-input (s)
  (< (ws-in-pos s) (ws-in-end s)))

(defun %fill-blocking (s)
  "Until there is a decoded byte or the input has ended."
  (loop until (or (%have-input s) (ws-eof s)) do (%read-frame s)))

(defmethod sb-gray:stream-read-byte ((s ws-stream))
  (%fill-blocking s)
  (if (%have-input s)
      (prog1 (aref (ws-in s) (ws-in-pos s)) (incf (ws-in-pos s)))
      :eof))

(defmethod sb-gray:stream-read-sequence ((s ws-stream) seq &optional (start 0) end)
  (let ((end (or end (length seq))) (at start))
    (loop while (< at end)
          do (%fill-blocking s)
             (unless (%have-input s) (return))
             (let ((k (min (- end at) (- (ws-in-end s) (ws-in-pos s)))))
               (replace seq (ws-in s) :start1 at :start2 (ws-in-pos s) :end2 (+ (ws-in-pos s) k))
               (incf at k)
               (incf (ws-in-pos s) k)))
    at))

(defmethod sb-gray:stream-listen ((s ws-stream))
  ;; T at the end too, as a readable socket is: the read that follows sees :EOF and the
  ;; network thread drops the viewer
  (loop until (or (%have-input s) (ws-eof s) (not (%inner-ready-p s)))
        do (%read-frame s))
  (or (%have-input s) (ws-eof s)))

(defun %out-room (s n)
  (let ((out (ws-out s)) (fill (ws-out-fill s)))
    (when (> (+ fill n) (length out))
      (let ((new (make-array (max (* 2 (length out)) (+ fill n)) :element-type '(unsigned-byte 8))))
        (replace new out :end2 fill)
        (setf (ws-out s) new)))))

(defmethod sb-gray:stream-write-byte ((s ws-stream) byte)
  (%out-room s 1)
  (setf (aref (ws-out s) (ws-out-fill s)) byte)
  (incf (ws-out-fill s))
  byte)

(defmethod sb-gray:stream-write-sequence ((s ws-stream) seq &optional (start 0) end)
  (let* ((end (or end (length seq))) (n (- end start)))
    (%out-room s n)
    (replace (ws-out s) seq :start1 (ws-out-fill s) :start2 start :end2 end)
    (incf (ws-out-fill s) n)
    seq))

(defun %flush (s)
  (when (plusp (ws-out-fill s))
    (let ((n (ws-out-fill s)))
      (setf (ws-out-fill s) 0)
      (%send-frame s #x2 (ws-out s) 0 n)))
  nil)

(defmethod sb-gray:stream-force-output ((s ws-stream)) (%flush s))
(defmethod sb-gray:stream-finish-output ((s ws-stream)) (%flush s))

(defmethod stream-element-type ((s ws-stream)) '(unsigned-byte 8))

(defmethod close ((s ws-stream) &key abort)
  (unless abort (ignore-errors (%flush s)))
  (%send-close s 1000)
  (ignore-errors (close (ws-inner s) :abort abort))
  (call-next-method))

;;; ---- HTTP ----------------------------------------------------------------------------------

(defconstant +max-request+ 16384 "The longest request head the web endpoint reads.")

(defun %read-request-head (stream)
  "The request's head, through the blank line, as a string; NIL if the connection ended or the
   head ran past +MAX-REQUEST+."
  (let ((buf (make-string +max-request+)) (n 0))
    (loop
      (let ((b (read-byte stream nil nil)))
        (unless b (return nil))
        (when (>= n +max-request+) (return nil))
        (setf (char buf n) (code-char b))
        (incf n)
        (when (and (>= n 4) (char= (char buf (- n 1)) #\Newline) (char= (char buf (- n 2)) #\Return)
                   (char= (char buf (- n 3)) #\Newline) (char= (char buf (- n 4)) #\Return))
          (return (subseq buf 0 n)))))))

(defun %split (string char)
  (loop with start = 0
        for p = (position char string :start start)
        collect (subseq string start (or p (length string)))
        while p do (setf start (1+ p))))

(defun %parse-request (head)
  "(values METHOD TARGET HEADERS), HEADERS an alist of (lower-case-name . value), or NIL."
  (let* ((lines (remove "" (mapcar (lambda (l) (string-right-trim '(#\Return) l))
                                   (%split head #\Newline))
                        :test #'string=))
         (first (first lines))
         (sp1 (and first (position #\Space first)))
         (sp2 (and sp1 (position #\Space first :start (1+ sp1)))))
    (when sp2
      (values (subseq first 0 sp1)
              (subseq first (1+ sp1) sp2)
              (loop for l in (rest lines)
                    for c = (position #\: l)
                    when c collect (cons (string-downcase (string-trim " " (subseq l 0 c)))
                                         (string-trim " 	" (subseq l (1+ c)))))))))

(defun %header (headers name) (cdr (assoc name headers :test #'string=)))

(defun %token-p (value token)
  "Does the comma-separated header VALUE list TOKEN (case-insensitively)?"
  (and value (some (lambda (x) (string-equal (string-trim " 	" x) token)) (%split value #\,))))

(defun %respond (stream status reason &key (type "text/plain; charset=utf-8") body head-only extra)
  (let* ((body (or body (%ascii (format nil "~d ~a~%" status reason))))
         (head (format nil "HTTP/1.1 ~d ~a~c~cContent-Type: ~a~c~cContent-Length: ~d~c~c~
                            Cache-Control: no-cache~c~cX-Content-Type-Options: nosniff~c~c~
                            ~{~a~c~c~}Connection: close~c~c~c~c"
                       status reason #\Return #\Newline type #\Return #\Newline (length body)
                       #\Return #\Newline #\Return #\Newline #\Return #\Newline
                       (loop for e in extra append (list e #\Return #\Newline))
                       #\Return #\Newline #\Return #\Newline)))
    (write-sequence (%ascii head) stream)
    (unless head-only (write-sequence body stream))
    (force-output stream)))

(defparameter *content-types*
  '(("js" . "text/javascript; charset=utf-8") ("mjs" . "text/javascript; charset=utf-8")
    ("html" . "text/html; charset=utf-8") ("css" . "text/css; charset=utf-8")
    ("json" . "application/json") ("svg" . "image/svg+xml") ("png" . "image/png")
    ("ico" . "image/x-icon") ("wasm" . "application/wasm") ("txt" . "text/plain; charset=utf-8")))

(defun %content-type (rel)
  (let ((dot (position #\. rel :from-end t)))
    (or (and dot (cdr (assoc (subseq rel (1+ dot)) *content-types* :test #'string-equal)))
        "application/octet-stream")))

(defun %safe-path (target)
  "The path TARGET asks for, relative to the noVNC root, or NIL if it is not one this server
   serves: under core/ or vendor/, its characters from [A-Za-z0-9._/-], no empty, `.' or `..'
   segment, and no segment that starts with a dot.  (A %-escape is not decoded, so none is
   accepted: noVNC's file names need none.)"
  (let* ((q (position-if (lambda (c) (member c '(#\? #\#))) target))
         (path (subseq target 0 q)))
    (when (and (> (length path) 1) (char= (char path 0) #\/))
      (let* ((rel (subseq path 1))
             (segs (%split rel #\/)))
        (and (every (lambda (c) (or (alphanumericp c) (member c '(#\. #\_ #\- #\/)))) rel)
             (every (lambda (c) (< (char-code c) 128)) rel)
             (member (first segs) '("core" "vendor") :test #'string=)
             (every (lambda (seg) (and (plusp (length seg)) (char/= (char seg 0) #\.))) segs)
             rel)))))

(defun %loopback-host-p (host)
  "Does the Host header HOST (name[:port]) name this machine's loopback?"
  (let* ((name (cond ((and (plusp (length host)) (char= (char host 0) #\[))
                      (subseq host 0 (1+ (or (position #\] host) (1- (length host))))))
                     (t (subseq host 0 (position #\: host))))))
    (or (string-equal name "localhost") (string= name "[::1]")
        (and (> (length name) 4) (string= name "127." :end1 4)))))

(defun %loopback-address-p (address)
  (or (null address) (string-equal address "localhost") (string= address "::1")
      (and (> (length address) 4) (string= address "127." :end1 4))))

(defun %accept-key (key)
  (natrium:base64-encode
   (natrium:sha1 (%ascii (concatenate 'string key "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")))))

(defun make-web-opener (&key root address (index (web-index)) log)
  "An RFB-ENDPOINT OPENER (rfb.lisp) serving the desk's page, noVNC's files from ROOT (default
   WEB-ROOT) and RFB over a WebSocket at /websockify.  ADDRESS is the address the listener is
   bound to: when it is loopback, only requests naming loopback in Host are answered.  LOG, if
   true, writes one line per request to *ERROR-OUTPUT*.  ROOT and INDEX are found through ASDF
   by default; on modus, where a worker thread may not ask ASDF, find them (WEB-ROOT, WEB-INDEX)
   on the main thread and pass them in."
  (let* ((root (or root (web-root)))
         (index-octets (and index (probe-file index) (%read-octets index)))
         (loopback (%loopback-address-p address)))
    (lambda (stream fd)
      (let ((head (%read-request-head stream)))
        (unless head (ignore-errors (close stream)))
        (multiple-value-bind (method target headers) (and head (%parse-request head))
          (let* ((host (%header headers "host"))
                 (origin (%header headers "origin"))
                 (get (or (equal method "GET") (equal method "HEAD")))
                 (head-only (equal method "HEAD")))
            (flet ((answer (status reason &rest keys)
                     (when log
                       (format *error-output* "~&glass web: ~a ~a -> ~d~%" method target status)
                       (force-output *error-output*))
                     (ignore-errors (apply #'%respond stream status reason :head-only head-only keys))
                     (ignore-errors (close stream))
                     nil))
              (cond
                ((null head) nil)
                ((null method) (answer 400 "Bad Request"))
                ((or (null host) (and loopback (not (%loopback-host-p host))))
                 (answer 403 "Forbidden"))
                ((not get) (answer 405 "Method Not Allowed" :extra '("Allow: GET, HEAD")))
                ((and (string= (subseq target 0 (position #\? target)) "/websockify")
                      (%token-p (%header headers "upgrade") "websocket"))
                 (let ((key (%header headers "sec-websocket-key")))
                   (cond ((and origin (not (string-equal origin (concatenate 'string "http://" host))))
                          (answer 403 "Forbidden"))
                         ((or (null key) (not (%token-p (%header headers "connection") "upgrade")))
                          (answer 400 "Bad Request"))
                         ((not (equal (%header headers "sec-websocket-version") "13"))
                          (answer 426 "Upgrade Required" :extra '("Sec-WebSocket-Version: 13")))
                         (t
                          (let ((binary (%token-p (%header headers "sec-websocket-protocol") "binary")))
                            (write-sequence
                             (%ascii (format nil "HTTP/1.1 101 Switching Protocols~c~c~
                                                  Upgrade: websocket~c~cConnection: Upgrade~c~c~
                                                  Sec-WebSocket-Accept: ~a~c~c~@[~a~]~c~c"
                                             #\Return #\Newline #\Return #\Newline #\Return #\Newline
                                             (%accept-key key) #\Return #\Newline
                                             (and binary (format nil "Sec-WebSocket-Protocol: binary~c~c"
                                                                 #\Return #\Newline))
                                             #\Return #\Newline))
                             stream)
                            (force-output stream)
                            (when log
                              (format *error-output* "~&glass web: ~a ~a -> 101 (RFB over WebSocket)~%"
                                      method target)
                              (force-output *error-output*))
                            (values (make-instance 'ws-stream :inner stream :fd fd) fd))))))
                ((member (subseq target 0 (position #\? target)) '("/" "/index.html") :test #'string=)
                 (if index-octets
                     (answer 200 "OK" :type "text/html; charset=utf-8" :body index-octets)
                     (answer 404 "Not Found")))
                (t
                 (let* ((rel (%safe-path target))
                        (file (and rel root (probe-file (merge-pathnames rel root)))))
                   (cond ((null rel) (answer 403 "Forbidden"))
                         ((or (null file) (null (pathname-name file))) (answer 404 "Not Found"))
                         (t (answer 200 "OK" :type (%content-type rel)
                                             :body (%read-octets file))))))))))))))
