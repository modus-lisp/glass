;;;; site-fakes.lisp — a Nostr relay and a Blossom server on loopback, for publishing with NO network.
;;;;
;;;;   (glass-site-fakes:start-fakes)   ->  (values relay-url blossom-url)
;;;;
;;;; Both are as small as a test can make them and as strict as the real thing where it matters to a
;;;; publish: the relay VERIFIES every EVENT (id and BIP-340 signature) before it says OK, and the
;;;; Blossom server verifies the kind-24242 Authorization (signature, kind, and that its `x' tag is
;;;; the SHA-256 of the body it was sent) before it stores anything.  A publish that "worked" against
;;;; these is a publish whose signatures a real relay would have taken.
;;;;
;;;; Bound to 127.0.0.1 on a kernel-chosen port, and only for as long as the test image lives.

(defpackage #:glass-site-fakes
  (:use #:cl)
  (:export #:start-fakes #:stop-fakes #:*events* #:*blobs* #:*auth-events*))
(in-package #:glass-site-fakes)

(defvar *events* '() "Every EVENT the fake relay accepted, as CL-NOSTR.EVENT:EVENT, newest first.")
(defvar *auth-events* '() "Every kind-24242 Authorization the fake Blossom server accepted.")
(defvar *blobs* (make-hash-table :test 'equal) "hash-hex -> octets the fake Blossom server holds.")
(defvar *lock* (sb-thread:make-mutex :name "site-fakes"))
(defvar *listeners* '())

(defun %listen ()
  (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address s) t)
    (sb-bsd-sockets:socket-bind s #(127 0 0 1) 0)
    (sb-bsd-sockets:socket-listen s 16)
    (push s *listeners*)
    (values s (nth-value 1 (sb-bsd-sockets:socket-name s)))))

(defun %serve (listener handler name)
  (sb-thread:make-thread
   (lambda ()
     (loop
       (let ((c (handler-case (sb-bsd-sockets:socket-accept listener) (error () (return)))))
         (sb-thread:make-thread
          (lambda ()
            (let ((st (sb-bsd-sockets:socket-make-stream c :input t :output t
                                                           :element-type '(unsigned-byte 8)
                                                           :buffering :full)))
              (unwind-protect (ignore-errors (funcall handler st))
                (ignore-errors (close st))
                (ignore-errors (sb-bsd-sockets:socket-close c)))))
          :name name))))
   :name name))

(defun %line (st)
  "One CRLF line as a string (ASCII), or NIL at end of stream."
  (let ((out (make-array 64 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop for b = (read-byte st nil nil)
          do (cond ((null b) (return (and (plusp (length out)) (coerce out 'simple-string))))
                   ((= b 13))
                   ((= b 10) (return (coerce out 'simple-string)))
                   (t (vector-push-extend (code-char b) out))))))

(defun %headers (st)
  (loop for line = (%line st)
        while (and line (plusp (length line)))
        for c = (position #\: line)
        when c collect (cons (string-downcase (subseq line 0 c))
                             (string-trim " " (subseq line (1+ c))))))

(defun %write-ascii (st string)
  (write-sequence (map '(vector (unsigned-byte 8)) #'char-code string) st))

;;; ---- the relay ---------------------------------------------------------------

(defun %ws-send (st opcode payload)
  "One unmasked server frame (RFC 6455 §5.1: a server never masks)."
  (let ((n (length payload)))
    (write-byte (logior #x80 opcode) st)
    (cond ((<= n 125) (write-byte n st))
          ((<= n 65535) (write-byte 126 st) (write-byte (ash n -8) st) (write-byte (logand n 255) st))
          (t (write-byte 127 st) (loop for sh from 56 downto 0 by 8 do (write-byte (ldb (byte 8 sh) n) st))))
    (write-sequence payload st)
    (finish-output st)))

(defun %ws-text (st string) (%ws-send st 1 (seal:utf8-encode string)))

(defun %relay (st)
  (%line st)                                             ; GET / HTTP/1.1
  (let* ((h (%headers st))
         (key (cdr (assoc "sec-websocket-key" h :test #'string=))))
    (%write-ascii st (format nil "HTTP/1.1 101 Switching Protocols~c~cUpgrade: websocket~c~c~
                                  Connection: Upgrade~c~cSec-WebSocket-Accept: ~a~c~c~c~c"
                             #\Return #\Newline #\Return #\Newline #\Return #\Newline
                             (seal.websocket::expected-accept key)
                             #\Return #\Newline #\Return #\Newline))
    (finish-output st)
    (loop
      (multiple-value-bind (fin opcode payload) (seal.websocket::read-frame st)
        (declare (ignore fin))
        (case opcode
          (8 (ignore-errors (%ws-send st 8 #())) (return))
          (9 (%ws-send st 10 payload))
          (1 (let* ((msg (json-simple:parse (seal:utf8-decode payload)))
                    (verb (and (vectorp msg) (plusp (length msg)) (aref msg 0))))
               (cond
                 ((equal verb "EVENT")
                  (let* ((ev (cl-nostr.event:json->event (aref msg 1)))
                         (good (cl-nostr.event:valid-event-p ev)))
                    (when good (sb-thread:with-mutex (*lock*) (push ev *events*)))
                    (%ws-text st (format nil "[\"OK\",\"~a\",~:[false~;true~],\"~:[invalid: bad signature~;~]\"]"
                                         (cl-nostr.event:event-id ev) good good))))
                 ((equal verb "REQ")
                  (%ws-text st (format nil "[\"EOSE\",\"~a\"]" (aref msg 1))))))))))))

;;; ---- the Blossom server ------------------------------------------------------

(defun %respond (st status body)
  (let ((b (seal:utf8-encode body)))
    (%write-ascii st (format nil "HTTP/1.1 ~d X~c~cContent-Type: application/json~c~c~
                                  Content-Length: ~d~c~cConnection: close~c~c~c~c"
                             status #\Return #\Newline #\Return #\Newline (length b)
                             #\Return #\Newline #\Return #\Newline #\Return #\Newline))
    (write-sequence b st)
    (finish-output st)))

(defun %auth-ok-p (header hash)
  "T iff HEADER is `Nostr <base64 event>' with a valid kind-24242 event whose x tag is HASH."
  (let* ((b64 (and header (> (length header) 6) (subseq header 6)))
         (ev (ignore-errors
              (cl-nostr.event:json->event (cl-base64:base64-string-to-string b64)))))
    (and ev (cl-nostr.event:valid-event-p ev)
         (= 24242 (cl-nostr.event:event-kind ev))
         (find-if (lambda (tag) (and (equal (first tag) "x") (equal (second tag) hash)))
                  (map 'list (lambda (tag) (coerce tag 'list)) (cl-nostr.event:event-tags ev)))
         (progn (sb-thread:with-mutex (*lock*) (push ev *auth-events*)) t))))

(defun %blossom (port)
  (lambda (st)
    (let* ((request (%line st))
           (h (%headers st))
           (n (parse-integer (or (cdr (assoc "content-length" h :test #'string=)) "0")))
           (body (make-array n :element-type '(unsigned-byte 8))))
      (read-sequence body st)
      (cond
        ((and request (eql 0 (search "PUT /upload" request)))
         (let ((hash (cl-nostr.util:bytes->hex (cl-nostr.util:sha256 body))))
           (if (%auth-ok-p (cdr (assoc "authorization" h :test #'string=)) hash)
               (progn (sb-thread:with-mutex (*lock*) (setf (gethash hash *blobs*) body))
                      (%respond st 200 (format nil "{\"url\":\"http://127.0.0.1:~d/~a\",\"sha256\":\"~a\",~
                                                    \"size\":~d,\"type\":\"text/html\",\"uploaded\":0}"
                                               port hash hash n)))
               (%respond st 401 "{\"message\":\"bad authorization\"}"))))
        (t (%respond st 404 "{}"))))))

(defun start-fakes ()
  "Start both on loopback.  Returns (values RELAY-URL BLOSSOM-URL)."
  (multiple-value-bind (rl rport) (%listen)
    (multiple-value-bind (bl bport) (%listen)
      (%serve rl #'%relay "fake-relay")
      (%serve bl (%blossom bport) "fake-blossom")
      (values (format nil "ws://127.0.0.1:~d" rport)
              (format nil "http://127.0.0.1:~d" bport)))))

(defun stop-fakes ()
  (dolist (l *listeners*) (ignore-errors (sb-bsd-sockets:socket-close l)))
  (setf *listeners* '()))
