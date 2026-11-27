;;;; test/send-login-link-test.lisp — a box saying `here is the way in', unprompted, end to end.
;;;;
;;;;   GLASS_LINK_TO=f4f6a5667475b3b52468751c478faad9ea15075c79adeca9f5288311ef176443 \
;;;;     sbcl --non-interactive --load test/send-login-link-test.lisp     (or modus --script)
;;;;
;;;; SEND-LOGIN-LINK mints a link FOR somebody and gift-wraps it to them through the bot's relays;
;;;; START-SESSION-NOSTR does the same for GLASS_LINK_TO as the session comes up.  Both are checked
;;;; from the recipient's side: a client identity on a relay on loopback must receive a wrap from
;;;; the box carrying a link whose code the store recorded FOR it, and a second link must supersede
;;;; the first.  The relay and the service mode are as in test/nostr-bot-test.lisp ($GLASS_TEST_RELAY,
;;;; $BOT_SERVICE).  The session's admission service listens on a socket FILE under $TMPDIR, so no
;;;; port is taken.  GLASS_LINK_TO must name this test's client (above): without it, that section is
;;;; reported as not run, and the verdict is FAIL.

(require :asdf)
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream))) (asdf:load-system :glass/nostr)))

(defpackage #:glass-send-login-link-test (:use #:cl))
(in-package #:glass-send-login-link-test)

(defvar *fails* 0)
(defvar *checks* 0)
(defun check (name ok &optional detail)
  (incf *checks*)
  (unless ok (incf *fails*))
  (format t "  ~:[FAIL~;ok  ~] ~a~@[ — ~a~]~%" ok name detail)
  (finish-output)
  ok)

(defun env (name) (let ((v (sb-ext:posix-getenv name))) (and v (plusp (length v)) v)))

(defparameter *mode*
  (let ((m (env "BOT_SERVICE")))
    (cond ((null m) #+modus :manual #-modus :thread)
          ((string-equal m "thread") :thread)
          ((string-equal m "hook") :hook)
          (t :manual))))
(defun service () (ecase *mode* (:thread :thread) (:hook :hook) (:manual nil)))

(defparameter *tmp* (let ((d (or (env "TMPDIR") "/tmp")))
                      (if (char= (char d (1- (length d))) #\/) d (concatenate 'string d "/"))))
(defparameter *tag* (random 1000000 (make-random-state t)))
(defparameter *fixture* (format nil "~aglass-send-link-test-~d-devices" *tmp* *tag*))
(defparameter *socket* (format nil "~aglass-send-link-test-~d.sock" *tmp* *tag*))

;;; THE CLIENT.  A fixed key, because GLASS_LINK_TO has to name it before this process starts.
(defparameter *client*
  (cl-nostr.keys:keypair-from-secret "c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1"))
(defparameter *client-pub* (string-downcase (cl-nostr.keys:public-hex *client*)))

(setf glass:*enrolment-file* *fixture*
      glass::*enrolments-mtime* nil
      glass:*box-secret* (cl-nostr.util:bytes->hex (cl-nostr.util:random-bytes 32))
      glass:*login-url-base* "https://example.invalid/k.html")
(glass:refresh-nostr-allow "")
(clrhash glass:*enrolments*)
(setf glass:*login-codes* (make-instance 'glass:login-code-store))

(defparameter *fake* nil)
(defparameter *url*
  (or (env "GLASS_TEST_RELAY")
      (progn
        (load (asdf:system-relative-pathname "cl-nostr" "inspect/fake-relay.lisp"))
        (let ((port (funcall (find-symbol "FREE-PORT" "CL-NOSTR.FAKE-RELAY"))))
          (setf *fake* (funcall (find-symbol "START-FAKE-RELAY" "CL-NOSTR.FAKE-RELAY") :port port))
          (format nil "ws://127.0.0.1:~d" port)))))
(format t "~&send-login-link test: relay ~a, service ~(~a~)~%" *url* *mode*)

;;; ---- the recipient --------------------------------------------------------------

(defparameter *inbox* (bobbin:make-channel :capacity (* 1024 1024) :name "test client"))
(defparameter *client-pool* (cl-nostr.pool:make-pool (list *url*) :timeout 10))
(cl-nostr.pool:pool-subscribe
 *client-pool*
 (cl-nostr.filter:make-filter :kinds '(1059) :tags (list (cons "p" (list *client-pub*))))
 :on-event (lambda (wrap relay)
             (declare (ignore relay))
             (ignore-errors (bobbin:channel-send *inbox* (cl-nostr.event:event->json wrap) :timeout 1))))

(defvar *dms* '() "(text . sender) of every wrap for the client, newest first.")
(defvar *wraps* 0 "How many wraps arrived at all.")
(defvar *unwrap-errors* '())
(defun take-dms ()
  (loop (multiple-value-bind (json ok) (bobbin:channel-try-receive *inbox*)
          (unless ok (return))
          (incf *wraps*)
          (multiple-value-bind (text sender)
              (handler-case (cl-nostr.nip59:unwrap-giftwrap *client* (cl-nostr.event:json->event json))
                (error (e) (push (princ-to-string e) *unwrap-errors*) nil))
            (when text (push (cons text sender) *dms*)))))
  *dms*)
(defun dm-with (url)
  (take-dms)
  (find-if (lambda (d) (search url (car d))) *dms*))

(defvar *bot* nil)
(defun drive (seconds pred)
  (let ((end (+ (get-internal-real-time) (round (* seconds internal-time-units-per-second)))))
    (loop
      (let ((v (funcall pred))) (when v (return v)))
      (when (>= (get-internal-real-time) end) (return nil))
      (ecase *mode*
        (:manual (if *bot* (glass:nostr-bot-service *bot* 0.05) (glass:nostr-owner-service 0.05)))
        (:hook (glass:run-owner-hooks) (sleep 0.05))
        (:thread (sleep 0.05))))))

(defparameter *wait* #+modus 240 #-modus 60)

(defun code-of (url)
  "The login code a link URL carries."
  (let* ((at (search "code=" url))
         (end (and at (position-if (lambda (c) (member c '(#\& #\Space #\Newline))) url :start at))))
    (and at (subseq url (+ at 5) end))))

(defun row-of (url)
  (let ((nonce (nth-value 1 (glass:verify-login-token (code-of url)))))
    (and nonce (glass:find-login-code nonce))))

;;; ==============================================================================
(format t "~&-- SEND-LOGIN-LINK, on a running bot --~%")
;;; ==============================================================================

(defvar *said* '())
(defvar *said-lock* (sb-thread:make-mutex :name "said"))
(defun said-matching (&rest words)
  (find-if (lambda (l) (every (lambda (w) (search w l)) words))
           (sb-thread:with-mutex (*said-lock*) (copy-list *said*))))

(check "with no bot running, it sends nothing and says so (NIL)"
       (null (let ((*error-output* (make-broadcast-stream)))
               (glass:send-login-link *client-pub* :bot nil))))
(setf *bot* (glass:start-nostr-bot :relays (list *url*) :service (service) :install nil
                                   :log (lambda (m) (sb-thread:with-mutex (*said-lock*) (push m *said*)))))
(check "the bot started" (glass:nostr-bot-p *bot*))
(check "an unresolvable target is NIL"
       (null (let ((*error-output* (make-broadcast-stream)))
               (glass:send-login-link "not-a-key" :bot *bot*))))

(defparameter *first* (let ((*error-output* (make-broadcast-stream)))
                        (glass:send-login-link *client-pub* :bot *bot*)))
(check "SEND-LOGIN-LINK returns the link" (and *first* (search "example.invalid/k.html" *first*)) *first*)
(check "  …naming this box and carrying a code that verifies"
       (and *first* (search (format nil "box=~a" (glass:box-npub)) *first*)
            (glass:verify-login-token (code-of *first*))))
(check "  …which the store recorded as outstanding, FOR the recipient"
       (let ((r (and *first* (row-of *first*))))
         (and r (eq :outstanding (glass:code-state r)) (equal *client-pub* (glass:code-recipient r)))))
(let ((d (drive *wait* (lambda () (and *first* (dm-with *first*))))))
  (check "the recipient received it, gift-wrapped" d)
  (check "  …from the box's key" (and d (equal (string-downcase (cdr d)) (glass:box-pubkey))))
  (check "  …saying the desktop is up" (and d (search "is up" (car d)))))
(check "the log says a relay accepted it (the relay's OK, pumped by the owner)"
       (drive 30 (lambda () (said-matching "login link to" "accepted by" *url*)))
       (sb-thread:with-mutex (*said-lock*) (copy-list *said*)))

;; by npub this time, and it replaces the first
(defparameter *second* (let ((*error-output* (make-broadcast-stream)))
                         (glass:send-login-link (cl-nostr.bech32:npub-encode *client-pub*) :bot *bot*)))
(check "a second link, addressed by npub" (and *second* (string/= *second* *first*)))
(check "  …supersedes the first in the store"
       (let ((r (row-of *first*))) (and r (eq :superseded (glass:code-state r)))))
(let ((d (drive *wait* (lambda () (and *second* (dm-with *second*))))))
  (check "  …and says so in the DM" (and d (search "replaces any earlier link" (car d)))))
(glass:stop-nostr-bot *bot*)
(check "the bot stopped" (drive 10 (lambda () (glass:nostr-bot-stopped-p *bot*))))
(setf *bot* nil)

;;; ==============================================================================
(format t "~&-- GLASS_LINK_TO, through START-SESSION-NOSTR --~%")
;;; ==============================================================================

(cond
  ((not (equal (glass:normalize-pubkey (or (env "GLASS_LINK_TO") "")) *client-pub*))
   (check "GLASS_LINK_TO names this test's client (run with GLASS_LINK_TO=<its pubkey>; see header)"
          nil (env "GLASS_LINK_TO")))
  (t
   (glass:refresh-nostr-allow *client-pub*)
   (multiple-value-bind (srv bot)
       (let ((*error-output* (make-broadcast-stream)))
         (glass:start-session-nostr :path *socket* :relays (list *url*) :service (service)))
     (setf *bot* bot)
     (check "the session started: an admission service and a bot" (and srv bot))
     (check "the admission service answers on its socket file"
            (equal (getf (glass:admission-ping :path *socket*) :box) (glass:box-pubkey))
            (glass:admission-ping :path *socket*))
     (let ((d (drive *wait* (lambda () (take-dms)
                              (find-if (lambda (d) (and (search "is up" (car d))
                                                        (not (search (or *first* "?") (car d)))
                                                        (not (search (or *second* "?") (car d)))))
                                       *dms*)))))
       (check "GLASS_LINK_TO got a link as the session came up" d
              (format nil "~d wraps, unwrap errors ~s; ~d DMs; threads ~s; relay ~s" *wraps* *unwrap-errors* (length *dms*)
                      (mapcar #'sb-thread:thread-name
                              (remove-if-not #'sb-thread:thread-alive-p (sb-thread:list-all-threads)))
                      (and *fake* (ignore-errors (funcall (find-symbol "FAKE-RELAY-STATS" "CL-NOSTR.FAKE-RELAY") *fake*)))))
       (let ((url (and d (subseq (car d) (or (search "https://" (car d)) 0)))))
         (check "  …whose code the store recorded for it"
                (let ((r (and url (row-of (string-trim '(#\Newline #\Space) url)))))
                  (and r (equal *client-pub* (glass:code-recipient r)))))))
     (glass:stop-nostr-bot bot)
     (glass:stop-admission-service srv)
     (check "the session's bot stopped" (drive 10 (lambda () (glass:nostr-bot-stopped-p bot))))
     (check "the admission service is gone from its socket file"
            (drive 5 (lambda () (null (probe-file *socket*))))))))

(cl-nostr.pool:close-pool *client-pool*)
(when *fake* (funcall (find-symbol "STOP-FAKE-RELAY" "CL-NOSTR.FAKE-RELAY") *fake*))
(ignore-errors (delete-file *fixture*))
(ignore-errors (delete-file (concatenate 'string *fixture* ".codes")))
(ignore-errors (delete-file *socket*))
(format t "~&checks: ~d   failures: ~d   => ~:[FAIL~;PASS~]~%" *checks* *fails* (zerop *fails*))
(finish-output)
(sb-ext:exit :code (if (zerop *fails*) 0 1) :abort t)
