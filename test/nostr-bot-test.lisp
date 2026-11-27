;;;; test/nostr-bot-test.lisp — the DM command bot end to end, through a relay on loopback.
;;;;
;;;;   sbcl --non-interactive --load test/nostr-bot-test.lisp        (glass/nostr findable by ASDF)
;;;;   modus --script test/nostr-bot-test.lisp
;;;;
;;;; A client identity DMs the bot `help' and `link' through a relay on 127.0.0.1 and must get the
;;;; answers back as gift wraps from the box; the bot's log must say a relay ACCEPTED each reply
;;;; (the relay's OK, which only arrives when the bot's owner pumps); a stranger's `devices' must be
;;;; denied with silence; chatter must be ignored; and STOP-NOSTR-BOT must end every thread it
;;;; started.  Prints a verdict, exits 0 iff every check passes.
;;;;
;;;; THE RELAY: $GLASS_TEST_RELAY (ws://127.0.0.1:PORT, e.g. `nak serve'), else cl-nostr's
;;;; inspect/fake-relay.lisp in this process, on an ephemeral port, stopped at the end.
;;;; WHO SERVICES THE BOT: $BOT_SERVICE -- `thread' (START-NOSTR-BOT :SERVICE :THREAD, SBCL's
;;;; default), `hook' (:HOOK; this test's loop calls GLASS:RUN-OWNER-HOOKS, as SERVE-WM's listener
;;;; does) or `manual' (NIL; this loop calls GLASS:NOSTR-BOT-SERVICE).  Default: thread on SBCL,
;;;; manual on modus.
;;;; The enrolment and login-code stores are fixtures under $TMPDIR (else /tmp).

(require :asdf)
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream))) (asdf:load-system :glass/nostr)))

(defpackage #:glass-nostr-bot-test (:use #:cl))
(in-package #:glass-nostr-bot-test)

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

;;; ---- the fixture --------------------------------------------------------------

(defparameter *tmp* (let ((d (or (env "TMPDIR") "/tmp")))
                      (if (char= (char d (1- (length d))) #\/) d (concatenate 'string d "/"))))
(defparameter *fixture* (format nil "~aglass-nostr-bot-test-~d-devices" *tmp* (random 1000000 (make-random-state t))))

(defun hex-secret ()
  (cl-nostr.util:bytes->hex (cl-nostr.util:random-bytes 32)))

(defparameter *box-sec* (hex-secret))
(defparameter *client* (cl-nostr.keys:keypair-from-secret (hex-secret)))
(defparameter *stranger* (cl-nostr.keys:keypair-from-secret (hex-secret)))
(defun pub (kp) (string-downcase (cl-nostr.keys:public-hex kp)))

(setf glass:*enrolment-file* *fixture*
      glass::*enrolments-mtime* nil
      glass:*box-secret* *box-sec*
      glass:*login-url-base* "https://example.invalid/k.html")
(glass:refresh-nostr-allow (pub *client*))
(clrhash glass:*enrolments*)
(setf glass:*login-codes* (make-instance 'glass:login-code-store))

;;; ---- the relay -------------------------------------------------------------------

(defparameter *fake* nil)
(defparameter *url*
  (or (env "GLASS_TEST_RELAY")
      (progn
        (load (asdf:system-relative-pathname "cl-nostr" "inspect/fake-relay.lisp"))
        (let ((port (funcall (find-symbol "FREE-PORT" "CL-NOSTR.FAKE-RELAY"))))
          (setf *fake* (funcall (find-symbol "START-FAKE-RELAY" "CL-NOSTR.FAKE-RELAY") :port port))
          (format nil "ws://127.0.0.1:~d" port)))))

(format t "~&glass nostr bot test: relay ~a, service ~(~a~)~%" *url* *mode*)

;;; ---- the bot --------------------------------------------------------------------

(defvar *said* '())
(defvar *said-lock* (sb-thread:make-mutex :name "said"))
(defun said () (sb-thread:with-mutex (*said-lock*) (copy-list *said*)))
(defun said-matching (&rest words)
  (find-if (lambda (l) (every (lambda (w) (search w l)) words)) (said)))

(defparameter *bot*
  (glass:start-nostr-bot :relays (list *url*)
                         :service (ecase *mode* (:thread :thread) (:hook :hook) (:manual nil))
                         :log (lambda (m) (sb-thread:with-mutex (*said-lock*) (push m *said*)))))

(defun drive (seconds pred)
  "Do the owner's part for up to SECONDS, until PRED.  The value of PRED, or NIL."
  (let ((end (+ (get-internal-real-time) (round (* seconds internal-time-units-per-second)))))
    (loop
      (let ((v (funcall pred))) (when v (return v)))
      (when (>= (get-internal-real-time) end) (return nil))
      (ecase *mode*
        (:manual (glass:nostr-bot-service *bot* 0.05))
        (:hook (glass:run-owner-hooks) (sleep 0.05))
        (:thread (sleep 0.05))))))

(check "the bot started" (glass:nostr-bot-p *bot*))
(check "it is the session's bot" (eq glass:*session-nostr-bot* *bot*))
(check "an owner hook is registered iff :HOOK"
       (eq (eq *mode* :hook) (and (assoc *bot* glass:*owner-hooks*) t)))
(check "its pubkey is the box's" (equal (glass:nostr-bot-pubkey *bot*) (glass:box-pubkey)))

;;; ---- the client -------------------------------------------------------------------

(defparameter *inbox* (bobbin:make-channel :capacity (* 1024 1024) :name "test client"))
(defparameter *client-pool* (cl-nostr.pool:make-pool (list *url*) :timeout 10))
(cl-nostr.pool:pool-subscribe
 *client-pool*
 (cl-nostr.filter:make-filter :kinds '(1059) :tags (list (cons "p" (list (pub *client*)))))
 :on-event (lambda (wrap relay)
             (declare (ignore relay))
             (ignore-errors (bobbin:channel-send *inbox* (cl-nostr.event:event->json wrap) :timeout 1))))

(defvar *replies* '())
(defun take-replies ()
  "Every wrap that has come for the client and unwraps from the box: (plaintext . sender)."
  (loop (multiple-value-bind (json ok) (bobbin:channel-try-receive *inbox*)
          (unless ok (return))
          (multiple-value-bind (text sender)
              (ignore-errors (cl-nostr.nip59:unwrap-giftwrap *client* (cl-nostr.event:json->event json)))
            (when text (push (cons text sender) *replies*)))))
  *replies*)
(defun reply-matching (word)
  (take-replies)
  (find-if (lambda (r) (search word (car r))) *replies*))

(defun dm (from text)
  "Gift-wrap TEXT from FROM to the box and publish it; T if the relay accepted it."
  (let ((acks (cl-nostr.pool:pool-publish-sync
               *client-pool* (cl-nostr.nip59:build-giftwrap from (glass:box-pubkey) text))))
    (and acks (every #'cl-nostr.pool:ack-accepted-p acks))))

(defparameter *wait* #+modus 240 #-modus 60)

;;; help
(check "the client's `help' DM was accepted by the relay" (dm *client* "help"))
(let ((r (drive *wait* (lambda () (reply-matching "Commands:")))))
  (check "the bot answered `help'" r (and r (subseq (car r) 0 (min 40 (length (car r))))))
  (check "  …from the box's key" (and r (equal (string-downcase (cdr r)) (glass:box-pubkey))))
  (check "  …with the allowlist's commands" (and r (search "devices" (car r)))))
(check "the log says a relay ACCEPTED the reply (its OK, pumped by the owner)"
       (drive 30 (lambda () (said-matching "help reply to" "accepted by" *url*)))
       (said-matching "help reply"))

;;; link: a command that writes the login-code store, which is the owner's
(check "the client's `link' DM was accepted" (dm *client* "link"))
(let ((r (drive *wait* (lambda () (reply-matching "example.invalid/k.html")))))
  (check "the bot answered `link' with a login link" r)
  (check "  …carrying a code" (and r (search "code=" (car r)))))
(check "the code was recorded in the store (on the owner)"
       (plusp (glass:login-code-count)) (glass:login-code-count))
(check "the log says a relay accepted the link reply"
       (drive 30 (lambda () (said-matching "link reply to" "accepted by"))))

;;; a stranger is denied, with silence
(check "the stranger's `devices' DM was accepted by the relay" (dm *stranger* "devices"))
(check "the bot denied it" (drive *wait* (lambda () (plusp (glass:nostr-bot-denied *bot*))))
       (glass:nostr-bot-report *bot*))
(check "  …and said so in its log" (said-matching "DENIED"))

;;; chatter is not a command
(check "a long non-command DM was accepted" (dm *client* (make-string 120 :initial-element #\x)))
(check "the bot ignored it" (drive *wait* (lambda () (plusp (glass:nostr-bot-ignored *bot*))))
       (glass:nostr-bot-report *bot*))

(check "counters: 4 received, 2 answered, 1 denied, 1 ignored"
       (and (= 4 (glass:nostr-bot-received *bot*)) (= 2 (glass:nostr-bot-answered *bot*))
            (= 1 (glass:nostr-bot-denied *bot*)) (= 1 (glass:nostr-bot-ignored *bot*)))
       (glass:nostr-bot-report *bot*))
(check "the stranger got no reply (only `help' and `link' came back)"
       (progn (take-replies) (= 2 (length *replies*))) (length *replies*))

;;; stop
(check "STOP-NOSTR-BOT returns T" (eq t (glass:stop-nostr-bot *bot*)))
(check "every thread the bot started has ended"
       (drive 10 (lambda () (glass:nostr-bot-stopped-p *bot*))))
(check "it is no longer the session's bot" (null glass:*session-nostr-bot*))
(check "no owner hook is left behind" (null (assoc *bot* glass:*owner-hooks*)))
(cl-nostr.pool:close-pool *client-pool*)
(when *fake* (funcall (find-symbol "STOP-FAKE-RELAY" "CL-NOSTR.FAKE-RELAY") *fake*))
(defun threads-named (prefix)
  (remove-if-not (lambda (th) (and (sb-thread:thread-alive-p th)
                                   (let ((n (sb-thread:thread-name th)))
                                     (and (stringp n) (eql 0 (search prefix n))))))
                 (sb-thread:list-all-threads)))
(check "no glass-nostr thread is running"
       (drive 10 (lambda () (null (threads-named "glass-nostr")))))
(check "no cl-nostr relay or delivery thread is running"
       (drive 10 (lambda () (and (null (threads-named "cl-nostr relay"))
                                 (null (threads-named "cl-nostr delivery")))))
       (mapcar #'sb-thread:thread-name (append (threads-named "cl-nostr"))))

(ignore-errors (delete-file *fixture*))
(ignore-errors (delete-file (concatenate 'string *fixture* ".codes")))
(format t "~&checks: ~d   failures: ~d   => ~:[FAIL~;PASS~]~%" *checks* *fails* (zerop *fails*))
(finish-output)
(sb-ext:exit :code (if (zerop *fails*) 0 1) :abort t)
