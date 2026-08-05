;;;; inspect/clipboard-gate.lisp — the session clipboard, and the claims it exists to make good on.
;;;;
;;;; A clipboard is easy to write in a way that works for one writer and one reader and is wrong
;;;; the moment a session has two transports, which is precisely the case it exists for.  So the
;;;; load-bearing checks here are the ones a store-a-string test cannot make:
;;;;
;;;;   * a foreign DISOWN does not clear somebody else's selection.  A clipboard that is a bare
;;;;     string passes every other check in this file and fails this one, because it has no way
;;;;     to tell whose selection it is holding — and that is the check that makes "the selection
;;;;     went away when that app closed" safe to implement at all.
;;;;   * the same text re-asserted is not a change.  Without that, two viewers of one session
;;;;     hand one string back and forth forever: A copies, we tell B, B's viewer sets its local
;;;;     clipboard and sends it back, and around again.
;;;;   * a client is never sent its own cut text.  Same loop, one hop shorter.
;;;;   * a provider is called when somebody PASTES, not when somebody copies, and once per value
;;;;     however many consumers read it.
;;;;
;;;; The RFB half is checked against a real socket — handshake, ClientCutText in, ServerCutText
;;;; out — because the interesting failures there (a negative length read as unsigned, cut text
;;;; written from the reader thread into the middle of a rect) are invisible to a unit test that
;;;; calls the functions directly.
;;;;
;;;;   sbcl --non-interactive --load inspect/clipboard-gate.lisp

(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (asdf:load-system :glass)))

(defpackage #:glass-clipboard-gate (:use #:cl)) (in-package #:glass-clipboard-gate)

(defvar *pass* 0) (defvar *fail* 0)
(defun check (name got want)
  (if (equal got want) (progn (incf *pass*) (format t "  ok   ~a = ~s~%" name got))
      (progn (incf *fail*) (format t "  FAIL ~a: got ~s, want ~s~%" name got want)))
  (finish-output))
(defun check-that (name ok &optional detail)
  (if ok (progn (incf *pass*) (format t "  ok   ~a~@[ — ~a~]~%" name detail))
      (progn (incf *fail*) (format t "  FAIL ~a~@[ — ~a~]~%" name detail)))
  (finish-output))

;;; ---- ownership -------------------------------------------------------------

(format t "~&~%-- ownership --~%")
(let ((cb (glass:make-clipboard))
      (a (list :app-a)) (b (list :app-b)) (events '()))
  (glass:clipboard-listen cb :probe (lambda (c s o) (declare (ignore c)) (push (list s o) events)))
  (check-that "an empty clipboard has no selection" (null (glass:clipboard-text cb)))
  (check "and serial 0" (glass:clipboard-serial cb) 0)
  (glass:clipboard-own cb a :text "hello" :name "A")
  (check "text after own" (glass:clipboard-text cb) "hello")
  (check "serial bumped" (glass:clipboard-serial cb) 1)
  (check-that "the owner is who wrote it" (eq (glass:clipboard-owner cb) a))
  (check "one notification" (length events) 1)

  ;; not a mix: the second writer wins entirely
  (glass:clipboard-own cb b :text "world" :name "B")
  (check "last write wins whole" (glass:clipboard-text cb) "world")

  ;; the loop guard
  (let ((n (length events)))
    (glass:clipboard-own cb a :text "world" :name "A")
    (check "the same text re-asserted is not a change" (glass:clipboard-serial cb) 2)
    (check "and notifies nobody" (length events) n)
    (check-that "though ownership does move" (eq (glass:clipboard-owner cb) a)))

  ;; the check a bare string cannot make
  (check-that "B cannot disown A's selection" (not (glass:clipboard-disown cb b)))
  (check "which is still there" (glass:clipboard-text cb) "world")
  (check-that "A can disown its own" (glass:clipboard-disown cb a))
  (check-that "and then there is no selection" (null (glass:clipboard-text cb)))
  (check-that "a disown notifies with no owner" (null (second (first events))))

  (glass:clipboard-unlisten cb :probe)
  (let ((n (length events)))
    (glass:clipboard-set cb "after")
    (check "unlisten stops the notifications" (length events) n)))

(format t "~&~%-- a provider answers a late reader --~%")
(let ((cb (glass:make-clipboard)) (calls 0))
  (glass:clipboard-own cb :owner :provider (lambda () (incf calls) "lazy value") :name "provider")
  (check "declaring the selection does not serialize it" calls 0)
  (check "a read does" (glass:clipboard-text cb) "lazy value")
  (glass:clipboard-text cb) (glass:clipboard-text cb)
  (check "and only once per value, however many consumers read" calls 1)
  (glass:clipboard-set cb "plain")
  (check "a new value drops the memo" (glass:clipboard-text cb) "plain"))

(format t "~&~%-- one broken consumer is not everybody's problem --~%")
(let ((cb (glass:make-clipboard)) (other 0))
  (glass:clipboard-listen cb :bad (lambda (c s o) (declare (ignore c s o)) (error "boom")))
  (glass:clipboard-listen cb :good (lambda (c s o) (declare (ignore c s o)) (incf other)))
  (let ((*error-output* (make-broadcast-stream))) (glass:clipboard-set cb "x"))
  (check "a signalling listener does not fail the write" (glass:clipboard-text cb) "x")
  (check "and the next listener still runs" other 1))

;;; ---- Latin-1 ---------------------------------------------------------------

(format t "~&~%-- Latin-1, completely --~%")
(flet ((rt (s) (glass:latin1-string (glass:latin1-bytes s))))
  (check "empty" (rt "") "")
  (check "ascii" (rt "hello world") "hello world")
  (check "high bytes" (rt "café ÿÀÐ½") "café ÿÀÐ½")
  (check "0xff" (rt (string (code-char 255))) (string (code-char 255)))
  (check "0xa0 (nbsp)" (rt (string (code-char 160))) (string (code-char 160)))
  (check "embedded newlines" (rt (format nil "a~%b~%c")) (format nil "a~%b~%c"))
  (check "CRLF folds to LF" (rt (format nil "a~c~%b" #\Return)) (format nil "a~%b"))
  (check "a lone CR becomes LF" (rt (format nil "a~cb" #\Return)) (format nil "a~%b"))
  (check "a trailing CR becomes LF" (rt (format nil "a~c" #\Return)) (format nil "a~%"))
  (check "NUL is a byte like any other" (length (glass:latin1-bytes (string (code-char 0)))) 1)
  (check "above Latin-1 substitutes" (rt "sn☃w") "sn?w")
  (check "substitution keeps the length" (length (glass:latin1-bytes "☃☃☃")) 3)
  (let ((big (make-string 200000 :initial-element #\z)))
    (check-that "200k round-trips" (string= (rt big) big)))
  (let ((all (coerce (loop for i from 0 to 255 unless (= i 13) collect (code-char i)) 'string)))
    (check-that "every byte but CR round-trips" (string= (rt all) all)
                "CR is not a byte the protocol carries — it is an LF")))

;;; ---- keysyms ---------------------------------------------------------------

(format t "~&~%-- paste types what a keysym can carry --~%")
(check "ascii keysyms are codepoints" (glass:paste-keysyms "Ab") '(65 98))
(check "newline is Return" (glass:paste-keysyms (format nil "a~%")) '(97 #xff0d))
(check "CRLF is ONE Return" (glass:paste-keysyms (format nil "a~c~%" #\Return)) '(97 #xff0d))
(check "tab is Tab" (glass:paste-keysyms (string #\Tab)) '(#xff09))
(check "high Latin-1 is its own keysym" (glass:paste-keysyms "é") (list (char-code #\é)))
(check-that "a control character is dropped, not typed as garbage"
            (null (glass:paste-keysyms (string (code-char 7)))))
(check "a URL" (glass:paste-keysyms "http://x/") (map 'list #'char-code "http://x/"))

(let* ((typed '())
       (glass:*key-injector* (lambda (down k) (when down (push k typed)))))
  (glass:paste-text "hi!" :delay 0 :wait t)
  (check "the injector saw the text" (map 'string #'code-char (nreverse typed)) "hi!"))
(let* ((pairs '())
       (glass:*key-injector* (lambda (down k) (push (cons down k) pairs))))
  (glass:paste-text "a" :delay 0 :wait t)
  (check "every down has its up" (reverse pairs) '((t . 97) (nil . 97))))
(let* ((n 0) (glass:*key-injector* (lambda (d k) (declare (ignore d k)) (incf n)))
       (glass:*paste-max-chars* 10) (*error-output* (make-broadcast-stream)))
  (glass:paste-text (make-string 100 :initial-element #\a) :delay 0 :wait t)
  (check "a huge paste is truncated, not refused" n 20))
(let ((glass:*key-injector* nil))
  (check "no injector = no keys and no error" (glass:paste-text "x" :wait t) 0))

;;; ---- the RFB transport, on a real socket -----------------------------------

(format t "~&~%-- RFB cut text, both directions --~%")

(defun rd (s n) (let ((b (make-array n :element-type '(unsigned-byte 8)))) (read-sequence b s) b))
(defun u32 (s) (let ((b (rd s 4))) (logior (ash (aref b 0) 24) (ash (aref b 1) 16)
                                           (ash (aref b 2) 8) (aref b 3))))
(defun u16 (s) (let ((b (rd s 2))) (logior (ash (aref b 0) 8) (aref b 1))))
(defun wu8 (s v) (write-byte (logand v #xff) s))
(defun wu16 (s v) (wu8 s (ash v -8)) (wu8 s v))
(defun wu32 (s v) (wu16 s (ash v -16)) (wu16 s v))

(defvar *port* 5999)
(defvar *typed* '())
(defvar *fb* (glass:make-framebuffer 64 48))

(defun connect ()
  "RFB 3.8, None auth.  Returns the stream just after ServerInit."
  (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (sb-bsd-sockets:socket-connect sock #(127 0 0 1) *port*)
    (let ((s (sb-bsd-sockets:socket-make-stream sock :input t :output t
                                                :element-type '(unsigned-byte 8) :buffering :full)))
      (rd s 12)
      (write-sequence (map 'vector #'char-code (format nil "RFB 003.008~c" #\Newline)) s)
      (force-output s)
      (rd s (read-byte s))                          ; security types
      (wu8 s 1) (force-output s)                    ; None
      (u32 s)                                       ; security result
      (wu8 s 1) (force-output s)                    ; ClientInit: shared
      (u16 s) (u16 s) (rd s 16) (rd s (u32 s))      ; ServerInit
      s)))

(defun client-cut (s text &key bytes len)
  (let ((b (or bytes (glass:latin1-bytes text))))
    (wu8 s 6) (wu8 s 0) (wu8 s 0) (wu8 s 0)
    (wu32 s (or len (length b))) (write-sequence b s) (force-output s)))

(defun server-cut (s &key (timeout 3))
  "The next ServerCutText's text, or NIL if none arrives within TIMEOUT."
  (let ((deadline (+ (get-internal-real-time) (round (* timeout internal-time-units-per-second)))))
    (loop
      (cond ((> (get-internal-real-time) deadline) (return nil))
            ((not (listen s)) (sleep 0.01))
            (t (let ((type (read-byte s)))
                 (if (= type 3)
                     (progn (rd s 3) (return (glass:latin1-string (rd s (u32 s)))))
                     (return (list :unexpected-message type)))))))))

(defun fresh-clipboard ()
  ;; SETF, not a LET binding: a rebinding is thread-local, and the server's threads are not this
  ;; one — they would go on reading the global.
  (setf glass:*session-clipboard* (glass:make-clipboard)))

(sb-thread:make-thread
 (lambda ()
   (let ((*error-output* (make-broadcast-stream)))
     (glass:serve *fb* *port* :on-key (lambda (down k) (when down (push k *typed*)))
                              :name "clipboard-gate")))
 :name "clipboard-gate-server")
(sleep 0.6)

;;; paste in: ClientCutText -> the session clipboard -> keys into whatever has focus
(fresh-clipboard)
(let ((s (connect)))
  (setf *typed* '())
  (client-cut s "http://example.com/x")
  (sleep 0.4)
  (check "ClientCutText lands on the session clipboard"
         (glass:clipboard-text (glass:session-clipboard)) "http://example.com/x")
  (check-that "owned by the client that sent it"
              (not (eq (glass:clipboard-owner (glass:session-clipboard)) :local)))
  (glass:clipboard-paste :wait t)
  (check "and pasting types it through the server's own on-key"
         (map 'string #'code-char (reverse *typed*)) "http://example.com/x")

  (client-cut s (format nil "a~%b~cç" #\Return))
  (sleep 0.3)
  (check "high bytes and CR survive the wire"
         (glass:clipboard-text (glass:session-clipboard)) (format nil "a~%b~%ç"))
  (client-cut s "")
  (sleep 0.3)
  (check "an empty ClientCutText is the empty selection, not no selection"
         (glass:clipboard-text (glass:session-clipboard)) "")
  (let ((big (with-output-to-string (o)
               (dotimes (i 20000) (write-char (code-char (+ 32 (mod i 95))) o)))))
    (client-cut s big)
    (sleep 1.0)
    (check-that "20k arrives whole, over however many packets"
                (equal (glass:clipboard-text (glass:session-clipboard)) big)))
  ;; a negative length is the extended-clipboard message, not a 4 GB allocation
  (client-cut s "" :bytes (glass:latin1-bytes "0123") :len #xfffffffc)
  (sleep 0.3)
  (client-cut s "still alive")
  (sleep 0.3)
  (check "an extended-clipboard (negative) length is consumed, not fatal"
         (glass:clipboard-text (glass:session-clipboard)) "still alive")
  (close s))

;;; copy out
(fresh-clipboard)
(let ((s (connect)))
  (sleep 0.3)
  (glass:clipboard-set (glass:session-clipboard) "copied out ÿ")
  (check "a change reaches the client as ServerCutText" (server-cut s) "copied out ÿ")
  (glass:clipboard-set (glass:session-clipboard) (format nil "two~%lines"))
  (check "and so does the next" (server-cut s) (format nil "two~%lines"))
  (glass:clipboard-set (glass:session-clipboard) (format nil "two~%lines"))
  (check-that "an unchanged selection sends nothing" (null (server-cut s :timeout 0.5)))
  (glass:clipboard-set (glass:session-clipboard) "")
  (check "the empty selection is a zero-length message" (server-cut s) "")
  (close s))

(fresh-clipboard)
(let ((s (connect)))
  (sleep 0.3)
  (client-cut s "mine")
  (check-that "a client is never sent back its own cut text" (null (server-cut s :timeout 0.8))
              "the shorter half of the two-viewer ping-pong"))

(fresh-clipboard)
(glass:clipboard-set (glass:session-clipboard) "already here")
(let ((s (connect)))
  (check "a client joining a session that has a selection is told it" (server-cut s) "already here")
  (close s))

;;; the paste chord
(fresh-clipboard)
(glass:clipboard-set (glass:session-clipboard) "chord")
(let ((s (connect)))
  (setf *typed* '())
  (sleep 0.3)
  (flet ((key (down k) (wu8 s 4) (wu8 s (if down 1 0)) (wu8 s 0) (wu8 s 0) (wu32 s k)
           (force-output s)))
    (key t #xffe1) (key t #xff63) (key nil #xff63) (key nil #xffe1))
  (sleep 0.6)
  (check "Shift+Insert pastes"
         (map 'string #'code-char (remove-if (lambda (k) (> k 255)) (reverse *typed*))) "chord")
  (check-that "and consumes the Insert" (not (member #xff63 *typed*)))
  (check-that "but not the Shift" (and (member #xffe1 *typed*) t)
              "the app underneath still has to track its own modifiers")
  (close s))

;;; and none of it disturbs the pixels
(fresh-clipboard)
(let ((s (connect)))
  (wu8 s 2) (wu8 s 0) (wu16 s 1) (wu32 s 0) (force-output s)          ; SetEncodings: Raw
  (glass:fb-fill *fb* (glass:rgb 10 20 30))
  (glass:clipboard-set (glass:session-clipboard) "traffic during a frame")
  (sleep 0.2)
  (wu8 s 3) (wu8 s 0) (wu16 s 0) (wu16 s 0) (wu16 s 64) (wu16 s 48) (force-output s)
  (let ((cut nil) (update nil) (px nil)
        (deadline (+ (get-internal-real-time) (* 4 internal-time-units-per-second))))
    (loop while (and (< (get-internal-real-time) deadline)
                     (not (and (eq update t) cut)) (not (eq update :bad)))
          do (if (not (listen s))
                 (sleep 0.02)
                 (case (read-byte s)
                   (3 (rd s 3) (rd s (u32 s)) (setf cut t))
                   (0 (rd s 1)
                      (dotimes (i (u16 s))
                        (u16 s) (u16 s)
                        (let ((w (u16 s)) (h (u16 s)) (enc (u32 s)))
                          (when (zerop enc)
                            (let ((b (rd s (* 4 w h))))
                              (setf px (logior (ash (aref b 2) 16) (ash (aref b 1) 8) (aref b 0)))))))
                      (setf update t))
                   (t (setf update :bad)))))
    (check-that "a framebuffer update still arrives alongside cut text" (eq update t))
    (check "with the right pixels" px (glass:rgb 10 20 30))
    (check-that "and the cut text arrived too" cut))
  (close s))

(format t "~&~%~a~%" (glass:clipboard-report (glass:session-clipboard)))
(format t "~&~%~d passed, ~d failed~%" *pass* *fail*)
(finish-output)
(sb-ext:exit :code (if (plusp *fail*) 1 0))
