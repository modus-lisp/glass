;;;; dictation-gate.lisp — the ear typing into an app that has never heard of it.
;;;;
;;;;   sbcl --control-stack-size 256 --dynamic-space-size 8192 --non-interactive \
;;;;        --load backend/inspect/dictation-gate.lisp
;;;;
;;;; src/dictation.lisp claims that speech reaches the focused window as ORDINARY KEYSTROKES,
;;;; through the injector the RFB server installs, so that no app needs to know the ear exists.
;;;; A gate that checked it by watching *DICTATION-LAST* would prove only that a function ran.
;;;; So the target here is THE SPEAK WINDOW — a different app, in a different package, written
;;;; before dictation existed, whose text box is a plain input field and whose only relationship
;;;; to the ear is that both happen to be on this desktop.  Words appearing in that box came the
;;;; whole way: mix -> sink -> level gate -> decoder -> utterance flush -> keysyms -> the RFB
;;;; server's own on-key -> CLIM's focused sheet -> Drei.
;;;;
;;;; What each check is really for:
;;;;
;;;;   * SHAPING is pure and is checked without a desktop, including the case that is easy to get
;;;;     wrong (empty in, empty out — not a bare space, which would type a space per silence).
;;;;   * DICTATION OFF MUST TYPE NOTHING.  An ear that types when nobody asked is worse than an
;;;;     ear that does not work, because it types into whatever the user was doing.
;;;;   * THE BOX FILLS UP WITH WORDS NOBODY TYPED.  The feature.  Scored on word error against
;;;;     what the recognizer says about the same audio, so a keysym translation that drops
;;;;     characters shows up as error rather than as a shorter sentence nobody compared.
;;;;   * PARTIALS NEVER TYPE.  Checked by watching the box WHILE an utterance is being decoded:
;;;;     the transcript is already growing and the box must still be empty.  This is the one that
;;;;     protects against the unfixable bug — text typed and then revised.
;;;;   * THE DESKTOP MUST NOT TYPE WHAT IT SAID ITSELF.  Speak a sentence with dictation on; the
;;;;     ear hears it (it is on the mix), and the box must stay as it was.
;;;;   * STOP means stop, and starting twice types once.
;;;;
;;;; Serves on 5949 to stay clear of the live desktops and of listen-app-gate's 5948.

(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:mcclim :mcclim-render :sb-concurrency))
    (asdf:load-asd "/home/claude/glass/backend/mcclim-glass.asd")
    (asdf:load-system :mcclim-glass)
    (asdf:load-system :mcclim-glass/speak)
    (asdf:load-system :glass/dictation)
    (ignore-errors (asdf:load-system :glass/speech))))

(defpackage #:glass-dictation-gate (:use #:cl)) (in-package #:glass-dictation-gate)

(defvar *pass* 0) (defvar *fail* 0)
(defun check-that (name ok &optional detail)
  (if ok (progn (incf *pass*) (format t "  ok   ~a~@[ — ~a~]~%" name detail))
      (progn (incf *fail*) (format t "  FAIL ~a~@[ — ~a~]~%" name detail)))
  (finish-output))

(defparameter *port* 5949)
(defparameter *wav* "/mnt/lisp/stave/models/zipformer-en/test_wavs/0.wav")

(unless glass:*hearing-models*
  (let ((here "/mnt/lisp/stave/export/"))
    (when (probe-file here) (setf glass:*hearing-models* here))))
(unless glass:*speech-voice*
  (let ((here "/mnt/lisp/chord/export/en_US-lessac-medium.graph"))
    (when (probe-file here) (setf glass:*speech-voice* here))))

;;; ---- an RFB client with no screen ------------------------------------------
;;;
;;; It decodes nothing.  It exists so that GLASS:SERVE is really serving — which is what fills
;;; *KEY-INJECTOR*, and therefore the only reason any of this can type at all.

(defun w8 (s v) (write-byte (logand v #xff) s))
(defun w16 (s v) (w8 s (ash v -8)) (w8 s v))
(defun w32 (s v) (w16 s (ash v -16)) (w16 s v))
(defun rn (s n) (let ((b (make-array n :element-type '(unsigned-byte 8)))) (read-sequence b s) b))
(defun r8 (s) (read-byte s))
(defun r16 (s) (logior (ash (r8 s) 8) (r8 s)))
(defun r32 (s) (logior (ash (r16 s) 16) (r16 s)))

(defun connect (port)
  (loop repeat 600
        do (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
             (handler-case
                 (progn (sb-bsd-sockets:socket-connect
                         sock (sb-bsd-sockets:make-inet-address "127.0.0.1") port)
                        (return-from connect
                          (sb-bsd-sockets:socket-make-stream
                           sock :input t :output t :element-type '(unsigned-byte 8)
                                :buffering :full)))
               (error () (ignore-errors (sb-bsd-sockets:socket-close sock)) (sleep 0.05))))
        finally (error "dictation gate: nothing listening on ~d" port)))

(defun handshake (s)
  (rn s 12) (write-sequence (map 'vector #'char-code "RFB 003.008") s) (w8 s 10) (force-output s)
  (let ((n (r8 s))) (rn s n)) (w8 s 1) (force-output s) (r32 s)
  (w8 s 1) (force-output s)
  (let ((w (r16 s)) (h (r16 s)))
    (rn s 16) (let ((nl (r32 s))) (rn s nl))
    (w8 s 2) (w8 s 0) (w16 s 1) (w32 s 0)
    (force-output s)
    (values w h)))

;;; ---- helpers ----------------------------------------------------------------

(defun await (test &key (timeout 90))
  (let ((start (get-internal-real-time)))
    (loop until (funcall test)
          do (when (> (/ (- (get-internal-real-time) start) internal-time-units-per-second) timeout)
               (return-from await nil))
             (sleep 0.1))
    t))

(defun words (text)
  (let ((out '()) (cur (make-string-output-stream)))
    (loop for c across text
          do (if (member c '(#\Space #\Tab #\Newline #\Return))
                 (let ((w (get-output-stream-string cur)))
                   (when (plusp (length w)) (push w out)))
                 (write-char c cur)))
    (let ((w (get-output-stream-string cur))) (when (plusp (length w)) (push w out)))
    (nreverse out)))

(defun to-pcm16 (samples)
  (let ((pcm (reed:make-pcm16 (length samples))))
    (dotimes (i (length samples) pcm)
      (setf (aref pcm i) (reed:clamp16 (round (* 32767 (aref samples i))))))))

(defun wer (got want)
  "Word error rate, CASE-FOLDED.  stave's scorer compares words literally, which is right for
stave and wrong here: sentence-casing is the feature, so scoring `After' against `AFTER' as a
substitution would report 100% error on a perfect transcription — and it did, the first time this
gate ran.  Case is checked separately, by the check that looks at case."
  (values (funcall (find-symbol "WORD-ERROR-RATE" :stave)
                   (string-upcase want) (string-upcase got))))

;;; ---- 1. shaping, with no desktop at all -------------------------------------

(format t "~&dictation gate — ears ~:[NONE~;~:*~a~]~%~%" glass:*hearing-models*)
(format t "~&-- 1. what gets typed --------------------------------------------------~%")

(let ((got (glass:dictation-text "AFTER EARLY NIGHTFALL THE YELLOW LAMPS WOULD LIGHT UP")))
  (check-that "the recognizer's shouting becomes a sentence"
              (string= got "After early nightfall the yellow lamps would light up ")
              (format nil "~s" got)))
(check-that "and it ends in one space, so utterances do not run together"
            (let ((got (glass:dictation-text "HELLO")))
              (and (string= got "Hello ") (= 1 (- (length got) (length (string-right-trim " " got))))))
            (format nil "~s" (glass:dictation-text "HELLO")))
;; empty in, empty out: a silence that produced no words must not type a space
(check-that "nothing heard types nothing" (string= "" (glass:dictation-text ""))
            (format nil "~s" (glass:dictation-text "")))
(check-that "and neither does whitespace" (string= "" (glass:dictation-text "   "))
            (format nil "~s" (glass:dictation-text "   ")))

;;; ---- the window that will be typed into -------------------------------------
;;;
;;; The SPEAK window, deliberately: a different app in a different package, whose box is a plain
;;; text field.  Nothing in glass-speak knows the ear exists.

(defvar *frame*
  (let* ((p (clim-glass::find-glass-port :port *port*))
         (fm (clim:find-frame-manager :port p)))
    (climi::restart-port p)
    (clim:make-application-frame 'glass-speak:speak-box :width 520 :height 300
                                                       :frame-manager fm)))

(sb-thread:make-thread
 (lambda ()
   (handler-case (clim:run-frame-top-level *frame*)
     (serious-condition (e) (format t "~&FRAME DIED: ~a~%" e) (finish-output))))
 :name "speak-box")

(defvar *client* (connect *port*))
(handshake *client*)
(sleep 2.5)

(defun box () (string (clim:gadget-value (clim:find-pane-named *frame* 'glass-speak::input))))
(defun clear-box () (setf (clim:gadget-value (clim:find-pane-named *frame* 'glass-speak::input)) ""))

(format t "~&-- 2. a desktop with a keyboard and no dictation -----------------------~%")

(check-that "the server filled the injector — there is something to type into"
            glass:*key-injector* "glass:*key-injector* is set by SERVE")
(check-that "dictation starts off" (not (glass:dictating-p)) (glass:dictation-report))
(check-that "the box starts empty" (zerop (length (box))) (format nil "box reads ~s" (box)))

(cond
  ((null glass:*hearing-models*)
   (format t "~&-- no recognizer installed: the dictation checks CANNOT be run ---------~%")
   (incf *fail*))
  ((not (probe-file *wav*))
   (format t "~&-- no ~a: the dictation checks CANNOT be run --~%" *wav*)
   (incf *fail*))
  (t
   (multiple-value-bind (samples rate) (funcall (find-symbol "READ-WAV" :stave) *wav*)
     (let* ((pcm (to-pcm16 samples))
            (seconds (/ (length samples) (float rate 1d0)))
            (rec (funcall (find-symbol "LOAD-RECOGNIZER" :stave) glass:*hearing-models*))
            (want (funcall (find-symbol "RECOGNIZE-SAMPLES" :stave) rec samples)))

       ;; ---- 3. the ear is on, dictation is not --------------------------------
       (format t "~&-- 3. listening WITHOUT dictation --------------------------------------~%")
       (glass:start-listening)
       (check-that "the ear becomes ready" (await #'glass:hearing-ready-p :timeout 300)
                   (glass:hearing-report))
       (glass:mixer-play (glass:session-mixer) pcm :name "gate-off" :rate rate)
       (check-that "the ear hears it"
                   (await (lambda () (plusp (length (glass:hearing-heard))))
                          :timeout (+ 120 (* 4 seconds)))
                   (format nil "~d utterance(s)" (length (glass:hearing-heard))))
       ;; the important half: it heard a whole sentence and typed none of it
       (check-that "an ear with dictation OFF types nothing" (zerop (length (box)))
                   (format nil "box reads ~s" (box)))
       (glass:hearing-clear)

       ;; ---- 4. the feature ----------------------------------------------------
       (format t "~&-- 4. dictation ---------------------------------------------------------~%")
       (glass:start-dictation)
       (check-that "dictation is on and has a keyboard" (glass:dictating-p)
                   (glass:dictation-report))
       ;; wait out the start-up deafness before playing, or the guard eats the utterance
       (sleep (+ 0.2 glass:*dictation-tail-seconds*))
       (format t "     (~,1f s of speech into the session mix — nothing typed)~%" seconds)
       (glass:mixer-play (glass:session-mixer) pcm :name "gate-on" :rate rate)

       ;; PARTIALS MUST NOT TYPE: catch the moment the transcript exists and the box does not
       (let ((saw-partial (await (lambda () (plusp (length (glass:hearing-partial))))
                                 :timeout (+ 60 (* 2 seconds)))))
         (check-that "a partial transcript does not type as it is revised"
                     (and saw-partial (zerop (length (box))))
                     (format nil "partial ~s while the box reads ~s"
                             (glass:hearing-partial) (box))))

       (check-that "THE WORDS ARRIVE AS KEYSTROKES, in a window that knows nothing about the ear"
                   (await (lambda () (plusp (length (box)))) :timeout (+ 180 (* 4 seconds)))
                   (format nil "box reads ~s" (box)))
       ;; let the rest of the keys land — typing is asynchronous and paced
       (await (lambda () (>= (length (words (box))) (length (words want))))
              :timeout 60)
       (format t "     want: ~a~%     box:  ~a~%" want (box))
       (let ((rate% (* 100 (wer (box) want))))
         (check-that "and they are the words that were said"
                     (< rate% 15d0)
                     (format nil "~,1f% word error, mix -> ear -> keysyms -> Drei" rate%)))
       (check-that "typed as a sentence, not as shouting"
                   (let ((b (string-trim " " (box))))
                     (and (plusp (length b))
                          (upper-case-p (char b 0))
                          (notevery #'upper-case-p (remove-if-not #'alpha-char-p b))))
                   (format nil "box reads ~s" (box)))
       (check-that "the transcript still says what the recognizer really said"
                   (let ((tx (glass:hearing-text)))
                     (and (plusp (length tx))
                          (every (lambda (c) (or (not (alpha-char-p c)) (upper-case-p c))) tx)))
                   (format nil "transcript ~s" (glass:hearing-text)))

       ;; ---- 5. the desktop must not type what it said itself -------------------
       (format t "~&-- 5. the self-hearing guard --------------------------------------------~%")
       (cond
         ((null glass:*speech-voice*)
          (format t "     (no chord voice: the loop cannot be closed, so it CANNOT be checked)~%")
          (incf *fail*))
         (t
          (glass:hearing-clear)
          (clear-box)
          (let ((muted-before glass:*dictation-muted*)
                (typed-before glass:*dictation-typed*))
            (glass:speak "THE DESKTOP MUST NOT TYPE WHAT IT SAYS ITSELF")
            (await (lambda () (glass:speaking-p)) :timeout 120)
            (await (lambda () (not (glass:speaking-p))) :timeout 180)
            ;; give the ear time to finish the utterance it certainly heard
            (await (lambda () (plusp (length (glass:hearing-heard)))) :timeout 120)
            (sleep 2)
            (check-that "the ear DID hear the desktop's own voice"
                        (plusp (length (glass:hearing-text)))
                        (format nil "transcript ~s" (glass:hearing-text)))
            (check-that "and dictation typed none of it" (zerop (length (box)))
                        (format nil "box reads ~s" (box)))
            (check-that "and said so rather than dropping it silently"
                        (and (> glass:*dictation-muted* muted-before)
                             (= glass:*dictation-typed* typed-before))
                        (glass:dictation-report)))))

       ;; ---- 6. the switch ------------------------------------------------------
       (format t "~&-- 6. the switch --------------------------------------------------------~%")
       (glass:start-dictation)
       (glass:start-dictation)
       (check-that "switching dictation on twice registers one listener, not two"
                   (= 1 (length (glass::ear-listeners glass:*session-ears*)))
                   (format nil "~d listener(s) on the ear"
                           (length (glass::ear-listeners glass:*session-ears*))))
       (glass:hearing-clear)
       (clear-box)
       (check-that "Stop dictating returns T when it had been on" (glass:stop-dictation))
       (check-that "and it is off" (not (glass:dictating-p)) (glass:dictation-report))
       (check-that "and its listener is off the ear too"
                   (zerop (length (glass::ear-listeners glass:*session-ears*)))
                   (format nil "~d listener(s) on the ear"
                           (length (glass::ear-listeners glass:*session-ears*))))
       (sleep (+ 0.2 glass:*dictation-tail-seconds*))
       (glass:mixer-play (glass:session-mixer) pcm :name "gate-stopped" :rate rate)
       (await (lambda () (plusp (length (glass:hearing-heard)))) :timeout (+ 180 (* 4 seconds)))
       (sleep 3)
       (check-that "and a stopped dictation types nothing while the ear keeps listening"
                   (and (zerop (length (box))) (plusp (length (glass:hearing-text))))
                   (format nil "box ~s, transcript ~s"
                           (box) (subseq (glass:hearing-text)
                                         0 (min 40 (length (glass:hearing-text))))))
       (glass:stop-listening)))))

(format t "~&dictation gate: ~d passed, ~d failed~%" *pass* *fail*)
(finish-output)
(sb-ext:exit :code (if (plusp *fail*) 1 0))
