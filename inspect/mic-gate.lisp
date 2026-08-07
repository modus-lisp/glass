;;;; mic-gate.lisp — a peer's microphone, into the desktop's ear.
;;;;
;;;;   sbcl --dynamic-space-size 8192 --non-interactive --load inspect/mic-gate.lisp
;;;;
;;;; inspect/hearing-gate.lisp closes the loop through the MIXER: the desktop speaks and the
;;;; desktop hears itself.  This closes the other one, which is the one the ear was actually built
;;;; for — audio that arrived from somewhere else entirely, over a socket, at the wrong rate.
;;;;
;;;; Nothing is handed to the recognizer directly.  A known recording is band-limited to 8 kHz,
;;;; run through G.711 mu-law exactly as a phone's packet is, and pushed frame by frame at 20 ms
;;;; per frame into GLASS:MIC-SEND — which is the call webrtc-media's :ON-RX-PCM makes on the
;;;; gateway's receive path, with the same arguments, minus SRTP.  It goes over the wire, is
;;;; resampled back to 16 kHz by the desktop's end, is chosen by the ear over the session mix, and
;;;; comes out as text.  The score is word error rate against what stave says about the same
;;;; recording at full bandwidth, so what is being measured is the CHANNEL: the telephone band and
;;;; a codec designed in 1972 cost a recognizer something, and the number says how much.
;;;;
;;;; The other three checks are the ones that are about not breaking a desktop:
;;;;
;;;;   * NO PEER is not an error — no microphone, no crash, no log, and the ear on the mix.
;;;;   * A STALLED DESKTOP does not wedge the sender.  MIC-SEND runs on the thread that decrypts
;;;;     inbound media; if it can ever block, a slow ear stops a phone call's video.
;;;;   * ASKING DOES NOT CREATE.  LISTENING-P must not be what reads a quarter of a gigabyte of
;;;;     weights — the round before this one found exactly that wart in SPEAKING-P.

(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (asdf:load-system :glass/mic-stream)
    (ignore-errors (asdf:load-system :glass/hearing))))

(defpackage #:glass-mic-gate (:use #:cl)) (in-package #:glass-mic-gate)

(defvar *pass* 0) (defvar *fail* 0) (defvar *skip* 0)
(defun check-that (name ok &optional detail)
  (if ok (progn (incf *pass*) (format t "  ok   ~a~@[ — ~a~]~%" name detail))
      (progn (incf *fail*) (format t "  FAIL ~a~@[ — ~a~]~%" name detail)))
  (finish-output))
(defun skip (name why)
  (incf *skip*) (format t "  skip ~a — ~a~%" name why) (finish-output))

(unless glass:*hearing-models*
  (let ((here "/mnt/lisp/stave/export/"))
    (when (probe-file here) (setf glass:*hearing-models* here))))

(defparameter *wav* "/mnt/lisp/stave/models/zipformer-en/test_wavs/0.wav")
(defparameter *port* 5915)             ; a test port: the desktop's own is *MIC-STREAM-PORT*

(defun wait-for (test &key (seconds 60) (step 0.25))
  (let ((deadline (+ (get-internal-real-time)
                     (round (* seconds internal-time-units-per-second))))
        (last nil))
    (loop (setf last (funcall test))
          (when last (return last))
          (when (> (get-internal-real-time) deadline) (return last))
          (sleep step))))

(defun to-pcm16 (samples)
  (let ((pcm (reed:make-pcm16 (length samples))))
    (dotimes (i (length samples) pcm)
      (setf (aref pcm i) (reed:clamp16 (round (* 32767 (aref samples i))))))))

(defun telephone (pcm rate)
  "PCM at RATE as it would leave a phone: 8 kHz mono, mu-law encoded and decoded again.

Both halves matter.  The resampling is what makes this an 8 kHz signal and not a 16 kHz one with
a claim attached, and the codec round trip is the quantization G.711 actually costs — which is
the part a resampler alone would flatter."
  (let* ((rs (reed:make-resampler rate 8000))
         (body (reed:resample rs pcm))
         (tail (reed:resample rs (reed:make-pcm16 0) :final t))
         (all (reed:make-pcm16 (+ (length body) (length tail)))))
    (replace all body)
    (replace all tail :start1 (length body))
    (reed:pcmu-decode (reed:pcmu-encode all))))

(defun push-as-peer (sender pcm8 &key (frame 160))
  "Push PCM8 into SENDER 20 ms at a time, on a deadline — which is the pace RTP arrives at, and
therefore the only pace at which a bounded queue means what it says."
  (let* ((n (length pcm8))
         (tick (max 1 (round (* 0.02d0 internal-time-units-per-second))))
         (next (+ (get-internal-real-time) tick)))
    (loop for i from 0 below n by frame
          do (glass:mic-send sender (subseq pcm8 i (min n (+ i frame))))
             (let ((now (get-internal-real-time)))
               (when (> next now)
                 (sleep (/ (- next now) (float internal-time-units-per-second 1d0))))
               (setf next (if (< (+ next tick) now) (+ now tick) (+ next tick)))))))

;;; ---- with nobody on the line -----------------------------------------------

(defun check-no-peer ()
  (let ((ok (handler-case
                (progn (check-that "no microphone is not an error"
                                   (and (null (glass:session-mic))
                                        (null (glass:mic-next-frame))
                                        (search "none" (glass:mic-report))
                                        (search "not running" (glass:mic-stream-report)))
                                   (glass:mic-stream-report))
                       t)
              (serious-condition (e) (check-that "no microphone is not an error" nil (princ-to-string e))
                nil))))
    ok))

(defun check-asking-does-not-create ()
  "LISTENING-P, HEARING-TEXT and HEARING-REPORT are questions.  A question that loads a model is a
question nobody can afford to ask from a status line three times a second."
  (let ((before glass:*session-ears*))
    (glass:listening-p)
    (glass:hearing-text)
    (glass:hearing-partial)
    (glass:hearing-level)
    (glass:hearing-report)
    (glass:hearing-ready-p)
    (check-that "asking about the ear does not create one"
                (and (null before) (null glass:*session-ears*))
                "listening-p / hearing-text / hearing-report / hearing-ready-p")))

;;; ---- a sender whose desktop is not reading ---------------------------------

(defun check-non-blocking ()
  "The receive path must never wait for the ear.

Two ways for the far end to be useless: nowhere to connect at all, and connected but not draining.
Both are exercised against the SAME sender, and what is measured is the worst time MIC-SEND ever
took — because that time is time webrtc-media's inbound thread is not decrypting packets."
  (let* ((frame (reed:make-pcm16 160))
         (dead (glass:make-mic-sender :host "127.0.0.1" :port 5999 :name "gate-dead"))
         (worst 0d0))
    ;; 1. nowhere to connect: the writer thread is in its reconnect loop, the ring fills, and
    ;;    MIC-SEND drops.  5 s of microphone in as fast as it will go.
    (dotimes (i 250)
      (let ((t0 (get-internal-real-time)))
        (glass:mic-send dead frame)
        (setf worst (max worst (/ (- (get-internal-real-time) t0)
                                  (float internal-time-units-per-second 1d0))))))
    (check-that "no desktop: the sender drops and the caller returns"
                (and (< worst 0.02d0) (plusp (glass:mic-sender-dropped dead)))
                (format nil "worst mic-send ~,2f ms, ~a" (* 1000 worst) (glass:mic-sender-report dead)))
    (glass:mic-sender-stop dead)
    ;; 2. connected to a desktop that reads the header and then stops reading.  The kernel's
    ;;    buffers absorb a few hundred KB, then the socket backs up and the writer refuses to
    ;;    write into it — which is the guard %MIC-SENDER-SESSION exists for.
    (let* ((listener (glass:tcp-listen 5998 :address "127.0.0.1"))
           (server (sb-thread:make-thread
                    (lambda ()
                      (ignore-errors
                       (let* ((sock (sb-bsd-sockets:socket-accept listener))
                              (s (sb-bsd-sockets:socket-make-stream sock :input t :output t
                                                                    :element-type '(unsigned-byte 8)
                                                                    :buffering :full)))
                         ;; read the one line we owe an answer to, answer it, then go to sleep
                         ;; holding the connection open: a desktop whose ear has wedged
                         (loop for b = (read-byte s nil nil) until (or (null b) (= b 10)))
                         (loop for ch across (format nil "glass-mic/1 rate=16000 channels=1 frame=320 format=s16le ok~%")
                               do (write-byte (char-code ch) s))
                         (force-output s)
                         (sleep 60))))
                    :name "gate-stalled-desktop"))
           (stalled (glass:make-mic-sender :host "127.0.0.1" :port 5998 :name "gate-stalled"))
           (worst2 0d0))
      (sleep 0.5)
      (dotimes (i 3000)                 ; a minute of microphone, pushed as fast as it will go
        (let ((t0 (get-internal-real-time)))
          (glass:mic-send stalled frame)
          (setf worst2 (max worst2 (/ (- (get-internal-real-time) t0)
                                      (float internal-time-units-per-second 1d0))))))
      (sleep 1)
      (check-that "a stalled desktop does not wedge the sender"
                  (and (< worst2 0.02d0) (plusp (glass:mic-sender-dropped stalled)))
                  (format nil "worst mic-send ~,2f ms, ~a" (* 1000 worst2)
                          (glass:mic-sender-report stalled)))
      (glass:mic-sender-stop stalled)
      (ignore-errors (sb-thread:terminate-thread server))
      (ignore-errors (sb-bsd-sockets:socket-close listener)))))

;;; ---- the whole path --------------------------------------------------------

(defun check-the-microphone ()
  (cond
    ((not (probe-file *wav*)) (skip "a peer's microphone is transcribed" (format nil "no ~a" *wav*)))
    ((null glass:*hearing-models*) (skip "a peer's microphone is transcribed" "no stave models"))
    (t
     (multiple-value-bind (samples rate) (funcall (find-symbol "READ-WAV" :stave) *wav*)
       (let* ((rec (funcall (find-symbol "LOAD-RECOGNIZER" :stave) glass:*hearing-models*))
              (want (funcall (find-symbol "RECOGNIZE-SAMPLES" :stave) rec samples))
              (seconds (/ (length samples) (float rate 1d0)))
              (phone (telephone (to-pcm16 samples) rate))
              (srv (glass:start-mic-stream :port *port*))
              (sender (glass:make-mic-sender
                       :port *port* :name "gate"
                       :log (lambda (m) (format t "    [mic] ~a~%" m)))))
         (declare (ignore srv))
         (unwind-protect
              (progn
                ;; the ear, with NO source named: it should find the microphone by itself
                (glass:start-listening)
                (format t "  loading the recognizer...~%") (finish-output)
                (wait-for #'glass:hearing-ready-p :seconds 300)
                (check-that "with no peer talking, the ear is on the session mix"
                            (eq (glass::ear-listening-to glass:*session-ears*) :mix)
                            (glass:hearing-report))
                (format t "  ~,1f s of 8 kHz mu-law, pushed at 20 ms a frame~%" seconds)
                (finish-output)
                (push-as-peer sender phone)
                (check-that "the ear switched to the peer's microphone"
                            (eq (glass::ear-listening-to glass:*session-ears*) :peer)
                            (glass:mic-report))
                (let ((got (or (wait-for (lambda ()
                                           (and (plusp (length (glass:hearing-heard)))
                                                (string= "" (glass:hearing-partial))
                                                (glass:hearing-text)))
                                         :seconds (+ 60 (* 4 seconds)))
                               (glass:hearing-text))))
                  (format t "    want: ~a~%    got:  ~a~%" want got)
                  (let ((wer% (* 100 (funcall (find-symbol "WORD-ERROR-RATE" :stave) want got))))
                    (check-that "a peer's microphone is transcribed"
                                (< wer% 40d0)
                                (format nil "~,1f% word error through 8k mu-law + socket + 16k"
                                        wer%))))
                (format t "    ~a~%    ~a~%" (glass:mic-stream-report) (glass:hearing-report)))
           (glass:mic-sender-stop sender)
           (glass:stop-mic-stream)))))))

;;; ---- and back to nothing ---------------------------------------------------

(defun check-fallback ()
  "The microphone goes away and the ear goes back to the mix, without being restarted."
  (if (null glass:*session-ears*)
      (skip "the ear falls back to the mix" "no ear was started")
      (let ((back (wait-for (lambda () (eq (glass::ear-listening-to glass:*session-ears*) :mix))
                            :seconds 15)))
        (check-that "the ear falls back to the mix when the peer hangs up"
                    back (glass:hearing-report)))))

;;; ---- run -------------------------------------------------------------------

(format t "~&glass microphone gate~%")
(format t "  ears: ~:[none (set GLASS_EARS)~;~:*~a~]~%~%" glass:*hearing-models*)

(check-no-peer)
(check-asking-does-not-create)
(check-non-blocking)
(check-the-microphone)
(check-fallback)
(ignore-errors (glass:stop-listening))

(format t "~%~d passed, ~d failed, ~d skipped — ~:[GATE RED~;GATE GREEN~]~%"
        *pass* *fail* *skip* (zerop *fail*))
(finish-output)
(sb-ext:exit :code (if (plusp *fail*) 1 0))
