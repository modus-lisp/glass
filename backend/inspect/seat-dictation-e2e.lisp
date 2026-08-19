;;;; seat-dictation-e2e.lisp — two people speak, and each one's words land in their own window.
;;;;
;;;; seat-audio-gate.lisp checks the routing with the recognizer left out, because routing is
;;;; what it is about.  This runs the whole thing for real, end to end, twice at once:
;;;;
;;;;   chord synthesizes a sentence  ->  it is pushed into ONE SEAT's microphone port over a
;;;;   real socket, by the same GLASS:MAKE-MIC-SENDER the WebRTC gateway uses  ->  that seat's
;;;;   MIC-STREAM converts it  ->  that seat's EAR (a real stave recognizer) gates it, decodes
;;;;   it and finishes the utterance  ->  that seat's DICTATION types it on that seat's keyboard
;;;;   ->  the window THAT SEAT has focused writes it down.
;;;;
;;;; Two seats, two sentences, at the same time, on two ports.  The claim is that neither one's
;;;; words appear in the other's window — which is the whole of "my microphone is mine".
;;;;
;;;; SLOW ON PURPOSE: two recognizers are half a gigabyte of weights and take a couple of
;;;; minutes to read.  This is not the gate you run in a loop (that is seat-audio-gate.lisp);
;;;; it is the one that proves the gate is checking the right thing.  Needs stave and chord:
;;;; GLASS_EARS / GLASS_VOICE, or the paths below.
;;;;
;;;;   sbcl --control-stack-size 256 --dynamic-space-size 8192 \
;;;;        --non-interactive --load backend/inspect/seat-dictation-e2e.lisp

(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:glass :mcclim :mcclim-render :sb-concurrency))
    (asdf:load-system :glass/headset)
    (asdf:load-system :glass/hearing)
    (asdf:load-system :glass/dictation)
    (asdf:load-system :glass/speech)
    (asdf:load-asd (merge-pathnames "../mcclim-glass.asd" *load-truename*))
    (asdf:load-system :mcclim-glass)))
(in-package :clim-glass)

(unless glass:*hearing-models*
  (setf glass:*hearing-models* "/mnt/lisp/stave/export/"))
(unless glass:*speech-voice*
  (setf glass:*speech-voice* "/mnt/lisp/chord/export/en_US-lessac-medium.graph"))

(defvar *fail* 0)
(defun check (ok fmt &rest args)
  (format t "  [~:[FAIL~;pass~]] ~?~%" ok fmt args)
  (finish-output)
  (unless ok (incf *fail*)))
(defun say (fmt &rest args) (format t "~&~?~%" fmt args) (finish-output))

;;; ---- a sentence, as 8 kHz microphone samples ---------------------------------

(defun synth-8k (voice text)
  "TEXT through chord, at the rate a telephone microphone would deliver it — which is what a
WebRTC peer's G.711 gives the gateway, and therefore what arrives on the microphone port."
  (let ((out '()))
    (dolist (sentence (funcall (find-symbol "TEXT-TO-IPA" "CHORD") text))
      (when (plusp (length sentence))
        (multiple-value-bind (samples rate)
            (funcall (find-symbol "SYNTHESIZE" "CHORD") voice sentence)
          (let ((pcm (reed:make-pcm16 (length samples))))
            (dotimes (i (length samples))
              (setf (aref pcm i) (reed:clamp16 (round (* 30000 (aref samples i))))))
            (let ((rs (reed:make-resampler rate 8000)))
              (push (concatenate '(simple-array (signed-byte 16) (*))
                                 (reed:resample rs pcm)
                                 (reed:resample rs (reed:make-pcm16 0) :final t))
                    out))))))
    (apply #'concatenate '(simple-array (signed-byte 16) (*)) (nreverse out))))

(defun push-microphone (sender pcm &key (tail 2.0))
  "Push PCM into SENDER at 8 kHz in 20 ms frames, in real time, then TAIL seconds of quiet —
the silence is not padding, it is what tells the ear the sentence ENDED."
  (loop for at from 0 below (length pcm) by 160
        do (glass:mic-send sender (subseq pcm at (min (length pcm) (+ at 160))))
           (sleep 0.02))
  (let ((quiet (reed:make-pcm16 160)))
    (dotimes (i (round (/ tail 0.02)))
      (glass:mic-send sender quiet)
      (sleep 0.02))))

(defun add-typist (port title)
  (let* ((typed (make-array 0 :element-type 'character :adjustable t :fill-pointer t))
         (surf (add-surface port
                            (lambda (fb) (glass:fb-fill fb (glass:rgb 240 240 240))
                              (values (lambda (down k)
                                        (when (and down (<= 32 k 126))
                                          (vector-push-extend (code-char k) typed)))
                                      nil (constantly nil)))
                            :title title :width 300 :height 200)))
    (values surf (lambda () (coerce typed 'string)))))

;;; ---- the session, and two people on it ---------------------------------------

(defparameter +a-rfb+ 5941)
(defparameter +b-rfb+ 5961)

(defvar *port* (make-instance 'glass-port :port +a-rfb+))
(setf (glass-port-wm-p *port*) t)
(defvar *a* (glass-port-default-seat *port*))
(setf (seat-screen-w *a*) 800 (seat-screen-h *a*) 600
      (seat-fb *a*) (glass:make-framebuffer 800 600 +wm-teal+))
(defvar *b* (add-seat *port* :name "seat-B" :port-num +b-rfb+ :width 640 :height 480
                             :fb (glass:make-framebuffer 640 480 +wm-teal+)))
(start-seat-server *a*)
(start-seat-server *b*)
(sleep 0.3)

(defvar *ha* (start-seat-audio *port* :seat *a*))
(defvar *hb* (start-seat-audio *port* :seat *b*))
(say "~%seat A: screen :~d, mix :~d, microphone :~d"
     +a-rfb+ (glass:headset-audio-port *ha*) (glass:headset-mic-port *ha*))
(say "seat B: screen :~d, mix :~d, microphone :~d"
     +b-rfb+ (glass:headset-audio-port *hb*) (glass:headset-mic-port *hb*))

;;; the ears — two of them, reading their weights in parallel
(say "~%reading two recognizers (this is the slow part)...")
(defvar *t0* (get-internal-real-time))
(defvar *ea* (glass:headset-listen *ha*))
(defvar *eb* (glass:headset-listen *hb*))
(loop repeat 900
      until (and (glass:hearing-ready-p *ea*) (glass:hearing-ready-p *eb*))
      do (sleep 0.5))
(say "both ears ready after ~,0f s"
     (/ (- (get-internal-real-time) *t0*) internal-time-units-per-second))
(check (and (glass:hearing-ready-p *ea*) (glass:hearing-ready-p *eb*))
       "two seats, two ears, both listening")

;;; two windows, two focuses, two dictations
(multiple-value-bind (surf-a read-a) (add-typist *port* "A's window")
  (multiple-value-bind (surf-b read-b) (add-typist *port* "B's window")
    (setf (seat-focus-surface *a*) surf-a
          (seat-focus-surface *b*) surf-b)
    (let ((da (glass:headset-dictate *ha*))
          (db (glass:headset-dictate *hb*)))
      (check (and da db) "both seats are dictating, each on its own keyboard")

      ;; two sentences, said into two microphones, at the same time
      (let* ((voice (funcall (find-symbol "LOAD-VOICE" "CHORD") glass:*speech-voice*))
             (text-a "The first seat is counting one two three.")
             (text-b "The second seat is reading a different sentence.")
             (pcm-a (synth-8k voice text-a))
             (pcm-b (synth-8k voice text-b))
             (sa (glass:make-mic-sender :port (glass:headset-mic-port *ha*) :name "phone-a"))
             (sb (glass:make-mic-sender :port (glass:headset-mic-port *hb*) :name "phone-b")))
        (sleep 0.8)
        (say "~%speaking ~,1f s into A's microphone and ~,1f s into B's, at the same time"
             (/ (length pcm-a) 8000.0) (/ (length pcm-b) 8000.0))
        (let ((ta (sb-thread:make-thread (lambda () (push-microphone sa pcm-a)) :name "phone-a"))
              (tb (sb-thread:make-thread (lambda () (push-microphone sb pcm-b)) :name "phone-b")))
          (sb-thread:join-thread ta) (sb-thread:join-thread tb))
        ;; the words have to be decoded, then typed a keystroke at a time
        (loop repeat 120
              until (and (plusp (length (funcall read-a))) (plusp (length (funcall read-b))))
              do (sleep 0.5))
        (sleep 2)

        (let ((wa (funcall read-a)) (wb (funcall read-b)))
          (say "~%A's ear heard : ~s" (glass:hearing-text *ea*))
          (say "B's ear heard : ~s" (glass:hearing-text *eb*))
          (say "A's window has: ~s" wa)
          (say "B's window has: ~s" wb)
          (check (plusp (length wa)) "seat A's window was typed into")
          (check (plusp (length wb)) "seat B's window was typed into")
          (check (not (string= wa wb)) "and they did not receive the same words")
          (check (search "second" (string-downcase wb))
                 "B's window has what B's microphone said")
          (check (not (search "second" (string-downcase wa)))
                 "and A's window does not — A never said it")
          (check (search "one" (string-downcase wa))
                 "A's window has what A's microphone said")
          (check (not (search "one two three" (string-downcase wb)))
                 "and B's window does not")
          (check (eq (seat-focus-surface *a*) surf-a) "A's focus is where A left it")
          (check (eq (seat-focus-surface *b*) surf-b) "and B's is where B left it"))

        (say "~%~a" (glass:dictation-report da))
        (say "~a" (glass:dictation-report db))
        (glass:mic-sender-stop sa) (glass:mic-sender-stop sb)))))

(sleep 0.3)
(glass:stop-listening *ea*) (glass:stop-listening *eb*)
(stop-seat-audio *port* *a*) (stop-seat-audio *port* *b*)
(glass:mixer-stop (glass:session-mixer))

(format t "~&~%~:[~d check~:p FAILED~;all checks passed~]~%" (zerop *fail*) *fail*)
(finish-output)
(sb-ext:exit :code (if (plusp *fail*) 1 0))
