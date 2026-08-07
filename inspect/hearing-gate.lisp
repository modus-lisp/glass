;;;; hearing-gate.lisp — the desktop hears the desktop.
;;;;
;;;;   sbcl --dynamic-space-size 8192 --non-interactive --load inspect/hearing-gate.lisp
;;;;
;;;; src/hearing.lisp claims stave on the session mix is the mirror of chord on the session mix.
;;;; The only test of that worth having is the loop closed: put audio in the mix the way an
;;;; application does, and read back what the ear wrote down.  Nothing is handed to the
;;;; recognizer directly — every sample goes 16 kHz in, up to the mixer's 48, back down through
;;;; a sink, and through the level gate, which is exactly the path a window's Listen button
;;;; opens and exactly where a resampler or a threshold can quietly ruin a transcript.
;;;;
;;;; Two checks, and the second is the one that is worth having:
;;;;
;;;;   * A KNOWN RECORDING.  0.wav, whose transcript stave's own gates hold to the word.  Round
;;;;     tripping it through 48 kHz is not bit-exact and is not expected to be, so this is scored
;;;;     on word error rate against what stave says about the same file directly — the question
;;;;     being whether the MIX is a lossy channel, and how lossy.
;;;;
;;;;   * THE VOICE, HEARD BY THE EAR.  SPEAK a sentence and read it back off the ear.  Nobody
;;;;     wrote a file, nothing was decoded from disk: chord synthesized it, the mixer carried it,
;;;;     stave transcribed it, all in one image and all in Lisp.  Scored on word error rate too,
;;;;     because a TTS voice saying a sentence is not a recording of a person saying it and the
;;;;     recognizer is entitled to hear it slightly differently.
;;;;
;;;; Skipped rather than failed when the models are absent: GLASS_EARS for stave's export
;;;; directory, GLASS_VOICE for chord's .graph.  A box without them can still run the gate and
;;;; be told what it could not check.

(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (asdf:load-system :glass/hearing)
    (ignore-errors (asdf:load-system :glass/speech))))

(defpackage #:glass-hearing-gate (:use #:cl)) (in-package #:glass-hearing-gate)

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
(unless glass:*speech-voice*
  (let ((here "/mnt/lisp/chord/export/en_US-lessac-medium.graph"))
    (when (probe-file here) (setf glass:*speech-voice* here))))

(defparameter *wav* "/mnt/lisp/stave/models/zipformer-en/test_wavs/0.wav")

;;; ---- scoring ---------------------------------------------------------------

(defun wer (got want)
  "Word error rate of GOT against WANT — the reference is what we meant to hear."
  (values (funcall (find-symbol "WORD-ERROR-RATE" :stave) want got)))

(defun to-pcm16 (samples)
  "stave's floats in [-1, 1] as the signed 16-bit the mixer plays."
  (let ((pcm (reed:make-pcm16 (length samples))))
    (dotimes (i (length samples) pcm)
      (setf (aref pcm i) (reed:clamp16 (round (* 32767 (aref samples i))))))))

(defun wait-for (test &key (seconds 60) (step 0.25))
  "Poll TEST until it is true or SECONDS run out.  Returns what TEST last said."
  (let ((deadline (+ (get-internal-real-time)
                     (round (* seconds internal-time-units-per-second))))
        (last nil))
    (loop (setf last (funcall test))
          (when last (return last))
          (when (> (get-internal-real-time) deadline) (return last))
          (sleep step))))

;;; ---- the checks ------------------------------------------------------------

(defun hear-through-the-mix (samples rate seconds label)
  "Play SAMPLES into the session mix and return what the ear made of them."
  (progn
    (glass:start-listening)
    ;; a quarter of a gigabyte of weights has to be read before the ear collects anything, and
    ;; audio played into the mix meanwhile is played to nobody — which is not a fact about the
    ;; recognizer and must not be scored as one
    (wait-for #'glass:hearing-ready-p :seconds 300)
    (format t "  ~a: ~,1f s of audio into the mix~%" label seconds)
    (finish-output)
    (glass:mixer-play (glass:session-mixer) samples :name "gate" :rate rate)
    ;; wait for the utterance to be picked up, then for it to end in silence
    (wait-for (lambda () (plusp (length (glass:hearing-text)))) :seconds 180)
    (wait-for (lambda () (and (plusp (length (glass:hearing-heard)))
                              (string= "" (glass:hearing-partial))))
              :seconds (+ 120 (* 4 seconds)))
    (prog1 (glass:hearing-text)
      (glass:hearing-clear))))

(defun check-recording ()
  (cond
    ((not (probe-file *wav*)) (skip "a known recording" (format nil "no ~a" *wav*)))
    ((null glass:*hearing-models*) (skip "a known recording" "no stave models (set GLASS_EARS)"))
    (t
     (multiple-value-bind (samples rate)
         (funcall (find-symbol "READ-WAV" :stave) *wav*)
       (let* ((rec (funcall (find-symbol "LOAD-RECOGNIZER" :stave) glass:*hearing-models*))
              (want (funcall (find-symbol "RECOGNIZE-SAMPLES" :stave) rec samples))
              (seconds (/ (length samples) (float rate 1d0)))
              (pcm (to-pcm16 samples))
              (got (hear-through-the-mix pcm rate seconds "0.wav")))
         (format t "    want: ~a~%    got:  ~a~%" want got)
         (let ((rate% (* 100 (wer got want))))
           (check-that "a known recording survives the mix"
                       (< rate% 15d0)
                       (format nil "~,1f% word error through 16k->48k->16k" rate%))))))))

(defun check-the-voice ()
  (let ((line "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG"))
    (cond
      ((null glass:*hearing-models*) (skip "the desktop hears its own voice" "no stave models"))
      ((null glass:*speech-voice*) (skip "the desktop hears its own voice" "no chord voice"))
      (t
       (glass:start-listening)
       (glass:speak line)
       (wait-for (lambda () (glass:speaking-p)) :seconds 120)     ; synthesis started
       (wait-for (lambda () (not (glass:speaking-p))) :seconds 180) ; ...and finished
       (let ((got (wait-for (lambda () (and (plusp (length (glass:hearing-heard)))
                                            (string= "" (glass:hearing-partial))
                                            (glass:hearing-text)))
                            :seconds 120)))
         (format t "    said:  ~a~%    heard: ~a~%" line (or got ""))
         (let ((rate% (* 100 (wer (or got "") line))))
           (check-that "the desktop hears its own voice"
                       (< rate% 25d0)
                       (format nil "~,1f% word error, chord -> mixer -> stave" rate%))))))))

;;; ---- run -------------------------------------------------------------------

(format t "~&glass hearing gate~%")
(format t "  ears:  ~:[none (set GLASS_EARS)~;~:*~a~]~%" glass:*hearing-models*)
(format t "  voice: ~:[none (set GLASS_VOICE)~;~:*~a~]~%~%" glass:*speech-voice*)

(check-recording)
(check-the-voice)
(format t "~%  ~a~%" (glass:hearing-report))
(glass:stop-listening)

(format t "~%~d passed, ~d failed, ~d skipped — ~:[GATE RED~;GATE GREEN~]~%"
        *pass* *fail* *skip* (zerop *fail*))
(finish-output)
(sb-ext:exit :code (if (plusp *fail*) 1 0))
