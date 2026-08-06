;;;; inspect/speech-gate.lisp — the desktop's voice, before it goes near the live desktop.
;;;;
;;;; A voice is easy to write in a way that makes a WAV and is wrong on a mixer.  The claims this
;;;; file exists to check are the ones a "did it produce samples" test cannot make:
;;;;
;;;;   * SPEAK returns before the words exist.  A caller is often the thing being announced about
;;;;     — a notification, a job finishing — and it cannot afford a second of synthesis.
;;;;   * the source thunk never blocks the clock.  It is called every 20 ms from the mixer's
;;;;     thread; if it waited on the synthesizer, every listener would hear the whole desktop
;;;;     stall, not just the speech.  Measured here as the worst single call, not the average.
;;;;   * two utterances queue rather than overlap, and their total length is the sum.  One voice,
;;;;     not one source per clip.
;;;;   * HUSH stops it, including the rest of a paragraph that is still being synthesized.
;;;;   * a sentence the engine cannot say is one sentence lost, counted, and not a dead voice.
;;;;
;;;; The mix is driven by hand (MIXER-TICK, no clock thread) so the test is deterministic, and the
;;;; audio is checked as audio: RMS in the band, and a duration that matches the words.
;;;;
;;;;   sbcl --dynamic-space-size 4096 --non-interactive --load inspect/speech-gate.lisp
;;;;
;;;; The voice comes from GLASS_VOICE, or the lessac medium export if that is unset.  It writes
;;;; /tmp/glass-speech-gate.wav so the run can be listened to as well as asserted about.

(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (asdf:load-system :glass/speech)))

(defpackage #:glass-speech-gate (:use #:cl)) (in-package #:glass-speech-gate)

(defvar *pass* 0) (defvar *fail* 0)
(defun check-that (name ok &optional detail)
  (if ok (progn (incf *pass*) (format t "  ok   ~a~@[ — ~a~]~%" name detail))
      (progn (incf *fail*) (format t "  FAIL ~a~@[ — ~a~]~%" name detail)))
  (finish-output))

(unless glass:*speech-voice*
  (setf glass:*speech-voice* "/mnt/lisp/quill/export/en_US-lessac-medium.graph"))

(defun rms (pcm)
  (if (zerop (length pcm))
      0d0
      (let ((sum 0d0))
        (dotimes (i (length pcm)) (let ((v (float (aref pcm i) 1d0))) (incf sum (* v v))))
        (sqrt (/ sum (length pcm))))))

(defun secs (pcm) (/ (length pcm) 48000d0))

(defun await (test &key (timeout 120) (what "condition"))
  "Wait for TEST, polling.  Returns how long it took, or signals — a gate that hangs forever
tells you nothing, and a gate that silently proceeds tells you something false."
  (let ((start (get-internal-real-time)))
    (loop until (funcall test)
          do (when (> (/ (- (get-internal-real-time) start) internal-time-units-per-second)
                      timeout)
               (error "speech gate: timed out after ~d s waiting for ~a" timeout what))
             (sleep 0.02))
    (/ (- (get-internal-real-time) start) (float internal-time-units-per-second 1d0))))

(defun drain (spk mixer &key (quiet-frames 10) (max-seconds 30))
  "Everything the speaker has to say, pulled through the mixer by hand.  Stops when the voice is
finished and QUIET-FRAMES frames have been silent, and returns the audio and the worst time a
single source-thunk call took (which is the number the clock cares about).

MAX-SECONDS is a stop, not a tuning knob: a hand-driven mix has no clock to run out, so a loop
that waits for audio that is never coming does not hang — it fills the heap."
  (let ((sink (glass:mixer-subscribe mixer :name "gate" :lead 0))
        (max-frames (round (* max-seconds 50)))
        (frames '()) (n 0) (loud 0) (quiet 0) (worst 0d0))
    (loop
      ;; nothing buffered but the engine is still building: tick on anyway (the pause is real
      ;; audio) at roughly a real clock's pace rather than spinning as fast as the CPU allows
      (when (and (null (glass::spk-ready spk)) (glass:speaking-p spk)) (sleep 0.005))
      (let ((t0 (get-internal-real-time)))
        (glass:mixer-tick mixer)
        (setf worst (max worst (/ (- (get-internal-real-time) t0)
                                  (float internal-time-units-per-second 1d0)))))
      (let ((f (glass:sink-next-frame sink)))
        (when f
          (push f frames)
          (incf n)
          (if (> (rms f) 20d0) (progn (incf loud) (setf quiet 0)) (incf quiet))))
      (when (or (and (>= quiet quiet-frames) (not (glass:speaking-p spk)))
                (>= n max-frames))
        (return)))
    (let* ((fs (nreverse frames))
           (total (reduce #'+ fs :key #'length))
           (v (reed:make-pcm16 total))
           (at 0))
      (dolist (f fs) (replace v f :start1 at) (incf at (length f)))
      ;; the third value is speech, not wall time: a hand-driven mix ticks faster than a clock
      ;; while the engine is building, so the pauses in the collected audio are as long as the
      ;; loop felt like making them and only the frames with sound in them mean anything.
      (values v worst (* 0.02d0 loud)))))

;;; ---- 1. speaking is queueing ----------------------------------------------
(format t "~&=== SPEAK returns before the words exist ===~%")
(defparameter *m* (glass:make-mixer))
(defparameter *spk* (glass:make-speaker :mixer *m*))

(let ((t0 (get-internal-real-time)))
  (glass:speak "Glass can speak." :speaker *spk*)
  (let ((elapsed (/ (- (get-internal-real-time) t0)
                    (float internal-time-units-per-second 1d0))))
    (check-that "SPEAK returns immediately" (< elapsed 0.05)
                (format nil "~,1f ms" (* 1000 elapsed)))))

(check-that "and the voice is on the mix as ONE source"
            (= 1 (length (glass:mixer-sources *m*)))
            (format nil "~s" (mapcar #'glass:src-name (glass:mixer-sources *m*))))

(let ((wait (await (lambda () (plusp (glass::spk-said *spk*)))
                   :what "the first utterance to be synthesized")))
  (check-that "the first utterance synthesizes off the clock" (zerop (glass::spk-failed *spk*))
              (format nil "~,2f s to build~@[, last error: ~a~]"
                      wait (glass::spk-last-error *spk*))))

(multiple-value-bind (audio worst) (drain *spk* *m*)
  (check-that "and the mix carries speech, not silence" (> (rms audio) 200d0)
              (format nil "rms ~,0f over ~,2f s" (rms audio) (secs audio)))
  (check-that "of about the length those three words take" (< 0.8 (secs audio) 3.0)
              (format nil "~,2f s" (secs audio)))
  ;; the real deadline: a 20 ms frame is due every 20 ms, so a tick that takes longer than that
  ;; is a listener hearing a gap.  Synthesis takes ~1 s; if any of it leaked onto this thread
  ;; the worst tick would be three orders of magnitude out.
  (check-that "no tick came near the 20 ms deadline" (< worst 0.005)
              (format nil "worst tick ~,2f ms" (* 1000 worst)))
  (reed:write-wav-file (reed:make-pcm :samples audio :sample-rate 48000 :channels 1)
                       "/tmp/glass-speech-gate.wav"))

;;; ---- 1b. a paragraph is not just its first sentence -----------------------
;;;
;;; Sentences are synthesized one at a time, so between two of them the queue is empty and the
;;; buffer is empty and the engine is still working.  A voice that reports itself finished there
;;; truncates every paragraph anything waits on — which is exactly how the live capture lost the
;;; back two thirds of the first thing the desktop ever said.
(format t "~&~%=== a paragraph keeps going between sentences ===~%")
(let ((m (glass:make-mixer)))
  (let ((spk (glass:make-speaker :mixer m)))
    (glass:speak "Go. And now a considerably longer second sentence, which takes longer to build
                  than the first one takes to say." :speaker spk)
    (await (lambda () (glass::spk-ready spk)) :what "the first sentence")
    ;; the gap the bug lived in: read the first sentence out — a listener that is faster than the
    ;; engine is the normal case for a short sentence followed by a long one — and look at the
    ;; voice while the buffer is dry and the second sentence is still in the graph
    (loop repeat 100000 until (null (glass::spk-ready spk)) do (glass:mixer-tick m))
    (check-that "SPEAKING-P stays true while the next sentence is being built"
                (glass:speaking-p spk) (glass:speech-report spk))
    (multiple-value-bind (audio worst spoken) (drain spk m)
      (declare (ignore worst))
      (check-that "and the rest of the paragraph still arrives" (> spoken 2.5)
                  (format nil "~,2f s of speech after the first sentence was already read out, ~
                               out of ~,2f s collected" spoken (secs audio))))
    (check-that "counted as one utterance, not three" (= 1 (glass::spk-said spk)))
    (glass:stop-speaker spk)))

;;; ---- 2. one voice, not one per utterance ----------------------------------
(format t "~&~%=== two utterances queue, they do not overlap ===~%")
(defparameter *one* nil)
(let ((m (glass:make-mixer)))
  (let ((spk (glass:make-speaker :mixer m)))
    (glass:speak "The desktop is listening." :speaker spk)
    (await (lambda () (plusp (glass::spk-said spk))) :what "one utterance")
    (setf *one* (secs (drain spk m)))
    (glass:stop-speaker spk)))

(let ((m (glass:make-mixer)))
  (let ((spk (glass:make-speaker :mixer m)))
    (glass:speak "The desktop is listening." :speaker spk)
    (glass:speak "The desktop is listening." :speaker spk)
    (await (lambda () (= 2 (glass::spk-said spk))) :what "two utterances")
    (let ((two (secs (drain spk m))))
      ;; overlapping would give ~one utterance's length at twice the level; queueing gives two.
      (check-that "two of the same utterance take twice as long as one"
                  (< 1.7 (/ two *one*) 2.4)
                  (format nil "~,2f s vs ~,2f s (~,2fx)" two *one* (/ two *one*))))
    (check-that "and the mix still has exactly one speech source"
                (= 1 (count "speech" (glass:mixer-sources m) :key #'glass:src-name :test #'equal)))
    (glass:stop-speaker spk)))

;;; ---- 3. hush --------------------------------------------------------------
(format t "~&~%=== HUSH stops it, including what is still being built ===~%")
(let ((m (glass:make-mixer)))
  (let ((spk (glass:make-speaker :mixer m)))
    ;; four sentences: the first is spoken while the rest are still queued or in flight, which is
    ;; the case that a queue-only HUSH gets wrong.
    (glass:speak "One fish. Two fish. Red fish. Blue fish." :speaker spk)
    (await (lambda () (glass::spk-ready spk)) :what "the first sentence")
    (glass:hush spk)
    (check-that "HUSH empties the queue at once" (not (glass:speaking-p spk)))
    (sleep 3)     ; long enough that anything still synthesizing would have landed
    (check-that "and nothing lands after it" (not (glass:speaking-p spk))
                (glass:speech-report spk))
    (let ((sink (glass:mixer-subscribe m :name "after-hush" :lead 0))
          (loud 0))
      (dotimes (i 40)
        (glass:mixer-tick m)
        (let ((f (glass:sink-next-frame sink))) (when (and f (> (rms f) 20d0)) (incf loud))))
      (check-that "the mix goes quiet" (zerop loud) (format nil "~d loud frames of 40" loud)))
    (glass:stop-speaker spk)))

;;; ---- 4. a sentence it cannot say ------------------------------------------
(format t "~&~%=== a failure is one sentence, counted ===~%")
(let ((m (glass:make-mixer)))
  (let ((spk (glass:make-speaker :mixer m)))
    ;; force the failure at the engine seam rather than inventing an input that happens to break
    ;; today's G2P — what is under test is the voice's response to a throwing synthesizer.
    (let ((real (symbol-function (find-symbol "SYNTHESIZE" "QUILL"))))
      (setf (symbol-function (find-symbol "SYNTHESIZE" "QUILL"))
            (lambda (&rest args) (declare (ignore args)) (error "gate: pretend phoneme")))
      (glass:speak "This one cannot be said." :speaker spk)
      (await (lambda () (plusp (glass::spk-failed spk))) :timeout 30 :what "the failure")
      (setf (symbol-function (find-symbol "SYNTHESIZE" "QUILL")) real))
    (check-that "the failure is counted, with the reason kept"
                (and (= 1 (glass::spk-failed spk))
                     (search "pretend phoneme" (or (glass::spk-last-error spk) "")))
                (glass:speech-report spk))
    (glass:speak "But this one can." :speaker spk)
    (await (lambda () (plusp (glass::spk-said spk))) :what "the next utterance")
    (check-that "and the voice still works afterwards" (> (rms (drain spk m)) 200d0))
    (glass:stop-speaker spk)))

;;; ---- 5. no voice installed ------------------------------------------------
(format t "~&~%=== a desktop with no voice says so ===~%")
(let ((m (glass:make-mixer)))
  (let ((spk (glass:make-speaker :mixer m))
        (installed glass:*speech-voice*))
    ;; assigned, not bound: the synthesizer runs in its own thread and would not see a LET.
    (setf glass:*speech-voice* nil)
    (unwind-protect
         (progn
           (glass:speak "Anyone there?" :speaker spk)
           (await (lambda () (plusp (glass::spk-failed spk))) :timeout 30 :what "the refusal")
           (check-that "an unset voice is an error naming the knob, not silence"
                       (search "GLASS_VOICE" (or (glass::spk-last-error spk) ""))
                       (glass::spk-last-error spk)))
      (setf glass:*speech-voice* installed))
    (glass:stop-speaker spk)))

(glass:stop-speaker *spk*)

(format t "~&~%~a~%" (glass:speech-report *spk*))
(format t "~&wrote /tmp/glass-speech-gate.wav~%")
(format t "~&~%~d passed, ~d failed~%" *pass* *fail*)
(finish-output)
(sb-ext:exit :code (if (plusp *fail*) 1 0))
