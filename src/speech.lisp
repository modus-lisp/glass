;;;; src/speech.lisp — the desktop's voice.
;;;;
;;;; src/audio.lisp says a desktop whose applications make no noise is honestly silent.  This
;;;; is the first application that makes noise: quill, a neural text-to-speech engine that is
;;;; also pure Common Lisp, wired in as one source in the session mix.  SPEAK from anywhere in
;;;; the image — a notification, a long-running job announcing it finished, an app reading a
;;;; selection — and every listener on the session hears it, because the mixer is the session's
;;;; and not any one transport's.
;;;;
;;;; Two things about the shape here are load-bearing, and both come from the mixer's clock.
;;;;
;;;; SYNTHESIS NEVER RUNS ON THE MIXER'S THREAD.  A frame is due every 20 ms and quill takes
;;;; most of a second to build a sentence — around 1.2x realtime with all its worker threads.
;;;; A source thunk that synthesized would miss its deadline by fifty frames and every listener
;;;; would hear the whole desktop stall, not just the speech.  So there are two queues and a
;;;; thread between them: SPEAK enqueues text, the speech thread turns text into 48 kHz samples
;;;; at whatever pace it can, and the thunk on the clock only ever copies out of a buffer.
;;;;
;;;; THERE IS ONE VOICE, NOT ONE PER UTTERANCE.  MIXER-PLAY would register a finite source per
;;;; clip, so two things speaking at once would talk over each other — which is what a mixer is
;;;; for with music and exactly wrong for speech.  A single long-lived source draining a queue
;;;; serializes utterances, which is what having a voice means.
;;;;
;;;; Latency is therefore honest and visible: about a second of thinking before the first word,
;;;; then continuous.  Sentences are synthesized one at a time (quill's TEXT-TO-IPA already
;;;; splits them) so a paragraph starts speaking after its first sentence rather than after its
;;;; last.
;;;;
;;;; quill is looked up at run time, not depended on at read time, for the same reason the
;;;; desktop looks up START-SESSION-AUDIO by name: a build without a voice must still be a
;;;; working desktop.

(in-package #:glass)

(defparameter *speech-voice* (sb-ext:posix-getenv "GLASS_VOICE")
  "Path to the quill .graph to speak with; GLASS_VOICE by default.  The voice's .bin and
.config.json are expected beside it.  NIL means the desktop has no voice installed, and SPEAK
says so rather than guessing at a path.")

(defparameter *speech-gap-ms* 220
  "Silence inserted between sentences.  Each is synthesized on its own, so without this they
run together — the model puts no pause at a boundary it never saw.")

(defparameter *speech-gain* 1.0d0
  "Gain of the voice in the mix.  Speech that has to compete with music wants to win.")

;;; ---- the speaker -----------------------------------------------------------

(defstruct (speaker (:constructor %make-speaker) (:conc-name spk-))
  (mixer nil)
  (source nil)                        ; the MIXER-SOURCE handle, for gain and removal
  (voice nil)                         ; quill's loaded voice, on first use
  (lock (sb-thread:make-mutex :name "glass-speech"))
  (wake (sb-thread:make-semaphore :name "glass-speech-wake"))
  (pending '())                       ; text waiting to be synthesized, oldest first
  (ready '())                         ; (simple-array (signed-byte 16) (*)) at the mix rate
  (offset 0 :type fixnum)             ; how far into the first READY buffer the thunk has read
  (thread nil)
  (busy nil)                          ; generation of the utterance being synthesized, or NIL
  (generation 0 :type fixnum)         ; bumped by HUSH, so work in flight is dropped
  (said 0 :type fixnum)               ; utterances synthesized
  (failed 0 :type fixnum)
  (last-error nil)
  (running nil))

(defvar *session-speaker* nil)
(defvar *session-speaker-lock* (sb-thread:make-mutex :name "glass-session-speaker"))

(defun %quill (name)
  "The quill function NAME, or an error saying the engine is not loaded.  Resolved per call so
that loading quill into a running desktop is enough to give it a voice — no restart."
  (let ((sym (and (find-package "QUILL") (find-symbol (string name) "QUILL"))))
    (unless (and sym (fboundp sym))
      (error "glass speech: quill is not loaded (no ~a) — (asdf:load-system :quill)" name))
    (symbol-function sym)))

(defun %speech-voice (spk)
  "The loaded voice, loading it on first use.  Loading reads 60 MB and takes a moment; doing it
lazily keeps a desktop that never speaks from paying for a voice."
  (or (spk-voice spk)
      (let ((path (or *speech-voice*
                      (error "glass speech: no voice — set GLASS_VOICE or glass:*speech-voice* ~
                              to a quill .graph"))))
        (setf (spk-voice spk) (funcall (%quill "LOAD-VOICE") path)))))

;;; ---- text in ---------------------------------------------------------------

(defun speak (text &key (speaker (session-speaker)))
  "Say TEXT on the session mix.  Returns immediately — this queues.

Utterances are spoken in the order they were queued, one at a time.  A caller that wants to
interrupt what is already being said calls HUSH first."
  (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return) (string text))))
    (when (plusp (length text))
      (sb-thread:with-mutex ((spk-lock speaker))
        (setf (spk-pending speaker) (append (spk-pending speaker) (list text))))
      (sb-thread:signal-semaphore (spk-wake speaker))
      t)))

(defun hush (&optional (speaker (session-speaker)))
  "Stop talking: drop what is queued and what is already synthesized.  A sentence being
synthesized right now is dropped when it finishes rather than interrupted mid-graph — the
generation counter is what makes its result land nowhere."
  (sb-thread:with-mutex ((spk-lock speaker))
    (incf (spk-generation speaker))
    (setf (spk-pending speaker) '()
          (spk-ready speaker) '()
          (spk-offset speaker) 0))
  t)

(defun speaking-p (&optional (speaker (session-speaker)))
  "True while there is anything left to say.

BUSY is the third case and the one that is easy to leave out: between two sentences of the same
paragraph the queue is empty AND the buffer is empty, because the first sentence has been read
out and the second is still in the graph.  A SPEAKING-P without it goes false for a second in
the middle of a sentence, and anything waiting on the voice to finish — a recorder, a caller
that speaks the next thing when this one is done — stops early."
  (sb-thread:with-mutex ((spk-lock speaker))
    (and (or (spk-pending speaker)
             (spk-ready speaker)
             ;; work in flight only counts if HUSH has not already disowned it
             (eql (spk-busy speaker) (spk-generation speaker)))
         t)))

;;; ---- samples out -----------------------------------------------------------

(defun %speech-frame (spk n)
  "N samples of speech, or NIL when there is nothing to say.  Runs on the mixer's clock, so it
does no work beyond copying: no synthesis, no allocation past the frame itself.

A sentence that ends mid-frame is followed by the next one in the SAME frame rather than by a
padded short frame — a hole at every sentence boundary is a click, and it is louder than the
speech."
  (sb-thread:with-mutex ((spk-lock spk))
    (when (spk-ready spk)
      (let ((out (reed:make-pcm16 n))
            (at 0))
        (loop while (and (< at n) (spk-ready spk))
              do (let* ((buf (first (spk-ready spk)))
                        (take (min (- n at) (- (length buf) (spk-offset spk)))))
                   (replace out buf :start1 at
                                    :start2 (spk-offset spk)
                                    :end2 (+ (spk-offset spk) take))
                   (incf at take)
                   (incf (spk-offset spk) take)
                   (when (>= (spk-offset spk) (length buf))
                     (pop (spk-ready spk))
                     (setf (spk-offset spk) 0))))
        out))))

;;; ---- the thread between them -----------------------------------------------

(defun %to-mix-rate (spk samples rate)
  "SAMPLES (quill's floats in [-1, 1] at RATE) as signed 16-bit at the mixer's rate."
  (let* ((n (length samples))
         (pcm (reed:make-pcm16 n))
         (mix-rate (mixer-rate (spk-mixer spk))))
    (dotimes (i n)
      (setf (aref pcm i) (reed:clamp16 (round (* 32767 (aref samples i))))))
    (if (= rate mix-rate)
        pcm
        (let ((rs (reed:make-resampler rate mix-rate)))
          (concatenate '(simple-array (signed-byte 16) (*))
                       (reed:resample rs pcm)
                       (reed:resample rs (reed:make-pcm16 0) :final t))))))

(defun %speech-gap (spk)
  (reed:make-pcm16 (round (* (mixer-rate (spk-mixer spk)) *speech-gap-ms*) 1000)))

(defun %say-one (spk text)
  "Synthesize TEXT and hand it to the thunk, sentence by sentence.  Each sentence is appended
as soon as it exists, which is why a paragraph starts speaking after the first one."
  (let ((voice (%speech-voice spk))
        (gen (sb-thread:with-mutex ((spk-lock spk)) (spk-generation spk)))
        (synthesize (%quill "SYNTHESIZE"))
        (first t))
    (dolist (sentence (funcall (%quill "TEXT-TO-IPA") text))
      ;; HUSH while a paragraph is being synthesized must stop the REST of it too
      (when (/= gen (sb-thread:with-mutex ((spk-lock spk)) (spk-generation spk)))
        (return))
      (when (plusp (length sentence))
        (multiple-value-bind (samples rate) (funcall synthesize voice sentence)
          (let ((pcm (%to-mix-rate spk samples rate))
                (gap (unless first (%speech-gap spk))))
            (setf first nil)
            (sb-thread:with-mutex ((spk-lock spk))
              (when (= gen (spk-generation spk))
                (setf (spk-ready spk)
                      (append (spk-ready spk)
                              (if gap (list gap pcm) (list pcm))))))))))))

(defun %speech-loop (spk)
  (loop while (spk-running spk) do
    (sb-thread:wait-on-semaphore (spk-wake spk) :timeout 1)
    (loop for text = (sb-thread:with-mutex ((spk-lock spk))
                       ;; taking the work and marking the voice busy have to happen under one
                       ;; hold of the lock, or SPEAKING-P sees the gap between them as silence
                       (let ((next (pop (spk-pending spk))))
                         (setf (spk-busy spk) (and next (spk-generation spk)))
                         next))
          while text
          do (handler-case (%say-one spk text)
               ;; A sentence quill cannot say — an unknown phoneme, a missing voice file — is
               ;; one sentence lost and recorded as such.  It is not a dead voice, and it is
               ;; certainly not something to paper over with silence and no trace.
               (serious-condition (e)
                 ;; the reason first, then the count — a watcher polls the count, and one that
                 ;; reads it between the two increments would report a failure with no reason
                 (setf (spk-last-error spk) (princ-to-string e))
                 (incf (spk-failed spk))
                 (ignore-errors
                  (format *error-output* "~&glass speech: ~s failed: ~a~%"
                          (subseq text 0 (min 40 (length text))) e)
                  (force-output *error-output*)))
               (:no-error (&rest r) (declare (ignore r)) (incf (spk-said spk)))))))

;;; ---- the session's voice ---------------------------------------------------

(defun make-speaker (&key (mixer (session-mixer)) (gain *speech-gain*))
  "A voice on MIXER: one source in the mix and one thread behind it, both started."
  (let ((spk (%make-speaker :mixer mixer)))
    (setf (spk-running spk) t
          (spk-thread spk) (sb-thread:make-thread (lambda () (%speech-loop spk))
                                                  :name "glass-speech")
          (spk-source spk) (mixer-add-source
                            mixer
                            (lambda () (%speech-frame spk (mixer-frame-samples mixer)))
                            :name "speech" :gain gain))
    spk))

(defun session-speaker ()
  "This process's voice, created on first use.  Idempotent, like SESSION-MIXER."
  (sb-thread:with-mutex (*session-speaker-lock*)
    (or *session-speaker* (setf *session-speaker* (make-speaker)))))

(defun stop-speaker (&optional (speaker *session-speaker*))
  (when speaker
    (hush speaker)
    (setf (spk-running speaker) nil)
    (sb-thread:signal-semaphore (spk-wake speaker))
    (when (spk-source speaker) (mixer-remove-source (spk-mixer speaker) (spk-source speaker)))
    (let ((th (spk-thread speaker)))
      (when th (ignore-errors (sb-thread:join-thread th :timeout 2))))
    (setf (spk-thread speaker) nil)
    (when (eq speaker *session-speaker*) (setf *session-speaker* nil)))
  t)

(defun speech-report (&optional (speaker *session-speaker*))
  "One line about the voice, for the control socket."
  (if (null speaker)
      "speech: no voice created"
      (sb-thread:with-mutex ((spk-lock speaker))
        (format nil "speech: ~:[silent~;speaking~] queued ~d, buffered ~,2f s, said ~d, failed ~d~
                     ~@[ (last: ~a)~] voice ~:[NOT LOADED~;loaded~]"
                (or (spk-pending speaker) (spk-ready speaker)
                    (eql (spk-busy speaker) (spk-generation speaker)))
                (length (spk-pending speaker))
                (/ (- (reduce #'+ (spk-ready speaker) :key #'length :initial-value 0)
                      (spk-offset speaker))
                   (float (mixer-rate (spk-mixer speaker)) 1d0))
                (spk-said speaker) (spk-failed speaker) (spk-last-error speaker)
                (spk-voice speaker)))))
