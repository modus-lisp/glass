;;;; src/hearing.lisp — the desktop's ear.
;;;;
;;;; src/speech.lisp gives the image a voice: chord as one source in the session mix, and SPEAK
;;;; from anywhere.  This is the same wiring run backwards — stave as one SINK on the same mix,
;;;; and a transcript anywhere in the image can read.  The two are deliberately symmetric,
;;;; because they are the same relationship: audio.lisp owns the session's sound, and an engine
;;;; is a thing that converts at one end of it.
;;;;
;;;; IT HEARS THE SESSION, WHICH INCLUDES THE SESSION'S OWN VOICE.  A sink on the mixer is a
;;;; listener like any other, so the desktop hears whatever the desktop is playing: SPEAK a
;;;; sentence and the ear writes it down.  That is a real loop and it is the intended one — it
;;;; is what makes this testable on a box with no microphone, and a microphone is exactly what
;;;; this box does not have (four HDMI playbacks and no capture device anywhere in
;;;; /proc/asound).  A peer's microphone arriving over WebRTC is a different SOURCE and not a
;;;; different design: :SOURCE is a thunk returning a frame, and the mixer's sink is only the
;;;; default one.  What it is NOT is a good idea to point at a microphone in the same room as
;;;; the speakers, which would transcribe the desktop's own voice back to it forever.
;;;;
;;;; RECOGNITION NEVER RUNS ON THE MIXER'S THREAD, for exactly the reason synthesis does not:
;;;; a frame is due every 20 ms, and a chunk of speech costs a quarter of the time it covers.
;;;; A sink is polled from this file's own thread, on its own clock, and the mixer never waits.
;;;;
;;;; SILENCE IS NOT DECODED.  The recognizer's cost is per second of audio and not per word, so
;;;; an ear left on in a quiet room would burn a quarter of a core saying nothing, forever.  A
;;;; level gate keeps the utterance boundaries instead: below the threshold nothing is fed, and
;;;; the ear is idle at the cost of one root-mean-square per frame.  A PRE-ROLL of a fifth of a
;;;; second is kept while idle and fed when speech starts, because the gate notices a word after
;;;; the word has begun and the front end needs what came before it.
;;;;
;;;; AN UTTERANCE IS BOUNDED.  stave's listener indexes its audio from the beginning of the
;;;; utterance, so something has to end one; silence does, and failing that the clock does.  A
;;;; monologue longer than *HEARING-MAX-SECONDS* is cut and continued rather than grown without
;;;; limit, which costs the transducer its context across the seam and costs nothing else.
;;;;
;;;; A PEER'S MICROPHONE IS PREFERRED OVER THE MIX, once one exists.  src/mic-stream.lisp carries
;;;; a phone's microphone from the WebRTC gateway to this image, and the moment one is live the ear
;;;; listens to THAT instead of the mixer — because an ear on the mixer hears the desktop's own
;;;; voice, which is the right loop for a box with no capture device and the wrong one the instant
;;;; a real microphone arrives.  The fall-back is not "nothing": with no peer connected the ear
;;;; goes back to the mix, so a desktop alone in a room still hears itself speak and the Listen
;;;; window still demonstrates something.  The switch happens per frame, in the source thunk, so a
;;;; phone dialing in or hanging up mid-session moves the ear without restarting it and without
;;;; reading a quarter of a gigabyte of weights again; the utterance in progress is CUT at the
;;;; seam, because a sentence half from the mix and half from a phone is not a sentence.
;;;;
;;;; stave is looked up at run time, not depended on at read time — same as chord in speech.lisp,
;;;; and for the same reason: a build with no recognizer must still be a working desktop.  So is
;;;; the microphone: :glass/mic-stream is not a dependency of this system, and an image without it
;;;; has an ear on the mix and no idea that anything else was possible.

(in-package #:glass)

(defparameter *hearing-models* (sb-ext:posix-getenv "GLASS_EARS")
  "Directory holding stave's three .graph files and tokens.txt; GLASS_EARS by default.  NIL
means the desktop has no ear installed, and LISTEN says so rather than guessing at a path.")

(defparameter *hearing-rate* 16000
  "The rate the recognizer is fed at.  Not a preference: it is the rate the model's filterbank
front end was built for, and the mixer converts DOWN from 48 kHz to reach it, so nothing is
invented.  Feeding 8 kHz audio here as though it were 16 produces confident nonsense.")

(defparameter *hearing-threshold* 0.004d0
  "Root-mean-square, in the same [0, 1] the samples are in, above which a frame is speech.
Digital silence is exactly zero, so this only has to clear the noise floor of whatever is
playing; too low costs idle decoding, too high clips the first word.")

(defparameter *hearing-gap-seconds* 0.8d0
  "Silence that ends an utterance.  Long enough to survive the stop in the middle of a sentence,
short enough that a finished sentence appears while it is still interesting.")

(defparameter *hearing-max-seconds* 30d0
  "The longest an utterance is allowed to run before it is cut and continued.")

(defparameter *hearing-preroll-seconds* 0.2d0
  "Audio kept while idle and fed when the gate opens, so a word is not clipped by the gate
noticing it late.")

(defparameter *hearing-prefer-mic* t
  "Listen to a peer's microphone instead of the session mix whenever one is live.

T is the only sane default once a microphone can exist: the mix contains the desktop's own voice,
so an ear on it while somebody is talking into a phone would transcribe both halves of a
conversation as one speaker.  NIL pins the ear to the mixer, which is what the loop test wants —
SPEAK a sentence and read it back — with a peer connected at the same time.")

(defparameter *hearing-queue-seconds* 20d0
  "How much speech may be waiting to be decoded before the oldest of it is dropped.

It is a safety net and not a working buffer: decoding runs at about four times realtime, so the
queue is empty except while the model is being read off disk and after a moment when the machine
was busy.  A recogniser that fell permanently behind would otherwise grow this forever and report
a transcript further and further in the past, which is worse than a gap and a count.")

;;; ---- the ear ---------------------------------------------------------------

(defstruct (ears (:constructor %make-ears) (:conc-name ear-))
  (mixer nil)
  (sink nil)                          ; the MIXER-SINK this hears through, if it made one
  (source nil)                        ; the thunk it pulls frames from
  (rate 16000 :type fixnum)
  (frame-samples 320 :type fixnum)
  (rec nil)                           ; stave's recognizer, loaded once
  (listener nil)                      ; stave's listener, per utterance
  (lock (sb-thread:make-mutex :name "glass-hearing"))
  (wake (sb-thread:make-semaphore :name "glass-hearing-wake"))
  (pull-thread nil)
  (decode-thread nil)
  (running nil)
  ;; the gate's state, owned by the pull thread
  (listening-to :mix)                 ; :mix, :peer or :given — what the last frame came from
  (state :idle)                       ; :idle between utterances, :speech inside one
  (preroll '())                       ; frames held while idle, newest first
  (preroll-frames 0 :type fixnum)
  (quiet 0 :type fixnum)              ; consecutive frames below the threshold
  (spoken 0 :type fixnum)             ; frames posted for the utterance in progress
  (level 0d0 :type double-float)
  ;; the queue between the two threads: frames to decode and :END markers, newest first
  (pending '() :type list)
  (queued 0 :type fixnum)
  (dropped 0 :type fixnum)
  ;; the transcript, owned by the decode thread
  (partial "" :type string)           ; the utterance being decoded
  (heard '() :type list)              ; utterances finished, newest first
  (utterances 0 :type fixnum)
  (seconds 0d0 :type double-float)    ; audio actually decoded, not audio seen
  (loading nil)
  (ready nil)                         ; the model is loaded; the pull thread may start
  (last-error nil))

(defvar *session-ears* nil)
(defvar *session-ears-lock* (sb-thread:make-mutex :name "glass-session-ears"))

(defun %stave (name)
  "The stave function NAME, or an error saying the recognizer is not loaded.  Resolved per call,
so loading stave into a running desktop is enough to give it an ear — no restart."
  (let ((sym (and (find-package "STAVE") (find-symbol (string name) "STAVE"))))
    (unless (and sym (fboundp sym))
      (error "glass hearing: stave is not loaded (no ~a) — (asdf:load-system :stave)" name))
    (symbol-function sym)))

(defun %hearing-recognizer (ear)
  "The loaded recognizer, loading it on first use.  Reading three graphs and a quarter of a
gigabyte of weights takes a moment; doing it lazily keeps a desktop that never listens from
paying for an ear."
  (or (ear-rec ear)
      (let ((dir (or *hearing-models*
                     (error "glass hearing: no recognizer — set GLASS_EARS or ~
                             glass:*hearing-models* to a directory of stave graphs"))))
        (setf (ear-loading ear) t)
        (unwind-protect
             (setf (ear-rec ear) (funcall (%stave "LOAD-RECOGNIZER") dir))
          (setf (ear-loading ear) nil)))))

(defun %peer-mic ()
  "The peer microphone the session is being spoken into, if one is live.

Looked up by name at run time — the symbols are exported by packages.lisp whether or not anything
defines them, and FBOUNDP is the question actually being asked: is :glass/mic-stream loaded?  Same
trick as %STAVE, for the same reason, and it is what keeps this system's dependency list at
(glass/audio stave) while still preferring a microphone the moment one can exist."
  (and *hearing-prefer-mic*
       (fboundp 'session-mic)
       (let ((mic (funcall 'session-mic)))
         (and mic (funcall 'mic-live-p mic) mic))))

(defun %hearing-source (ear sink)
  "The ear's source when the caller named none: the peer's microphone while one is live, and the
session mix the rest of the time.

The choice is made per frame rather than once, because a phone dials in and hangs up during the
life of an ear and neither event should cost a model load.  A microphone at the wrong rate is not
used at all: converting it here would be a second resampler in a file that has no business owning
one, and the transport already converts to exactly this rate — a mismatch means somebody started
the two ends with different arithmetic, and silently listening to the mix instead is a better
answer than confident nonsense."
  (lambda ()
    (let ((mic (%peer-mic)))
      (cond ((and mic (= (funcall 'mic-rate mic) (ear-rate ear)))
             (setf (ear-listening-to ear) :peer)
             (funcall 'mic-next-frame mic))
            (sink
             (setf (ear-listening-to ear) :mix)
             (sink-next-frame sink))))))

;;; ---- text out --------------------------------------------------------------

(defun hearing-partial (&optional (ear *session-ears*))
  "The utterance being decoded right now, as far as it has got."
  (if ear (sb-thread:with-mutex ((ear-lock ear)) (ear-partial ear)) ""))

(defun hearing-heard (&optional (ear *session-ears*))
  "The utterances that have ended, oldest first."
  (if ear (sb-thread:with-mutex ((ear-lock ear)) (reverse (ear-heard ear))) '()))

(defun hearing-text (&optional (ear *session-ears*))
  "Everything heard since the ear was cleared, as one string — finished utterances and the one
in progress, which is what a window showing a live transcript wants."
  (if (null ear)
      ""
      (sb-thread:with-mutex ((ear-lock ear))
        (let ((parts (append (reverse (ear-heard ear))
                             (and (plusp (length (ear-partial ear))) (list (ear-partial ear))))))
          (format nil "~{~a~^ ~}" parts)))))

(defun hearing-clear (&optional (ear *session-ears*))
  "Forget the transcript.  Does not stop listening, and does not disturb the utterance in
progress — clearing what has been heard is not a statement about what is being said."
  (when ear
    (sb-thread:with-mutex ((ear-lock ear)) (setf (ear-heard ear) '())))
  t)

(defun hearing-level (&optional (ear *session-ears*))
  "The last frame's root-mean-square, for a level meter."
  (if ear (ear-level ear) 0d0))

(defun listening-p (&optional (ear *session-ears*))
  (and ear (ear-running ear) t))

(defun hearing-ready-p (&optional (ear *session-ears*))
  "True once the model is read and the ear is actually collecting audio.

Worth asking, and worth saying out loud in a window: reading the weights takes long enough that
anything said in the meantime is said to nobody.  A Listen button that goes straight to LISTENING
is a button that lies for the first half-minute after a cold start."
  (and ear (ear-running ear) (ear-ready ear) t))

;;; ---- the level gate, on the clock -----------------------------------------
;;;
;;; This half runs on the mixer's pace and must never do recognizer work, for exactly the reason
;;; speech.lisp's thunk never synthesizes.  A chunk of speech is 0.32 s of audio and costs about
;;; 80 ms to decode; a puller that decoded would spend that 80 ms not collecting, and the sink —
;;; which drops what its consumer did not come and get, by design — would hand back a stream with
;;; a hole in it every four chunks.  That is not a hypothetical: it cost the first five words of
;;; every utterance before the two threads were separated, and it looked like a bad recognizer
;;; rather than a bad clock.
;;;
;;; So all this does is convert, measure, and decide.  What it decides is where an utterance
;;; starts and stops, which is a question about TIME and therefore belongs on the thread that has
;;; a clock.

(defun %hearing-floats (pcm)
  "PCM (signed 16-bit) as single-floats in [-1, 1], which is the scale stave's own wav reader
produces and therefore the scale its model was trained under.

A fresh vector per frame, not a reused one: it is handed to another thread, and 320 floats every
20 ms is nothing next to what the recognizer allocates."
  (let ((out (make-array (length pcm) :element-type 'single-float)))
    (dotimes (i (length pcm) out)
      (setf (aref out i) (/ (float (aref pcm i) 1.0) 32768.0)))))

(defun %hearing-rms (samples)
  (let ((acc 0d0) (n (length samples)))
    (if (zerop n)
        0d0
        (progn (dotimes (i n) (incf acc (* (aref samples i) (aref samples i))))
               (sqrt (/ acc n))))))

(defun %hearing-post (ear item)
  "Put ITEM — a frame of samples, or :END — on the queue for the decode thread.

Drops the OLDEST frames when the queue is over its bound, which is the same judgement the audio
tap makes about a slow consumer: a listener that has fallen behind wants a gap and the truth
about it, not everything it missed delivered late."
  (let ((cap (max 1 (round (* (ear-rate ear) *hearing-queue-seconds*) (ear-frame-samples ear)))))
    (sb-thread:with-mutex ((ear-lock ear))
      (push item (ear-pending ear))
      (incf (ear-queued ear))
      (when (> (ear-queued ear) cap)
        ;; PENDING is newest first, so the oldest are at the end
        (let ((keep (subseq (ear-pending ear) 0 cap)))
          (incf (ear-dropped ear) (- (ear-queued ear) cap))
          (setf (ear-pending ear) keep
                (ear-queued ear) cap)))))
  (sb-thread:signal-semaphore (ear-wake ear)))

(defun %hearing-hold-preroll (ear samples)
  (let ((cap (max 1 (round (* (ear-rate ear) *hearing-preroll-seconds*)
                           (ear-frame-samples ear)))))
    (push samples (ear-preroll ear))
    (incf (ear-preroll-frames ear))
    (when (> (ear-preroll-frames ear) cap)
      (setf (ear-preroll ear) (subseq (ear-preroll ear) 0 cap)
            (ear-preroll-frames ear) cap))))

(defun %hearing-gate (ear samples)
  "One frame of audio, through the level gate and onto the queue or into the bin."
  (let* ((rms (%hearing-rms samples))
         (loud (> rms *hearing-threshold*))
         (gap-frames (max 1 (round (* (ear-rate ear) *hearing-gap-seconds*)
                                   (ear-frame-samples ear))))
         (max-frames (max 1 (round (* (ear-rate ear) *hearing-max-seconds*)
                                   (ear-frame-samples ear)))))
    (setf (ear-level ear) rms)
    (ecase (ear-state ear)
      (:idle
       (cond (loud
              ;; the pre-roll first, oldest first: the gate opened on a frame that was already
              ;; partway into a word, and the front end needs what came before it
              (dolist (frame (reverse (ear-preroll ear))) (%hearing-post ear frame))
              (setf (ear-preroll ear) '() (ear-preroll-frames ear) 0
                    (ear-state ear) :speech
                    (ear-quiet ear) 0
                    (ear-spoken ear) 0)
              (%hearing-post ear samples)
              (incf (ear-spoken ear)))
             (t (%hearing-hold-preroll ear samples))))
      (:speech
       (if loud (setf (ear-quiet ear) 0) (incf (ear-quiet ear)))
       (%hearing-post ear samples)
       (incf (ear-spoken ear))
       (cond
         ;; ended by silence: the ordinary way a sentence stops
         ((>= (ear-quiet ear) gap-frames)
          (%hearing-post ear :end)
          (setf (ear-state ear) :idle (ear-quiet ear) 0))
         ;; ...or by the clock, which is not a sentence boundary and is a bound on the buffer
         ((>= (ear-spoken ear) max-frames)
          (%hearing-post ear :end)
          (setf (ear-spoken ear) 0)))))))

(defun %hearing-cut (ear)
  "End the utterance in progress without waiting for silence to do it.

What ends a sentence is normally a gap; a change of SOURCE ends one too, and more sharply — the
words before the seam and the words after it were said by different people in different rooms,
and handing them to one listener as one utterance is how a phone's first sentence arrives with the
desktop's last three words welded to the front of it."
  (when (eq (ear-state ear) :speech)
    (%hearing-post ear :end))
  (setf (ear-state ear) :idle
        (ear-quiet ear) 0
        (ear-spoken ear) 0
        (ear-preroll ear) '()
        (ear-preroll-frames ear) 0))

(defun %hearing-pull-loop (ear)
  "The ear's own clock.  Its own deadline, like the mixer's and the audio stream's."
  ;; nothing to do until there is something to decode into.  The sink keeps filling and dropping
  ;; behind us meanwhile, which is what a sink is for.
  (loop while (and (ear-running ear) (not (ear-ready ear))) do (sleep 0.1))
  (let* ((period (/ (ear-frame-samples ear) (float (ear-rate ear) 1d0)))
         (units (float internal-time-units-per-second 1d0))
         (tick (max 1 (round (* period internal-time-units-per-second))))
         (next (+ (get-internal-real-time) tick))
         (was (ear-listening-to ear)))
    (loop while (ear-running ear) do
      (handler-case
          (let ((pcm (funcall (ear-source ear))))
            ;; the source records what it decided to be this time round; a change is a seam
            (unless (eq was (ear-listening-to ear))
              (setf was (ear-listening-to ear))
              (%hearing-cut ear))
            (when pcm (%hearing-gate ear (%hearing-floats pcm))))
        ;; an escaping condition in any thread is fatal under --disable-debugger, which is how
        ;; the desktop runs; one bad frame must cost one frame
        (serious-condition (e) (setf (ear-last-error ear) (princ-to-string e))))
      (let ((now (get-internal-real-time)))
        (when (> next now) (sleep (/ (- next now) units)))
        ;; late is late: firing the backlog now would collect nothing new, only spin
        (setf next (if (< (+ next tick) now) (+ now tick) (+ next tick)))))
    ;; whatever was being said when the ear was switched off is still worth writing down
    (when (eq (ear-state ear) :speech) (%hearing-post ear :end))))

;;; ---- the recognizer, off the clock -----------------------------------------

(defun %hearing-feed (ear samples)
  "Hand SAMPLES to the listener and take whatever the transcript has become."
  (unless (ear-listener ear)
    (setf (ear-listener ear) (funcall (%stave "MAKE-LISTENER") (ear-rec ear)
                                      :rate (ear-rate ear))))
  (let ((text (funcall (%stave "LISTENER-FEED") (ear-listener ear) samples)))
    (sb-thread:with-mutex ((ear-lock ear))
      (setf (ear-partial ear) text)
      (incf (ear-seconds ear) (/ (length samples) (float (ear-rate ear) 1d0))))))

(defun %hearing-end-utterance (ear)
  "Flush the recognizer and move the utterance from partial to heard.

The flush is what a stream owes a file: until it happens the last words are still inside the
encoder's lookahead, so an utterance that was never ended is an utterance missing its ending."
  (when (ear-listener ear)
    (let ((text (funcall (%stave "LISTENER-FINISH") (ear-listener ear))))
      (sb-thread:with-mutex ((ear-lock ear))
        (setf (ear-partial ear) "")
        (when (plusp (length text))
          (push text (ear-heard ear))
          (incf (ear-utterances ear))))
      (setf (ear-listener ear) nil))))

(defun %hearing-take (ear)
  "Everything queued, oldest first, in one go.  Batching is what lets the decoder catch up after
a stall instead of paying a lock and a semaphore per 20 ms frame."
  (sb-thread:with-mutex ((ear-lock ear))
    (let ((items (ear-pending ear)))
      (setf (ear-pending ear) '() (ear-queued ear) 0)
      (nreverse items))))

(defun %hearing-decode-loop (ear)
  "Load the model, then drain the queue for as long as the ear is running.

This thread has no deadline at all, which is the point of it: it may take a minute to read the
weights and 80 ms to decode a chunk, and neither costs the puller a single frame."
  (handler-case (%hearing-recognizer ear)
    (serious-condition (e)
      (setf (ear-last-error ear) (princ-to-string e))
      (ignore-errors
       (format *error-output* "~&glass hearing: no ear — ~a~%" e)
       (force-output *error-output*))
      (setf (ear-running ear) nil)
      (return-from %hearing-decode-loop)))
  (setf (ear-ready ear) t)
  (loop while (or (ear-running ear) (ear-pending ear)) do
    (sb-thread:wait-on-semaphore (ear-wake ear) :timeout 0.5)
    (handler-case
        (dolist (item (%hearing-take ear))
          (if (eq item :end)
              (%hearing-end-utterance ear)
              (%hearing-feed ear item)))
      ;; a chunk that would not decode is that utterance lost and recorded as such
      (serious-condition (e)
        (setf (ear-last-error ear) (princ-to-string e)
              (ear-listener ear) nil)
        (ignore-errors
         (format *error-output* "~&glass hearing: ~a~%" e)
         (force-output *error-output*))
        (sleep 0.2))))
  (ignore-errors (%hearing-end-utterance ear)))

;;; ---- starting and stopping -------------------------------------------------

(defun make-ears (&key (mixer (session-mixer)) (rate *hearing-rate*) source (gain 1.0d0))
  "An ear on MIXER: one sink in the mix and TWO threads behind it, all started.

SOURCE overrides everything below — a thunk returning the next frame of signed 16-bit mono at
RATE, or NIL when there is none, which is reed's contract and therefore also an audio tap's.  It
is how audio from anywhere at all gets transcribed, and it is what a test hands in.

With no SOURCE the ear listens to A PEER'S MICROPHONE WHILE ONE IS LIVE and to the session mix
the rest of the time (%HEARING-SOURCE).  The sink is subscribed either way, because the fall-back
has to be already running when the phone hangs up — a sink created at that moment would be a
sink with an empty cushion, which is a hole in the audio exactly where the microphone stopped.

The decoder starts first and the puller waits for it to say READY.  Reading a quarter of a
gigabyte of weights takes the better part of a minute, and audio pulled during it is audio that
goes stale in the queue or — past the queue's bound — is dropped.  An ear that is not ready yet
has heard nothing, which is the truth and is cheap to say."
  (let* ((frame (max 1 (round (* rate (mixer-period mixer)))))
         (sink (unless source
                 (mixer-subscribe mixer :name "ears" :rate rate :frame-samples frame :gain gain)))
         (ear (%make-ears :mixer mixer :sink sink :rate rate :frame-samples frame
                          :listening-to (if source :given :mix))))
    (setf (ear-source ear) (or source (%hearing-source ear sink)))
    (setf (ear-running ear) t
          (ear-decode-thread ear) (sb-thread:make-thread (lambda () (%hearing-decode-loop ear))
                                                         :name "glass-hearing-decode")
          (ear-pull-thread ear) (sb-thread:make-thread (lambda () (%hearing-pull-loop ear))
                                                       :name "glass-hearing-pull"))
    ear))

(defun start-listening (&key source)
  "Begin transcribing the session.  Returns the EARS.

Idempotent, and deliberately session-wide: two windows asking to listen share one ear rather
than loading a second quarter-gigabyte of weights and putting a second sink on the mix."
  (sb-thread:with-mutex (*session-ears-lock*)
    (or *session-ears* (setf *session-ears* (make-ears :source source)))))

(defun stop-listening (&optional (ear *session-ears*))
  "Stop transcribing, keeping what was heard.  The sink goes with it — an ear that is not
listening should not be one of the mix's consumers."
  (when ear
    (setf (ear-running ear) nil)
    ;; the puller first: it owes the decoder a final :END for whatever was being said, and the
    ;; decoder drains what is left before it goes.  Joined the other way round the last utterance
    ;; would be posted to a thread that had already stopped reading.
    (let ((th (ear-pull-thread ear)))
      (when th (ignore-errors (sb-thread:join-thread th :timeout 3))))
    (setf (ear-pull-thread ear) nil)
    (sb-thread:signal-semaphore (ear-wake ear))
    (let ((th (ear-decode-thread ear)))
      (when th (ignore-errors (sb-thread:join-thread th :timeout 30))))
    (setf (ear-decode-thread ear) nil (ear-ready ear) nil)
    (when (ear-sink ear)
      (ignore-errors (mixer-unsubscribe (ear-mixer ear) (ear-sink ear)))
      (setf (ear-sink ear) nil))
    (when (eq ear *session-ears*) (setf *session-ears* nil)))
  t)

(defun hearing-report (&optional (ear *session-ears*))
  "One line about the ear, for the control socket."
  (if (null ear)
      "hearing: no ear created"
      (format nil "hearing: ~:[stopped~;~:[listening~;LOADING~]~] ~a to ~a, level ~,4f, ~
                   ~d utterance~:p, ~,1f s decoded, ~d queued~[~:;, ~:*~d DROPPED~]~
                   ~@[ (last: ~a)~] model ~:[NOT LOADED~;loaded~]"
              (ear-running ear) (ear-loading ear)
              (string-downcase (ear-state ear))
              (ecase (ear-listening-to ear)
                (:peer "a peer's microphone") (:mix "the session mix") (:given "a given source"))
              (ear-level ear) (ear-utterances ear) (ear-seconds ear)
              (ear-queued ear) (ear-dropped ear)
              (ear-last-error ear) (ear-rec ear))))
