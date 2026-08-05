;;;; src/audio.lisp — the desktop's sound, mixed once and read by whoever is listening.
;;;;
;;;; glass exports a screen to whatever connects.  This exports the other half: one mix of
;;;; everything the session is making noise with, readable by more than one consumer at a time.
;;;;
;;;; The shape follows from a single fact — a session has SEVERAL possible listeners.  A WebRTC
;;;; peer is one, a VNC client (RFB carries a QEMU audio pseudo-encoding) is another, a local
;;;; device or a recorder is a third.  Putting the mixer inside a transport would mean only that
;;;; transport's clients ever hear anything, and the next transport would build a second mixer
;;;; with its own idea of what the session sounds like.  So the mixer lives here, beside the
;;;; framebuffer, and a transport is a thin thing that converts.
;;;;
;;;; Three consequences, each of which is a bug if you get it wrong:
;;;;
;;;; THE CLOCK IS THE MIXER'S OWN.  If the mix advanced when a consumer asked for it, then with
;;;; two consumers pulling at different moments whoever asked first would advance the mix and the
;;;; other would get a hole — silence in the middle of a sound that was playing.  A timer thread
;;;; mixes on its own 20 ms deadline whether anybody is listening or not, exactly like the
;;;; framebuffer keeps existing when no client is connected.
;;;;
;;;; CONSUMERS PULL, THEY ARE NOT PUSHED TO.  Each sink holds a cursor into a small ring of
;;;; recent frames.  A consumer that stalls falls behind its cursor and drops frames — it does
;;;; not stall the mixer, and it does not stall the other consumers.  Falling behind sounds like
;;;; a gap; blocking the mix would sound like a gap to EVERYONE.
;;;;
;;;; THE MIX IS AT NATIVE RATE, 48 kHz.  Each consumer converts down to whatever it needs (a
;;;; G.711 phone channel needs 8 kHz).  Mixing at the lowest consumer's rate would make the whole
;;;; desktop sound like a phone call forever, including to a listener that could have had the
;;;; full bandwidth.  The conversion is per-sink, in the sink.
;;;;
;;;; The arithmetic — resampling, gain, summing — is reed's; this file is the session policy on
;;;; top of it.  A SOURCE is reed's source contract unchanged: a thunk returning the next frame
;;;; of mono samples, or NIL when it has nothing.  So reed:make-mp3-source plugs in with no
;;;; adapter, and so does anything else that already speaks it.

(in-package #:glass)

(defparameter *mixer-frame-ms* 20
  "Mix quantum.  20 ms is what every RTP audio payload uses, so a transport that wants 20 ms
frames gets them without rebuffering; nothing here depends on the number.")

(defparameter *mixer-rate* 48000
  "Native mix rate.  Consumers convert DOWN from this; nothing converts up.")

;;; ---- a source in the mix ---------------------------------------------------

(defstruct (mixer-source (:conc-name src-) (:constructor %make-src))
  (id 0 :type fixnum)
  (name "" :type string)
  thunk
  (gain 1.0d0 :type double-float)   ; live: (setf (glass:src-gain s) 0.5d0) while it plays
  (finite nil)                      ; T = remove it the first time it returns NIL (a file ends)
  (frames 0 :type fixnum)
  (errors 0 :type fixnum))

;;; ---- the mixer -------------------------------------------------------------

(defstruct (mixer (:constructor %make-mixer))
  (rate 48000 :type fixnum)
  (frame-samples 960 :type fixnum)
  (period 0.02d0 :type double-float)
  (lock (sb-thread:make-mutex :name "glass-mixer"))
  (sources '())
  (next-id 1 :type fixnum)
  (ring #() :type simple-vector)    ; recent frames; a sink reads from here at its own pace
  (seq 0 :type fixnum)              ; frames mixed since start — the sink cursors' coordinate
  (sinks '())
  (thread nil)
  (running nil)
  (level 0.0d0 :type double-float)  ; RMS of the last frame, 0-1, for a meter
  (late 0 :type fixnum))            ; ticks that ran past their deadline

(defun make-mixer (&key (rate *mixer-rate*) (frame-ms *mixer-frame-ms*) (capacity 64))
  "A session mixer at RATE with FRAME-MS frames, keeping CAPACITY frames of history.

CAPACITY is how far behind a consumer may fall before it loses audio: 64 frames of 20 ms is
1.3 s, which is generous for a network sink and cheap (64 x 960 x 2 bytes = 123 KB)."
  (let ((n (round (* rate frame-ms) 1000)))
    (%make-mixer :rate rate :frame-samples n
                 :period (/ frame-ms 1000d0)
                 :ring (make-array capacity :initial-element nil))))

(defun mixer-add-source (m source &key (name "source") (gain 1.0d0) finite)
  "Register SOURCE (a thunk returning the next frame of mono samples at the mixer's rate, or
NIL) and return its handle.  FINITE t removes it the first time it returns NIL — right for a
file, wrong for a capture device, which returns NIL whenever it simply has nothing yet."
  (sb-thread:with-mutex ((mixer-lock m))
    (let ((s (%make-src :id (mixer-next-id m) :name name :thunk source
                        :gain (float gain 1d0) :finite finite)))
      (incf (mixer-next-id m))
      (setf (mixer-sources m) (append (mixer-sources m) (list s)))
      s)))

(defun mixer-remove-source (m source-or-id)
  "Drop a source.  Returns T if it was there."
  (sb-thread:with-mutex ((mixer-lock m))
    (let* ((id (if (mixer-source-p source-or-id) (src-id source-or-id) source-or-id))
           (hit (find id (mixer-sources m) :key #'src-id)))
      (when hit
        (setf (mixer-sources m) (remove hit (mixer-sources m)))
        t))))

(defun mixer-play (m samples &key (name "clip") (gain 1.0d0) rate)
  "Play SAMPLES once and then forget them — a notification, a bell, a test tone.  RATE, if
given and different from the mixer's, is converted first."
  (let ((buf (if (and rate (/= rate (mixer-rate m)))
                 (let ((rs (reed:make-resampler rate (mixer-rate m))))
                   (concatenate '(simple-array (signed-byte 16) (*))
                                (reed:resample rs samples)
                                (reed:resample rs (reed:make-pcm16 0) :final t)))
                 samples)))
    (mixer-add-source m (reed:make-buffer-source buf :frame-samples (mixer-frame-samples m))
                      :name name :gain gain :finite t)))

(defun audio-tone (hz secs &key (rate *mixer-rate*) (amplitude 8000))
  "A sine, for a bell or a gate.  Ramped at both ends: a tone that starts at full amplitude
clicks, and the click is louder than the tone."
  (let* ((n (round (* rate secs)))
         (out (reed:make-pcm16 n))
         (ramp (max 1 (round (* rate 0.005)))))    ; 5 ms
    (dotimes (i n out)
      (let ((env (min 1d0 (/ (min i (- n i 1)) (float ramp 1d0)))))
        (setf (aref out i)
              (round (* amplitude env (sin (/ (* 2 pi hz i) (float rate 1d0))))))))))

;;; ---- mixing one frame ------------------------------------------------------

(defun %mix-tick (m)
  "Ask every source for a frame and sum them.  Runs on the clock thread, under no lock while
the sources run — a source is caller code and may be slow; holding the mixer lock across it
would block subscribe/add for as long as the slowest source takes."
  (let ((sources (sb-thread:with-mutex ((mixer-lock m)) (copy-list (mixer-sources m))))
        (n (mixer-frame-samples m))
        (bufs '()) (gains '()) (done '()))
    (dolist (s sources)
      (let ((buf (handler-case (funcall (src-thunk s))
                   (serious-condition (e)
                     ;; a throwing source is a silent frame, not a dead mix; three strikes and
                     ;; it is out, so a permanently broken source does not burn the clock.
                     (when (>= (incf (src-errors s)) 3) (push s done))
                     (values nil e)))))
        (cond ((and buf (plusp (length buf)))
               (incf (src-frames s))
               (push buf bufs)
               (push (src-gain s) gains))
              ((src-finite s) (push s done)))))
    (dolist (s done) (mixer-remove-source m s))
    (if bufs
        (reed:mix (nreverse bufs) :gains (nreverse gains) :length n)
        (reed:make-pcm16 n))))     ; nobody playing: silence, and the clock still advances

(defun %rms (pcm)
  (if (zerop (length pcm))
      0d0
      (let ((sum 0d0))
        (dotimes (i (length pcm))
          (let ((v (float (aref pcm i) 1d0))) (incf sum (* v v))))
        (min 1d0 (/ (sqrt (/ sum (length pcm))) 8000d0)))))

(defun %ring-push (m frame)
  (sb-thread:with-mutex ((mixer-lock m))
    (let ((ring (mixer-ring m)))
      (setf (svref ring (mod (mixer-seq m) (length ring))) frame)
      (incf (mixer-seq m))
      (setf (mixer-level m) (%rms frame)))))

(defun %mixer-loop (m)
  (let* ((units (float internal-time-units-per-second 1d0))
         (tick (max 1 (round (* (mixer-period m) internal-time-units-per-second))))
         (next (+ (get-internal-real-time) tick)))
    (loop while (mixer-running m) do
      ;; ANY unhandled condition in ANY thread kills a --disable-debugger image; a bad frame
      ;; must cost one frame.
      (handler-case (%ring-push m (%mix-tick m))
        (serious-condition (e)
          (ignore-errors
           (format *error-output* "~&glass audio: mix tick failed: ~a~%" e)
           (force-output *error-output*))))
      (let ((now (get-internal-real-time)))
        (when (> next now)
          (sleep (/ (- next now) units)))
        ;; Forgive the debt rather than burst.  A tick that ran long is already late; firing the
        ;; next N frames back-to-back to "catch up" delivers them as one lump, which a listener
        ;; hears as a stutter and not as recovered time.
        (setf next (if (< (+ next tick) now)
                       (progn (incf (mixer-late m)) (+ now tick))
                       (+ next tick)))))))

(defun mixer-start (m)
  "Start the clock.  Idempotent."
  (sb-thread:with-mutex ((mixer-lock m))
    (unless (mixer-running m)
      (setf (mixer-running m) t
            (mixer-thread m)
            (sb-thread:make-thread (lambda () (%mixer-loop m)) :name "glass-mixer"))))
  m)

(defun mixer-stop (m)
  (setf (mixer-running m) nil)
  (let ((th (mixer-thread m)))
    (when th
      (ignore-errors (sb-thread:join-thread th :timeout 1))
      (setf (mixer-thread m) nil)))
  m)

(defun mixer-tick (m)
  "Mix exactly one frame, synchronously.  For a test, or for a caller driving the mix off its
own clock instead of ours; MIXER-START is the normal way."
  (%ring-push m (%mix-tick m))
  m)

;;; ---- a listener ------------------------------------------------------------

(defstruct (sink (:constructor %make-sink))
  mixer
  (name "" :type string)
  (rate 8000 :type fixnum)
  (frame-samples 160 :type fixnum)
  (gain 1.0d0 :type double-float)
  resampler
  (cursor 0 :type fixnum)           ; next mixer frame this sink wants
  (pending (reed:make-pcm16 0))     ; converted samples not yet handed out
  (fill 0 :type fixnum)
  (lead 2 :type fixnum)             ; frames of cushion before the first hand-out
  (primed nil)
  (frames 0 :type fixnum)
  (drops 0 :type fixnum)            ; mixer frames this sink never saw (it fell behind)
  (underruns 0 :type fixnum))       ; times it asked and the mix had nothing yet

(defun mixer-subscribe (m &key (name "sink") (rate nil) (frame-samples nil) (gain 1.0d0) (lead 2))
  "A private read cursor on the mix, converted to RATE and handed out FRAME-SAMPLES at a time.

Each sink has its own cursor, its own resampler and its own buffer, so two listeners hear the
same mix without interfering — that is the whole reason the mixer is here and not in a
transport.  LEAD frames of cushion are collected before the first hand-out; that is the sink's
share of latency, and it is what absorbs the jitter between two clocks that are both 20 ms and
neither of which is the other's."
  (let* ((rate (or rate (mixer-rate m)))
         (n (or frame-samples (round (* rate (mixer-period m)))))
         (s (%make-sink :mixer m :name name :rate rate :frame-samples n
                        :gain (float gain 1d0) :lead lead
                        :resampler (unless (= rate (mixer-rate m))
                                     (reed:make-resampler (mixer-rate m) rate)))))
    (sb-thread:with-mutex ((mixer-lock m))
      (setf (sink-cursor s) (mixer-seq m))
      (push s (mixer-sinks m)))
    s))

(defun mixer-unsubscribe (m s)
  (sb-thread:with-mutex ((mixer-lock m))
    (setf (mixer-sinks m) (remove s (mixer-sinks m))))
  t)

(defun %sink-push (s samples)
  (let ((need (+ (sink-fill s) (length samples))))
    (when (< (length (sink-pending s)) need)
      (let ((new (reed:make-pcm16 (max need (* 2 (length (sink-pending s)))))))
        (replace new (sink-pending s) :end2 (sink-fill s))
        (setf (sink-pending s) new)))
    (replace (sink-pending s) samples :start1 (sink-fill s))
    (setf (sink-fill s) need)))

(defun %sink-drain (s)
  "Collect every mixer frame this sink has not read yet, converting as it goes."
  (let* ((m (sink-mixer s))
         (ring (mixer-ring m))
         (cap (length ring))
         (frames '()))
    (sb-thread:with-mutex ((mixer-lock m))
      (let ((seq (mixer-seq m)))
        ;; Fell further behind than the ring is deep: the frames are gone.  Skip to the oldest
        ;; one still held and count the loss, rather than replaying whatever overwrote them.
        (when (< (sink-cursor s) (- seq cap))
          (incf (sink-drops s) (- (- seq cap) (sink-cursor s)))
          (setf (sink-cursor s) (- seq cap)))
        (loop while (< (sink-cursor s) seq)
              do (let ((f (svref ring (mod (sink-cursor s) cap))))
                   (when f (push f frames))
                   (incf (sink-cursor s))))))
    (dolist (f (nreverse frames))
      (let ((converted (if (sink-resampler s) (reed:resample (sink-resampler s) f) f)))
        (unless (= 1.0d0 (sink-gain s))
          (setf converted (reed:apply-gain (copy-seq converted) (sink-gain s))))
        (%sink-push s converted)))))

(defun %sink-trim (s)
  "Drop the oldest pending samples if the sink has let a backlog build.

A consumer slower than the mixer would otherwise accumulate latency without bound: it would
still hear everything, just further and further behind, which for live audio is worse than a
gap.  Trimming resyncs it to the present."
  ;; KEEP is the cushion SINK-NEXT-FRAME primes on, not one less: trimming to a backlog smaller
  ;; than the priming threshold leaves a sink that has plenty of audio reporting an underrun and
  ;; handing back NIL, forever.
  ;; One frame over the cushion is the trigger, and the correction is exactly one frame: two
  ;; clocks that are both "20 ms" drift, so this fires occasionally forever, and a listener would
  ;; rather lose 20 ms now and then than 80 ms in a lump every so often.
  (let ((maxfill (* (+ (sink-lead s) 2) (sink-frame-samples s))))
    (when (> (sink-fill s) maxfill)
      (let* ((keep (* (1+ (sink-lead s)) (sink-frame-samples s)))
             (drop (- (sink-fill s) keep)))
        (replace (sink-pending s) (sink-pending s) :start2 drop :end2 (sink-fill s))
        (decf (sink-fill s) drop)
        (incf (sink-drops s) (ceiling drop (sink-frame-samples s)))))))

(defun sink-next-frame (s)
  "The next FRAME-SAMPLES of the mix at this sink's rate, or NIL if the mix has not produced
them yet.  NIL is not an error and not a gap in the stream — it means ask again; a transport
sends silence for that slot and keeps its own clock, which is what its packets' timing carries."
  (%sink-drain s)
  (%sink-trim s)
  (let ((n (sink-frame-samples s)))
    (cond
      ;; before the first hand-out, wait for the cushion — starting empty means underrunning on
      ;; every jitter for the life of the stream.
      ((and (not (sink-primed s)) (< (sink-fill s) (* (1+ (sink-lead s)) n)))
       (incf (sink-underruns s))
       nil)
      ((< (sink-fill s) n) (incf (sink-underruns s)) nil)
      (t (setf (sink-primed s) t)
         (let ((out (subseq (sink-pending s) 0 n)))
           (replace (sink-pending s) (sink-pending s) :start2 n :end2 (sink-fill s))
           (decf (sink-fill s) n)
           (incf (sink-frames s))
           out)))))

(defun sink-source (s)
  "The sink as a bare source thunk — reed's contract, so it drops straight into
webrtc-media's :source, or anything else that asks for the next frame."
  (lambda () (sink-next-frame s)))

(defun mixer-report (m)
  "A line about the mix, for a control socket."
  (sb-thread:with-mutex ((mixer-lock m))
    (format nil "mixer ~dHz/~dsmp seq=~d late=~d level=~,3f sources=(~{~a~^ ~}) sinks=(~{~a~^ ~})"
            (mixer-rate m) (mixer-frame-samples m) (mixer-seq m) (mixer-late m) (mixer-level m)
            (mapcar (lambda (s) (format nil "~a:~d" (src-name s) (src-frames s))) (mixer-sources m))
            (mapcar (lambda (s) (format nil "~a@~d:~d/-~d/u~d" (sink-name s) (sink-rate s)
                                        (sink-frames s) (sink-drops s) (sink-underruns s)))
                    (mixer-sinks m)))))
