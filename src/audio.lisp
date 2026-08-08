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
;;;;
;;;; ---- ONE BUS, SEVERAL MIXES: the same split the screen has ------------------------------
;;;;
;;;; Everything above is about TRANSPORTS, and it is unchanged: a mixer inside a transport is
;;;; still wrong for all the reasons given.  SEATS are a different axis (backend/seat.lisp): a
;;;; seat is one PERSON watching a shared session, and two seats are two people, each of whom is
;;;; entitled to their own idea of how loud the podcast is — exactly as each is entitled to their
;;;; own arrangement of the same windows.  All of one seat's transports still share one mix; two
;;;; seats do not.  (The clipboard header draws the same distinction, and it is the same one.)
;;;;
;;;; The naive way to give a seat its own mix is to give it its own MIXER over the same sources,
;;;; and it is broken in a way that is worth stating because it is invisible until you listen: a
;;;; source is a DESTRUCTIVE PULL.  (funcall (src-thunk s)) returns the NEXT frame and advances
;;;; the source; it is not a framebuffer that any number of compositors may read.  Two mixers on
;;;; two clocks pulling one podcast would take alternate frames of it, and BOTH seats would hear
;;;; a stream running at double speed with half of it missing.  That is the same fact this file's
;;;; clock rule already rests on — "if the mix advanced when a consumer asked for it… the other
;;;; would get a hole" — read one level out.
;;;;
;;;; So the decomposition follows the compositor's, exactly:
;;;;
;;;;   THE PULL IS THE PAINT.  One clock asks every source for its frame ONCE per 20 ms, the way
;;;;   an application paints its window once however many seats are looking at it.  What comes
;;;;   back is CONTENT: this frame of the podcast, this frame of the voice.
;;;;
;;;;   THE SUM IS THE COMPOSITE.  A MIX is one listener's composite of that content — its own
;;;;   selection, its own per-source gain, its own ring, its own sinks and cursors.  Adding a
;;;;   seat adds a sum over the same frames; it never adds a pull, and it never adds a clock.
;;;;
;;;; A MIXER therefore has a DEFAULT MIX, which is the session's own and which every call that
;;;; predates seats still means (MIXER-SUBSCRIBE, MIXER-SEQ, MIXER-LEVEL and the rest all read
;;;; it).  A one-seat desktop has exactly one mix and does exactly the work it did before.
;;;;
;;;; A source may also name an AUDIENCE — the mixes that hear it, NIL meaning all of them.  That
;;;; is the source's half of the same question the mix's gains answer from the listener's half,
;;;; and it is what "say this to the person who asked" needs: a selection belongs to one seat, so
;;;; reading it aloud belongs to one seat.  Sound with no audience is the desktop's own noise and
;;;; everybody watching hears it, which is what a session-wide notification wants.

(in-package #:glass)

(defparameter *mixer-frame-ms* 20
  "Mix quantum.  20 ms is what every RTP audio payload uses, so a transport that wants 20 ms
frames gets them without rebuffering; nothing here depends on the number.")

(defparameter *mixer-rate* 48000
  "Native mix rate.  Consumers convert DOWN from this; nothing converts up.")

;;; ---- a source in the mix ---------------------------------------------------

(define-record (mixer-source (:conc-name src-) (:constructor %make-src))
  (id 0 :type fixnum)
  (name "" :type string)
  thunk
  (gain 1.0d0 :type double-float)   ; live: (setf (glass:src-gain s) 0.5d0) while it plays
  (finite nil)                      ; T = remove it the first time it returns NIL (a file ends)
  ;; Who this sound is FOR.  NIL = the session: every mix composites it, which is what an
  ;; application making noise means.  A list of MIXes = only those, which is what reading one
  ;; person's selection aloud means.  It is live, so one voice can answer one seat and then
  ;; the room (see speech.lisp) without a second engine.
  (audience nil)
  (frames 0 :type fixnum)
  (errors 0 :type fixnum))

;;; ---- one listener's mix ------------------------------------------------------
;;;
;;; A MIX is to the sources what a SEAT's screen is to the windows: a composite, made per
;;; listener, over content that was produced once.  Its ring, its cursor arithmetic and its
;;; sinks are exactly what the mixer's own used to be, because the mixer's own IS one of these.
;;;
;;; A class and not a DEFINE-RECORD only because it is new: same reason everything long-lived
;;; here is a class (record.lisp), reached the direct way.

(defclass mix ()
  ((name  :initarg :name  :initform "mix" :accessor mix-name)
   (bus   :initarg :bus   :initform nil   :accessor mix-bus)   ; the MIXER whose sources it sums
   (lock  :initform (sb-thread:make-mutex :name "glass-mix") :accessor mix-lock)
   (ring  :initarg :ring  :initform #()   :accessor mix-ring)  ; recent frames; sinks read at their pace
   (seq   :initform 0     :accessor mix-seq)                   ; frames composited — the cursors' coordinate
   (sinks :initform '()   :accessor mix-sinks)
   (level :initform 0d0   :accessor mix-level)                 ; RMS of the last frame, for a meter
   ;; source id -> this listener's gain for it.  ABSENT means 1.0, so a mix that has never been
   ;; touched hears the whole session at its own volume — which is what a seat that just sat
   ;; down should hear, and what makes an empty table the one-seat case.
   (gains :initform (make-hash-table :test 'eql) :accessor mix-gains))
  (:documentation "One listener's composite of the session's sources: its selection, its gains,
   its ring and its sinks.  The mixer's DEFAULT-MIX is the session's own."))

(defmethod print-object ((m mix) stream)
  (print-unreadable-object (m stream :type t)
    (format stream "~s seq=~d sinks=~d" (slot-value m 'name) (slot-value m 'seq)
            (length (slot-value m 'sinks)))))

(defun mix-p (x) (typep x 'mix))

;;; ---- the mixer -------------------------------------------------------------

;; The mixer, its sources and its sinks are DEFINE-RECORDs and not DEFSTRUCTs — they are held for
;; the life of the session and their accessors run 50 times a second, not 50 million.  The pixel
;; path next door (FRAMEBUFFER, PXFMT, RFB-CLIENT) stays DEFSTRUCT for the opposite reason.
(define-record (mixer (:constructor %make-mixer))
  (rate 48000 :type fixnum)
  (frame-samples 960 :type fixnum)
  (period 0.02d0 :type double-float)
  (lock (sb-thread:make-mutex :name "glass-mixer"))   ; guards SOURCES and MIXES
  (sources '())
  (next-id 1 :type fixnum)
  (capacity 64 :type fixnum)        ; ring depth a mix made on this bus gets
  (default nil)                     ; the session's own mix — see MIXER-DEFAULT-MIX
  (mixes '())                       ; every mix composited on this bus, the default first
  (thread nil)
  (running nil)
  (late 0 :type fixnum))            ; ticks that ran past their deadline

;;; The four slots that moved into the default mix, still answering to their old names.  Same
;;; move, and the same reason, as the GLASS-PORT accessors that now delegate to the default SEAT
;;; (backend/backend.lisp): a session with one listener should not merely behave as it did, it
;;; should be the same call.
(defun mixer-ring  (m) (mix-ring  (mixer-default-mix m)))
(defun mixer-seq   (m) (mix-seq   (mixer-default-mix m)))
(defun mixer-sinks (m) (mix-sinks (mixer-default-mix m)))
(defun mixer-level (m) (mix-level (mixer-default-mix m)))

(defun %register-mix (m name capacity seq)
  "Make a mix and put it on the bus.  Called with MIXER-LOCK held."
  (let ((mix (make-instance 'mix :name name :bus m
                            :ring (make-array (or capacity (mixer-capacity m))
                                              :initial-element nil))))
    ;; Start level with the session: a mix that began at seq 0 would spend its first moment
    ;; believing every frame the session ever mixed is still owed to it.
    (setf (mix-seq mix) seq
          (mixer-mixes m) (append (mixer-mixes m) (list mix)))
    mix))

(defun mixer-default-mix (m)
  "The session's own mix: what MIXER-SUBSCRIBE and every other pre-seat call means.

Made on demand rather than only in MAKE-MIXER, so that hot-loading this file into a running
desktop leaves it with a working mix.  Redefining the class drops the slots the ring used to
live in (record.lisp buys the redefinition, it does not carry the data across), and a desktop
whose mixer came back from a recompile with no mix at all would be a desktop that went silent
until it was restarted — which is the outcome DEFINE-RECORD exists to avoid."
  (or (mixer-default m)
      (sb-thread:with-mutex ((mixer-lock m))
        (or (mixer-default m)
            (setf (mixer-default m) (%register-mix m "session" nil 0))))))

(defun make-mix (m &key (name "mix") capacity)
  "A private composite of M's sources: its own ring, cursor and sinks, hearing everything at
gain 1 until told otherwise.  Registered on M, so the one clock composites it.

This is what a second SEAT gets.  It costs one sum of the frames that were pulled anyway — no
thread, no clock, and above all no second pull of a source that can only be pulled once."
  (let ((seq (mix-seq (mixer-default-mix m))))
    (sb-thread:with-mutex ((mixer-lock m))
      (%register-mix m name capacity seq))))

(defun remove-mix (m mix)
  "Stop compositing MIX.  Its sinks stop being fed; nothing else changes, and the sources it
was hearing go on being heard by everybody else."
  (sb-thread:with-mutex ((mixer-lock m))
    (setf (mixer-mixes m) (remove mix (mixer-mixes m))))
  t)

(defun make-mixer (&key (rate *mixer-rate*) (frame-ms *mixer-frame-ms*) (capacity 64))
  "A session mixer at RATE with FRAME-MS frames, keeping CAPACITY frames of history.

CAPACITY is how far behind a consumer may fall before it loses audio: 64 frames of 20 ms is
1.3 s, which is generous for a network sink and cheap (64 x 960 x 2 bytes = 123 KB).  It is also
the depth every further MIX on this bus gets, for the same reason."
  (let* ((n (round (* rate frame-ms) 1000))
         (m (%make-mixer :rate rate :frame-samples n :capacity capacity
                         :period (/ frame-ms 1000d0))))
    (mixer-default-mix m)               ; the session's own mix, made here rather than lazily
    m))

(defun mixer-add-source (m source &key (name "source") (gain 1.0d0) finite audience)
  "Register SOURCE (a thunk returning the next frame of mono samples at the mixer's rate, or
NIL) and return its handle.  FINITE t removes it the first time it returns NIL — right for a
file, wrong for a capture device, which returns NIL whenever it simply has nothing yet.

AUDIENCE is the list of MIXes that hear it; NIL (the default) means all of them, which is what
an application playing audio means — it is making noise in the session, not talking to one
person."
  (sb-thread:with-mutex ((mixer-lock m))
    (let ((s (%make-src :id (mixer-next-id m) :name name :thunk source
                        :gain (float gain 1d0) :finite finite :audience audience)))
      (incf (mixer-next-id m))
      (setf (mixer-sources m) (append (mixer-sources m) (list s)))
      s)))

(defun mixer-find-source (m name)
  "The first source on M called NAME, or NIL.  What a control socket has to hand: `mute the
podcast for this seat' is asked by name, because the handle belongs to whoever registered it."
  (sb-thread:with-mutex ((mixer-lock m))
    (find name (mixer-sources m) :key #'src-name :test #'string=)))

;;; ---- what one mix hears ------------------------------------------------------

(defun %as-source (m thing)
  "A source handle, from a handle, an id, or a name.  So a mix's gains can be set from a
control socket, where nobody is holding the object."
  (cond ((mixer-source-p thing) thing)
        ((integerp thing) (find thing (mixer-sources m) :key #'src-id))
        ((stringp thing) (mixer-find-source m thing))))

(defun mix-source-gain (mix source)
  "THIS listener's gain for SOURCE — 1.0 unless it has been set, 0.0 when muted.  Multiplied
by the source's own gain, which is the session's idea of how loud that application is."
  (let ((s (%as-source (mix-bus mix) source)))
    (if s
        (sb-thread:with-mutex ((mix-lock mix)) (gethash (src-id s) (mix-gains mix) 1d0))
        1d0)))

(defun (setf mix-source-gain) (value mix source)
  "Set this listener's gain for SOURCE.  Takes effect on the next frame and affects nobody
else's mix — which is the whole of what a per-seat mix is for."
  (let ((s (%as-source (mix-bus mix) source)))
    (when s
      (sb-thread:with-mutex ((mix-lock mix))
        (setf (gethash (src-id s) (mix-gains mix)) (float value 1d0))))
    value))

(defun mix-mute (mix source)
  "Silence SOURCE for this listener only.  Returns T if there was such a source."
  (let ((s (%as-source (mix-bus mix) source)))
    (when s (setf (mix-source-gain mix s) 0d0) t)))

(defun mix-unmute (mix source)
  "Hear SOURCE again, at full gain."
  (let ((s (%as-source (mix-bus mix) source)))
    (when s (setf (mix-source-gain mix s) 1d0) t)))

(defun mix-hears-p (mix src)
  "Does MIX composite SRC this frame?  Two independent answers, and both are no's: the source
may be ADDRESSED elsewhere (its audience), and this listener may have MUTED it (its gain)."
  (let ((aud (src-audience src)))
    (and (or (null aud) (member mix aud))
         (plusp (sb-thread:with-mutex ((mix-lock mix))
                  (gethash (src-id src) (mix-gains mix) 1d0))))))

(defun mixer-remove-source (m source-or-id)
  "Drop a source.  Returns T if it was there."
  (sb-thread:with-mutex ((mixer-lock m))
    (let* ((id (if (mixer-source-p source-or-id) (src-id source-or-id) source-or-id))
           (hit (find id (mixer-sources m) :key #'src-id)))
      (when hit
        (setf (mixer-sources m) (remove hit (mixer-sources m)))
        t))))

(defun mixer-play (m samples &key (name "clip") (gain 1.0d0) rate audience)
  "Play SAMPLES once and then forget them — a notification, a bell, a test tone.  RATE, if
given and different from the mixer's, is converted first.  AUDIENCE, if given, is the mixes
that hear it: a bell for the person whose job finished, rather than for the room."
  (let ((buf (if (and rate (/= rate (mixer-rate m)))
                 (let ((rs (reed:make-resampler rate (mixer-rate m))))
                   (concatenate '(simple-array (signed-byte 16) (*))
                                (reed:resample rs samples)
                                (reed:resample rs (reed:make-pcm16 0) :final t)))
                 samples)))
    (mixer-add-source m (reed:make-buffer-source buf :frame-samples (mixer-frame-samples m))
                      :name name :gain gain :finite t :audience audience)))

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

(defun %pull-sources (m)
  "Ask every source for its frame, ONCE.  Returns ((SOURCE . FRAME) …), newest content first —
the audio equivalent of every window having painted itself, and the reason a second listener
adds a sum and never a pull.

Runs on the clock thread, under no lock while the sources run — a source is caller code and may
be slow; holding the mixer lock across it would block subscribe/add for as long as the slowest
source takes."
  (let ((sources (sb-thread:with-mutex ((mixer-lock m)) (copy-list (mixer-sources m))))
        (pairs '()) (done '()))
    (dolist (s sources)
      (let ((buf (handler-case (funcall (src-thunk s))
                   (serious-condition (e)
                     ;; a throwing source is a silent frame, not a dead mix; three strikes and
                     ;; it is out, so a permanently broken source does not burn the clock.
                     (when (>= (incf (src-errors s)) 3) (push s done))
                     (values nil e)))))
        (cond ((and buf (plusp (length buf)))
               (incf (src-frames s))
               (push (cons s buf) pairs))
              ((src-finite s) (push s done)))))
    (dolist (s done) (mixer-remove-source m s))
    (nreverse pairs)))

(defun %mix-frame (mix pairs n)
  "One listener's composite of this round's content: the frames it hears, at its own gains.

The source's own gain and the mix's are multiplied, and they mean different things — the first
is how loud that application is IN THE SESSION (spool sets it, and turning it down turns it
down for everybody), the second is how loud it is FOR THIS LISTENER.  Both are live."
  (let ((bufs '()) (gains '()))
    (dolist (p pairs)
      (let ((src (car p)))
        (when (mix-hears-p mix src)
          (push (cdr p) bufs)
          (push (* (src-gain src) (mix-source-gain mix src)) gains))))
    (if bufs
        (reed:mix (nreverse bufs) :gains (nreverse gains) :length n)
        (reed:make-pcm16 n))))     ; nobody this listener hears: silence, and the clock advances

(defun %mix-tick (m)
  "Mix one frame of the SESSION's own mix.  Kept because it is what this file meant by a mix
before there were several, and because a caller driving the arithmetic by hand still wants it."
  (%mix-frame (mixer-default-mix m) (%pull-sources m) (mixer-frame-samples m)))

(defun %rms (pcm)
  (if (zerop (length pcm))
      0d0
      (let ((sum 0d0))
        (dotimes (i (length pcm))
          (let ((v (float (aref pcm i) 1d0))) (incf sum (* v v))))
        (min 1d0 (/ (sqrt (/ sum (length pcm))) 8000d0)))))

(defun %ring-push (target frame)
  "Put FRAME in TARGET's ring.  TARGET is a MIX; a MIXER is accepted and means the session's,
which is what makes this file hot-loadable: the clock thread of a desktop that was running the
old code is still inside the OLD %MIXER-LOOP, which calls this with the mixer.  Accepting it
means a live patch keeps the desktop's sound instead of silencing it, and the new clock — the
one that composites every seat's mix — arrives with the next MIXER-STOP / MIXER-START, which is
one line at a control socket."
  (let ((mix (as-mix target)))
    (sb-thread:with-mutex ((mix-lock mix))
      (let ((ring (mix-ring mix)))
        (setf (svref ring (mod (mix-seq mix) (length ring))) frame)
        (incf (mix-seq mix))
        (setf (mix-level mix) (%rms frame))))))

(defun %mixer-round (m)
  "One tick: pull the content once, composite it for every listener.

The order is the whole design in three lines — one pull, then a sum per mix.  Reversing it (a
pull per mix) is the bug this shape exists to make unwritable."
  (mixer-default-mix m)                 ; a bus always has the session's mix, even after a reload
  (let ((pairs (%pull-sources m))
        (n (mixer-frame-samples m))
        (mixes (sb-thread:with-mutex ((mixer-lock m)) (copy-list (mixer-mixes m)))))
    (dolist (mix mixes)
      (%ring-push mix (%mix-frame mix pairs n)))))

(defun %mixer-loop (m)
  (let* ((units (float internal-time-units-per-second 1d0))
         (tick (max 1 (round (* (mixer-period m) internal-time-units-per-second))))
         (next (+ (get-internal-real-time) tick)))
    (loop while (mixer-running m) do
      ;; ANY unhandled condition in ANY thread kills a --disable-debugger image; a bad frame
      ;; must cost one frame.
      (handler-case (%mixer-round m)
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
  "Mix exactly one frame, synchronously — every listener's, from one pull.  For a test, or for
a caller driving the mix off its own clock instead of ours; MIXER-START is the normal way."
  (%mixer-round m)
  m)

;;; ---- a listener ------------------------------------------------------------

(define-record (sink (:constructor %make-sink))
  mix                               ; the composite it reads — the session's, or one seat's
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

(defun as-mix (m)
  "A MIX, from a mix or from a mixer.  Handed a mixer it answers the SESSION's mix, which is
what every caller written before there were seats meant and still means."
  (if (mix-p m) m (mixer-default-mix m)))

(defun sink-mixer (s)
  "The bus behind this sink's mix — its rate, its clock, its sources."
  (mix-bus (sink-mix s)))

(defun mixer-subscribe (m &key (name "sink") (rate nil) (frame-samples nil) (gain 1.0d0) (lead 2))
  "A private read cursor on a mix, converted to RATE and handed out FRAME-SAMPLES at a time.
M is a MIXER (meaning the session's mix) or a MIX (meaning one seat's).

Each sink has its own cursor, its own resampler and its own buffer, so two listeners hear the
same mix without interfering — that is the whole reason the mixer is here and not in a
transport.  LEAD frames of cushion are collected before the first hand-out; that is the sink's
share of latency, and it is what absorbs the jitter between two clocks that are both 20 ms and
neither of which is the other's."
  (let* ((mix (as-mix m))
         (bus (mix-bus mix))
         (rate (or rate (mixer-rate bus)))
         (n (or frame-samples (round (* rate (mixer-period bus)))))
         (s (%make-sink :mix mix :name name :rate rate :frame-samples n
                        :gain (float gain 1d0) :lead lead
                        :resampler (unless (= rate (mixer-rate bus))
                                     (reed:make-resampler (mixer-rate bus) rate)))))
    (sb-thread:with-mutex ((mix-lock mix))
      (setf (sink-cursor s) (mix-seq mix))
      (push s (mix-sinks mix)))
    s))

(defun sink-unsubscribe (s)
  "Take S off whichever mix it is reading.  The sink knows; the caller may not."
  (let ((mix (sink-mix s)))
    (sb-thread:with-mutex ((mix-lock mix))
      (setf (mix-sinks mix) (remove s (mix-sinks mix)))))
  t)

(defun mixer-unsubscribe (m s)
  "Drop sink S.  M is ignored beyond politeness: the sink records the mix it joined, and a
caller holding the bus must not be able to unsubscribe a seat's sink from the session's mix."
  (declare (ignorable m))
  (sink-unsubscribe s))

(defun %sink-push (s samples)
  (let ((need (+ (sink-fill s) (length samples))))
    (when (< (length (sink-pending s)) need)
      (let ((new (reed:make-pcm16 (max need (* 2 (length (sink-pending s)))))))
        (replace new (sink-pending s) :end2 (sink-fill s))
        (setf (sink-pending s) new)))
    (replace (sink-pending s) samples :start1 (sink-fill s))
    (setf (sink-fill s) need)))

(defun %sink-drain (s)
  "Collect every frame of this sink's mix that it has not read yet, converting as it goes."
  (let* ((m (sink-mix s))
         (ring (mix-ring m))
         (cap (length ring))
         (frames '()))
    (sb-thread:with-mutex ((mix-lock m))
      (let ((seq (mix-seq m)))
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

;;; ---- the session's mixer ---------------------------------------------------
;;;
;;; A session has ONE sound, so the process running the desktop has one mixer, and the things
;;; that want to reach it — a control socket eval, a transport, an app that beeps — should not
;;; each have to be handed it.  This is that one, created on first use.  It is a convenience, not
;;; a constraint: MAKE-MIXER still makes as many independent mixers as a caller wants (a test
;;; makes one per case), and nothing in this file consults *SESSION-MIXER*.

(defvar *session-mixer* nil
  "The mixer for this process's session, or NIL before anything asked for one.")

(defvar *session-mixer-lock* (sb-thread:make-mutex :name "glass-session-mixer"))

(defun session-mixer (&key (start t))
  "This process's session mixer, started, creating it on first call.  Idempotent — two callers
racing at startup get the same mix, not one each."
  (sb-thread:with-mutex (*session-mixer-lock*)
    (or *session-mixer*
        (let ((m (make-mixer)))
          (when start (mixer-start m))
          (setf *session-mixer* m)))))

(defun %decode-audio-file (path)
  (let ((type (string-downcase (or (pathname-type path) ""))))
    (cond ((string= type "mp3") (reed:decode-mp3-file path))
          ((member type '("aac" "adts" "m4a") :test #'string=) (reed:decode-aac-file path))
          ((member type '("opus" "ogg") :test #'string=) (reed:decode-opus-file path))
          (t (error "glass audio: no decoder for ~a" path)))))

(defun mixer-add-file (m path &key loop (gain 1.0d0) name audience)
  "Decode PATH (reed: mp3/aac/opus), convert it to the mix's rate once, and register it.

Decoded up front rather than streamed, because the caller that wants this wants a sound to be
THERE — a notification, a track a listener joining halfway through should already hear playing —
and a decoder feeding the mix from the clock thread would make the mix's deadline depend on how
fast a file decodes.  LOOP t wraps forever; without it the source is finite and removes itself
when the file ends.  GAIN rides on the source (SETF SRC-GAIN adjusts it while it plays) rather
than being burned into the samples.

Returns (values SOURCE SECONDS)."
  (let* ((pcm (%decode-audio-file path))
         (mono (reed:downmix (reed:pcm-samples pcm) (reed:pcm-channels pcm)))
         (buf (if (= (reed:pcm-sample-rate pcm) (mixer-rate m))
                  mono
                  (let ((rs (reed:make-resampler (reed:pcm-sample-rate pcm) (mixer-rate m))))
                    (concatenate '(simple-array (signed-byte 16) (*))
                                 (reed:resample rs mono)
                                 (reed:resample rs (reed:make-pcm16 0) :final t))))))
    (values (mixer-add-source m (reed:make-buffer-source
                                 buf :frame-samples (mixer-frame-samples m) :loop loop)
                              :name (or name (file-namestring path))
                              :gain gain :finite (not loop) :audience audience)
            (/ (length buf) (float (mixer-rate m) 1d0)))))

(defun mix-report (mix)
  "A line about one listener's composite: what it hears, how loud, and who is reading it."
  (let ((bus (mix-bus mix)))
    (format nil "mix ~a seq=~d level=~,3f gains=(~{~a~^ ~}) sinks=(~{~a~^ ~})"
            (mix-name mix) (mix-seq mix) (mix-level mix)
            ;; only the sources this listener has an OPINION about — an untouched mix hears the
            ;; session as it stands, and saying "podcast=1.0 speech=1.0" would hide the one that
            ;; is actually set to 0
            (let ((out '()))
              (dolist (s (sb-thread:with-mutex ((mixer-lock bus)) (copy-list (mixer-sources bus)))
                         (nreverse out))
                (multiple-value-bind (g there)
                    (sb-thread:with-mutex ((mix-lock mix)) (gethash (src-id s) (mix-gains mix)))
                  (when there (push (format nil "~a=~,2f" (src-name s) g) out)))))
            (mapcar (lambda (s) (format nil "~a@~d:~d/-~d/u~d" (sink-name s) (sink-rate s)
                                        (sink-frames s) (sink-drops s) (sink-underruns s)))
                    (mix-sinks mix)))))

(defun mixer-report (m)
  "A line about the bus, for a control socket: the sources once, then each listener's mix.

The shape of the line is the shape of the design — the sources are the session's and are
printed once however many people are listening; the mixes are the listeners'."
  (format nil "mixer ~dHz/~dsmp late=~d sources=(~{~a~^ ~})~{~%  ~a~}"
          (mixer-rate m) (mixer-frame-samples m) (mixer-late m)
          (mapcar (lambda (s)
                    (format nil "~a:~d~@[ ->~d listener~:p~]" (src-name s) (src-frames s)
                            (and (src-audience s) (length (src-audience s)))))
                  (sb-thread:with-mutex ((mixer-lock m)) (copy-list (mixer-sources m))))
          (mapcar #'mix-report
                  (sb-thread:with-mutex ((mixer-lock m)) (copy-list (mixer-mixes m))))))
