;;;; src/mic.lisp — a microphone, in this image, with no wire under it.
;;;;
;;;; THE SPLIT THIS FILE EXISTS FOR.  Audio out of a session has always had two shapes: a SINK,
;;;; which is an in-image read cursor on the mix (MIXER-SUBSCRIBE), and a socket that carries one
;;;; sink's frames to somebody else (audio-stream.lisp).  Audio IN had only the second — the
;;;; microphone was defined inside its transport, so the only way to have one was for something
;;;; to dial in and push frames at a port.
;;;;
;;;; That was true when the only peer was a browser on the other end of a gateway.  It stopped
;;;; being true when the desktop grew a viewer in its own process: a local microphone has no
;;;; connection to accept, no rate to negotiate and nobody to reconnect to, and making it open a
;;;; socket to itself would be a wire invented so an object could exist.
;;;;
;;;; So the OBJECT lives here and the transports are things that fill it.  Both directions now
;;;; read the same way:
;;;;
;;;;   out   MIXER-SUBSCRIBE -> sink -> SINK-NEXT-FRAME      audio-stream.lisp carries one away
;;;;   in    ATTACH-MIC      -> mic  -> MIC-NEXT-FRAME       mic-stream.lisp fills one from a socket
;;;;
;;;; The socket server is now one producer of microphones rather than the definition of what a
;;;; microphone is; SESSION-MIC answers whichever is attached and does not care which made it.
;;;; hearing.lisp is unchanged and did not need to change, which is the test of whether the seam
;;;; was in the right place: the ear asked for a microphone all along and never asked where from.
;;;;
;;;; THE MICROPHONE IS STILL NOT IN THE MIX, and for the reason mic-stream.lisp gives at length:
;;;; a voice on the session mixer is played out of the desktop's own audio and back to whoever
;;;; said it.  Local capture makes that worse rather than better — the round trip becomes a
;;;; speaker and a microphone in one room, which is not an echo but a feedback loop.

(in-package #:glass)

(defparameter *mic-rate* 16000
  "The rate a microphone is converted to before anything here hands it out.

Not a preference and not a coincidence: it is GLASS:*HEARING-RATE*, because the ear is the
consumer this exists for and its model's front end was built at 16 kHz.  Kept as its own
parameter rather than read out of hearing.lisp so that a desktop with no recognizer installed can
still carry a microphone — this system does not depend on stave and must not start to.")

(defparameter *mic-live-seconds* 2d0
  "How long a connected microphone that has sent nothing goes on counting as live.

It is the boundary between 'the room is quiet' and 'there is no microphone here', and only the
second one should make a listener go and listen to something else.  Long enough to survive a
browser's silence suppression and a bad half-second of network, short enough that a gateway that
wedged with the socket still open does not hold the ear forever.")

(defparameter *mic-gap-frames* 5
  "Consecutive empty pulls, on a LIVE connection, after which the microphone hands out silence
instead of NIL.

A consumer with a clock — the ear is one — measures the length of a pause by counting the frames
it received during it, so a microphone that answers NIL while its peer says nothing is a
microphone whose silences have no duration, and an utterance that ended is never noticed to have
ended.  Five frames is a tenth of a second: long enough that ordinary jitter is still reported
honestly as an underrun, short enough that a real pause is a real pause.")

;;; ---- the microphone itself -------------------------------------------------
;;;
;;; The consumer's end of one connection: converted, re-framed, bounded, and NEVER BLOCKING.  It
;;; is the audio tap's shape (audio-stream.lisp) with the conversion moved inside, because on this
;;; side the receiver is the one that knows what the consumer wants.

(defstruct (mic (:constructor %make-mic) (:conc-name mic-))
  (name "peer" :type string)
  (wire-rate 8000 :type fixnum)         ; what the sender said it is pushing
  (wire-frame 160 :type fixnum)
  (rate 16000 :type fixnum)             ; what this hands out
  (frame-samples 320 :type fixnum)
  resampler
  sock                                  ; kept so a retired connection can be closed, not waited on
  (lock (sb-thread:make-mutex :name "glass-mic"))
  (pending (reed:make-pcm16 0))         ; converted samples not yet whole frames
  (fill 0 :type fixnum)
  (ring #() :type simple-vector)
  (head 0 :type fixnum) (tail 0 :type fixnum)
  (prime 2 :type fixnum)
  (primed nil)
  (dry 0 :type fixnum)                  ; consecutive empty pulls
  (open nil)
  (stamp 0 :type integer)               ; internal-real-time of the last frame IN
  (received 0 :type fixnum)             ; frames off the wire
  (frames 0 :type fixnum)               ; frames handed to the consumer
  (silence 0 :type fixnum)              ; frames of manufactured silence handed out
  (drops 0 :type fixnum)                ; arrived while the queue was full — the consumer is behind
  (underruns 0 :type fixnum))           ; asked and had nothing — the peer is behind, or quiet

(defun make-mic (&key (name "mic") (wire-rate *mic-rate*) (wire-frame 0)
                      (rate *mic-rate*) frame-samples (depth 8))
  "A microphone with nothing behind it yet: push frames in with MIC-PUSH.

   The public constructor, because a microphone no longer has to arrive over a socket.  WIRE-RATE
   is the rate frames will be pushed AT and RATE the rate they are handed out at; when they differ
   a resampler is made once here, which is the same arrangement the socket path uses and for the
   same reason — the producer knows its own rate and the consumer should not have to.

   A local capture device typically pushes at the rate it was given by the platform and wants
   nothing converted, so the default is the two being equal and no resampler at all."
  (let* ((wf (if (plusp wire-frame) wire-frame (round (* wire-rate 20) 1000)))
         (fs (or frame-samples (round (* rate 20) 1000))))
    (%make-mic :name name :wire-rate wire-rate :wire-frame wf
               :rate rate :frame-samples fs
               ;; OPEN from the start, because there is no connection to wait for: a producer
               ;; that called this has one.  It is still not LIVE until a frame arrives — see
               ;; MIC-LIVE-P, which asks whether anything has ever been said as well as when.
               ;; That is the same distinction the socket path draws between connected and
               ;; speaking, and it keeps a listener from waiting on a device nobody is using.
               :open t
               :ring (make-array (max 2 depth) :initial-element nil)
               :resampler (unless (= wire-rate rate)
                            (reed:make-resampler wire-rate rate)))))

(defun mic-push (mic pcm)
  "Feed PCM (signed 16-bit samples at MIC's wire rate) into MIC.

   The public face of %MIC-PUSH, so a producer that is not a socket has a supported way in.
   NEVER BLOCKS: a full queue drops the oldest, because a microphone that stalls its producer
   stalls whatever is capturing, and a capture callback that is late is a click."
  (when (and mic pcm (plusp (length pcm))) (%mic-push mic pcm))
  mic)

(defun mic-live-p (&optional (mic (session-mic)))
  "True while this microphone is connected AND has sent something recently.

Both halves matter to the only question anyone asks of it — 'is there a microphone to listen
to?'.  A socket that is open and silent for *MIC-LIVE-SECONDS* is a gateway that is up and a
phone that is gone, and answering yes to that is how a listener ends up hearing nothing forever."
  (and mic (mic-open mic)
       ;; HAS IT EVER SPOKEN.  Not implied by the clock test below: GET-INTERNAL-REAL-TIME
       ;; counts from process start, so a STAMP of 0 — the value that means "no frame has ever
       ;; arrived" — reads as a frame that arrived at time zero, which is recent for the first
       ;; *MIC-LIVE-SECONDS* of the process.  A microphone attached during startup was
       ;; therefore live before anything had been said into it, and a listener would have
       ;; waited on it.  Latent while the only microphones came from sockets on a box that had
       ;; been up for hours; immediate for one attached by a viewer as the desktop boots.
       (plusp (mic-received mic))
       (< (- (get-internal-real-time) (mic-stamp mic))
          (* *mic-live-seconds* internal-time-units-per-second))
       t))

(defun %mic-emit (mic frame)
  "One whole frame into the ring.  Called with the lock held."
  (let* ((ring (mic-ring mic)) (cap (length ring))
         (avail (- (mic-tail mic) (mic-head mic))))
    ;; Same trim, and the same arithmetic, as the sink's and the tap's: a consumer slower than the
    ;; sender would otherwise ride at the queue's full depth forever, hearing everything a fixed
    ;; distance in the past.  One frame at a time, because two clocks that are both 20 ms drift.
    (when (or (= avail cap) (> avail (+ (mic-prime mic) 2)))
      (incf (mic-head mic))
      (incf (mic-drops mic)))
    (setf (svref ring (mod (mic-tail mic) cap)) frame)
    (incf (mic-tail mic))))

(defun %mic-push (mic pcm)
  "Samples off the wire: converted to this microphone's rate, cut into whole frames, queued.

Runs on the connection's reader thread, which has nothing else to do, so the conversion is here
rather than on the consumer's clock — which is the same reason the sink converts where it does."
  (let ((converted (if (mic-resampler mic) (reed:resample (mic-resampler mic) pcm) pcm)))
    (sb-thread:with-mutex ((mic-lock mic))
      (incf (mic-received mic))
      (setf (mic-stamp mic) (get-internal-real-time))
      ;; append, then take whole frames off the front
      (let ((need (+ (mic-fill mic) (length converted))))
        (when (< (length (mic-pending mic)) need)
          (let ((new (reed:make-pcm16 (max need (* 2 (length (mic-pending mic)))))))
            (replace new (mic-pending mic) :end2 (mic-fill mic))
            (setf (mic-pending mic) new)))
        (replace (mic-pending mic) converted :start1 (mic-fill mic))
        (setf (mic-fill mic) need))
      (let ((n (mic-frame-samples mic)))
        (loop while (>= (mic-fill mic) n)
              do (let ((frame (reed:make-pcm16 n)))
                   (replace frame (mic-pending mic) :end2 n)
                   (replace (mic-pending mic) (mic-pending mic) :start2 n :end2 (mic-fill mic))
                   (decf (mic-fill mic) n)
                   (%mic-emit mic frame)))))))

(defun mic-next-frame (&optional (mic (session-mic)))
  "The next frame of the peer's microphone at MIC-RATE, or NIL if none has arrived.

NEVER BLOCKS: this is reed's source contract, which is the ear's :SOURCE contract, which is why a
microphone drops into MAKE-EARS with no adapter at all.  NIL means 'ask again' — except on a live
connection that has been dry for *MIC-GAP-FRAMES*, where the honest answer is silence, because a
quiet room is a thing that takes time and a consumer measures time by frames."
  (when mic
    (sb-thread:with-mutex ((mic-lock mic))
      (let ((avail (- (mic-tail mic) (mic-head mic))))
        (cond
          ;; prime before the first hand-out, and again after running dry: handing out the instant
          ;; one frame exists means underrunning on the next packet of jitter, forever
          ((or (zerop avail) (and (not (mic-primed mic)) (< avail (mic-prime mic))))
           (setf (mic-primed mic) nil)
           (incf (mic-underruns mic))
           (incf (mic-dry mic))
           (when (and (mic-open mic)
                      (> (mic-dry mic) *mic-gap-frames*)
                      (< (- (get-internal-real-time) (mic-stamp mic))
                         (* *mic-live-seconds* internal-time-units-per-second)))
             (incf (mic-silence mic))
             (reed:make-pcm16 (mic-frame-samples mic))))
          (t (setf (mic-primed mic) t (mic-dry mic) 0)
             (let ((f (svref (mic-ring mic) (mod (mic-head mic) (length (mic-ring mic))))))
               (incf (mic-head mic))
               (incf (mic-frames mic))
               f)))))))

(defun mic-source (&optional (mic (session-mic)))
  "The microphone as a bare source thunk, for MAKE-EARS :SOURCE or anything else that takes one."
  (lambda () (mic-next-frame mic)))

(defun mic-report (&optional (mic (session-mic)))
  (if (null mic)
      "mic: none connected"
      (format nil "mic ~a ~:[closed~;~:[stale~;live~]~] ~dHz->~dHz rx=~d out=~d sil=~d -~d u~d"
              (mic-name mic) (mic-open mic) (mic-live-p mic)
              (mic-wire-rate mic) (mic-rate mic)
              (mic-received mic) (mic-frames mic) (mic-silence mic)
              (mic-drops mic) (mic-underruns mic))))

;;; ---- the server: the desktop's end -----------------------------------------

;;; ---- which microphone the session has --------------------------------------

(defvar *session-mic* nil
  "The microphone attached to this session, whatever attached it.

There is one because a microphone is the one thing here that is nobody else's business: two
people in two rooms talking into two phones are two microphones and never a mix, since mixing
them is what would put each one's voice into the other's ear.  Seats each have their own; this
is the SESSION's, which is what everything that asks without a seat to ask about means — the ear
a one-seat desktop starts, a control socket, MIC-REPORT.")

(defun session-mic ()
  "The microphone the session is currently being spoken into, or NIL.

NIL is the ordinary state of a desktop nobody has dialed into and nobody is sitting at, and every
caller is written so that NIL costs nothing: no error, no log line, and no microphone."
  *session-mic*)

(defun attach-mic (mic)
  "Make MIC the session's microphone and return it.  Called by whatever produced it — the socket
server when a peer connects, a local viewer when it opens a capture device.  Last one wins, which
is the same rule the socket server always applied to a second connection."
  (setf *session-mic* mic))

(defun detach-mic (&optional (mic *session-mic*))
  "Take MIC off the session, if it is still the one attached.  The guard matters: a connection
retiring after a newer one replaced it must not unhook the newer one."
  (when (and mic (eq mic *session-mic*)) (setf *session-mic* nil))
  nil)
