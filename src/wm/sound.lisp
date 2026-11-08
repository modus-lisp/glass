;;;; src/wm/sound.lisp — the window manager's sound: mixed by the compositor, served by the network.
;;;;
;;;; The McCLIM desktop had a session mixer with its own 20 ms clock thread, and START-SESSION-AUDIO
;;;; served that mix on the RFB port + 10 (glass-audio/1, src/audio-stream.lisp) with a thread per
;;;; listener.  The WebRTC gateway is such a listener: it asks for 8 kHz, 160-sample frames and
;;;; sends them to the phone as the peer connection's audio track.  None of those threads can exist
;;;; where threads share nothing (modus): a clock thread pulling a source the compositor's player
;;;; owns, a listener thread reading a mix the clock thread writes.  So the work goes to the two
;;;; threads SERVE-WM already has, split the way the pixels are:
;;;;
;;;;   THE COMPOSITOR MIXES.  It owns the apps, so it owns their sound: a mixer with NO clock thread
;;;;   (glass:make-mixer, never started), whose sources are the apps' (the Media player's thunk),
;;;;   ticked by the compositor's own loop once per 20 ms of wall time that has passed -- kiln's
;;;;   phone does the same with its speaker.  The mixer is still the player's clock (it counts the
;;;;   samples taken), so a film's picture keeps pace with its sound.  While somebody is listening,
;;;;   each mixed frame (48 kHz mono s16) is posted to the network's mailbox, shared on the way.
;;;;
;;;;   THE NETWORK SERVES.  SERVE-MULTI's TICK, on the network thread: accept glass-audio/1
;;;;   listeners, read their optional request line, and write each one its frames.  The per-listener
;;;;   conversion is glass/audio's own sinks (rate, frame size, gain, cushion, drift trim), on a
;;;;   second clockless mixer this thread owns whose one source is the queue of frames the
;;;;   compositor sent.  A listener that does not drain loses frames (socket-unsent-bytes); it
;;;;   never blocks the thread every viewer's pixels go out on.
;;;;
;;;; The wire is exactly START-AUDIO-STREAM's, so the gateway and `nc' cannot tell the difference.

(in-package #:glass.wm)

;;; ---- the compositor's half -------------------------------------------------------------------

(defparameter *sound-catch-up* 10
  "At most this many 20 ms frames mixed in one compositor turn.  A turn that was later than that
   (a long decode, a big composite) forgives the rest: the listener hears a gap and the player's
   clock -- samples taken -- simply did not advance, rather than a burst of stale audio.")

(defstruct (wm-sound (:constructor %make-wm-sound))
  mixer sink outbox
  (tick 0 :type fixnum)                 ; one frame, in internal time units
  (next 0 :type integer)                ; when the next frame is due
  (listening nil)                       ; the network says somebody is: post frames
  (frames 0 :type fixnum)               ; mixed
  (posted 0 :type fixnum)
  (forgiven 0 :type fixnum))            ; frames skipped because a turn was very late

(defun wm-sound-open (outbox)
  "The compositor's mixer: no clock thread, ticked by WM-SOUND-TURN.  OUTBOX is the network's
   mailbox for mixed frames."
  (let* ((m (glass:make-mixer))
         (sink (glass:mixer-subscribe m :name "wm-out" :lead 0)))
    (%make-wm-sound :mixer m :sink sink :outbox outbox
                    :tick (max 1 (round (* (glass::mixer-period m) internal-time-units-per-second)))
                    :next (get-internal-real-time))))

(defun wm-sound-turn (snd)
  "Mix every frame that has come due since the last turn; post them while somebody listens."
  (let ((now (get-internal-real-time)) (tick (wm-sound-tick snd)))
    (when (> (- now (wm-sound-next snd)) (* *sound-catch-up* tick))
      (let ((skip (- (floor (- now (wm-sound-next snd)) tick) *sound-catch-up*)))
        (incf (wm-sound-forgiven snd) skip)
        (incf (wm-sound-next snd) (* skip tick))))
    (loop while (<= (wm-sound-next snd) now)
          do (glass:mixer-tick (wm-sound-mixer snd))
             (incf (wm-sound-frames snd))
             (incf (wm-sound-next snd) tick)
             (let ((f (glass:sink-next-frame (wm-sound-sink snd))))
               (when (and f (wm-sound-listening snd))
                 (glass::mb-post (wm-sound-outbox snd) f)
                 (incf (wm-sound-posted snd)))))))

(defun wm-sound-wait (snd longest)
  "How long the compositor may wait: LONGEST, or less if a frame comes due sooner."
  (min longest
       (max 0.001d0 (/ (- (wm-sound-next snd) (get-internal-real-time))
                       (float internal-time-units-per-second 1d0)))))

(defun wm-sound-listen (snd n)
  "The network says N listeners are connected: post frames only while there are any."
  (setf (wm-sound-listening snd) (plusp n)))

;;; ---- the network's half ----------------------------------------------------------------------

(defparameter *audio-hello-ms* 250
  "How long a new listener has to send its request line before it gets the native mix.")

(defparameter *audio-backlog-frames* 25
  "A listener with more than this many frames unsent is not draining: its frames are dropped.")

(defstruct (audio-out (:constructor %make-audio-out))
  listener lfd inbox
  mixer                                 ; clockless: one tick per frame the compositor sent
  (queue '())                           ; frames waiting for that tick, oldest first
  (clients '())
  (served 0 :type fixnum)
  notify)                               ; (lambda (n)): tell the compositor how many listen

(defstruct (audio-client (:conc-name ac-))
  stream fd sink (line (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
  (deadline 0 :type integer) octets silence (stalled nil) (written 0 :type fixnum)
  (drops 0 :type fixnum))

(defun audio-out-open (inbox &key port path (address "127.0.0.1") notify)
  "Listen for glass-audio/1 listeners on PORT (or the UNIX socket PATH), on the calling thread
   -- the network's.  INBOX is the mailbox the compositor posts mixed frames to; NOTIFY is
   called with the number of listeners whenever it changes."
  (let* ((l (if path
                (glass::open-listener :unix :path path :backlog 4)
                (glass::open-listener :tcp :port port :address address :backlog 4)))
         (out (%make-audio-out :listener l :lfd (glass::%listener-fd l) :inbox inbox
                               :mixer (glass:make-mixer) :notify notify)))
    ;; the frames the compositor mixed, as this thread's one source: the mixer is clockless, so
    ;; it pulls exactly once per frame received (AUDIO-OUT-TURN ticks it once per frame queued)
    (glass:mixer-add-source (audio-out-mixer out)
                            (lambda () (pop (audio-out-queue out)))
                            :name "wm")
    (format *error-output* "~&glass wm: audio (glass-audio/1) on ~a~%"
            (glass::listener-endpoint l))
    (force-output *error-output*)
    out))

(defun %audio-count-changed (out)
  (when (audio-out-notify out)
    (funcall (audio-out-notify out)
             (count-if #'ac-sink (audio-out-clients out)))))

(defun %audio-drop (out c)
  (ignore-errors (close (ac-stream c)))
  (when (ac-sink c) (ignore-errors (glass:sink-unsubscribe (ac-sink c))))
  (setf (audio-out-clients out) (remove c (audio-out-clients out)))
  (%audio-count-changed out))

(defun %audio-hello (out c)
  "Read what the listener has sent of its request line, without waiting; once it is complete
   (or the listener took too long to send one), subscribe it and send the header."
  (let ((s (ac-stream c)) (done nil))
    ;; LISTEN alone is not enough: it answers for the stream's buffer, and on modus an empty
    ;; buffer says NIL whatever the socket holds -- ask the descriptor too (as %PEER-INPUT-P)
    (loop while (and (not done)
                     (or (listen s) (and (ac-fd c) (sb-sys:wait-until-fd-usable (ac-fd c) :input 0))))
          do (let ((b (read-byte s nil nil)))
               (cond ((null b) (error "listener hung up"))
                     ((= b 10) (setf done t))
                     ((< (length (ac-line c)) 256) (vector-push-extend (code-char b) (ac-line c))))))
    (when (or done (> (get-internal-real-time) (ac-deadline c)))
      (let* ((req (glass::%parse-params (string-right-trim '(#\Return) (coerce (ac-line c) 'simple-string))))
             (m (audio-out-mixer out))
             (rate (or (getf req :rate) (glass::mixer-rate m)))
             (frame (or (getf req :frame) (max 1 (round (* rate (glass::mixer-period m))))))
             (gain (or (getf req :gain) 1d0)))
        (setf (ac-sink c) (glass:mixer-subscribe m :name (or (getf req :name) "listener")
                                                   :rate rate :frame-samples frame :gain gain :lead 2)
              (ac-octets c) (make-array (* 2 frame) :element-type '(unsigned-byte 8)))
        (glass::%write-ascii s (format nil "glass-audio/1 rate=~d channels=1 frame=~d format=s16le~%"
                                       rate frame))
        (force-output s)
        (%audio-count-changed out)))))

(defun %audio-send (c)
  "Every frame the listener's sink has, unless its socket is backed up (then they are dropped:
   the sink's cursor has moved on, so the listener hears a gap, never a rewind)."
  (let* ((sink (ac-sink c)) (octets (ac-octets c)) (s (ac-stream c))
         (limit (* *audio-backlog-frames* (length octets))) (wrote nil))
    (loop for f = (glass:sink-next-frame sink)
          while f
          do (if (and (ac-fd c) (> (glass::socket-unsent-bytes (ac-fd c)) limit))
                 (progn (incf (ac-drops c))
                        (unless (ac-stalled c) (setf (ac-stalled c) (get-internal-real-time))))
                 (progn (setf (ac-stalled c) nil)
                        (glass::%put-s16le f octets)
                        (write-sequence octets s)
                        (incf (ac-written c))
                        (setf wrote t))))
    (when wrote (force-output s))
    ;; a listener that has drained nothing for 30 s is gone
    (when (and (ac-stalled c)
               (> (- (get-internal-real-time) (ac-stalled c)) (* 30 internal-time-units-per-second)))
      (error "listener stopped draining"))))

(defun audio-out-turn (out)
  "One turn of the network thread's audio: new listeners, request lines, then the frames the
   compositor has sent, to everybody listening.  T if it did anything."
  (let ((busy nil))
    ;; new listeners
    (when (sb-sys:wait-until-fd-usable (audio-out-lfd out) :input 0)
      (let ((stream (ignore-errors (glass::accept-stream (audio-out-listener out)))))
        (when stream
          (setf busy t)
          (incf (audio-out-served out))
          (setf (audio-out-clients out)
                (append (audio-out-clients out)
                        (list (make-audio-client
                               :stream stream
                               :fd (ignore-errors (sb-sys:fd-stream-fd stream))
                               :deadline (+ (get-internal-real-time)
                                            (round (* *audio-hello-ms* internal-time-units-per-second)
                                                   1000)))))))))
    ;; request lines
    (dolist (c (audio-out-clients out))
      (unless (ac-sink c)
        (handler-case (%audio-hello out c)
          (error () (%audio-drop out c)))))
    ;; the compositor's frames, through this thread's mixer into every listener's sink
    (let ((frames (glass::mb-take (audio-out-inbox out))))
      (when frames
        (setf busy t)
        (dolist (f frames)
          (setf (audio-out-queue out) (list f))
          (glass:mixer-tick (audio-out-mixer out)))
        (setf (audio-out-queue out) '())
        (dolist (c (audio-out-clients out))
          (when (ac-sink c)
            (handler-case (%audio-send c)
              (error () (%audio-drop out c)))))))
    busy))

(defun audio-out-close (out)
  (dolist (c (audio-out-clients out)) (ignore-errors (close (ac-stream c))))
  (setf (audio-out-clients out) '())
  (ignore-errors (glass::close-listener (audio-out-listener out))))

(defun audio-out-report (out)
  (format nil "audio-out ~a served=~d listeners=(~{~a~^ ~})"
          (glass::listener-endpoint (audio-out-listener out)) (audio-out-served out)
          (mapcar (lambda (c)
                    (let ((s (ac-sink c)))
                      (if s
                          (format nil "~a@~d:wrote=~d drops=~d/~d under=~d" (glass::sink-name s)
                                  (glass::sink-rate s) (ac-written c) (ac-drops c)
                                  (glass::sink-drops s) (glass::sink-underruns s))
                          "(hello)")))
                  (audio-out-clients out))))
