;;;; src/audio-stream.lisp — the session's mix, over a socket, to a listener in another process.
;;;;
;;;; audio.lisp says a transport is a thin thing that converts.  This is one: it does not decide
;;;; what the session sounds like, it does not resample (the SINK does, per listener), and it does
;;;; not buffer anybody's audio but its own connection's.  It exists because the desktop and the
;;;; things that want to hear it are SEPARATE PROCESSES — the glass desktop runs the mixer, and a
;;;; WebRTC gateway, a recorder or a second box cannot call MIXER-SUBSCRIBE across a process
;;;; boundary.  So: one connection, one MIXER-SUBSCRIBE, frames on the wire.
;;;;
;;;; A DEDICATED SOCKET, not the RFB stream.  RFB carries a QEMU audio pseudo-encoding, and
;;;; teaching glass's server to speak it would let a plain VNC client hear the desktop with no
;;;; extra port — that is the right follow-on, and it is a strictly bigger change (a new encoding
;;;; negotiated inside an existing client's framebuffer session).  A listener that is not a VNC
;;;; client at all, which is the case that exists today, needs none of it.
;;;;
;;;; THE PROTOCOL IS SELF-DESCRIBING AND TEXT-FRAMED AT THE TOP, so `nc localhost 5913` is a
;;;; diagnostic and not a hex dump:
;;;;
;;;;   client -> server (optional, one line):  glass-audio/1 rate=8000 frame=160 gain=1.0
;;;;   server -> client (always, one line):    glass-audio/1 rate=8000 channels=1 frame=160 format=s16le
;;;;   server -> client (forever):             FRAME bytes of signed 16-bit little-endian mono
;;;;
;;;; The request is optional because a listener that just wants "whatever the box sounds like"
;;;; should not have to know the box's rate to ask; omitting it gets the native mix.  The reply
;;;; header is NOT optional, because a reader that assumes a rate is a reader that plays the
;;;; desktop back at the wrong speed the first time the mix rate changes.
;;;;
;;;; ONE FRAME PER FRAME-PERIOD, INCLUDING SILENCE.  The stream is a wall-clock timeline, not a
;;;; sequence of interesting events: a reader that has 50 frames has one second of audio, and the
;;;; sink underrunning is a silent frame rather than a pause in the byte stream, which a reader
;;;; would have no way to tell from the network stalling.
;;;;
;;;; A SLOW READER LOSES AUDIO, IT DOES NOT SLOW THE MIX.  Two independent guards: the sink drops
;;;; frames it never collected (audio.lisp's invariant, inherited unchanged, including the
;;;; one-frame-at-a-time drift correction), and this file refuses to WRITE into a socket whose
;;;; send queue the peer is not draining.  Blocking in a write would hold this connection's thread,
;;;; and a connection thread that blocks for a minute is a connection that then delivers a minute
;;;; of stale audio in a burst.  Dropping is what live audio wants.

(in-package #:glass)

(defparameter *audio-stream-port* 5913
  "Default port for the session's audio stream.  Next to the VNC port by convention (5903 ->
5913), and bound to loopback by default: the mix is a session's private sound, and a listener
off the box should come through something that authenticates.")

(defparameter *audio-port-offset* 10
  "How far a seat's audio port sits from its RFB screen port.

It is the convention that already existed — the desktop on 5903 serves its mix on 5913 — read as
arithmetic instead of as a number typed into a startup script, so that a SECOND seat's ports
follow from the one thing a seat already has: the port its screen is on.  The live WebRTC
gateway finds 5913 from GLASS_AUDIO_PORT with exactly that default, so the primary seat's
numbers do not move and nothing outside has to learn anything.

The pair is +10/+11, so two seats want their screen ports at least two apart; the desktop-side
convention is one decade apart (5903 -> 5913/5914, 5923 -> 5933/5934), which also keeps the
audio ports of one seat away from the screen port of another.")

(defparameter *mic-port-offset* 11
  "How far a seat's microphone port sits from its RFB screen port (5903 -> 5914).")

(defun seat-audio-port (rfb-port) (+ rfb-port *audio-port-offset*))
(defun seat-mic-port   (rfb-port) (+ rfb-port *mic-port-offset*))

;;; ---- wire format -----------------------------------------------------------

(defun %put-s16le (pcm octets)
  "PCM samples into OCTETS as signed 16-bit little-endian.  OCTETS must hold 2 bytes per sample."
  (dotimes (i (length pcm) octets)
    (let ((v (ldb (byte 16 0) (aref pcm i))))
      (setf (aref octets (* 2 i)) (ldb (byte 8 0) v)
            (aref octets (1+ (* 2 i))) (ldb (byte 8 8) v)))))

(defun %take-s16le (octets n)
  "N samples out of OCTETS, as a fresh PCM vector."
  (let ((pcm (reed:make-pcm16 n)))
    (dotimes (i n pcm)
      (let ((v (logior (aref octets (* 2 i)) (ash (aref octets (1+ (* 2 i))) 8))))
        (setf (aref pcm i) (if (>= v 32768) (- v 65536) v))))))

(defun %write-ascii (stream string)
  (loop for ch across string do (write-byte (char-code ch) stream)))

(defun %read-ascii-line (stream &key (limit 256))
  "One newline-terminated ASCII line, or NIL at end of stream.  LIMIT caps a peer that opens a
connection and then sends an unbounded line without ever ending it."
  (let ((out (make-string-output-stream)))
    (loop repeat limit
          for b = (read-byte stream nil nil)
          do (cond ((null b) (return-from %read-ascii-line
                               (let ((s (get-output-stream-string out)))
                                 (and (plusp (length s)) s))))
                   ((= b 10) (return-from %read-ascii-line (get-output-stream-string out)))
                   ((/= b 13) (write-char (code-char b) out))))
    (get-output-stream-string out)))

(defun %parse-params (line)
  "\"glass-audio/1 rate=8000 frame=160\" -> (:rate 8000 :frame 160).  Unknown keys are ignored
rather than refused: a newer client asking for something this server has not heard of should get
audio, not a disconnect."
  (let ((out '()) (start 0) (n (length line)))
    (flet ((token (s e)
             (let ((eq (position #\= line :start s :end e)))
               (when eq
                 (let ((k (string-downcase (subseq line s eq)))
                       (v (subseq line (1+ eq) e)))
                   (cond ((string= k "rate") (setf (getf out :rate) (ignore-errors (parse-integer v))))
                         ((string= k "frame") (setf (getf out :frame) (ignore-errors (parse-integer v))))
                         ((string= k "gain") (setf (getf out :gain) (ignore-errors (let ((*read-eval* nil))
                                                                                    (float (read-from-string v) 1d0)))))
                         ((string= k "name") (setf (getf out :name) v))))))))
      (loop for i = (position #\Space line :start start)
            do (token start (or i n))
               (if i (setf start (1+ i)) (return))))
    out))

;;; ---- the server ------------------------------------------------------------

(defstruct (audio-stream (:constructor %make-audio-stream))
  mix                               ; the composite this port serves: the session's, or a seat's
  (port 0 :type fixnum)
  socket
  thread
  (running nil)
  (lead 2 :type fixnum)
  (lock (sb-thread:make-mutex :name "glass-audio-stream"))
  (clients '())
  (served 0 :type fixnum))

(defun audio-stream-mixer (srv)
  "The bus behind the mix this port serves.  Kept under its old name because that is what a
caller asking wants — the session's rate and clock — and a seat's mix is on the same one."
  (mix-bus (audio-stream-mix srv)))

(defun %audio-client-name (sock)
  "Who connected, for the log and for the sink's name.

On a UNIX socket there IS no peername (it is an unnamed autobind address), and PEER-NAME
answers with the thing that is both true and more useful than an address ever was — the
peer's uid and pid, from SO_PEERCRED."
  (peer-name sock))

(defun %serve-audio-client (srv sock)
  "One connection: subscribe a private sink, announce it, then a frame per period until the peer
goes away.  Runs on its own thread, so a peer that stops reading costs exactly this thread."
  (let* ((mix (audio-stream-mix srv))
         (m (mix-bus mix))
         (stream nil) (sink nil) (who (%audio-client-name sock)))
    (unwind-protect
         (handler-case
             (progn
               (ignore-errors (setf (sb-bsd-sockets:sockopt-tcp-nodelay sock) t))
               (setf stream (sb-bsd-sockets:socket-make-stream
                             sock :input t :output t :element-type '(unsigned-byte 8)
                             :buffering :full :timeout 30))
               ;; The request is optional: wait briefly for one, then serve the native mix.  A
               ;; long wait here would make a listener that never speaks look like a hung server.
               (let* ((req (when (%wait-readable stream 0.25) (%parse-params (or (%read-ascii-line stream) ""))))
                      (rate (or (getf req :rate) (mixer-rate m)))
                      (frame (or (getf req :frame) (max 1 (round (* rate (mixer-period m))))))
                      (gain (or (getf req :gain) 1.0d0))
                      (name (or (getf req :name) who))
                      (period (/ frame (float rate 1d0)))
                      (bytes (* 2 frame))
                      (octets (make-array bytes :element-type '(unsigned-byte 8)))
                      (silence (reed:make-pcm16 frame)))
                 (setf sink (mixer-subscribe mix :name name :rate rate :frame-samples frame
                                                 :gain gain :lead (audio-stream-lead srv)))
                 (sb-thread:with-mutex ((audio-stream-lock srv))
                   (push sink (audio-stream-clients srv))
                   (incf (audio-stream-served srv)))
                 (%write-ascii stream (format nil "glass-audio/1 rate=~d channels=1 frame=~d format=s16le~%"
                                              rate frame))
                 (force-output stream)
                 (%stream-frames srv stream sock sink silence octets period)))
           (serious-condition () nil))          ; a peer hanging up is the normal way this ends
      (when sink
        (sb-thread:with-mutex ((audio-stream-lock srv))
          (setf (audio-stream-clients srv) (remove sink (audio-stream-clients srv))))
        (ignore-errors (sink-unsubscribe sink)))
      (ignore-errors (when stream (close stream)))
      (ignore-errors (sb-bsd-sockets:socket-close sock)))))

(defun %wait-readable (stream secs)
  (let ((deadline (+ (get-internal-real-time) (round (* secs internal-time-units-per-second)))))
    (loop (when (listen stream) (return t))
          (when (> (get-internal-real-time) deadline) (return nil))
          (sleep 0.005))))

(defun %stream-frames (srv stream sock sink silence octets period)
  "The connection's clock.  Its own deadline, like the mixer's and for the same reason: the
consumer's read pace must not become the rate at which the session is sampled."
  (let* ((fd (sb-bsd-sockets:socket-file-descriptor sock))
         ;; A peer is 'not draining' when it leaves more than half a second of audio unsent.  Below
         ;; that, TCP is just being TCP and the frames still arrive in time to be worth hearing.
         (backlog-limit (* 25 (length octets)))
         (units (float internal-time-units-per-second 1d0))
         (tick (max 1 (round (* period internal-time-units-per-second))))
         (next (+ (get-internal-real-time) tick))
         (stalled-since nil))
    (loop while (and (audio-stream-running srv) (open-stream-p stream)) do
      (let ((frame (or (sink-next-frame sink) silence))
            (backed-up (> (socket-unsent-bytes fd) backlog-limit)))
        (cond
          (backed-up
           ;; Drop it.  The sink's cursor has already advanced past this frame, so what the peer
           ;; missed is a gap and not a rewind — which is what a listener that fell behind should
           ;; get.  A peer that never drains at all is eventually hung up on, or this thread would
           ;; live as long as the process.
           (unless stalled-since (setf stalled-since (get-internal-real-time)))
           (when (> (- (get-internal-real-time) stalled-since) (* 30 internal-time-units-per-second))
             (return)))
          (t
           (setf stalled-since nil)
           (%put-s16le frame octets)
           (write-sequence octets stream)
           (force-output stream))))
      (let ((now (get-internal-real-time)))
        (when (> next now) (sleep (/ (- next now) units)))
        ;; Late is late: firing the backlog now would hand the peer a burst, which is the jitter
        ;; its buffer exists to absorb.  Same judgement as the mixer's clock.
        (setf next (if (< (+ next tick) now) (+ now tick) (+ next tick)))))))

(defun start-audio-stream (&key mixer mix (port *audio-stream-port*) (address "127.0.0.1")
                                path (peer-policy *peer-policy*) (lead 2))
  "Serve a mix on PORT: one MIXER-SUBSCRIBE per connection, 20 ms frames, forever.

MIX is the composite to serve — one SEAT's, when a seat has one of its own.  MIXER (or neither)
means the session's own mix, which is what a one-seat desktop has and what every caller written
before there were seats meant.

LEAD is each connection's cushion in the sink, in frames — the jitter absorbed between the
mixer's clock and this connection's before a reader ever sees a gap.  It is latency you are
choosing to spend; 2 frames (40 ms) is enough for two clocks on one box, and a reader that adds
its own queue on the far side should not need more.

PATH serves the same mix on a UNIX-domain socket instead of a port: the same header, the same
frames, the same protocol — `nc -U <path>' where it was `nc localhost 5913' — and access control
the kernel enforces (mode 0600) rather than `you must already be on this box', which is what a
loopback PORT actually means.  PEER-POLICY is who may connect (see GLASS:*PEER-POLICY*); it is meaningless
on TCP, where there is nobody to ask about.

Returns an AUDIO-STREAM; STOP-AUDIO-STREAM closes it.  Safe to call with no listeners and safe
to leave running with none — the mix advances regardless, which is the point of it having its
own clock."
  (let* ((target (as-mix (or mix mixer (session-mixer))))
         (listener (if path
                       (open-listener :unix :path path :backlog 4 :peer-policy peer-policy)
                       (open-listener :tcp :port port :address address :backlog 4)))
         (srv (%make-audio-stream :mix target :port (if path 0 port) :lead lead
                                  :socket listener
                                  :running t)))
    (setf (audio-stream-thread srv)
          (sb-thread:make-thread
           (lambda ()
             (loop while (audio-stream-running srv) do
               (handler-case
                   (let ((sock (listener-accept (audio-stream-socket srv))))
                     (sb-thread:make-thread (lambda () (%serve-audio-client srv sock))
                                            :name "glass-audio-client"))
                 (serious-condition () (sleep 0.2)))))
           :name "glass-audio-stream"))
    srv))

(defun stop-audio-stream (srv)
  (setf (audio-stream-running srv) nil)
  ;; CLOSE-LISTENER, not SOCKET-CLOSE: a thread is parked in accept() on this socket, and a bare
  ;; close leaves the kernel listening on the port (or the socket file in place).  This has always
  ;; been the wrong call here; it became visible when the listener became an object that knows what
  ;; closing means.  See GLASS:CLOSE-LISTENER.
  (ignore-errors (close-listener (audio-stream-socket srv)))
  ;; A stopped stream is not there to be adopted: leaving it in *SESSION-AUDIO-STREAM* would
  ;; hand the next headset a closed socket to call the session's sound.  (STOP-MIC-STREAM has
  ;; always done the same for its half.)
  (when (eq srv *session-audio-stream*) (setf *session-audio-stream* nil))
  srv)

(defun audio-stream-report (srv)
  (sb-thread:with-mutex ((audio-stream-lock srv))
    (format nil "audio-stream ~a [~a] served=~d listening=~a clients=(~{~a~^ ~})"
            (listener-endpoint (audio-stream-socket srv)) (mix-name (audio-stream-mix srv))
            (audio-stream-served srv) (audio-stream-running srv)
            (mapcar (lambda (s) (format nil "~a@~d:~d/-~d/u~d" (sink-name s) (sink-rate s)
                                        (sink-frames s) (sink-drops s) (sink-underruns s)))
                    (audio-stream-clients srv)))))

(defvar *session-audio-stream* nil
  "The port this image is serving the SESSION's mix on, if any — the primary seat's.

Kept so that a seat asking for audio it already has does not open a second listener on a port
that is already answering: the desktop's startup script starts this one directly (and the live
WebRTC gateway is connected to it), so a headset for the primary seat ADOPTS it rather than
racing it.  A further seat's stream is its own and is not this.")

(defun start-session-audio (&key (port *audio-stream-port*) (address "127.0.0.1") path (lead 2)
                                 (file (sb-ext:posix-getenv "GLASS_AUDIO_MP3")) (gain 0.6d0) (loop t))
  "Everything a desktop needs to have a sound: the session mixer, started, serving on PORT.

This is the one line a desktop startup script wants, and it is deliberately total — a mixer that
fails to start must not take the desktop down with it, because a desktop with no audio is a
working desktop and a desktop that did not start is not.  Returns the AUDIO-STREAM, or NIL if
audio could not come up (already reported, once).

FILE (default: the GLASS_AUDIO_MP3 environment variable) is decoded by reed and registered as a
looping source, so there is something in the mix for a listener that dials in at any moment.
Without it the mix is silence — which is the honest state of a desktop whose applications do not
yet make any noise, and the stream still runs."
  (handler-case
      (let ((m (session-mixer)))
        (when (and file (plusp (length file)))
          (handler-case
              (multiple-value-bind (src secs) (mixer-add-file m file :loop loop :gain gain :name "music")
                (declare (ignore src))
                (format *error-output* "~&@@ audio: ~a (~,1f s~:[~; looping~], gain ~,2f)~%"
                        file secs loop gain))
            (serious-condition (e)
              (format *error-output* "~&@@ audio: ~a not usable (~a) — the mix is silence~%" file e))))
        (let ((srv (start-audio-stream :mixer m :port port :address address :path path :lead lead)))
          (setf *session-audio-stream* srv)
          (format *error-output* "~&@@ audio stream on ~a — ~a~%"
                  (endpoint-string :host address :port port :path path) (mixer-report m))
          (finish-output *error-output*)
          srv))
    (serious-condition (e)
      (ignore-errors
       (format *error-output* "~&@@ audio unavailable: ~a~%" e)
       (finish-output *error-output*))
      nil)))

;;; ---- the listener's end ----------------------------------------------------
;;;
;;; The other half of the same thin thing.  A consumer of live audio almost always has a clock of
;;; its own that must not block (an RTP sender's, a device's), so the shape is fixed: a reader
;;; thread that may block on the socket, a bounded queue, and a NON-BLOCKING pull that returns NIL
;;; when there is nothing — which is reed's source contract, and webrtc-media's :source, unchanged.

(defstruct (audio-tap (:constructor %make-audio-tap))
  (host "127.0.0.1" :type string)
  (port 0 :type fixnum)
  (rate 8000 :type fixnum)
  (frame-samples 160 :type fixnum)
  (name "tap" :type string)
  (gain 1.0d0 :type double-float)
  (depth 8 :type fixnum)
  (prime 2 :type fixnum)
  (lock (sb-thread:make-mutex :name "glass-audio-tap"))
  (ring #() :type simple-vector)
  (head 0 :type fixnum) (tail 0 :type fixnum)
  (primed nil)
  (connected nil)
  (thread nil)
  (running nil)
  (frames 0 :type fixnum)          ; frames handed to the consumer
  (received 0 :type fixnum)
  (drops 0 :type fixnum)           ; arrived while the queue was full — the consumer is behind
  (underruns 0 :type fixnum)       ; asked and had nothing — the source is behind, or absent
  (reconnects 0 :type fixnum)
  log)

(defun %tap-push (tap frame)
  (sb-thread:with-mutex ((audio-tap-lock tap))
    (let* ((ring (audio-tap-ring tap)) (cap (length ring)))
      (incf (audio-tap-received tap))
      ;; A consumer slower than the stream would otherwise ride at the queue's full depth
      ;; forever: it hears everything, permanently DEPTH frames late, which for live audio is
      ;; worse than a gap.  So trim toward the cushion — and by ONE FRAME, never by a lump: two
      ;; clocks that are both 20 ms drift, this fires occasionally forever, and 20 ms lost now
      ;; and then is a listener's better deal than 100 ms lost at once.  Same judgement, and the
      ;; same arithmetic, as the sink's own trim in audio.lisp; a cushion of PRIME + 2 is what
      ;; is tolerated before correcting.
      (let ((avail (- (audio-tap-tail tap) (audio-tap-head tap))))
        (when (or (= avail cap) (> avail (+ (audio-tap-prime tap) 2)))
          (incf (audio-tap-head tap))
          (incf (audio-tap-drops tap))))
      (setf (svref ring (mod (audio-tap-tail tap) cap)) frame)
      (incf (audio-tap-tail tap)))))

(defun tap-next-frame (tap)
  "The next frame, or NIL if none has arrived yet.  NEVER BLOCKS — this is what runs on a
sender's thread, and a sender that waits here stops being a clock.  NIL means send silence and
keep your own timing; it is not an error, and with no desktop reachable at all it is simply what
this returns forever."
  (sb-thread:with-mutex ((audio-tap-lock tap))
    (let ((avail (- (audio-tap-tail tap) (audio-tap-head tap))))
      (cond
        ;; Prime before the first hand-out, and again after running dry: handing out the instant
        ;; one frame exists means underrunning on the next packet of jitter, forever.
        ((or (zerop avail) (and (not (audio-tap-primed tap)) (< avail (audio-tap-prime tap))))
         (setf (audio-tap-primed tap) nil)
         (incf (audio-tap-underruns tap))
         nil)
        (t (setf (audio-tap-primed tap) t)
           (let ((f (svref (audio-tap-ring tap) (mod (audio-tap-head tap) (length (audio-tap-ring tap))))))
             (incf (audio-tap-head tap))
             (incf (audio-tap-frames tap))
             f))))))

(defun tap-source (tap)
  "The tap as a bare source thunk — reed's contract, so it drops straight into webrtc-media's
:source with no adapter."
  (lambda () (tap-next-frame tap)))

(defun %tap-where (tap)
  "Where this tap is pointed, as one phrase — a path or a host:port."
  (endpoint-string :host (audio-tap-host tap) :port (audio-tap-port tap)))

(defun %tap-say (tap fmt &rest args)
  (let ((log (audio-tap-log tap)))
    (when log (ignore-errors (funcall log (apply #'format nil fmt args))))))

(defun %tap-session (tap)
  "One connection, from request to end of stream.  Signals on any failure; the caller reconnects."
  (let ((sock nil)
        (stream nil))
    (unwind-protect
         (progn
           ;; HOST carries the endpoint, both kinds: "127.0.0.1" is a hostname beside a port, and
           ;; "unix:/…/audio.sock" (or a bare absolute path) is a socket file.  ONE STRING, because
           ;; a caller's configuration is one string — an env var, a flag, a slot — and a second
           ;; variable to keep in step with the first is a second thing to get wrong.  No value
           ;; anybody already has starts with `/' or `unix:', so nothing already written changes.
           (multiple-value-setq (sock stream)
             (open-connection :host (audio-tap-host tap) :port (audio-tap-port tap)))
           (%write-ascii stream (format nil "glass-audio/1 rate=~d frame=~d gain=~,3f name=~a~%"
                                        (audio-tap-rate tap) (audio-tap-frame-samples tap)
                                        (audio-tap-gain tap) (audio-tap-name tap)))
           (force-output stream)
           (let* ((hdr (or (%read-ascii-line stream) (error "no header")))
                  (p (%parse-params hdr))
                  ;; The HEADER is authoritative, not what we asked for: a server that serves a
                  ;; different rate than requested must not be read at the rate we hoped for.
                  (rate (or (getf p :rate) (audio-tap-rate tap)))
                  (frame (or (getf p :frame) (audio-tap-frame-samples tap)))
                  (octets (make-array (* 2 frame) :element-type '(unsigned-byte 8))))
             (setf (audio-tap-rate tap) rate
                   (audio-tap-frame-samples tap) frame
                   (audio-tap-connected tap) t)
             (%tap-say tap "connected to ~a — ~a" (%tap-where tap) hdr)
             (loop while (audio-tap-running tap) do
               (let ((got (read-sequence octets stream)))
                 (when (< got (length octets)) (return))         ; end of stream
                 (%tap-push tap (%take-s16le octets frame))))))
      (setf (audio-tap-connected tap) nil)
      (ignore-errors (when stream (close stream)))
      (ignore-errors (sb-bsd-sockets:socket-close sock)))))

(defun %tap-loop (tap)
  "Connect, stream, and reconnect for as long as the tap is running.

Quietly: a desktop that is not up yet, or is being restarted, is the ordinary case, and a
reconnect loop that logs every attempt turns a missing feature into a screenful a minute.  The
first failure is reported and then nothing until something changes."
  (let ((complained nil))
    (loop while (audio-tap-running tap) do
      (handler-case (progn (%tap-session tap)
                           (when (audio-tap-running tap)
                             (%tap-say tap "stream ended — reconnecting")
                             (setf complained nil)
                             (incf (audio-tap-reconnects tap))))
        (serious-condition (e)
          (unless complained
            (setf complained t)
            (%tap-say tap "no audio from ~a (~a) — silence until it answers"
                      (%tap-where tap) e))
          (incf (audio-tap-reconnects tap))))
      (when (audio-tap-running tap) (sleep 1)))))

(defun make-audio-tap (&key (host "127.0.0.1") (port *audio-stream-port*) (rate 8000)
                            (frame-samples 160) (name "tap") (gain 1.0d0) (depth 8) (prime 2) log)
  "Listen to another process's session mix and hand it out a frame at a time, without blocking.

DEPTH bounds the queue in frames, and therefore bounds latency: a consumer slower than the
stream loses the oldest frames instead of drifting further behind forever.  PRIME frames are
collected before the first hand-out — the far side's sink has a cushion of its own for the
mixer's clock, and this is the cushion for the network between us.

Starts a reader thread immediately and keeps it, reconnecting if the desktop restarts.  With
nothing listening on PORT this is not an error: TAP-NEXT-FRAME returns NIL, a sender sends
silence, and the moment the desktop comes up audio starts."
  (let ((tap (%make-audio-tap :host host :port port :rate rate :frame-samples frame-samples
                              :name name :gain (float gain 1d0) :depth depth :prime prime
                              :ring (make-array depth :initial-element nil)
                              :running t :log log)))
    (setf (audio-tap-thread tap)
          (sb-thread:make-thread (lambda () (%tap-loop tap)) :name "glass-audio-tap"))
    tap))

(defun tap-stop (tap)
  (setf (audio-tap-running tap) nil)
  tap)

(defun tap-report (tap)
  (format nil "audio-tap ~a ~a@~dHz rx=~d out=~d -~d u~d reconn=~d"
          (%tap-where tap)
          (if (audio-tap-connected tap) "up" "down")
          (audio-tap-rate tap) (audio-tap-received tap) (audio-tap-frames tap)
          (audio-tap-drops tap) (audio-tap-underruns tap) (audio-tap-reconnects tap)))
