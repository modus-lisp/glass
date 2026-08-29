;;;; src/mic-stream.lisp — a peer's microphone, over a socket, into the desktop's ear.
;;;;
;;;; audio-stream.lisp carries the session's mix OUT: the desktop listens, a gateway dials in, and
;;;; frames flow desktop -> gateway.  This is the other direction of the same relationship and not
;;;; a second design — the desktop listens, the gateway dials in, and frames flow gateway ->
;;;; desktop.  What arrives is one peer's microphone, decoded from G.711 by webrtc-media on the
;;;; gateway's receive path, which until now was measured for a level meter and thrown away.
;;;;
;;;; A SECOND PORT, NOT A SECOND DIRECTION ON 5913.  Both would work; this one is thinner.  A
;;;; connection on 5913 is a MIXER-SUBSCRIBE with a clock: it writes one frame per period forever
;;;; and its whole shape is "the consumer's read pace must not become the rate at which the session
;;;; is sampled".  A microphone has none of that — it has no clock here (the peer's is the clock),
;;;; it has no cursor, it never underruns into silence, and it does not belong to the mixer at all.
;;;; Folding it into the same connection would give that connection two rates, two framings, two
;;;; lifetimes and a reason to care about a direction it currently ignores, and it would force a
;;;; listener that only wants to HEAR the desktop — a recorder, a second box — to speak first.  A
;;;; transport is a thin thing that converts; two thin things are cheaper than one thick one, and
;;;; 5914 costs a line in a firewall that already has 5913 in it.
;;;;
;;;; THE MICROPHONE IS NOT IN THE MIX, and that is deliberate.  Putting the peer's voice on the
;;;; session mixer would play it out of the desktop's own audio and straight back down 5913 to the
;;;; peer that said it, which is an echo with a network round trip in it; it would also be heard by
;;;; every other listener, who did not dial in to hear somebody else's room.  So this carries the
;;;; microphone to whoever asked for it — today the ear (src/hearing.lisp) — and nowhere else.
;;;;
;;;; THE WIRE IS THE SAME WIRE, text-framed at the top so `nc` is a diagnostic:
;;;;
;;;;   client -> server (REQUIRED, one line):  glass-mic/1 rate=8000 frame=160 name=peer
;;;;   server -> client (one line):            glass-mic/1 rate=16000 frame=320 format=s16le ok
;;;;   client -> server (forever):             FRAME bytes of signed 16-bit little-endian mono
;;;;
;;;; The request is REQUIRED here where 5913's is optional, and for the reason hearing.lisp gives
;;;; about its own rate: a server cannot guess the rate of bytes being pushed at it, and audio fed
;;;; to a recognizer at the wrong rate produces confident nonsense rather than an error.  The reply
;;;; announces what the desktop converted TO, so a sender can see the conversion it caused.
;;;;
;;;; THE RATE IS CONVERTED HERE, BY REED, ONCE PER CONNECTION.  The microphone is 8 kHz because
;;;; G.711 is 8 kHz; the ear is 16 kHz because that is what the model's filterbank was built for.
;;;; That is exactly the conversion a SINK does per listener on the way out (audio.lisp: "each
;;;; consumer converts"), with the same reed:MAKE-RESAMPLER and the same streaming contract, so a
;;;; stream converted in 20 ms pieces is the stream converted whole.  Nothing here invents a
;;;; resampler and nothing here invents bandwidth: 8 kHz audio resampled to 16 is still 8 kHz
;;;; audio, and the recognizer is entitled to find a telephone harder to read than a microphone.
;;;;
;;;; ONE MICROPHONE AT A TIME, AND IT IS THE NEWEST.  Two peers talking into one ear is a mix, and
;;;; mixing is the mixer's job; there is no microphone mixer and inventing one here would put the
;;;; second design back.  So a new connection RETIRES the previous one, which is also what makes a
;;;; phone that dropped off and dialed back in work: the stale connection is closed rather than
;;;; left holding the ear.
;;;;
;;;; A GAP IS NOT A CLOCK.  5913 sends one frame per period including silence, because it is a wall
;;;; clock and a reader counts frames to know the time.  This stream is the opposite: it carries
;;;; what ARRIVED, and a gap means the peer's browser sent nothing.  So the reader has a cushion
;;;; and drops the oldest when the consumer falls behind (live audio, same judgement as everywhere
;;;; else here), and a connection that has been dry for *MIC-LIVE-SECONDS* stops being live —
;;;; which is how the ear knows to go back to what it was listening to before.

(in-package #:glass)

(defparameter *mic-stream-port* 5914
  "Default port for a peer's microphone arriving at the desktop.  One past the mix's 5913 by the
same convention that put 5913 one decade past the screen's 5903, and bound to loopback for the
same reason: what reaches this port is somebody's room, and a sender off the box should come
through something that authenticates.")

;;; ---- who is talking to this box --------------------------------------------

(defstruct (mic-stream (:constructor %make-mic-stream))
  (port 0 :type fixnum)
  (rate 16000 :type fixnum)
  (frame-samples 320 :type fixnum)
  (depth 8 :type fixnum)
  (prime 2 :type fixnum)
  socket
  thread
  (running nil)
  (lock (sb-thread:make-mutex :name "glass-mic-stream"))
  (current nil)                         ; the MIC of the newest connection
  (served 0 :type fixnum))

(defvar *session-mic-stream* nil
  "The microphone port the SESSION is serving, if any — the primary seat's.

There was one of these because there was one person.  With seats there is one PER SEAT, and they
do not merge: a microphone is the one thing here that is nobody else's business, and two people
in two rooms talking into two phones are two microphones and never a mix (mixing them is what
would put each one's voice into the other's ear over a network round trip).  This variable stays
the SESSION's — the primary seat's — because everything that asks without a seat to ask about
(the ear a one-seat desktop starts, a control socket, MIC-REPORT) means that one.")

;;; ---- the microphone itself lives in src/mic.lisp ---------------------------
;;;
;;; It used to be defined here, which made a socket the only way to have one: a local viewer
;;; has no connection to accept and nobody to reconnect to, so it would have had to open a
;;; wire to itself for the object to exist.  The object moved.  This file is now one PRODUCER
;;; of microphones rather than the definition of what a microphone is, and it says so by
;;; calling ATTACH-MIC like any other producer.

(defun %retire-mic (mic why)
  "Close a microphone's connection.  Its reader thread is blocked in a read and will not notice a
flag; closing the socket underneath it is what wakes it, and it exits on the error."
  (when mic
    (setf (mic-open mic) nil)
    (ignore-errors
     (format *error-output* "~&@@ mic: ~a retired (~a)~%" (mic-name mic) why)
     (finish-output *error-output*))
    (ignore-errors (sb-bsd-sockets:socket-close (mic-sock mic)))))

(defun %serve-mic-client (srv sock)
  "One connection: read the header, become the session's microphone, then convert what arrives
until the peer goes away.  Its own thread, so a sender that stalls costs exactly this thread."
  (let ((stream nil) (mic nil) (who (%audio-client-name sock)))
    (unwind-protect
         (handler-case
             (progn
               (ignore-errors (setf (sb-bsd-sockets:sockopt-tcp-nodelay sock) t))
               (setf stream (sb-bsd-sockets:socket-make-stream
                             sock :input t :output t :element-type '(unsigned-byte 8)
                             :buffering :full :timeout 30))
               ;; The header is required, so waiting for it is not a hang — but it is bounded, so a
               ;; port scanner that opens a socket and says nothing is not a thread held forever.
               (unless (%wait-readable stream 5)
                 (error "no glass-mic header within 5 s"))
               (let* ((req (%parse-params (or (%read-ascii-line stream) "")))
                      (wire-rate (or (getf req :rate)
                                     (error "glass-mic header carries no rate")))
                      (wire-frame (or (getf req :frame) (max 1 (round (* wire-rate 0.02d0)))))
                      (name (or (getf req :name) who))
                      (rate (mic-stream-rate srv))
                      (frame (mic-stream-frame-samples srv))
                      (octets (make-array (* 2 wire-frame) :element-type '(unsigned-byte 8))))
                 (setf mic (%make-mic :name name :wire-rate wire-rate :wire-frame wire-frame
                                      :rate rate :frame-samples frame :sock sock :open t
                                      :prime (mic-stream-prime srv)
                                      ;; STAMP stays 0 until a frame actually arrives: a socket
                                      ;; that connected and has said nothing is not a microphone
                                      ;; anyone should be listening to yet.
                                      :ring (make-array (mic-stream-depth srv) :initial-element nil)
                                      :resampler (unless (= wire-rate rate)
                                                   (reed:make-resampler wire-rate rate))))
                 ;; The newest microphone is the session's microphone.  ATTACH-MIC as well as
                 ;; the server's own slot: this file is one producer among others now, and what
                 ;; the SESSION has should not depend on which kind of producer attached it.
                 (let ((old (sb-thread:with-mutex ((mic-stream-lock srv))
                              (prog1 (mic-stream-current srv)
                                (setf (mic-stream-current srv) mic)
                                (incf (mic-stream-served srv))))))
                   (attach-mic mic)
                   (when (and old (not (eq old mic))) (%retire-mic old "replaced")))
                 (%write-ascii stream (format nil "glass-mic/1 rate=~d channels=1 frame=~d format=s16le ok~%"
                                              rate frame))
                 (force-output stream)
                 (ignore-errors
                  (format *error-output* "~&@@ mic: ~a connected — ~dHz/~d -> ~dHz/~d~%"
                          name wire-rate wire-frame rate frame)
                  (finish-output *error-output*))
                 (loop while (and (mic-stream-running srv) (mic-open mic))
                       do (let ((got (read-sequence octets stream)))
                            (when (< got (length octets)) (return))   ; end of stream
                            (%mic-push mic (%take-s16le octets wire-frame))))))
           (serious-condition () nil))       ; a peer hanging up is the normal way this ends
      (when mic
        (setf (mic-open mic) nil)
        ;; DETACH-MIC guards on identity, so a connection retiring after a newer one replaced it
        ;; cannot unhook the newer one — the same care the line below takes.
        (detach-mic mic)
        (sb-thread:with-mutex ((mic-stream-lock srv))
          (when (eq (mic-stream-current srv) mic) (setf (mic-stream-current srv) nil))))
      (ignore-errors (when stream (close stream)))
      (ignore-errors (sb-bsd-sockets:socket-close sock)))))

(defun stream-mic (&optional (srv *session-mic-stream*))
  "The microphone currently connected to THIS port, or NIL.  SESSION-MIC is this asked of the
session's; a seat's ear asks it of the seat's, which is what makes my microphone mine."
  (and srv (sb-thread:with-mutex ((mic-stream-lock srv)) (mic-stream-current srv))))

(defun start-mic-stream (&key (port *mic-stream-port*) (address "127.0.0.1")
                              path (peer-policy *peer-policy*)
                              (rate *mic-rate*) frame-samples (depth 16) (prime 4)
                              (install t))
  "Accept a peer's microphone on PORT and convert it to RATE for whoever asks.

INSTALL t (the default, and what a one-seat desktop wants) also makes this the SESSION's
microphone port — *SESSION-MIC-STREAM*, which is what SESSION-MIC and everything without a seat
to ask reads.  A further seat passes NIL: its microphone is its own, and installing it would
hand the session's ear somebody else's room.

PRIME is deliberately deeper than the audio tap's 2 in the other direction, because the consumer
here is a RECOGNIZER and not a loudspeaker.  A listener would rather lose 20 ms than hear it 80 ms
late; a recognizer would very much rather be 80 ms late than have a hole punched in the middle of
a word, and the audio on this side of the wire has already crossed a real network with a phone at
the far end of it.  DEPTH is the bound on that trade going wrong: past it, the oldest goes.

PATH receives the same microphone on a UNIX-domain socket instead of a port: the same header, the
same frames, and a door the kernel keeps (mode 0600) instead of one every process on the box can
walk through.  PEER-POLICY is who may connect (GLASS:*PEER-POLICY*) — a question only a UNIX
socket can answer, because only there is there a peer to ask about.  IT MATTERS MOST HERE: this
port is a MICROPHONE, and anything that can connect to it becomes what the desktop is listening
to.  Under TCP the qualification for that was `run something on this machine'.

Returns a MIC-STREAM and installs it as the session's; SESSION-MIC is then the microphone of
whoever is connected, or NIL.  Safe to run with nobody connected forever — that is the normal
state of the port, and it costs one thread asleep in accept."
  (let* ((frame (or frame-samples (max 1 (round (* rate 0.02d0)))))
         (listener (if path
                       (open-listener :unix :path path :backlog 2 :peer-policy peer-policy)
                       (open-listener :tcp :port port :address address :backlog 2)))
         (srv (%make-mic-stream :port (if path 0 port) :rate rate :frame-samples frame
                                :depth depth :prime prime
                                :socket listener
                                :running t)))
    (setf (mic-stream-thread srv)
          (sb-thread:make-thread
           (lambda ()
             (loop while (mic-stream-running srv) do
               (handler-case
                   (let ((sock (listener-accept (mic-stream-socket srv))))
                     (sb-thread:make-thread (lambda () (%serve-mic-client srv sock))
                                            :name "glass-mic-client"))
                 (serious-condition () (sleep 0.2)))))
           :name "glass-mic-stream"))
    (when install (setf *session-mic-stream* srv))
    srv))

(defun stop-mic-stream (&optional (srv *session-mic-stream*))
  (when srv
    (setf (mic-stream-running srv) nil)
    (detach-mic (mic-stream-current srv))
    (%retire-mic (mic-stream-current srv) "stopping")
    (sb-thread:with-mutex ((mic-stream-lock srv)) (setf (mic-stream-current srv) nil))
    ;; CLOSE-LISTENER, not SOCKET-CLOSE: a thread is parked in accept() on it, and a bare close
    ;; leaves the kernel accepting on a port this process no longer has a descriptor for.  See
    ;; GLASS:CLOSE-LISTENER — and for a UNIX listener it is also what unlinks the socket file.
    (ignore-errors (close-listener (mic-stream-socket srv)))
    (when (eq srv *session-mic-stream*) (setf *session-mic-stream* nil)))
  t)

(defun mic-stream-report (&optional (srv *session-mic-stream*))
  (if (null srv)
      "mic-stream: not running"
      (format nil "mic-stream ~a served=~d rate=~d frame=~d — ~a"
              (listener-endpoint (mic-stream-socket srv)) (mic-stream-served srv)
              (mic-stream-rate srv) (mic-stream-frame-samples srv)
              (mic-report (mic-stream-current srv)))))

(defun start-session-mic (&key (port *mic-stream-port*) (address "127.0.0.1") path
                               (rate *mic-rate*))
  "The one line a desktop startup script wants for the inbound half, and deliberately total for
the same reason START-SESSION-AUDIO is: a desktop with no microphone port is a working desktop,
and a desktop that did not start is not.  Returns the MIC-STREAM, or NIL (already reported)."
  (handler-case
      (let ((srv (start-mic-stream :port port :address address :path path :rate rate)))
        (format *error-output* "~&@@ mic stream on ~a — a peer's microphone, converted to ~d Hz~%"
                (endpoint-string :host address :port port :path path) rate)
        (finish-output *error-output*)
        srv)
    (serious-condition (e)
      (ignore-errors
       (format *error-output* "~&@@ mic stream unavailable: ~a~%" e)
       (finish-output *error-output*))
      nil)))

;;; ---- the sender: the gateway's end -----------------------------------------
;;;
;;; The other half of the same thin thing, and the mirror of MAKE-AUDIO-TAP.  Its one hard
;;; requirement comes from where it is called: webrtc-media's :ON-RX-PCM runs on the RECEIVE path,
;;; the thread that decrypts and decodes every packet that arrives, and anything that blocks there
;;; stalls the session's inbound media — audio and video both.  So MIC-SEND does a copy and a
;;; mutex and nothing else; a writer thread owns the socket, the connecting, the reconnecting, and
;;; every way any of that can go slow.

(defstruct (mic-sender (:constructor %make-mic-sender))
  (host "127.0.0.1" :type string)
  (port 0 :type fixnum)
  (rate 8000 :type fixnum)
  (frame-samples 160 :type fixnum)
  (name "peer" :type string)
  (depth 25 :type fixnum)               ; half a second of microphone; past that it is stale anyway
  (lock (sb-thread:make-mutex :name "glass-mic-sender"))
  (wake (sb-thread:make-semaphore :name "glass-mic-sender-wake"))
  (pending (reed:make-pcm16 0))         ; samples not yet a whole frame
  (fill 0 :type fixnum)
  (ring #() :type simple-vector)
  (head 0 :type fixnum) (tail 0 :type fixnum)
  (thread nil)
  (running nil)
  (connected nil)
  (offered 0 :type fixnum)              ; frames handed in by the receive path
  (sent 0 :type fixnum)
  (dropped 0 :type fixnum)              ; the desktop is not draining, or is not there
  (reconnects 0 :type fixnum)
  log)

(defun mic-send (sender pcm)
  "Hand SENDER a frame of microphone samples.  Returns immediately, always.

Re-frames as it goes, because the wire is fixed-size frames and a peer is entitled to change its
packetization mid-call; and DROPS THE OLDEST when the queue is full, which is the same judgement
the tap makes in the other direction — a listener that has fallen behind wants a gap and the truth
about it, not everything it missed delivered late.  For a microphone it is stronger than that:
audio half a second old is not worth a recognizer's time even if it arrives."
  (when (and sender pcm (plusp (length pcm)))
    (sb-thread:with-mutex ((mic-sender-lock sender))
      (let ((need (+ (mic-sender-fill sender) (length pcm))))
        (when (< (length (mic-sender-pending sender)) need)
          (let ((new (reed:make-pcm16 (max need (* 2 (length (mic-sender-pending sender)))))))
            (replace new (mic-sender-pending sender) :end2 (mic-sender-fill sender))
            (setf (mic-sender-pending sender) new)))
        (replace (mic-sender-pending sender) pcm :start1 (mic-sender-fill sender))
        (setf (mic-sender-fill sender) need))
      (let* ((n (mic-sender-frame-samples sender))
             (ring (mic-sender-ring sender))
             (cap (length ring)))
        (loop while (>= (mic-sender-fill sender) n)
              do (let ((frame (reed:make-pcm16 n)))
                   (replace frame (mic-sender-pending sender) :end2 n)
                   (replace (mic-sender-pending sender) (mic-sender-pending sender)
                            :start2 n :end2 (mic-sender-fill sender))
                   (decf (mic-sender-fill sender) n)
                   (incf (mic-sender-offered sender))
                   (when (= (- (mic-sender-tail sender) (mic-sender-head sender)) cap)
                     (incf (mic-sender-head sender))
                     (incf (mic-sender-dropped sender)))
                   (setf (svref ring (mod (mic-sender-tail sender) cap)) frame)
                   (incf (mic-sender-tail sender))))))
    (sb-thread:signal-semaphore (mic-sender-wake sender)))
  t)

(defun mic-feed (sender)
  "SENDER as a webrtc-media :ON-RX-PCM handler — (PCM RTP), the RTP header ignored.

A jitter buffer would want that header; there is no jitter buffer here and one packet late is one
frame late, which for a recognizer with a level gate in front of it is a click and not a sentence."
  (lambda (pcm rtp) (declare (ignore rtp)) (mic-send sender pcm)))

(defun %mic-sender-take (sender)
  "Every whole frame queued, oldest first, in one go."
  (sb-thread:with-mutex ((mic-sender-lock sender))
    (let* ((ring (mic-sender-ring sender)) (cap (length ring)) (out '()))
      (loop while (< (mic-sender-head sender) (mic-sender-tail sender))
            do (push (svref ring (mod (mic-sender-head sender) cap)) out)
               (incf (mic-sender-head sender)))
      (nreverse out))))

(defun %sender-where (sender)
  "Where this sender is pointed, as one phrase — a path or a host:port."
  (endpoint-string :host (mic-sender-host sender) :port (mic-sender-port sender)))

(defun %mic-sender-say (sender fmt &rest args)
  (let ((log (mic-sender-log sender)))
    (when log (ignore-errors (funcall log (apply #'format nil fmt args))))))

(defun %mic-sender-session (sender)
  "One connection, from header to end of stream.  Signals on any failure; the caller reconnects."
  (let ((sock nil)
        (stream nil))
    (unwind-protect
         (progn
           ;; HOST carries the endpoint, both kinds — "127.0.0.1" beside a port, or
           ;; "unix:/…/mic.sock" (or a bare absolute path) for a socket file.  Same one string as
           ;; the tap's, for the same reason: the gateway configures this from one env var.
           (multiple-value-setq (sock stream)
             (open-connection :host (mic-sender-host sender) :port (mic-sender-port sender)))
           (%write-ascii stream (format nil "glass-mic/1 rate=~d frame=~d name=~a~%"
                                        (mic-sender-rate sender) (mic-sender-frame-samples sender)
                                        (mic-sender-name sender)))
           (force-output stream)
           (let ((hdr (or (%read-ascii-line stream) (error "no header"))))
             (setf (mic-sender-connected sender) t)
             (%mic-sender-say sender "microphone -> ~a — ~a" (%sender-where sender) hdr))
           (let* ((fd (sb-bsd-sockets:socket-file-descriptor sock))
                  (bytes (* 2 (mic-sender-frame-samples sender)))
                  (octets (make-array bytes :element-type '(unsigned-byte 8)))
                  ;; Half a second unsent is a desktop that is not reading.  Below that, TCP is
                  ;; being TCP and the frames still arrive while they are worth having.
                  (backlog-limit (* 25 bytes)))
             (loop while (mic-sender-running sender) do
               (sb-thread:wait-on-semaphore (mic-sender-wake sender) :timeout 0.5)
               (let ((backed-up (> (socket-unsent-bytes fd) backlog-limit)))
                 (dolist (frame (%mic-sender-take sender))
                   (cond
                     ;; Do not write into a socket the peer is not draining: this thread would
                     ;; block, the ring behind it would fill, and MIC-SEND would go on dropping —
                     ;; which is the right outcome, reached the expensive way.  Dropping here
                     ;; reaches it without a thread stuck in a write for a minute.
                     (backed-up (incf (mic-sender-dropped sender)))
                     (t (%put-s16le frame octets)
                        (write-sequence octets stream)
                        (incf (mic-sender-sent sender)))))
                 (unless backed-up (force-output stream))))))
      (setf (mic-sender-connected sender) nil)
      (ignore-errors (when stream (close stream)))
      (ignore-errors (sb-bsd-sockets:socket-close sock)))))

(defun %mic-sender-loop (sender)
  "Connect, push, and reconnect for as long as the sender is running.

Quietly, for the reason the tap's loop is quiet: a desktop that is not up yet, or is restarting,
is the ordinary case, and a reconnect loop that logs every attempt turns a missing feature into a
screenful a minute."
  (let ((complained nil))
    (loop while (mic-sender-running sender) do
      (handler-case (progn (%mic-sender-session sender)
                           (when (mic-sender-running sender)
                             (%mic-sender-say sender "microphone stream ended — reconnecting")
                             (setf complained nil)
                             (incf (mic-sender-reconnects sender))))
        (serious-condition (e)
          (unless complained
            (setf complained t)
            (%mic-sender-say sender "no ear at ~a (~a) — the microphone goes nowhere"
                             (%sender-where sender) e))
          (incf (mic-sender-reconnects sender))))
      ;; whatever queued up while there was nowhere to send it is stale by now
      (when (mic-sender-running sender)
        (%mic-sender-take sender)
        (sleep 1)))))

(defun make-mic-sender (&key (host "127.0.0.1") (port *mic-stream-port*) (rate 8000)
                             (frame-samples 160) (name "peer") (depth 25) log)
  "Push a peer's microphone to a glass desktop, without ever blocking the caller.

Made once per session by whatever holds the peer — the WebRTC gateway does, on the connection it
already has — and fed by MIC-SEND, or by MIC-FEED as webrtc-media's :ON-RX-PCM.  Starts its writer
thread immediately and keeps it, reconnecting if the desktop restarts.  With nothing listening on
PORT this is not an error: the frames are dropped, the receive path is untouched, and the moment
the desktop comes up the microphone starts arriving."
  (let ((sender (%make-mic-sender :host host :port port :rate rate :frame-samples frame-samples
                                  :name name :depth depth :running t :log log
                                  :ring (make-array depth :initial-element nil))))
    (setf (mic-sender-thread sender)
          (sb-thread:make-thread (lambda () (%mic-sender-loop sender)) :name "glass-mic-sender"))
    sender))

(defun mic-sender-stop (sender)
  (when sender
    (setf (mic-sender-running sender) nil)
    (sb-thread:signal-semaphore (mic-sender-wake sender)))
  sender)

(defun mic-sender-report (sender)
  (if (null sender)
      "mic-sender: none"
      (format nil "mic-sender ~a ~a ~a@~dHz in=~d sent=~d -~d reconn=~d"
              (%sender-where sender)
              (if (mic-sender-connected sender) "up" "down")
              (mic-sender-name sender) (mic-sender-rate sender)
              (mic-sender-offered sender) (mic-sender-sent sender)
              (mic-sender-dropped sender) (mic-sender-reconnects sender))))
