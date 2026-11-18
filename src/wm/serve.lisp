;;;; src/wm/serve.lisp — the window manager as a served desktop: SERVE-WM, and its seats.
;;;;
;;;; THREE THREADS, EACH OWNING ITS OWN, as SERVE-DESK's two (src/desk-serve.lisp) plus one:
;;;;   NETWORK     SERVE-ENDPOINTS, every viewer of every seat.
;;;;   COMPOSITOR  the window manager, every window, every app and every seat.
;;;;   MAIN        the image's Lisp listener (glass-repl:RUN-LISTENER): what a Lisp window
;;;;               evaluates on.  Main, because on modus only main's objects are everybody's,
;;;;               and a DEFUN typed into a REPL is a store into the image everybody shares --
;;;;               refused from any other thread.  It also means a runaway form stops the
;;;;               listener and not the desktop.
;;;; Everything two threads touch -- the screen framebuffers, the mailboxes, the wakes -- is made
;;;; on main (or by WM-SEAT-PORT's caller, shared on the way), never by the two threads, and
;;;; handed over as :ARGUMENTS or in a message; input crosses as messages, pixels through the
;;;; shared framebuffers, damage and the CopyRect hint through FB-MARK-FRAME.  That is what makes
;;;; it run where threads share nothing (modus).  Hence an app is a NAME the compositor calls,
;;;; never a closure made here.
;;;;
;;;; SEATS.  The port START-WM is given is the HOME seat's: a client that knows nothing of seats
;;;; gets exactly the desktop it always got.  WM-SEAT-PORT gives a caller (the card's WebRTC
;;;; gateway, one call per phone session) another seat by KEY -- a device id -- as a loopback RFB
;;;; port of its own: its own screen, size, pointer, focus, menu, clipboard and arrangement over
;;;; the same windows (wm.lisp says what is whose).  The same key always gets the same seat back,
;;;; arrangement and all.  A seat is KEPT for the life of the session once made, viewer or no:
;;;; what it costs while nobody watches is its screen's pixels and a listening socket -- it is not
;;;; composited, and is repainted whole when somebody looks again -- and keeping it is what lets
;;;; a phone that dropped off the network come back to its own desk.

(in-package #:glass.wm)

;; Lisp and Terminal: (LABEL MAKE-FN &key WIDTH HEIGHT), as *STANDARD-APPS*.
(defparameter *wm-apps*
  (append glass.desk:*standard-apps*
          '(("Lisp" glass-repl:repl-app :width 640 :height 400)
            ("Terminal" glass-term:terminal-app :width 640 :height 400)))
  "SERVE-WM's root menu: the desk's applications, a Lisp listener and a shell terminal.")

;; The McCLIM desktop's other applications, each in a system of its own that glass must not depend
;; on (loom and warren depend on glass): (LABEL PACKAGE NAME WIDTH HEIGHT), the MAKE-FN found by
;; name.  Whoever composes the desktop loads the ones it wants; the menu offers what is loaded.
(defparameter *wm-optional-apps*
  '(("Files"    "WARREN"     "DESKTOP-SURFACE" 1000 640)     ; warren, the file browser
    ("Browser"  "LOOM.GLASS" "BROWSER-APP"      900 620)     ; loom/glass
    ("Podcasts" "SPOOL.DESK" "PODCASTS-APP"     980 660)     ; spool/desk
    ("Speak"    "GLASS.DESK" "SPEAK-APP"        400 300)     ; glass/voice-apps
    ("Listen"   "GLASS.DESK" "LISTEN-APP"       400 320)     ; glass/voice-apps
    ("Mixer"    "GLASS.DESK" "MIXER-APP"        460 340)))   ; glass/mixer-desk

(defun wm-available-apps (&optional (apps *wm-apps*))
  "APPS, then each of *WM-OPTIONAL-APPS* whose system is loaded."
  (append apps
          (loop for (label pkg name w h) in *wm-optional-apps*
                for s = (and (find-package pkg) (find-symbol name pkg))
                when (and s (fboundp s) (not (find label apps :key #'first :test #'string=)))
                  collect (list label s :width w :height h))))

(defun serve-wm (port &rest args &key width height apps open address clients stop audio-port
                                      audio-path)
  "Serve the window manager WIDTH x HEIGHT on PORT: a network thread (every viewer), a
   compositor thread, and this thread the Lisp listener REPL windows evaluate on.  APPS is a
   list of (LABEL MAKE-FN &key WIDTH HEIGHT), MAKE-FN a symbol or global function returning the
   app contract (see wm.lisp); OPEN lists labels to open at start.  CLIENTS and STOP are
   SERVE-MULTI's.  AUDIO-PORT (or AUDIO-PATH, a UNIX socket) serves the desktop's sound as
   glass-audio/1 -- the McCLIM desktop's convention is PORT + 10, where the WebRTC gateway
   looks -- and needs glass/wm-sound loaded (sound.lisp).  Returns when the network side does."
  (declare (ignore width height apps open address clients stop audio-port audio-path))
  (multiple-value-bind (listener network compositor) (apply #'start-wm port args)
    (unwind-protect (glass-repl:run-listener listener)
      (stop-wm listener network compositor))))

;;; ---- the session: what a caller holds to ask for seats ---------------------------------------

(defclass wm-session ()
  ((port     :initarg :port     :reader wm-session-port)      ; the HOME seat's RFB port
   (address  :initarg :address  :reader wm-session-address)
   (width    :initarg :width    :reader wm-session-width)     ; a new seat's first screen size
   (height   :initarg :height   :reader wm-session-height)
   (listener :initarg :listener :reader wm-session-listener)
   (inbox    :initarg :inbox    :reader %session-inbox)       ; to the compositor
   (wake     :initarg :wake     :reader %session-wake)
   (requests :initarg :requests :reader %session-requests)    ; to the network: open a seat's listener
   (replies  :initarg :replies  :reader %session-replies)     ; from the network: its port
   (lock     :initform (sb-thread:make-mutex :name "glass-wm-seats") :reader %session-lock)
   ;; KEY -> (ID PORT FB CLIPBOARD), every seat but the home one, for the life of the session
   (seats    :initform (make-hash-table :test 'equal) :reader %session-seats)
   (next-id  :initform 0 :accessor %session-next-id))
  (:documentation "One START-WM's desktop as its callers see it: the home port, and the seats asked
   for by key.  Made on the thread that called START-WM."))

(defmethod print-object ((s wm-session) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~a:~d, ~d seat~:p" (wm-session-address s) (wm-session-port s)
            (1+ (hash-table-count (%session-seats s))))))

(defvar *wm-sessions* '() "Every START-WM session in this image, the newest first.")
(defvar *wm-sessions-lock* (sb-thread:make-mutex :name "glass-wm-sessions"))

(defun wm-session (designator)
  "The session DESIGNATOR names: a WM-SESSION, the home seat's RFB port (an integer), or the
   glass-repl listener START-WM returned.  The newest wins; NIL if none."
  (if (typep designator 'wm-session)
      designator
      (sb-thread:with-mutex (*wm-sessions-lock*)
        (find-if (lambda (s) (if (integerp designator)
                                 (eql designator (wm-session-port s))
                                 (eq designator (wm-session-listener s))))
                 *wm-sessions*))))

(defun %seat-framebuffer (w h)
  "A seat's screen, its pixels in memory every thread may hold (FB-RESIZE allocates them so)."
  (let ((fb (glass:make-framebuffer 1 1 *wm-bg*)))
    (glass:fb-resize fb w h *wm-bg*)
    (glass::%share fb)))

(defun wm-seat-port (session key &key width height (timeout 10))
  "THE SEAT FOR KEY, as a loopback RFB port to connect to: (values PORT SEAT-ID).

   SESSION is what START-WM made -- its home port (an integer), its listener, or the WM-SESSION
   (START-WM's fourth value).  KEY is who is asking, an EQUAL-comparable value (a device id
   string); NIL is the home seat, whose port is the session's own.  The first call for a key makes
   the seat -- a WIDTH x HEIGHT screen (default: the session's starting size; the viewer then sizes
   it with SetDesktopSize), its own pointer, focus, menu, clipboard and arrangement, seeing every
   window where the session has it -- and has the network thread listen for it on a fresh port on
   the session's address.  Every later call for the same key returns the same port, and the seat
   behind it is as its last viewer left it.  Blocks (briefly: until the network thread's next turn)
   the first time; signals an error if the network does not answer within TIMEOUT seconds.

   Any thread but the desktop's own two may call it.  On modus, call it from the thread that
   called START-WM (the card's one-thread gateway does): what it makes is shared on the way, but
   that is the thread whose objects are everybody's."
  (let ((session (or (wm-session session) (error "glass wm: no desktop session ~s" session))))
    (when (null key) (return-from wm-seat-port (values (wm-session-port session) 0)))
    (sb-thread:with-mutex ((%session-lock session))
      (let ((have (gethash key (%session-seats session))))
        (when have (return-from wm-seat-port (values (second have) (first have)))))
      (let* ((id (incf (%session-next-id session)))
             (fb (%seat-framebuffer (or width (wm-session-width session))
                                    (or height (wm-session-height session))))
             (clipboard (glass::%share (glass:make-clipboard)))
             (key (glass::%share key))
             (deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second)))
             (port nil))
        ;; the compositor first, so the seat exists there before any input from its listener can
        (glass::mb-post (%session-inbox session) (list :seat id key fb clipboard))
        (glass:wake-signal (%session-wake session))
        (glass::mb-post (%session-requests session) (list :listen id fb clipboard))
        (loop until port do
          (dolist (m (glass::mb-take (%session-replies session)))
            (when (eql (second m) id)
              (if (eq (first m) :port)
                  (setf port (third m))
                  (error "glass wm: seat ~s: ~a" key (third m)))))
          (unless port
            (when (> (get-internal-real-time) deadline)
              (error "glass wm: seat ~s: the desktop's network did not answer" key))
            (sleep 0.002)))
        (setf (gethash key (%session-seats session)) (list id port fb clipboard))
        (values port id)))))

(defun wm-session-seat-keys (session)
  "The keys of every seat SESSION has made (the home seat, NIL, is not among them)."
  (let ((session (wm-session session)) (keys '()))
    (sb-thread:with-mutex ((%session-lock session))
      (maphash (lambda (k v) (declare (ignore v)) (push k keys)) (%session-seats session)))
    keys))

(defun wm-session-seat-clipboard (session key)
  "The selection of SESSION's seat KEY (NIL: the home seat's, GLASS:SESSION-CLIPBOARD), or NIL."
  (if (null key)
      (glass:session-clipboard)
      (let ((session (wm-session session)))
        (sb-thread:with-mutex ((%session-lock session))
          (fourth (gethash key (%session-seats session)))))))

;;; ---- starting and stopping --------------------------------------------------------------------

(defun stop-wm (listener network compositor)
  "After START-WM's LISTENER has stopped: wait for its two threads, and let the listener go."
  (ignore-errors (sb-thread:join-thread network))
  (ignore-errors (sb-thread:join-thread compositor))
  (sb-thread:with-mutex (*wm-sessions-lock*)
    (setf *wm-sessions* (remove listener *wm-sessions* :key #'wm-session-listener)))
  (when (eq glass-repl:*listener* listener) (setf glass-repl:*listener* nil)))

(defun start-wm (port &key (width 800) (height 600) (apps (wm-available-apps)) (open '("Clock"))
                           (address "127.0.0.1") clients stop audio-port audio-path)
  "SERVE-WM without its listener loop: start the network and compositor threads and return
   (values LISTENER NETWORK COMPOSITOR SESSION) at once.  THIS thread must then serve LISTENER --
   GLASS-REPL:RUN-LISTENER, or GLASS-REPL:LISTENER-TURN from a loop of its own (one process that
   is also a WebRTC gateway) -- and call STOP-WM when the listener stops.  PORT is the home seat's;
   SESSION (or PORT, or LISTENER) is what WM-SEAT-PORT takes to give a caller a seat of its own."
  ;; The faces, loaded HERE: the compositor draws text, and a font it loaded itself could not go
  ;; in the shared cache on modus.
  (glass:default-font) (glass:default-font t) (glass-term:term-fonts)
  (let* ((sound (and (or audio-port audio-path)
                     (if (fboundp 'audio-out-open)
                         t
                         (progn (warn "glass wm: no sound -- load glass/wm-sound for AUDIO-PORT") nil))))
         (fb (glass:make-framebuffer width height *wm-bg*))
         (inbox (glass::%share (glass::make-rfb-mailbox)))
         ;; the compositor's mixed frames, to the network (shared from birth, as INBOX)
         (sound-box (and sound (glass::%share (glass::make-rfb-mailbox))))
         ;; WM-SEAT-PORT's asks of the network, and its answers
         (requests (glass::%share (glass::make-rfb-mailbox)))
         (replies (glass::%share (glass::make-rfb-mailbox)))
         (wake (glass:make-wake))
         (listener (glass-repl:make-listener))
         (session (make-instance 'wm-session :port port :address address :width width :height height
                                             :listener listener :inbox inbox :wake wake
                                             :requests requests :replies replies))
         (compositor (progn
                       (setf glass-repl:*listener* listener)
                       (sb-thread:make-thread #'%wm-compositor
                                              :arguments (list fb inbox wake apps open sound-box)
                                              :name "glass-wm-compositor")))
         (network (sb-thread:make-thread #'%wm-network
                                         :arguments (list fb port address clients stop
                                                          inbox wake listener
                                                          sound sound-box audio-port audio-path
                                                          requests replies)
                                         :name "glass-wm-network")))
    (sb-thread:with-mutex (*wm-sessions-lock*) (push session *wm-sessions*))
    (values listener network compositor session)))

;;; ---- the network thread ----------------------------------------------------------------------

(defun %wm-network (fb port address clients stop inbox wake listener
                    sound sound-box audio-port audio-path requests replies)
  "The network thread: every seat's viewers, and the sound's listeners, until SERVE-ENDPOINTS
   returns; then stop the others."
  (let ((out nil))
    (flet ((post (m) (glass::mb-post inbox m) (glass:wake-signal wake)))
      (unwind-protect
           (handler-case
               (let* ((home (glass:make-rfb-endpoint
                             :listen (glass:tcp-listen port :address address) :fb fb
                             :clipboard (glass:session-clipboard)
                             :on-key (lambda (down key) (post (list :key down key)))
                             :on-pointer (lambda (mask x y) (post (list :pointer mask x y)))
                             ;; A CLIENT ASKING FOR A SIZE (SetDesktopSize) gets the screen it
                             ;; asked for: a phone a phone-shaped desktop, not a letterbox.  The
                             ;; compositor does it, between frames, as it does input.
                             :on-resize (lambda (w h) (post (list :resize w h)))))
                      (endpoints (list home))
                      (seats '()))                ; ((id endpoint . viewers-last-told) ...)
                 (format *error-output* "~&glass: RFB server (all clients, one thread) on port ~d (~dx~d)~%"
                         port (glass:fb-width fb) (glass:fb-height fb))
                 (force-output *error-output*)
                 ;; A desktop whose sound will not come up is a working desktop; one that did not
                 ;; start is not (START-SESSION-AUDIO's rule).  The compositor still mixes -- the
                 ;; mixer is the player's clock -- and posts nothing, nobody having said they listen.
                 (when sound
                   (handler-case
                       (setf out (funcall 'audio-out-open sound-box :port audio-port :path audio-path
                                                                    :address address
                                                                    :notify (lambda (n) (post (list :listeners n)))))
                     (error (e)
                       (format *error-output* "~&glass wm: no audio stream (~a); the desktop runs silent~%" e)
                       (force-output *error-output*))))
                 (labels ((open-seat (id sfb clipboard)
                            ;; a listener of the seat's own; its input goes to the compositor
                            ;; tagged with the seat
                            (let* ((sock (glass:tcp-listen 0 :address address))
                                   (sport (nth-value 1 (sb-bsd-sockets:socket-name sock)))
                                   (ep (glass:make-rfb-endpoint
                                        :listen sock :fb sfb :clipboard clipboard
                                        :on-key (lambda (down key) (post (list :key down key id)))
                                        :on-pointer (lambda (mask x y) (post (list :pointer mask x y id)))
                                        :on-resize (lambda (w h) (post (list :resize w h id))))))
                              (setf endpoints (append endpoints (list ep)))
                              (push (list* id ep 0) seats)
                              (format *error-output* "~&glass wm: seat ~d on port ~d~%" id sport)
                              (force-output *error-output*)
                              sport))
                          (turn ()
                            (dolist (m (glass::mb-take requests))
                              (destructuring-bind (op id sfb clipboard) m
                                (declare (ignore op))
                                (glass::mb-post replies
                                                (handler-case (list :port id (open-seat id sfb clipboard))
                                                  (error (e) (list :error id (princ-to-string e)))))))
                            ;; who is watching which seat: the compositor paints only those
                            (dolist (s seats)
                              (let ((n (glass:endpoint-viewers (second s))))
                                (unless (eql n (cddr s))
                                  (setf (cddr s) n)
                                  (post (list :viewers (first s) n)))))
                            (when out (funcall 'audio-out-turn out))))
                   (glass:serve-endpoints (lambda () endpoints)
                                          :clients clients :stop stop :tick #'turn)))
             (error (e)
               (ignore-errors (format *error-output* "~&glass wm: network: ~a~%" e)
                              (force-output *error-output*))))
        (post (list :stop))
        (when out (ignore-errors (funcall 'audio-out-close out)))
        (glass-repl:stop-listener listener)))))

;;; ---- the compositor thread -------------------------------------------------------------------

(defun %wm-backtrace (fault &optional condition)
  "The stack of the error being handled, to *ERROR-OUTPUT* (as glass.desk's)."
  (ignore-errors
   (let ((f (if fault
                (and (find-package "MODUS") (find-symbol "CONDITION-BACKTRACE" "MODUS"))
                (and (find-package "SB-DEBUG") (find-symbol "PRINT-BACKTRACE" "SB-DEBUG")))))
     (when (and f (fboundp f))
       (if fault
           (funcall f condition *error-output*)
           (funcall f :stream *error-output* :count 25))))))

(defmacro %app-guard (what &body body)
  "BODY, with an error logged with its backtrace and dropped (NIL), never ending the thread."
  (let ((traced (gensym "TRACED")))
    `(let ((,traced nil))
       (handler-case
           (handler-bind ((error (lambda (e)
                                   (ignore-errors (format *error-output* "~&glass wm: ~a: ~a~%" ,what e))
                                   (%wm-backtrace nil)
                                   (setq ,traced t))))
             (progn ,@body))
         (error (e)
           (unless ,traced
             (ignore-errors (format *error-output* "~&glass wm: ~a: ~a (fault)~%" ,what e))
             (%wm-backtrace t e))
           (ignore-errors (force-output *error-output*))
           nil)))))

(defun %wm-compositor (fb inbox wake apps open sound-box)
  "The compositor thread.  Caches the apps fill as they run are this thread's own, and so is the
   desktop's sound: the mixer the apps play into, ticked by this loop (sound.lisp)."
  (glass.desk:call-with-media-thread
   (lambda ()
     (let* ((snd (and sound-box (funcall 'wm-sound-open sound-box)))
            (glass.desk:*media-mixer* (and snd (funcall 'wm-sound-mixer snd))))
       ;; ONE DESKTOP, ONE MIX.  The voice (glass/speech) and the podcast player (spool) play into
       ;; GLASS:SESSION-MIXER, which would otherwise make a second mixer with its own clock that
       ;; nobody serves: the gateway hears THIS one, on AUDIO-PORT.  So this mixer is the
       ;; session's, unless the process already has one.  SBCL only: their threads add sources
       ;; under the mixer's lock, and where threads share nothing (modus) another thread may not
       ;; hold this one at all.  Every seat hears it: sound is not per-seat (yet).
       #-modus
       (when snd
         (sb-thread:with-mutex (glass::*session-mixer-lock*)
           (unless glass::*session-mixer* (setf glass::*session-mixer* glass.desk:*media-mixer*))))
       (glass:with-own-glyph-cache () (%wm-compositor-1 fb inbox wake apps open snd))))))

(defun %message-seat (m)
  "The seat id an input message is for: NIL (the home seat) unless it carries one."
  (case (first m)
    (:key (fourth m))
    (:pointer (fifth m))
    (:resize (fourth m))))

(defun %superseded-p (wm m rest)
  "Is M made redundant by a later message?  A :RESIZE is, by any later :RESIZE of the same seat:
   only the last size asked for matters, and each one clears the screen.  A pointer message is,
   by the next one, only while its seat is dragging or resizing a window: a motion followed by
   another motion of the same seat with the same buttons says nothing the second does not, and
   replaying a backlog of them one composite each is how a drag falls behind.  Never otherwise --
   Paint wants every point of a stroke."
  (let ((id (%message-seat m)))
    (or (and (eq (first m) :resize)
             (find-if (lambda (r) (and (eq (first r) :resize) (eql (%message-seat r) id))) rest))
        (and (eq (first m) :pointer)
             rest
             (eq (first (first rest)) :pointer)
             (eql (%message-seat (first rest)) id)
             (eql (second m) (second (first rest)))
             (let ((seat (wm-find-seat wm id))) (and seat (seat-drag seat)))
             t))))

(defun %wm-compositor-1 (fb inbox wake apps open snd)
  (let ((wm (make-wm fb))
        (input nil) (last-tick 0)
        (poll (floor internal-time-units-per-second 80)))   ; 1/60 s, less a wake's slop
    (dolist (a apps)
      (destructuring-bind (label make-fn &key (width 400) (height 300)) a
        (wm-register-app wm label make-fn :width width :height height)))
    (dolist (label open) (%app-guard "open" (wm-open wm label)))
    (wm-redraw wm)
    (loop
      (loop for (m . rest) on (let ((ms (glass::mb-take inbox))) (when ms (setf input t)) ms)
            do (when (eq (first m) :stop) (return-from %wm-compositor-1 nil))
               (unless (%superseded-p wm m rest)
                 ;; AN APP'S ERROR IS THE APP'S: input reaches app code, and a signal out of it
                 ;; must not end the compositor -- that is the whole desktop.
                 (%app-guard (format nil "~(~a~)" (first m))
                   (let ((seat (wm-find-seat wm (%message-seat m))))
                     (case (first m)
                       (:listeners (when snd (funcall 'wm-sound-listen snd (second m))))
                       (:seat (destructuring-bind (id key sfb clipboard) (rest m)
                                (wm-add-seat wm id sfb :key key :clipboard clipboard)))
                       (:viewers (let ((s (wm-find-seat wm (second m))))
                                   (when s (wm-seat-viewers wm s (third m)))))
                       (:key (when seat (wm-key wm (second m) (third m) seat)))
                       (:pointer (when seat
                                   (destructuring-bind (mask x y &rest r) (rest m)
                                     (declare (ignore r))
                                     (wm-pointer wm mask x y seat))))
                       (:resize (when seat (wm-resize-screen wm (second m) (third m) seat))))))))
      ;; the sound first: a frame due now is mixed from what the player has queued, and the
      ;; tick after it (the player's query) decodes the next
      (when snd (%app-guard "sound" (funcall 'wm-sound-turn snd)))
      ;; The windows are polled at most every 1/60 s, as without sound, unless input came: the
      ;; 50 Hz wakes the sound adds are for the mixer, and a poll is an app's whole query (warp's
      ;; Media re-runs its projection) -- polling at both rates spent a third more for nothing.
      (let ((now (get-internal-real-time)))
        (when (or input (>= (- now last-tick) poll))
          (setf input nil last-tick now)
          (%app-guard "tick" (wm-tick wm))))
      (glass::wake-wait wake (if snd (funcall 'wm-sound-wait snd 1/60) 1/60)))))
