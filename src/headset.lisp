;;;; src/headset.lisp — one person's audio: the mix in their ears, the microphone at their mouth.
;;;;
;;;; A SEAT (backend/seat.lisp) is who is watching: a screen, a pointer, a keyboard, a focus, an
;;;; arrangement of the session's windows.  Audio was left out of that refactor on the grounds
;;;; that it does not reach a listener over RFB at all — it reaches them over sockets of their
;;;; own — and that is exactly right, which is why this file is here and not there.  A headset is
;;;; the audio half of a seat, and it is deliberately a GLASS object with no idea that McCLIM,
;;;; window managers or RFB exist: a recorder, a test, or a second box can wear one.
;;;;
;;;; WHAT IS IN IT, AND WHY EACH ONE IS PER-PERSON:
;;;;
;;;;   THE MIX (out).  A composite of the SESSION's sources — the podcast plays once and both
;;;;   people hear it — with this listener's own selection and gains, its own ring and its own
;;;;   sinks.  See audio.lisp: one pull, a sum per listener, which is the compositor's split with
;;;;   samples instead of pixels.  The primary seat's mix IS the session's, so a one-seat desktop
;;;;   has exactly one and is exactly the old code path.
;;;;
;;;;   THE MICROPHONE (in).  Mine.  It is not in any mix and never will be: putting it there
;;;;   would play my room out of the desktop and back down the other socket to the phone that
;;;;   said it (mic-stream.lisp's header), and it would hand it to the other person, who did not
;;;;   dial in to listen to my room.  Two seats are two ports and no arbitration.
;;;;
;;;;   THE EAR.  Transcribes what this person's microphone is saying, falling back to their mix
;;;;   when no microphone is live — the same rule the session ear always had, asked of one seat.
;;;;
;;;;   DICTATION.  The ear as a keyboard, typing into the window THIS SEAT has focused, through
;;;;   the injector the seat's own RFB listener was built with.  Before seats there was one
;;;;   *KEY-INJECTOR* and it was filled by whichever listener started last, so this is the one
;;;;   place where a second seat was not merely unserved but actively wrong.
;;;;
;;;; THE PORTS COME FROM THE SCREEN'S.  A seat already has one number that identifies it — the
;;;; RFB port — and the desktop already put its mix one decade up from it (5903 -> 5913, 5914).
;;;; So the pair is derived rather than configured, and the primary seat's numbers do not move:
;;;; the live WebRTC gateway looks up GLASS_AUDIO_PORT with 5913 as its default and finds exactly
;;;; what it found before.
;;;;
;;;; THE OPTIONAL HALVES ARE LOOKED UP BY NAME.  An ear needs stave and a voice needs chord;
;;;; this file needs neither to give a seat sound, so MAKE-EARS and START-DICTATION are resolved
;;;; at call time exactly as hearing.lisp resolves the microphone and wm.lisp resolves SPEAK.  A
;;;; desktop with no recognizer installed has a headset with no ear, which is the truth.

(in-package #:glass)

(defclass headset ()
  ((name    :initarg :name    :initform "headset" :accessor headset-name)
   (primary :initarg :primary :initform nil :accessor headset-primary-p)
   (mixer   :initarg :mixer   :initform nil :accessor headset-mixer)   ; the session's bus
   (mix     :initarg :mix     :initform nil :accessor headset-mix)     ; this person's composite
   (audio   :initform nil :accessor headset-audio)      ; the AUDIO-STREAM serving that mix
   (mic     :initform nil :accessor headset-mic)        ; the MIC-STREAM their microphone arrives on
   (ears    :initform nil :accessor headset-ears)       ; their ear, once something asks for one
   (dictation :initform nil :accessor headset-dictation)
   ;; (DOWN-P KEYSYM) — this person's keyboard.  Dictation types on it; nothing else here does.
   (injector :initarg :injector :initform nil :accessor headset-injector)
   (audio-port :initarg :audio-port :initform 0 :accessor headset-audio-port)
   (mic-port   :initarg :mic-port   :initform 0 :accessor headset-mic-port))
  (:documentation "One person's audio on a shared session: their mix out, their microphone in,
   their ear, and the keyboard their dictation types on."))

(defmethod print-object ((h headset) stream)
  (print-unreadable-object (h stream :type t)
    (format stream "~s out :~d in :~d~:[~; primary~]"
            (slot-value h 'name) (slot-value h 'audio-port) (slot-value h 'mic-port)
            (slot-value h 'primary))))

(defun make-headset (&key name rfb-port audio-port mic-port (address "127.0.0.1")
                          mixer mix injector primary (lead 2) (mic t))
  "Give one person sound: a mix of their own on AUDIO-PORT and a microphone of their own on
MIC-PORT.  Returns the HEADSET; nothing is loaded and no model is read.

RFB-PORT is the seat's screen port and is the ordinary way to say where the audio goes: the pair
is derived from it (5903 -> 5913 out, 5914 in).  AUDIO-PORT / MIC-PORT override it.

PRIMARY t means this person IS the session: their mix is the session's mix, so everything that
plays into GLASS:SESSION-MIXER reaches them without being addressed, and their microphone port
is the session's, so an ear started with no seat to ask about hears it.  That is the one-seat
desktop, and it is the same objects it always had.

MIC nil serves no microphone port at all — for a listener that is a recorder rather than a
person.  A failure to open either port is reported and leaves that half NIL: a seat with no
sound is a working seat, and a seat that did not start is not."
  (let* ((bus (or mixer (and mix (mix-bus mix)) (session-mixer)))
         (name (or name (format nil "headset-~d" (or rfb-port audio-port 0))))
         (out (or audio-port (and rfb-port (seat-audio-port rfb-port)) *audio-stream-port*))
         (in  (or mic-port  (and rfb-port (seat-mic-port rfb-port))   *mic-stream-port*))
         (h (make-instance 'headset :name name :primary primary :mixer bus
                                    :injector injector :audio-port out :mic-port in
                                    :mix (or mix
                                             (if primary
                                                 (mixer-default-mix bus)
                                                 (make-mix bus :name name))))))
    ;; The primary seat ADOPTS whatever the session already has.  A desktop's startup script
    ;; starts the session's audio directly and a gateway is connected to it; opening a second
    ;; listener on that port would fail, and reporting "no audio" about a port that is answering
    ;; would be worse than the failure.
    (handler-case
        (setf (headset-audio h)
              (or (and primary *session-audio-stream*)
                  (start-audio-stream :mix (headset-mix h) :port out
                                      :address address :lead lead)))
      (serious-condition (e)
        (ignore-errors
         (format *error-output* "~&@@ ~a: no audio on ~a:~d (~a)~%" name address out e)
         (finish-output *error-output*))))
    (when (headset-audio h)
      (setf (headset-audio-port h) (audio-stream-port (headset-audio h))))
    (when mic
      (handler-case
          (setf (headset-mic h)
                (or (and primary *session-mic-stream*)
                    (start-mic-stream :port in :address address :install primary)))
        (serious-condition (e)
          (ignore-errors
           (format *error-output* "~&@@ ~a: no microphone on ~a:~d (~a)~%" name address in e)
           (finish-output *error-output*)))))
    (when (headset-mic h)
      (setf (headset-mic-port h) (mic-stream-port (headset-mic h))))
    h))

;;; ---- the ear, and the keyboard behind it -------------------------------------

(defun %glass-fn (name)
  "The bound GLASS function NAME, or NIL — the optional systems (:glass/hearing,
:glass/dictation) may not be in this image, and a headset is still a headset without them."
  (let ((s (find-symbol name '#:glass))) (and s (fboundp s) s)))

(defun headset-listen (h &key rec)
  "Start THIS person's ear: their microphone while one is live, their mix the rest of the time.
Returns the EARS, or NIL if this image has no recognizer at all.

The primary seat's ear is the SESSION's (START-LISTENING), so the Listen window, the control
socket and everything else that asks for `the ear' keeps finding it.  A further seat's is its
own — a second quarter of a gigabyte, which is what listening to a second person costs unless
REC is passed to share an already-loaded recognizer."
  (or (headset-ears h)
      (let ((make (%glass-fn "MAKE-EARS"))
            (start (%glass-fn "START-LISTENING")))
        (when make
          (setf (headset-ears h)
                (if (headset-primary-p h)
                    (funcall start)
                    (funcall make :mix (headset-mix h)
                                  :mic-stream (headset-mic h)
                                  :rec rec)))))))

(defun headset-stop-listening (h)
  "Stop this person's ear (and their dictation with it)."
  (headset-stop-dictating h)
  (let ((stop (%glass-fn "STOP-LISTENING")))
    (when (and stop (headset-ears h)) (funcall stop (headset-ears h))))
  (setf (headset-ears h) nil)
  t)

(defun headset-dictate (h &key rec)
  "Type what THIS person says into the window THIS person has focused.  Starts their ear if it
is not running.  Returns the DICTATION, or NIL if this image cannot hear or has no keyboard for
them.

The injector is the seat's, so the words go where that seat's keystrokes go.  For the primary
seat it is NIL and means the session's *KEY-INJECTOR*, which is the only keyboard a one-seat
desktop has and the one the Listen window's Dictate button has always used."
  (let ((start (%glass-fn "START-DICTATION")))
    (when (and start (headset-listen h :rec rec))
      (setf (headset-dictation h)
            (funcall start :ear (headset-ears h)
                           :injector (headset-injector h)
                           :dictation (headset-dictation h)
                           :name (headset-name h))))))

(defun headset-stop-dictating (h)
  (let ((stop (%glass-fn "STOP-DICTATION")))
    (when (and stop (headset-dictation h))
      (funcall stop :ear (headset-ears h) :dictation (headset-dictation h))))
  t)

;;; ---- taking it off -----------------------------------------------------------

(defun stop-headset (h)
  "Close this person's ports and stop their ear.  The session's sources are untouched: what was
playing goes on playing, to everybody who is still listening."
  (when h
    (ignore-errors (headset-stop-listening h))
    ;; A port this headset ADOPTED (the primary seat's, which is the session's) is not this
    ;; headset's to close: taking a seat off the desktop must not take the desktop's sound with
    ;; it, and the gateway is on the other end of that socket.
    (when (and (headset-audio h) (not (eq (headset-audio h) *session-audio-stream*)))
      (ignore-errors (stop-audio-stream (headset-audio h))))
    (setf (headset-audio h) nil)
    (when (and (headset-mic h) (not (eq (headset-mic h) *session-mic-stream*)))
      (ignore-errors (stop-mic-stream (headset-mic h))))
    (setf (headset-mic h) nil)
    ;; the mix goes last, and only if it is this person's own: the primary seat's IS the
    ;; session's, and taking that off the bus would silence the desktop rather than one listener
    (let ((mix (headset-mix h)))
      (when (and mix (not (eq mix (mixer-default-mix (headset-mixer h)))))
        (ignore-errors (remove-mix (headset-mixer h) mix)))))
  t)

(defun headset-report (h)
  "One line about one person's sound, for a control socket."
  (if (null h)
      "headset: none"
      (format nil "headset ~a~:[~; (primary)~] out :~d in :~d~%  ~a~%  ~a~@[~%  ~a~]~@[~%  ~a~]"
              (headset-name h) (headset-primary-p h)
              (headset-audio-port h) (headset-mic-port h)
              (if (headset-audio h) (audio-stream-report (headset-audio h)) "audio-stream: none")
              (if (headset-mic h) (mic-stream-report (headset-mic h)) "mic-stream: none")
              (let ((report (%glass-fn "HEARING-REPORT")))
                (and report (headset-ears h) (funcall report (headset-ears h))))
              (let ((report (%glass-fn "DICTATION-REPORT")))
                (and report (headset-dictation h) (funcall report (headset-dictation h)))))))
