;;;; dictation.lisp — the ear as a keyboard.
;;;;
;;;; The Listen window is one consumer of the transcript: it DISPLAYS what was said.  This is the
;;;; other one, and the more useful of the two: it TYPES what was said, into whatever window has
;;;; focus, so speech reaches an app that has never heard of glass's ear.
;;;;
;;;; Nothing here is a new input path.  Glass already has one — *KEY-INJECTOR*, which SERVE fills
;;;; with the server's own :ON-KEY callback, so an injected key takes the identical route a VNC
;;;; client's keypress takes: the WM's focused-surface rule, the terminal's pty write, loom's
;;;; editing state, the CLIM event queue.  It was built for pasting the clipboard into apps that
;;;; do not read a clipboard, and it turns out that "turn text into keystrokes for whoever has
;;;; focus" is exactly what dictation needs.  So dictation is a listener on one end and
;;;; PASTE-TEXT on the other, and this file is mostly about the three things in between that are
;;;; genuinely hard:
;;;;
;;;;   1. WHEN to type.  Only a FINISHED utterance is safe.  A partial transcript revises itself
;;;;      as more audio arrives — `AFTER EARLY NIGHT' becomes `AFTER EARLY NIGHTFALL' — and a
;;;;      keystroke cannot be taken back.  Backspacing would work in a text field and would be a
;;;;      disaster in vi.  So dictation waits for the flush (HEARING-LISTEN fires there and only
;;;;      there) and pays for it in latency: you speak, you stop, and then the words appear.
;;;;
;;;;   2. WHAT to type.  The recognizer emits bare uppercase words with no punctuation, which is
;;;;      what its training data looks like and not what anyone wants in a text field.
;;;;      DICTATION-TEXT sentence-cases it.  This is a presentation choice living outside the
;;;;      recognizer on purpose: HEARING-TEXT still shows what stave actually said, so the
;;;;      transcript stays honest and only the typed copy is prettied up.
;;;;
;;;;   3. NOT typing what we said ourselves.  The ear is a sink on the session mix and the voice
;;;;      is a source on it, which is the demonstration in :glass/hearing's description — the
;;;;      desktop can hear itself.  With dictation on, that stops being a demonstration and
;;;;      becomes a loop: chord speaks, the ear transcribes it, dictation types it into the
;;;;      focused window.  So dictation is deaf while the session is speaking, and for a moment
;;;;      after (the tail of an utterance arrives after the audio that ended it).
;;;;
;;;; ---- and WHOSE window ------------------------------------------------------------------
;;;;
;;;; With seats there is a fourth, and it was wrong here before it was written down: WHERE the
;;;; words go.  *KEY-INJECTOR* is filled by SERVE, so on a two-seat desktop it holds whichever
;;;; seat's listener started LAST — and dictation from the first seat's microphone typed into the
;;;; second seat's focused window.  Both halves of that are per-seat: the ear hears one person's
;;;; microphone, and the keyboard it types on is that person's, pointed at the window THEY have
;;;; focused.  So a DICTATION is an object with an ear and an injector, one per seat, and the
;;;; session's — the one a one-seat desktop has and the one the Listen window switches on — is
;;;; the same object with the injector left NIL, meaning "the session's keyboard", which is
;;;; exactly what *KEY-INJECTOR* is when there is only one person here.

(in-package #:glass)

(defparameter *dictation-tail-seconds* 2.5d0
  "How long after the voice stops that dictation stays deaf.

Not a guess at reverb — an ear on a digital mix has none.  It is the recognizer's own lag, and it
is bounded below by a number in the other file: the level gate needs *HEARING-GAP-SECONDS* (0.8)
of quiet before it will call an utterance finished, and the decoder then has to flush it, so the
transcript of what the desktop SAID lands well over a second after it stopped saying it.  A tail
shorter than that gap does not guard anything at all — the guard would expire before the thing it
guards against arrives — which is exactly how this was first written and exactly how the gate
caught it.")

;;; ---- one person's dictation --------------------------------------------------

(defvar *dictation-key* "dictation"
  "The key a dictation registers under on its ear.  Named, and re-registering replaces, so
switching dictation on twice types everything once.")

(defclass dictation ()
  ((name    :initarg :name    :initform "dictation" :accessor dict-name)
   (ear     :initarg :ear     :initform nil :accessor dict-ear)
   ;; The keyboard these words are typed on: a function (DOWN-P KEYSYM), which is precisely the
   ;; :ON-KEY a seat's RFB listener was started with, so a dictated key takes the identical route
   ;; that seat's typed keys take — through ITS focus.  NIL means the session's *KEY-INJECTOR*,
   ;; which is the right answer when there is one person and the only one available to a caller
   ;; that has no seat.
   (injector :initarg :injector :initform nil :accessor dict-injector)
   (key     :initarg :key     :initform *dictation-key* :accessor dict-key)
   (on      :initform nil :accessor dict-on)
   (typed   :initform 0   :accessor dict-typed)
   (muted   :initform 0   :accessor dict-muted)
   (last    :initform nil :accessor dict-last)
   (quiet-until :initform 0 :accessor dict-quiet-until)
   (watcher :initform nil :accessor dict-watcher))
  (:documentation "One person's speech reaching one person's keyboard: an ear, an injector,
   and the guard that keeps the desktop from typing what it said itself."))

(defmethod print-object ((d dictation) stream)
  (print-unreadable-object (d stream :type t)
    (format stream "~s ~:[off~;on~] ~d typed" (slot-value d 'name) (slot-value d 'on)
            (slot-value d 'typed))))

(defvar *session-dictation* nil
  "The session's dictation — the primary seat's, and the one a one-seat desktop has.")

(defvar *dictating* nil
  "True while the SESSION's ear is typing what it hears.  Read it rather than a window's button:
the Listen window's switch is this one.  A further seat's dictation is its own object and does
not touch this, for the same reason its clipboard is not the session's.")

(defvar *dictation-typed* 0
  "Utterances typed since the last START-DICTATION.  The honest counter — an utterance suppressed
by the self-hearing guard is counted separately, not here.")

(defvar *dictation-muted* 0
  "Utterances dropped because the desktop was speaking.  Kept because the alternative — dropping
them silently — makes a real dictation failure look identical to the guard doing its job.")

(defvar *dictation-last* nil
  "The last text dictation actually typed, or NIL.  For a status line, and for a test that wants
to know what landed without racing the app it landed in.")

;;; ---- what gets typed --------------------------------------------------------

(defun dictation-text (text)
  "TEXT as it should be TYPED: STAVE:SENTENCE-CASE plus one trailing space.

The recase — bare uppercase to sentence case — moved to stave, where it belongs: it is a fact about
what the recognizer emits, not about typing, and stave is what emitted it.  So `AFTER EARLY
NIGHTFALL' becomes `After early nightfall '.  What stays HERE, because it is genuinely a dictation
concern, is the TRAILING SPACE: it is what makes consecutive typed utterances read as prose instead
of running together (and it is also why there is nothing to capitalize after — there are no periods)."
  (let ((s (stave:sentence-case text)))
    (if (zerop (length s)) "" (concatenate 'string s " "))))

;;; ---- when it is safe to type ------------------------------------------------

(defvar *dictation-quiet-until* 0
  "The SESSION dictation's quiet window, mirrored out of the object for anything that reads it.")

(defvar *dictation-watcher* nil
  "The session dictation's watcher thread.")

(defparameter *dictation-watch-step* 0.1d0
  "How often the watcher samples the voice.  It has to be well under a sentence: the whole job is
to have SEEN the voice live at some point during it.")

(defun %dictation-watch (d)
  "Push the quiet window forward for as long as the desktop is speaking.

This exists because of the order events actually happen in.  The guard's natural home is the
moment an utterance arrives — but by then the voice has been silent for over a second (the ear
will not end an utterance until it has heard *HEARING-GAP-SECONDS* of quiet), so asking `are we
speaking?' at that moment always answers no, and the guard never fires.  It has to be asked WHILE
the voice is talking, which means something has to be awake then.  A 100 ms poll costs nothing
and needs no hook inside the optional :glass/speech system.

Catches SERIOUS-CONDITION around the whole body: under --disable-debugger an unhandled condition
on any thread quits the process, and a desktop must not be lost to its dictation watchdog."
  (handler-case
      (loop while (dict-on d)
            do (when (%dictation-speaking-p)
                 (%dict-set-quiet d (+ (get-internal-real-time)
                                       (round (* *dictation-tail-seconds*
                                                 internal-time-units-per-second)))))
               (sleep *dictation-watch-step*))
    (serious-condition (e)
      (ignore-errors
       (format *error-output* "~&glass dictation: watcher stopped — ~a~%" e)
       (force-output *error-output*)))))

(defun %dictation-speaking-p ()
  "True if the desktop's own voice is talking right now.

Resolved by FIND-SYMBOL rather than called directly because :glass/speech is optional — a desktop
with an ear and no voice is a working desktop, and it must not need chord loaded to dictate.  A
desktop with no voice cannot hear itself, so the answer there is NIL and the guard costs nothing."
  (let ((sym (find-symbol "SPEAKING-P" "GLASS")))
    (and sym (fboundp sym) (funcall sym) t)))

(defun %dictation-deaf-p (d)
  "True if this utterance must be dropped rather than typed.

Reads the window the watcher maintains rather than sampling the voice here, for the reason given
in %DICTATION-WATCH: at the moment an utterance lands, the voice that produced it stopped talking
a second and a half ago.  The check that looks obvious is the one that never fires."
  (< (get-internal-real-time) (dict-quiet-until d)))

;;; The session dictation's state is also four special variables, because the Listen window, the
;;; control socket and the gates read them and a one-seat desktop must go on meaning what it
;;; meant.  They are a MIRROR of the object and never the storage — a further seat writes its own
;;; object and leaves these alone, which is what stops a second person's dictation from making
;;; the first person's window say it is dictating.
(defun %dict-session-p (d) (eq d *session-dictation*))

(defun %dict-set-quiet (d until)
  (setf (dict-quiet-until d) until)
  (when (%dict-session-p d) (setf *dictation-quiet-until* until)))

(defun %dict-mirror (d)
  (when (%dict-session-p d)
    (setf *dictating* (dict-on d)
          *dictation-typed* (dict-typed d)
          *dictation-muted* (dict-muted d)
          *dictation-last* (dict-last d))))

(defun %dictation-hear (d text)
  "One finished utterance, from the ear's decode thread.  Type it, or record that we did not.

Runs PASTE-TEXT asynchronously, which matters more here than it does for a paste: this is the
thread that decodes audio, and typing 60 characters at *PASTE-KEY-DELAY* holds it for a quarter
of a second — a quarter second in which the queue behind it fills with the next thing said.

The injector is THIS dictation's, so the keys land in the window ITS seat has focused.  Passing
NIL is not a failure to route: it is the session's keyboard, which is the only one a desktop with
one person has."
  (cond
    ((not (dict-on d)) nil)
    ((%dictation-deaf-p d)
     (incf (dict-muted d))
     (%dict-mirror d)
     nil)
    (t (let ((typed (dictation-text text)))
         (when (plusp (length typed))
           (setf (dict-last d) typed)
           (incf (dict-typed d))
           (%dict-mirror d)
           (paste-text typed :injector (dict-injector d))
           typed)))))

;;; ---- the switch --------------------------------------------------------------

(defun start-dictation (&key (ear *session-ears*) injector dictation name)
  "Type what an ear hears into whatever the person it belongs to has focused.  Returns the
DICTATION, or NIL if there is no ear.

Does NOT start the ear: an ear is a quarter of a gigabyte of weights and a sink on the mix, and
deciding to have one is a bigger decision than deciding where its words go.  Callers that want
both call START-LISTENING first — the Listen window's Dictate button does.

INJECTOR is the keyboard to type on — a seat's :ON-KEY.  NIL means the session's, which is what
one person has.  DICTATION reuses an existing object (a seat holds its own); with none, the
session's is used and created on demand, so the no-argument call is unchanged.

Idempotent.  Registering under a fixed key means a second call replaces the first rather than
installing a second listener that types every word twice."
  (let ((ear (or ear *session-ears*)))
    (when ear
      (let ((d (or dictation
                   (if (eq ear *session-ears*)
                       (or *session-dictation*
                           (setf *session-dictation*
                                 (make-instance 'dictation :name "session" :ear ear)))
                       (make-instance 'dictation :name (or name "seat") :ear ear)))))
        (setf (dict-ear d) ear
              (dict-on d) t
              (dict-typed d) 0
              (dict-muted d) 0
              (dict-last d) nil)
        (when injector (setf (dict-injector d) injector))
        ;; deaf for one tail-length at the start: whatever was already in flight when the
        ;; switch was thrown was heard before anyone asked for it to be typed
        (%dict-set-quiet d (+ (get-internal-real-time)
                              (round (* *dictation-tail-seconds*
                                        internal-time-units-per-second))))
        (%dict-mirror d)
        (hearing-listen (dict-key d) (lambda (text) (%dictation-hear d text)) ear)
        (unless (and (dict-watcher d) (sb-thread:thread-alive-p (dict-watcher d)))
          (setf (dict-watcher d)
                (sb-thread:make-thread (lambda () (%dictation-watch d))
                                       :name "glass-dictation-watch")))
        (when (%dict-session-p d) (setf *dictation-watcher* (dict-watcher d)))
        d))))

(defun stop-dictation (&key (ear *session-ears*) dictation)
  "Stop typing what is heard.  The ear keeps listening — the transcript is still worth having,
and this is only about where it goes.  Returns T if dictation had been on."
  (let ((d (or dictation *session-dictation*)))
    (if (null d)
        nil
        (let ((was (dict-on d)))
          (setf (dict-on d) nil)        ; the watcher's loop condition — it retires on its own
          (%dict-mirror d)
          (hearing-unlisten (dict-key d) (or (dict-ear d) ear *session-ears*))
          (let ((th (dict-watcher d)))
            (setf (dict-watcher d) nil)
            (when (%dict-session-p d) (setf *dictation-watcher* nil))
            (when th (ignore-errors (sb-thread:join-thread th :timeout 2))))
          (and was t)))))

(defun dictating-p (&optional (dictation *session-dictation*))
  "True if speech is currently reaching a keyboard.  Both halves have to be true — dictation
switched on, and something to type into — so a desktop with no server running answers NIL."
  (let ((d dictation))
    (and d (dict-on d) (or (dict-injector d) *key-injector*) t)))

(defun dictation-report (&optional (dictation *session-dictation*))
  "One line: on or off, what it has typed, and what it held back.

`on, no keyboard' is called out rather than folded into `on' because it is the failure that looks
most like success — dictation switched on, the ear hearing perfectly, and every word going
nowhere because no server ever filled *KEY-INJECTOR*."
  (let ((d dictation))
    (if (null d)
        "dictation: off"
        (format nil "dictation~@[ [~a]~]: ~a, ~d typed, ~d muted~@[ — last ~s~]"
                (unless (%dict-session-p d) (dict-name d))
                (cond ((not (dict-on d)) "off")
                      ((not (or (dict-injector d) *key-injector*))
                       "on but NO KEYBOARD (no server running)")
                      (t "on"))
                (dict-typed d) (dict-muted d) (dict-last d)))))
