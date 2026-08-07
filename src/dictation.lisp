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

(defvar *dictating* nil
  "True while the ear is typing what it hears.  Read it rather than a window's button: the switch
is session-wide, because the keyboard is.")

(defvar *dictation-typed* 0
  "Utterances typed since the last START-DICTATION.  The honest counter — an utterance suppressed
by the self-hearing guard is counted separately, not here.")

(defvar *dictation-muted* 0
  "Utterances dropped because the desktop was speaking.  Kept because the alternative — dropping
them silently — makes a real dictation failure look identical to the guard doing its job.")

(defvar *dictation-last* nil
  "The last text dictation actually typed, or NIL.  For a status line, and for a test that wants
to know what landed without racing the app it landed in.")

(defvar *dictation-key* "dictation"
  "The key this registers under on the ear.  Named, and re-registering replaces, so switching
dictation on twice types everything once.")

;;; ---- what gets typed --------------------------------------------------------

(defun dictation-text (text)
  "TEXT as it should be TYPED: sentence-cased, one trailing space.

The recognizer's `AFTER EARLY NIGHTFALL THE YELLOW LAMPS WOULD LIGHT UP' becomes `After early
nightfall the yellow lamps would light up '.  Pure — no ear, no keyboard — so the shape of the
output can be checked without a desktop.

The trailing space is what makes consecutive utterances read as prose instead of running
together; it is also why this does NOT capitalize after a period, since there are no periods.
Lowercasing is lossy for proper nouns, and unavoidable: the recognizer did not tell us which
words were names, and inventing that would be inventing a fact."
  (let ((s (string-trim '(#\Space #\Tab #\Newline #\Return) (or text ""))))
    (if (zerop (length s))
        ""
        (let ((low (string-downcase s)))
          (setf (char low 0) (char-upcase (char low 0)))
          (concatenate 'string low " ")))))

;;; ---- when it is safe to type ------------------------------------------------

(defvar *dictation-quiet-until* 0
  "INTERNAL-REAL-TIME before which dictation stays deaf.  Pushed forward by the watcher below for
as long as the voice is talking, so it covers the tail after the voice stops as well as the
speech itself.")

(defvar *dictation-watcher* nil
  "The thread that keeps *DICTATION-QUIET-UNTIL* honest.")

(defparameter *dictation-watch-step* 0.1d0
  "How often the watcher samples the voice.  It has to be well under a sentence: the whole job is
to have SEEN the voice live at some point during it.")

(defun %dictation-watch ()
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
      (loop while *dictating*
            do (when (%dictation-speaking-p)
                 (setf *dictation-quiet-until*
                       (+ (get-internal-real-time)
                          (round (* *dictation-tail-seconds* internal-time-units-per-second)))))
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

(defun %dictation-deaf-p ()
  "True if this utterance must be dropped rather than typed.

Reads the window the watcher maintains rather than sampling the voice here, for the reason given
in %DICTATION-WATCH: at the moment an utterance lands, the voice that produced it stopped talking
a second and a half ago.  The check that looks obvious is the one that never fires."
  (< (get-internal-real-time) *dictation-quiet-until*))

(defun %dictation-hear (text)
  "One finished utterance, from the ear's decode thread.  Type it, or record that we did not.

Runs PASTE-TEXT asynchronously, which matters more here than it does for a paste: this is the
thread that decodes audio, and typing 60 characters at *PASTE-KEY-DELAY* holds it for a quarter
of a second — a quarter second in which the queue behind it fills with the next thing said."
  (cond
    ((not *dictating*) nil)
    ((%dictation-deaf-p)
     (incf *dictation-muted*)
     nil)
    (t (let ((typed (dictation-text text)))
         (when (plusp (length typed))
           (setf *dictation-last* typed)
           (incf *dictation-typed*)
           (paste-text typed)
           typed)))))

;;; ---- the switch --------------------------------------------------------------

(defun start-dictation (&key (ear *session-ears*))
  "Type what the desktop hears into whatever has focus.  Returns the EAR, or NIL if there is none.

Does NOT start the ear: an ear is a quarter of a gigabyte of weights and a sink on the mix, and
deciding to have one is a bigger decision than deciding where its words go.  Callers that want
both call START-LISTENING first — the Listen window's Dictate button does.

Idempotent.  Registering under a fixed key means a second call replaces the first rather than
installing a second listener that types every word twice."
  (let ((ear (or ear *session-ears*)))
    (when ear
      (setf *dictating* t *dictation-typed* 0 *dictation-muted* 0 *dictation-last* nil
            ;; deaf for one tail-length at the start: whatever was already in flight when the
            ;; switch was thrown was heard before anyone asked for it to be typed
            *dictation-quiet-until* (+ (get-internal-real-time)
                                       (round (* *dictation-tail-seconds*
                                                 internal-time-units-per-second))))
      (hearing-listen *dictation-key* #'%dictation-hear ear)
      (unless (and *dictation-watcher* (sb-thread:thread-alive-p *dictation-watcher*))
        (setf *dictation-watcher*
              (sb-thread:make-thread #'%dictation-watch :name "glass-dictation-watch")))
      ear)))

(defun stop-dictation (&key (ear *session-ears*))
  "Stop typing what is heard.  The ear keeps listening — the transcript is still worth having,
and this is only about where it goes.  Returns T if dictation had been on."
  (let ((was *dictating*))
    (setf *dictating* nil)              ; the watcher's loop condition — it retires on its own
    (hearing-unlisten *dictation-key* (or ear *session-ears*))
    (let ((th *dictation-watcher*))
      (setf *dictation-watcher* nil)
      (when th (ignore-errors (sb-thread:join-thread th :timeout 2))))
    (and was t)))

(defun dictating-p ()
  "True if speech is currently reaching the keyboard.  Both halves have to be true — dictation
switched on, and something to type into — so a desktop with no server running answers NIL."
  (and *dictating* *key-injector* t))

(defun dictation-report ()
  "One line: on or off, what it has typed, and what it held back.

`on, no keyboard' is called out rather than folded into `on' because it is the failure that looks
most like success — dictation switched on, the ear hearing perfectly, and every word going
nowhere because no server ever filled *KEY-INJECTOR*."
  (format nil "dictation: ~a, ~d typed, ~d muted~@[ — last ~s~]"
          (cond ((not *dictating*) "off")
                ((not *key-injector*) "on but NO KEYBOARD (no server running)")
                (t "on"))
          *dictation-typed* *dictation-muted* *dictation-last*))
