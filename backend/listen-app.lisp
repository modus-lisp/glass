;;;; listen-app.lisp — a window that fills itself in, so the desktop's ear is usable by hand.
;;;;
;;;; speak-app.lisp is a box you type into and a button that says it out loud.  This is that
;;;; window run backwards: press Listen and the box fills up with whatever the session is saying.
;;;; The two are the same shape on purpose, because they are the same relationship seen from
;;;; either end — src/speech.lisp puts chord into the mix, src/hearing.lisp takes stave out of it,
;;;; and neither engine knows the other exists.
;;;;
;;;; WHAT IT HEARS IS THE SESSION, which on this box includes the session's own voice: open this
;;;; and speak-app side by side, type a sentence, press Speak, and it appears over here.  That is
;;;; not a demo cheat, it is the only microphone the machine has (four HDMI playbacks, no capture
;;;; device anywhere in /proc/asound).  A peer's microphone over WebRTC is a different SOURCE and
;;;; the same window.
;;;;
;;;; The window holds NO recognizer state.  The ear belongs to the SESSION — one sink on the mix
;;;; and one loaded model, however many windows are watching it — so Stop here stops it for
;;;; everything, and text may appear that this window never asked for.  The one thing it owns is
;;;; the text box, which is a normal editable text box: what lands in it is yours to correct,
;;;; select and copy, and the ear will not overwrite an utterance it has already finished.
;;;;
;;;; PRESSING LISTEN DOES NOT MEAN IT IS LISTENING YET.  A quarter of a gigabyte of weights has
;;;; to be read first, which takes long enough that a status line saying `Listening' immediately
;;;; would be a lie for the first half-minute — and anything said during it is said to nobody.
;;;; So the state has a LOADING step, and it is the reason GLASS:HEARING-READY-P exists.

(defpackage #:glass-listen
  (:use #:cl)
  (:export #:listen-box #:run #:register))

(in-package #:glass-listen)

(defun ui-font (&optional (size 14)) (clim:make-text-style :sans-serif :roman size))
(defun ui-bold (&optional (size 14)) (clim:make-text-style :sans-serif :bold size))

;;; ---- the frame -------------------------------------------------------------

(clim:define-application-frame listen-box ()
  ((note :initform nil :accessor app-note
         :documentation "Something to say about the last button press, shown instead of the
ear's own report until the ear has something newer to say.")
   (shown :initform nil :accessor app-shown
          :documentation "The status as last DRAWN — the ticker compares against it so a window
with nothing new to report repaints nothing.  See %START-TICKER.")
   (written :initform nil :accessor app-written
            :documentation "The transcript as last PUT IN THE BOX.  Compared for the same reason,
and for one more: writing the box on every tick would fight the person editing it.")
   (ticker :initform nil :accessor app-ticker))
  (:menu-bar nil)
  (:panes
   ;; NCOLUMNS is a minimum and Drei's columns are generous; 28 keeps the window inside the
   ;; default size below, and the transcript wraps and takes the rest of the height.
   (transcript :text-editor :value "" :nlines 6 :ncolumns 28 :text-style (ui-font 15))
   (listen :push-button :label "Listen" :text-style (ui-bold 14) :activate-callback 'on-listen)
   (stop   :push-button :label "Stop"   :text-style (ui-bold 14) :activate-callback 'on-stop)
   (clear  :push-button :label "Clear"  :text-style (ui-bold 14) :activate-callback 'on-clear)
   ;; A TOGGLE and not a push-button, on its own row, because it is not an action on this window —
   ;; it is a mode the whole desktop is in, and one whose effects land somewhere else entirely.
   (dictate :toggle-button :label "Dictate into the focused window" :value nil
            :text-style (ui-font 13) :value-changed-callback 'on-dictate)
   ;; :WIDTH and :MAX-WIDTH are not decoration.  An application pane asks for as much room as the
   ;; last thing drawn into it needed, and what is drawn here is the ear's own report — which
   ;; grows the moment there is anything to report.  Unpinned, the pane demands 680 px in a 560 px
   ;; window, the layout obliges, and the Clear button ends up past the right edge: still
   ;; clickable by a robot sending coordinates and unreachable by a person.  Pinned, the status
   ;; line stretches to the window and never past it, and DRAW-STATUS clips its text to fit.
   (status :application :display-function 'draw-status :scroll-bars nil
           :height 42 :max-height 42 :width 200 :max-width clim:+fill+
           :text-style (ui-font 12)))
  (:layouts
   (default
    (clim:vertically (:spacing 6)
      (:fill transcript)
      (clim:horizontally (:spacing 8) listen stop clear)
      dictate
      status))))

;;; ---- what the buttons do ---------------------------------------------------

(defun on-listen (gadget)
  "Start the session's ear.

START-LISTENING returns before a word is heard — the model is read on the ear's own thread — so
this callback does not block the window on the one operation here that takes a minute.  It is
idempotent and session-wide: pressing it twice, or in two windows, is one ear."
  (let ((frame (clim:pane-frame gadget)))
    (cond ((null glass:*hearing-models*)
           (setf (app-note frame)
                 "No ear installed — set GLASS_EARS to a directory of stave graphs."))
          (t (setf (app-note frame) nil)
             (glass:start-listening)))))

(defun on-stop (gadget)
  "Stop the ear, keeping what it heard.  Guarded on an ear EXISTING rather than calling
GLASS:STOP-LISTENING's default for nothing."
  (let ((frame (clim:pane-frame gadget)))
    (setf (app-note frame) nil)
    (when glass:*session-ears* (glass:stop-listening))))

(defun on-clear (gadget)
  "Empty the box and the transcript behind it.

Both, and in that order: clearing only the box would leave the ear's own transcript to reappear
on the next tick, which reads as a window that ignores its own button."
  (let ((frame (clim:pane-frame gadget)))
    (glass:hearing-clear)
    (setf (app-note frame) nil
          (app-written frame) ""
          (clim:gadget-value (clim:find-pane-named frame 'transcript)) "")))

(defun on-dictate (gadget value)
  "Switch the desktop between watching what it hears and TYPING it.

Starts the ear if there is not one, because a Dictate that quietly did nothing until you also
pressed Listen would be a switch that lies.  Off is the reverse ONLY as far as dictation goes —
it leaves the ear running, since the transcript is still worth having and stopping it is what the
Stop button is for.

The note is the important part of this callback.  Dictation types into whatever has focus, and
what has focus at the moment you press this is almost certainly THIS window, whose box is already
being written by the ticker.  So it says where to click."
  (let ((frame (clim:pane-frame gadget)))
    (cond
      ((not value)
       (glass:stop-dictation)
       (setf (app-note frame) nil))
      ((null glass:*hearing-models*)
       (setf (clim:gadget-value gadget :invoke-callback nil) nil
             (app-note frame) "No ear installed — set GLASS_EARS to a directory of stave graphs."))
      ((null glass:*key-injector*)
       ;; a desktop with no server has nothing to type into, and dictation would look like it
       ;; worked while every word went nowhere
       (setf (clim:gadget-value gadget :invoke-callback nil) nil
             (app-note frame) "Nothing to type into — no VNC server is running on this session."))
      (t
       (glass:start-listening)
       (glass:start-dictation)
       (setf (app-note frame)
             "Dictating — click the window the words should go into.")))))

;;; ---- the status line -------------------------------------------------------

(defun %ear ()
  "The session's ear IF one exists — never one made on demand.  Drawing a status line must not be
what puts a sink on the mixer and reads a quarter of a gigabyte of weights."
  glass:*session-ears*)

(defun %state ()
  "The headline: what a glance should tell you."
  (cond ((null glass:*hearing-models*) "No ear installed")
        ((null (%ear)) "Idle")
        ((not (glass:listening-p (%ear))) "Stopped")
        ((not (glass:hearing-ready-p (%ear))) "Loading model...")
        ;; dictation outranks `Hearing...' deliberately: where the words are GOING is the more
        ;; surprising fact about the desktop, and the one you want to see at a glance before you
        ;; start talking near it
        ((glass:dictating-p) (if (plusp (length (glass:hearing-partial (%ear))))
                                 "Dictating..."
                                 "Dictating"))
        ((plusp (length (glass:hearing-partial (%ear)))) "Hearing...")
        (t "Listening")))

(defun %detail (frame)
  (or (app-note frame)
      (if glass:*hearing-models*
          (glass:hearing-report (%ear))
          "set GLASS_EARS (or glass:*hearing-models*) to a directory of stave graphs")))

(defun %status (frame)
  ;; the level is in here so the meter is part of what `has anything changed' means — but rounded
  ;; to the pixel it will be drawn at, so an idle room's noise floor does not repaint forever
  (list (%state) (%detail frame) (round (* 100 (%meter)))))

(defun %meter ()
  "The input level as a bar length in [0, 1].  Square-rooted because speech sits around a
hundredth of full scale and a linear meter of it is a flat line."
  (let ((level (if (%ear) (glass:hearing-level (%ear)) 0d0)))
    (min 1d0 (sqrt (* 4d0 (max 0d0 level))))))

(defun %fit (stream text style width)
  "TEXT, shortened until it fits in WIDTH — measured, not counted in characters, because the font
is proportional and an estimate is wrong in whichever direction hurts.  Nothing is lost that was
not going to be clipped anyway; what is gained is a pane that does not ask the layout to grow."
  (if (<= (clim:text-size stream text :text-style style) width)
      text
      (let ((cut (concatenate 'string (subseq text 0 (max 0 (1- (length text)))) "...")))
        (loop while (and (> (length cut) 4)
                         (> (clim:text-size stream cut :text-style style) width))
              do (setf cut (concatenate 'string (subseq cut 0 (- (length cut) 4)) "...")))
        cut)))

(defun draw-status (frame stream)
  (let* ((status (%status frame))
         (state (first status))
         (hot (member state '("Hearing..." "Listening" "Dictating" "Dictating...")
                      :test #'string=)))
    (setf (app-shown frame) status)
    (clim:draw-text* stream state 4 16
                     :text-style (ui-bold 14)
                     :ink (cond ((null glass:*hearing-models*) clim:+dark-red+)
                                ((glass:dictating-p) (clim:make-rgb-color 0.75 0.35 0.0))
                                ((string= state "Hearing...") clim:+dark-green+)
                                (t clim:+black+)))
    ;; the meter, right of the headline: a track, the level, and a tick at the threshold the gate
    ;; actually opens on — so a room too quiet to trigger it looks too quiet rather than broken
    (let* ((x 130) (w 120) (y 10) (h 8)
           (gate (min 1d0 (sqrt (* 4d0 glass:*hearing-threshold*)))))
      (clim:draw-rectangle* stream x y (+ x w) (+ y h) :ink (clim:make-gray-color 0.85))
      (when hot
        (clim:draw-rectangle* stream x y (+ x (* w (%meter))) (+ y h)
                              :ink (clim:make-rgb-color 0.15 0.55 0.2)))
      (clim:draw-line* stream (+ x (* w gate)) (- y 2) (+ x (* w gate)) (+ y h 2)
                       :ink (clim:make-gray-color 0.45)))
    (let ((room (- (clim:bounding-rectangle-width (clim:sheet-region stream)) 8)))
      (clim:draw-text* stream (%fit stream (second status) (ui-font 11) room) 4 33
                       :text-style (ui-font 11) :ink (clim:make-gray-color 0.35)))))

;;; ---- keeping it current ----------------------------------------------------

(defun %wrap (pane text style)
  "TEXT with a newline wherever it would otherwise run off the right edge of PANE.

Drei will not do this for us: AUTO-FILL-MODE fills a line as it is TYPED, and nothing here is
typed — the whole point of the window is that the text arrives on its own.  So the wrapping is
ours to do, measured against the pane rather than counted in characters, because the font is
proportional.  A transcript is a stream of words with no line structure of its own, which is the
one kind of text it is safe to break anywhere there is a space."
  (let ((room (- (clim:bounding-rectangle-width (clim:sheet-region pane)) 12)))
    (if (or (<= room 0) (zerop (length text)))
        text
        (with-output-to-string (out)
          (let ((line ""))
            (dolist (word (%words text))
              (let ((try (if (zerop (length line)) word (concatenate 'string line " " word))))
                (cond ((<= (clim:text-size pane try :text-style style) room) (setf line try))
                      ((zerop (length line))            ; one word wider than the pane: let it go
                       (write-string word out) (terpri out))
                      (t (write-string line out) (terpri out) (setf line word)))))
            (write-string line out))))))

(defun %words (text)
  (let ((words '()) (start nil))
    (dotimes (i (length text))
      (let ((space (member (char text i) '(#\Space #\Tab #\Newline #\Return))))
        (cond ((and space start) (push (subseq text start i) words) (setf start nil))
              ((and (not space) (null start)) (setf start i)))))
    (when start (push (subseq text start) words))
    (nreverse words)))

(define-listen-box-command (com-tick) ()
  ;; The frame's top level redisplays its panes after every command, which is what keeps the
  ;; status line current; the transcript is a gadget and has to be written.  Both happen HERE,
  ;; in the frame's own process, which is the whole reason this is a command.
  ;;
  ;; What is REMEMBERED is the unwrapped text, and what is WRITTEN is the wrapped copy: comparing
  ;; the wrapped one would make every resize look like new speech.
  (let* ((frame clim:*application-frame*)
         (text (glass:hearing-text)))
    ;; The Dictate button is a view of a DESKTOP-WIDE mode, not a thing this window owns, and the
    ;; mode can go off without anyone touching the button — pressing Stop takes the ear away, and
    ;; dictation goes with it.  So the toggle follows the desktop here rather than remembering
    ;; what it was last clicked to.  :INVOKE-CALLBACK NIL because this is the button catching up
    ;; with the truth, not a person asking for something.
    (let ((toggle (clim:find-pane-named frame 'dictate)))
      (unless (eq (and (clim:gadget-value toggle) t) glass:*dictating*)
        (setf (clim:gadget-value toggle :invoke-callback nil) glass:*dictating*)))
    (unless (equal text (app-written frame))
      (let ((pane (clim:find-pane-named frame 'transcript)))
        (setf (app-written frame) text
              (clim:gadget-value pane) (%wrap pane text (ui-font 15)))
        ;; and put the cursor at the end, so a transcript longer than the box shows the words that
        ;; just arrived rather than the ones from a minute ago
        (ignore-errors
         (let ((view (drei::view pane)))
           (setf (drei-buffer:offset (drei:point view))
                 (drei-buffer:size (drei:buffer view)))))))))

(define-listen-box-command (com-focus) ()
  ;; The box is editable, so point the keyboard at it the way speak-app does.  BOTH halves are
  ;; needed: STREAM-SET-INPUT-FOCUS routes key events here and ARMED-CALLBACK is what makes Drei
  ;; treat them as editing rather than drop them.
  (ignore-errors
   (let ((pane (clim:find-pane-named clim:*application-frame* 'transcript)))
     (clim:stream-set-input-focus pane)
     (clim:armed-callback pane (clim:gadget-client pane) (clim:gadget-id pane)))))

(defun %start-ticker (frame)
  "Redisplay when — and only when — there is something different to show.

EXECUTE-FRAME-COMMAND from another thread appends to the frame's event queue rather than running
anything here, which is what makes this safe.  The comparison matters as much: on this stack an
idle repaint three times a second is real damage, a recomposite and bytes on every VNC connection
for a window saying `Listening' about a silent room."
  (sb-thread:make-thread
   (lambda ()
     ;; after a beat: the panes are not realized when the top level starts, and focusing a pane
     ;; with no port yet is a no-op that looks like it worked
     (sleep 0.5)
     (ignore-errors (clim:execute-frame-command frame '(com-focus)))
     (loop
       (sleep 0.3)
       (unless (member (clim:frame-state frame) '(:enabled :shrunk)) (return))
       (handler-case
           (unless (and (equal (%status frame) (app-shown frame))
                        (equal (glass:hearing-text) (app-written frame)))
             (clim:execute-frame-command frame '(com-tick)))
         ;; the window is gone, or going: stop, quietly.  An escaping condition in any thread is
         ;; fatal under --disable-debugger, which is how the desktop runs.
         (serious-condition () (return)))))
   :name "listen-app tick"))

(defmethod clim:run-frame-top-level :around ((frame listen-box) &key)
  (setf (app-ticker frame) (%start-ticker frame))
  (unwind-protect (call-next-method)
    ;; the ear is NOT stopped here: it belongs to the session, another window may be watching it,
    ;; and closing a viewer is not a statement about whether the desktop is listening
    (ignore-errors (sb-thread:terminate-thread (app-ticker frame)))))

;;; ---- launching -------------------------------------------------------------

(defun run (&key (width 560) (height 360))
  "Run the window standalone (its own top level), for testing outside the desktop."
  (clim:run-frame-top-level (clim:make-application-frame 'listen-box :width width :height height)))

(defun register (&key (label "Listen") (width 560) (height 400))
  "Put the window in the glass desktop's root menu.  Found by name, so loading this system in an
image without the glass backend is not an error — the window is still usable through RUN."
  (let ((fn (and (find-package "CLIM-GLASS") (find-symbol "REGISTER-APP" "CLIM-GLASS"))))
    (when (and fn (fboundp fn))
      (funcall fn label (list 'listen-box :width width :height height :title label))
      label)))
