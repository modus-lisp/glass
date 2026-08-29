;;;; speak-app.lisp — a window to type into, so the desktop's voice is usable by hand.
;;;;
;;;; src/speech.lisp gives the IMAGE a voice: SPEAK is a function, which is what a program
;;;; wants.  This is the voice for a person — a text box, a Speak button, and one line that says
;;;; what the voice is doing.
;;;;
;;;; It is a McCLIM frame rather than a glass surface because the typing IS the app, and Drei is
;;;; already a text editor: clicking the box takes the keyboard on its own (drei-clim.lisp's
;;;; pointer-press handler calls STREAM-SET-INPUT-FOCUS), which is the one piece of this that
;;;; would otherwise have had to be written.
;;;;
;;;; The window holds NO speech state.  The voice belongs to the SESSION, so what is typed here
;;;; is heard by every listener the session has — a VNC viewer, a WebRTC peer, a recorder — and
;;;; utterances queued from anywhere else in the image share the same one-at-a-time voice.  Two
;;;; consequences worth knowing before they surprise anyone: closing this window mid-sentence
;;;; does not stop the sentence (Hush does), and the status line reports the whole session's
;;;; voice, including speech this window never queued.

(defpackage #:glass-speak
  (:use #:cl)
  (:export #:speak-box #:run #:register))

(in-package #:glass-speak)

(defun ui-font (&optional (size 14)) (clim:make-text-style :sans-serif :roman size))
(defun ui-bold (&optional (size 14)) (clim:make-text-style :sans-serif :bold size))

;;; ---- the frame -------------------------------------------------------------

(clim:define-application-frame speak-box ()
  ((note :initform nil :accessor app-note
         :documentation "Something to say about the last button press, shown instead of the
voice's own report until the voice has something newer to say.")
   (shown :initform nil :accessor app-shown
          :documentation "The status as last DRAWN.  The ticker compares against it, so a window
that has nothing new to report repaints nothing — see %START-TICKER.")
   (ticker :initform nil :accessor app-ticker))
  (:menu-bar nil)
  (:panes
   ;; NCOLUMNS is a MINIMUM width, and Drei's idea of a column is generous (about 18 px at this
   ;; size).  Ask for more than the window is given and the layout is simply wider than the
   ;; framebuffer — the last button ends up off the right edge, still clickable by a robot
   ;; sending coordinates and invisible to a person.  28 columns keeps the whole window inside
   ;; the default size below; the box wraps and takes the rest of the height anyway.
   (input :text-editor :value "" :nlines 4 :ncolumns 28 :text-style (ui-font 15))
   (say    :push-button :label "Speak" :text-style (ui-bold 14) :activate-callback 'on-speak)
   (hush   :push-button :label "Hush"  :text-style (ui-bold 14) :activate-callback 'on-hush)
   (clear  :push-button :label "Clear" :text-style (ui-bold 14) :activate-callback 'on-clear)
   (status :application :display-function 'draw-status :scroll-bars nil :height 42
           :text-style (ui-font 12)))
  (:layouts
   (default
    (clim:vertically (:spacing 6)
      (:fill input)
      (clim:horizontally (:spacing 8) say hush clear)
      status))))

;;; ---- what the buttons do ---------------------------------------------------

(defun %input-text (frame)
  (string-trim '(#\Space #\Tab #\Newline #\Return)
               (string (clim:gadget-value (clim:find-pane-named frame 'input)))))

(defun on-speak (gadget)
  "Queue what is in the box.

SPEAK returns before a word is said — synthesis is on the speaker's own thread — so this
callback does not block the window even on the first utterance, which also loads a 60 MB voice.
The box is deliberately NOT cleared: the common thing after hearing a sentence is to fix a word
and say it again."
  (let* ((frame (clim:pane-frame gadget))
         (text (%input-text frame)))
    (cond ((zerop (length text))
           (setf (app-note frame) "Nothing typed yet — type something, then press Speak."))
          (t
           (setf (app-note frame) nil)
           (glass:speak text)))))

(defun on-hush (gadget)
  "Stop talking.  Guarded on a speaker EXISTING rather than calling GLASS:HUSH's default, which
would create the session voice — and a source on the mix — in order to tell it to be quiet."
  (let ((frame (clim:pane-frame gadget)))
    (setf (app-note frame) nil)
    (when glass:*session-speaker* (glass:hush glass:*session-speaker*))))

(defun on-clear (gadget)
  (let ((frame (clim:pane-frame gadget)))
    (setf (app-note frame) nil
          (clim:gadget-value (clim:find-pane-named frame 'input)) "")))

;;; ---- the status line -------------------------------------------------------

(defun %speaker ()
  "The session's speaker IF one exists — never one made on demand.  Drawing a status line must
not be what gives a desktop a voice (and a source on its mixer)."
  glass:*session-speaker*)

(defun %state ()
  "The headline: what a glance should tell you."
  (cond ((null (glass:speech-voice)) "No voice installed")
        ((and (%speaker) (glass:speaking-p (%speaker))) "Speaking...")
        (t "Ready")))

(defun %detail (frame)
  (or (app-note frame)
      (if (glass:speech-voice)
          (glass:speech-report (%speaker))
          "set GLASS_VOICE (or glass:*speech-voice*) to a chord .graph")))

(defun %status (frame) (cons (%state) (%detail frame)))

(defun draw-status (frame stream)
  (let ((status (%status frame)))
    (setf (app-shown frame) status)
    (clim:draw-text* stream (car status) 4 16
                     :text-style (ui-bold 14)
                     :ink (cond ((null (glass:speech-voice)) clim:+dark-red+)
                                ((string= (car status) "Speaking...") clim:+dark-green+)
                                (t clim:+black+)))
    (clim:draw-text* stream (cdr status) 4 33
                     :text-style (ui-font 11) :ink (clim:make-gray-color 0.35))))

;;; ---- keeping it current ----------------------------------------------------

(define-speak-box-command (com-tick) ()
  ;; Nothing: the frame's top level redisplays its panes after every command, so being a
  ;; command is this one's whole job (same trick as spool's transport).
  nil)

(define-speak-box-command (com-focus) ()
  ;; Point the keyboard at the box so the window is typeable the moment it opens, rather than
  ;; after a click nobody told the user about.  It runs as a COMMAND because focus belongs to the
  ;; frame's own thread, and the ticker that asks for it does not.
  ;;
  ;; BOTH halves are needed, which is the part worth writing down: STREAM-SET-INPUT-FOCUS is what
  ;; routes key events here, and ARMED-CALLBACK is what makes Drei treat them as editing rather
  ;; than ignore them.  A click does both (drei-clim.lisp's pointer-press :before method); doing
  ;; only the first gives a window that looks focused and silently drops everything typed.
  (ignore-errors
   (let ((pane (clim:find-pane-named clim:*application-frame* 'input)))
     (clim:stream-set-input-focus pane)
     (clim:armed-callback pane (clim:gadget-client pane) (clim:gadget-id pane)))))

(defun %start-ticker (frame)
  "Redisplay the status when — and only when — it would say something different.

EXECUTE-FRAME-COMMAND from another thread APPENDS to the frame's event queue rather than running
anything here, which is what makes this safe: the redisplay still happens in the frame's own
process.  The comparison matters as much: on this stack an idle repaint once a second is real
damage, a recomposite and bytes on every VNC connection, forever, for a window saying `Ready'."
  (sb-thread:make-thread
   (lambda ()
     ;; after a beat, not immediately: the panes are not realized when the top level starts, and
     ;; focusing a pane with no port yet is a no-op that looks like it worked
     (sleep 0.5)
     (ignore-errors (clim:execute-frame-command frame '(com-focus)))
     (loop
       (sleep 0.3)
       (unless (member (clim:frame-state frame) '(:enabled :shrunk)) (return))
       (handler-case
           (unless (equal (%status frame) (app-shown frame))
             (clim:execute-frame-command frame '(com-tick)))
         ;; the window is gone, or going: stop, quietly.  An escaping condition in any thread
         ;; is fatal under --disable-debugger, which is how the desktop runs.
         (serious-condition () (return)))))
   :name "speak-app tick"))

(defmethod clim:run-frame-top-level :around ((frame speak-box) &key)
  (setf (app-ticker frame) (%start-ticker frame))
  (unwind-protect (call-next-method)
    ;; the thread notices the frame is gone on its own within a tick; this just makes it prompt
    (ignore-errors (sb-thread:terminate-thread (app-ticker frame)))))

;;; ---- launching -------------------------------------------------------------

(defun run (&key (width 560) (height 320))
  "Run the window standalone (its own top level), for testing outside the desktop."
  (clim:run-frame-top-level (clim:make-application-frame 'speak-box :width width :height height)))

(defun register (&key (label "Speak") (width 560) (height 320))
  "Put the window in the glass desktop's root menu.  Found by name, so loading this system in an
image without the glass backend is not an error — the window is still usable through RUN."
  (let ((fn (and (find-package "CLIM-GLASS") (find-symbol "REGISTER-APP" "CLIM-GLASS"))))
    (when (and fn (fboundp fn))
      (funcall fn label (list 'speak-box :width width :height height :title label))
      label)))
