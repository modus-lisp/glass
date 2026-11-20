;;;; voice-apps.lisp — Speak and Listen as desk applications: the desktop's voice and ear by hand, with no
;;;; McCLIM.
;;;;
;;;; They were CLIM frames (backend/speak-app.lisp, backend/listen-app.lisp), and the frames held no engine
;;;; state: the voice and the ear belong to whoever provides them.  That is kept, and made explicit, as a
;;;; small protocol: *VOICE* says and hushes, *EAR* listens and keeps a transcript.  By default they are the
;;;; SESSION's -- glass/speech (chord) and glass/hearing (stave), looked up by name, so a desktop without them
;;;; is still a working desktop -- and a host with a voice of its own installs that instead: the phone's is
;;;; the platform's (AVSpeechSynthesizer and SFSpeechRecognizer, vessel's speech.lisp).
;;;;
;;;; Both apps are the desk's MAKE-FN contract (desk-apps.lisp): ON-KEY ON-POINTER DIRTY-P COPY-P CLOSE-FN
;;;; RESIZE-FN TEXT-P, drawing into the window's framebuffer.  Status is polled on every DIRTY-P call and the
;;;; window repaints only when what it shows has changed.

(in-package #:glass.desk)

(export '(*voice* *ear* voice-say voice-hush voice-state voice-report
          ear-start ear-stop ear-clear ear-state ear-report ear-text ear-level
          session-voice session-ear speak-app listen-app *voice-apps*))

;;; ---- the protocol ------------------------------------------------------------------------------------

(defgeneric voice-say (voice text) (:documentation "Queue TEXT and return at once."))
(defgeneric voice-hush (voice) (:documentation "Stop talking."))
(defgeneric voice-state (voice) (:documentation "The headline: \"Speaking...\", \"Ready\", \"No voice installed\"."))
(defgeneric voice-report (voice) (:documentation "A line of detail under the headline, or NIL."))

(defgeneric ear-start (ear) (:documentation "Start listening; return at once (a model may still be loading)."))
(defgeneric ear-stop (ear) (:documentation "Stop listening, keeping the transcript."))
(defgeneric ear-clear (ear) (:documentation "Forget the transcript."))
(defgeneric ear-state (ear) (:documentation "The headline: \"Listening\", \"Loading model...\", \"Idle\"..."))
(defgeneric ear-report (ear) (:documentation "A line of detail under the headline, or NIL."))
(defgeneric ear-text (ear) (:documentation "The transcript so far: what was finished, then what is being heard."))
(defgeneric ear-level (ear) (:documentation "The input level, 0 to 1 of full scale."))

(defun %glass-fn (name)
  "glass's function NAME, when the system that defines it is loaded; else NIL."
  (let ((s (and (find-package "GLASS") (find-symbol name "GLASS"))))
    (and s (fboundp s) (symbol-function s))))

(defun %glass-var (name)
  (let ((s (and (find-package "GLASS") (find-symbol name "GLASS"))))
    (and s (boundp s) (symbol-value s))))

;;; ---- the session's voice and ear: glass/speech and glass/hearing, when loaded --------------------------

(defclass session-voice () ()
  (:documentation "The session's voice: chord on the session mixer (glass/speech), when it is loaded."))

(defmethod voice-say ((v session-voice) text)
  (let ((f (%glass-fn "SPEAK"))) (when f (funcall f text))))

(defmethod voice-hush ((v session-voice))
  ;; only a speaker that EXISTS: hushing must not be what gives a desktop a voice and a source on its mixer
  (let ((speaker (%glass-var "*SESSION-SPEAKER*")) (f (%glass-fn "HUSH")))
    (when (and speaker f) (funcall f speaker))))

(defmethod voice-state ((v session-voice))
  (let ((voice (%glass-fn "SPEECH-VOICE")) (speaking (%glass-fn "SPEAKING-P"))
        (speaker (%glass-var "*SESSION-SPEAKER*")))
    (cond ((or (null voice) (null (funcall voice))) "No voice installed")
          ((and speaker speaking (funcall speaking speaker)) "Speaking...")
          (t "Ready"))))

(defmethod voice-report ((v session-voice))
  (let ((voice (%glass-fn "SPEECH-VOICE")) (report (%glass-fn "SPEECH-REPORT")))
    (if (and voice (funcall voice))
        (and report (funcall report (%glass-var "*SESSION-SPEAKER*")))
        "set GLASS_VOICE (or glass:*speech-voice*) to a chord .graph")))

(defclass session-ear () ()
  (:documentation "The session's ear: stave on the session mixer (glass/hearing), when it is loaded."))

(defun %ears () (%glass-var "*SESSION-EARS*"))

(defmethod ear-start ((e session-ear))
  (let ((models (%glass-fn "HEARING-MODELS")) (start (%glass-fn "START-LISTENING")))
    (when (and models start (funcall models)) (funcall start))))

(defmethod ear-stop ((e session-ear))
  (let ((stop (%glass-fn "STOP-LISTENING"))) (when (and stop (%ears)) (funcall stop))))

(defmethod ear-clear ((e session-ear))
  (let ((clear (%glass-fn "HEARING-CLEAR"))) (when clear (funcall clear))))

(defmethod ear-state ((e session-ear))
  (let ((models (%glass-fn "HEARING-MODELS")) (ears (%ears)))
    (cond ((or (null models) (null (funcall models))) "No ear installed")
          ((null ears) "Idle")
          ((not (funcall (%glass-fn "LISTENING-P") ears)) "Stopped")
          ((not (funcall (%glass-fn "HEARING-READY-P") ears)) "Loading model...")
          ((plusp (length (funcall (%glass-fn "HEARING-PARTIAL") ears))) "Hearing...")
          (t "Listening"))))

(defmethod ear-report ((e session-ear))
  (let ((models (%glass-fn "HEARING-MODELS")) (report (%glass-fn "HEARING-REPORT")))
    (if (and models (funcall models))
        (and report (funcall report (%ears)))
        "set GLASS_EARS (or glass:*hearing-models*) to a directory of stave graphs")))

(defmethod ear-text ((e session-ear))
  (let ((text (%glass-fn "HEARING-TEXT")))
    (or (and text (%ears) (funcall text (%ears))) "")))

(defmethod ear-level ((e session-ear))
  (let ((level (%glass-fn "HEARING-LEVEL")))
    (if (and level (%ears)) (min 1d0 (max 0d0 (funcall level (%ears)))) 0d0)))

(defvar *voice* (make-instance 'session-voice)
  "The voice the Speak app says through.  The session's (glass/speech) unless a host installs its own.")
(defvar *ear* (make-instance 'session-ear)
  "The ear the Listen app shows.  The session's (glass/hearing) unless a host installs its own.")

;;; ---- drawing: buttons, a status line, a box of wrapped text ---------------------------------------------

(defparameter +va-bg+ #x10141a)
(defparameter +va-paper+ #x1c222b)
(defparameter +va-ink+ #xe6ebf2)
(defparameter +va-dim+ #x8b949e)
(defparameter +va-button+ #x2f3b4d)
(defparameter +va-accent+ #x5abe82)
(defparameter +va-caret+ #xe0a800)

(defstruct (%va-button (:constructor %va-button (label x y w h))) label x y w h)

(defun %va-buttons (labels y)
  "A row of buttons labelled LABELS at Y, left to right."
  (let ((x 12))
    (loop for label in labels
          collect (prog1 (%va-button label x y 88 34) (incf x 96)))))

(defun %va-hit (buttons x y)
  (find-if (lambda (b) (and (<= (%va-button-x b) x (+ (%va-button-x b) (%va-button-w b)))
                            (<= (%va-button-y b) y (+ (%va-button-y b) (%va-button-h b)))))
           buttons))

(defun %va-draw-buttons (fb buttons)
  (dolist (b buttons)
    (glass:fb-rect fb (%va-button-x b) (%va-button-y b) (%va-button-w b) (%va-button-h b) +va-button+)
    (let ((tw (glass:text-width (%va-button-label b) :size 15 :font (glass:default-font t))))
      (glass:fb-text fb (+ (%va-button-x b) (floor (- (%va-button-w b) tw) 2)) (+ (%va-button-y b) 8)
                     (%va-button-label b) :size 15 :color +va-ink+ :font (glass:default-font t)))))

(defun %va-draw-box (fb text y h &key caret)
  "TEXT wrapped in a box from Y, H tall; its last rows when it is longer than the box.  CARET: a caret
   after the last character."
  (let* ((w (glass:fb-width fb)) (size 16) (lh 21) (font (glass:default-font))
         (rows (loop for line in (%va-lines text) append (or (%wrap line (- w 40) size font) (list ""))))
         (fit (max 1 (floor (- h 16) lh)))
         (shown (last rows fit)))
    (glass:fb-rect fb 12 y (- w 24) h +va-paper+)
    (loop for row in shown for k from 0
          do (glass:fb-text fb 20 (+ y 8 (* k lh)) row :size size :color +va-ink+ :font font))
    (when caret
      (let* ((last-row (or (car (last shown)) ""))
             (cx (+ 20 (glass:text-width last-row :size size :font font)))
             (cy (+ y 8 (* (max 0 (1- (length shown))) lh))))
        (glass:fb-rect fb cx cy 2 (+ size 2) +va-caret+)))))

(defun %va-lines (text)
  (let ((lines '()) (start 0))
    (loop for k from 0 below (length text)
          when (char= (char text k) #\Newline)
            do (push (subseq text start k) lines) (setf start (1+ k)))
    (push (subseq text start) lines)
    (nreverse lines)))

(defun %va-draw-status (fb y headline detail)
  (glass:fb-text fb 12 y headline :size 15 :color +va-ink+ :font (glass:default-font t))
  (when detail
    (glass:fb-text fb 12 (+ y 22) (%va-clip detail (- (glass:fb-width fb) 24) 12) :size 12 :color +va-dim+)))

(defun %va-clip (text width size)
  "TEXT, cut with an ellipsis to fit WIDTH at SIZE."
  (if (<= (glass:text-width text :size size) width)
      text
      (loop for n downfrom (length text) to 0
            for s = (concatenate 'string (subseq text 0 n) "...")
            when (<= (glass:text-width s :size size) width) return s
            finally (return ""))))

;;; ---- Speak -----------------------------------------------------------------------------------------------

(defun speak-app (fb)
  "Speak: a box to type into, and Speak, Hush and Clear.  Return says it too.  The box is not cleared
   after speaking -- the common thing after hearing a sentence is to fix a word and say it again."
  (let ((text "") (note nil) (shown nil) (dirty t) (buttons nil))
    (labels ((layout () (setf buttons (%va-buttons '("Speak" "Hush" "Clear") (- (glass:fb-height fb) 100))))
             (status () (list (voice-state *voice*) (or note (voice-report *voice*))))
             (say ()
               (let ((s (string-trim '(#\Space #\Newline #\Tab) text)))
                 (if (zerop (length s))
                     (setf note "Nothing typed yet -- type something, then press Speak.")
                     (progn (setf note nil) (voice-say *voice* s))))
               (setf dirty t))
             (press (label)
               (cond ((string= label "Speak") (say))
                     ((string= label "Hush") (setf note nil) (voice-hush *voice*))
                     ((string= label "Clear") (setf text "" note nil)))
               (setf dirty t))
             (redraw ()
               (let ((h (glass:fb-height fb)))
                 (glass:fb-fill fb +va-bg+)
                 (%va-draw-box fb text 12 (- h 124) :caret t)
                 (%va-draw-buttons fb buttons)
                 (destructuring-bind (headline detail) shown
                   (%va-draw-status fb (- h 56) headline detail)))))
      (layout)
      (values (lambda (down ks)
                (when down
                  (cond ((= ks #xff08) (when (plusp (length text)) (setf text (subseq text 0 (1- (length text))))))
                        ((= ks #xff0d) (say))                              ; Return says it
                        (t (let ((ch (%keysym-char ks)))
                             (when ch (setf text (concatenate 'string text (string ch)))))))
                  (setf dirty t)))
              (let ((was nil))                    ; a press, not every move of a drag over a button
                (lambda (mask x y)
                  (let ((now (logbitp 0 mask)))
                    (when (and now (not was))
                      (let ((b (%va-hit buttons x y))) (when b (press (%va-button-label b)))))
                    (setf was now))))
              (lambda ()
                (let ((now (status)))
                  (unless (equal now shown) (setf shown now dirty t)))
                (when dirty (setf dirty nil) (redraw) t))
              nil nil
              (lambda (w h) (glass:fb-resize fb w h +va-bg+) (layout) (setf dirty t))
              t                           ; it takes typing: focusing it brings up the keyboard
              nil
              ;; DRAG-P: its buttons act on a press; no drag is its own (a phone's rack scrolls)
              (lambda (x y) (declare (ignore x y)) nil)))))

;;; ---- Listen ----------------------------------------------------------------------------------------------

(defun listen-app (fb)
  "Listen: a box that fills itself with what the ear hears, a level meter, and Listen, Stop and Clear.
   Pressing Listen does not mean it is listening yet -- a model may be loading, or the platform asking for
   permission -- so the status line says which."
  (let ((shown nil) (dirty t) (buttons nil))
    (labels ((layout () (setf buttons (%va-buttons '("Listen" "Stop" "Clear") (- (glass:fb-height fb) 100))))
             (status ()
               ;; the level rounded to the pixel it is drawn at, so an idle room does not repaint forever
               (list (ear-state *ear*) (ear-report *ear*) (ear-text *ear*)
                     (round (* 100 (min 1d0 (sqrt (* 4d0 (ear-level *ear*))))))))
             (press (label)
               (cond ((string= label "Listen") (ear-start *ear*))
                     ((string= label "Stop") (ear-stop *ear*))
                     ((string= label "Clear") (ear-clear *ear*)))
               (setf dirty t))
             (redraw ()
               (let ((h (glass:fb-height fb)) (w (glass:fb-width fb)))
                 (destructuring-bind (headline detail text meter) shown
                   (glass:fb-fill fb +va-bg+)
                   (%va-draw-box fb text 12 (- h 136))
                   ;; the meter, under the box
                   (glass:fb-rect fb 12 (- h 116) (- w 24) 8 +va-paper+)
                   (glass:fb-rect fb 12 (- h 116) (floor (* (- w 24) meter) 100) 8 +va-accent+)
                   (%va-draw-buttons fb buttons)
                   (%va-draw-status fb (- h 56) headline detail)))))
      (layout)
      (values nil
              (let ((was nil))                    ; a press, not every move of a drag over a button
                (lambda (mask x y)
                  (let ((now (logbitp 0 mask)))
                    (when (and now (not was))
                      (let ((b (%va-hit buttons x y))) (when b (press (%va-button-label b)))))
                    (setf was now))))
              (lambda ()
                (let ((now (status)))
                  (unless (equal now shown) (setf shown now dirty t)))
                (when dirty (setf dirty nil) (redraw) t))
              nil nil
              (lambda (w h) (glass:fb-resize fb w h +va-bg+) (layout) (setf dirty t))
              nil nil
              ;; DRAG-P: its buttons act on a press; no drag is its own (a phone's rack scrolls)
              (lambda (x y) (declare (ignore x y)) nil)))))

(defparameter *voice-apps*
  '(("Speak" speak-app :width 400 :height 300)
    ("Listen" listen-app :width 400 :height 320))
  "Speak and Listen, as SERVE-DESK takes applications: (LABEL MAKE-FN &key WIDTH HEIGHT).")
