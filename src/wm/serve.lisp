;;;; src/wm/serve.lisp — the window manager as a served desktop: SERVE-WM.
;;;;
;;;; THREE THREADS, EACH OWNING ITS OWN, as SERVE-DESK's two (src/desk-serve.lisp) plus one:
;;;;   NETWORK     SERVE-MULTI, every viewer.
;;;;   COMPOSITOR  the window manager, every window and every app.
;;;;   MAIN        the image's Lisp listener (glass-repl:RUN-LISTENER): what a Lisp window
;;;;               evaluates on.  Main, because on modus only main's objects are everybody's,
;;;;               and a DEFUN typed into a REPL is a store into the image everybody shares --
;;;;               refused from any other thread.  It also means a runaway form stops the
;;;;               listener and not the desktop.
;;;; Everything two threads touch -- the screen framebuffer, the mailboxes, the wakes -- is made
;;;; here, on main, before the others start, and handed over as :ARGUMENTS; input crosses as
;;;; messages, pixels through the shared framebuffer, damage and the CopyRect hint through
;;;; FB-MARK-FRAME.  That is what makes it run where threads share nothing (modus).  Hence an
;;;; app is a NAME the compositor calls, never a closure made here.

(in-package #:glass.wm)

;; Lisp and Terminal: (LABEL MAKE-FN &key WIDTH HEIGHT), as *STANDARD-APPS*.
(defparameter *wm-apps*
  (append glass.desk:*standard-apps*
          '(("Lisp" glass-repl:repl-app :width 640 :height 400)
            ("Terminal" glass-term:terminal-app :width 640 :height 400)))
  "SERVE-WM's root menu: the desk's applications, a Lisp listener and a shell terminal.")

(defun serve-wm (port &key (width 800) (height 600) (apps *wm-apps*) (open '("Clock"))
                           (address "127.0.0.1") clients stop audio-port audio-path)
  "Serve the window manager WIDTH x HEIGHT on PORT: a network thread (every viewer), a
   compositor thread, and this thread the Lisp listener REPL windows evaluate on.  APPS is a
   list of (LABEL MAKE-FN &key WIDTH HEIGHT), MAKE-FN a symbol or global function returning the
   app contract (see wm.lisp); OPEN lists labels to open at start.  CLIENTS and STOP are
   SERVE-MULTI's.  AUDIO-PORT (or AUDIO-PATH, a UNIX socket) serves the desktop's sound as
   glass-audio/1 -- the McCLIM desktop's convention is PORT + 10, where the WebRTC gateway
   looks -- and needs glass/wm-sound loaded (sound.lisp).  Returns when the network side does."
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
         (wake (glass:make-wake))
         (listener (glass-repl:make-listener))
         (compositor (progn
                       (setf glass-repl:*listener* listener)
                       (sb-thread:make-thread #'%wm-compositor
                                              :arguments (list fb inbox wake apps open sound-box)
                                              :name "glass-wm-compositor")))
         (network (sb-thread:make-thread #'%wm-network
                                         :arguments (list fb port address clients stop
                                                          inbox wake listener
                                                          sound sound-box audio-port audio-path)
                                         :name "glass-wm-network")))
    (unwind-protect (glass-repl:run-listener listener)
      (ignore-errors (sb-thread:join-thread network))
      (ignore-errors (sb-thread:join-thread compositor))
      (when (eq glass-repl:*listener* listener) (setf glass-repl:*listener* nil)))))

(defun %wm-network (fb port address clients stop inbox wake listener
                    sound sound-box audio-port audio-path)
  "The network thread: every viewer, and the sound's listeners, until SERVE-MULTI returns; then
   stop the others."
  (let ((out nil))
    (flet ((post (m) (glass::mb-post inbox m) (glass:wake-signal wake)))
      (unwind-protect
           (handler-case
               (progn
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
                 (glass:serve-multi fb port
                                    :address address :clients clients :stop stop
                                    :tick (and out (lambda () (funcall 'audio-out-turn out)))
                                    :on-key (lambda (down key) (post (list :key down key)))
                                    :on-pointer (lambda (mask x y) (post (list :pointer mask x y)))))
             (error (e)
               (ignore-errors (format *error-output* "~&glass wm: network: ~a~%" e)
                              (force-output *error-output*))))
        (post (list :stop))
        (when out (ignore-errors (funcall 'audio-out-close out)))
        (glass-repl:stop-listener listener)))))

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
       (glass:with-own-glyph-cache () (%wm-compositor-1 fb inbox wake apps open snd))))))

(defun %superseded-p (wm m rest)
  "Is pointer message M made redundant by the next one?  Only while a window is being dragged or
   resized: a motion followed by another motion with the same buttons says nothing the second
   does not, and replaying a backlog of them one composite each is how a drag falls behind.
   Never otherwise -- Paint wants every point of a stroke."
  (and (wm-drag wm)
       (eq (first m) :pointer)
       rest
       (eq (first (first rest)) :pointer)
       (eql (second m) (second (first rest)))))

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
                   (case (first m)
                     (:listeners (when snd (funcall 'wm-sound-listen snd (second m))))
                     (:key (wm-key wm (second m) (third m)))
                     (:pointer (destructuring-bind (mask x y) (rest m) (wm-pointer wm mask x y)))))))
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
