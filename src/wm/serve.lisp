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

(defparameter *wm-apps*
  (append glass.desk:*standard-apps*
          '(("Lisp" glass-repl:repl-app :width 640 :height 400)
            ("Terminal" glass-term:terminal-app :width 640 :height 400)))
  "SERVE-WM's root menu: the desk's applications, a Lisp listener and a shell terminal.")

(defun serve-wm (port &key (width 800) (height 600) (apps *wm-apps*) (open '("Clock"))
                           (address "127.0.0.1") clients stop)
  "Serve the window manager WIDTH x HEIGHT on PORT: a network thread (every viewer), a
   compositor thread, and this thread the Lisp listener REPL windows evaluate on.  APPS is a
   list of (LABEL MAKE-FN &key WIDTH HEIGHT), MAKE-FN a symbol or global function returning the
   app contract (see wm.lisp); OPEN lists labels to open at start.  CLIENTS and STOP are
   SERVE-MULTI's.  Returns when the network side does."
  ;; The faces, loaded HERE: the compositor draws text, and a font it loaded itself could not go
  ;; in the shared cache on modus.
  (glass:default-font) (glass:default-font t) (glass-term:term-fonts)
  (let* ((fb (glass:make-framebuffer width height *wm-bg*))
         (inbox (glass::%share (glass::make-rfb-mailbox)))
         (wake (glass:make-wake))
         (listener (glass-repl:make-listener))
         (compositor (progn
                       (setf glass-repl:*listener* listener)
                       (sb-thread:make-thread #'%wm-compositor
                                              :arguments (list fb inbox wake apps open)
                                              :name "glass-wm-compositor")))
         (network (sb-thread:make-thread #'%wm-network
                                         :arguments (list fb port address clients stop
                                                          inbox wake listener)
                                         :name "glass-wm-network")))
    (unwind-protect (glass-repl:run-listener listener)
      (ignore-errors (sb-thread:join-thread network))
      (ignore-errors (sb-thread:join-thread compositor))
      (when (eq glass-repl:*listener* listener) (setf glass-repl:*listener* nil)))))

(defun %wm-network (fb port address clients stop inbox wake listener)
  "The network thread: every viewer, until SERVE-MULTI returns; then stop the others."
  (flet ((post (m) (glass::mb-post inbox m) (glass:wake-signal wake)))
    (unwind-protect
         (handler-case
             (glass:serve-multi fb port
                                :address address :clients clients :stop stop
                                :on-key (lambda (down key) (post (list :key down key)))
                                :on-pointer (lambda (mask x y) (post (list :pointer mask x y))))
           (error (e)
             (ignore-errors (format *error-output* "~&glass wm: network: ~a~%" e)
                            (force-output *error-output*))))
      (post (list :stop))
      (glass-repl:stop-listener listener))))

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

(defun %wm-compositor (fb inbox wake apps open)
  "The compositor thread.  Caches the apps fill as they run are this thread's own."
  (let ((glass.desk::*media-library* nil))
    (glass:with-own-glyph-cache () (%wm-compositor-1 fb inbox wake apps open))))

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

(defun %wm-compositor-1 (fb inbox wake apps open)
  (let ((wm (make-wm fb)))
    (dolist (a apps)
      (destructuring-bind (label make-fn &key (width 400) (height 300)) a
        (wm-register-app wm label make-fn :width width :height height)))
    (dolist (label open) (%app-guard "open" (wm-open wm label)))
    (wm-redraw wm)
    (loop
      (loop for (m . rest) on (glass::mb-take inbox)
            do (when (eq (first m) :stop) (return-from %wm-compositor-1 nil))
               (unless (%superseded-p wm m rest)
                 ;; AN APP'S ERROR IS THE APP'S: input reaches app code, and a signal out of it
                 ;; must not end the compositor -- that is the whole desktop.
                 (%app-guard (format nil "~(~a~)" (first m))
                   (case (first m)
                     (:key (wm-key wm (second m) (third m)))
                     (:pointer (destructuring-bind (mask x y) (rest m) (wm-pointer wm mask x y)))))))
      (%app-guard "tick" (wm-tick wm))
      (glass::wake-wait wake 1/60))))
