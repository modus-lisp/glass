;;;; src/wm/serve.lisp — the window manager as a served desktop: SERVE-WM.
;;;;
;;;; SERVE-DESK's shape exactly (src/desk-serve.lisp), so a launcher can switch desktops by name:
;;;; the calling thread is the NETWORK (SERVE-MULTI, every viewer); ONE compositor thread owns the
;;;; window manager, every window and every app.  Everything both threads touch -- the screen
;;;; framebuffer, the input mailbox, the wake -- is made here before the compositor starts and
;;;; handed over as :ARGUMENTS; input crosses as messages, pixels through the shared framebuffer,
;;;; damage and the CopyRect hint through FB-MARK-FRAME.  That is what makes it run where threads
;;;; share nothing (modus).  Hence an app is a NAME the compositor calls, never a closure made here.

(in-package #:glass.wm)

(defun serve-wm (port &key (width 800) (height 600) (apps glass.desk:*standard-apps*) (open '("Clock"))
                           (address "127.0.0.1") clients stop)
  "Serve the window manager WIDTH x HEIGHT on PORT: this thread is the network (every viewer), a
   second thread composites.  APPS is a list of (LABEL MAKE-FN &key WIDTH HEIGHT), MAKE-FN a
   symbol or global function returning the app contract (see wm.lisp); OPEN lists labels to open
   at start.  CLIENTS and STOP are SERVE-MULTI's.  Returns when the network side does."
  ;; The faces, loaded HERE: the compositor draws text, and a font it loaded itself could not go
  ;; in the shared cache on modus.
  (glass:default-font) (glass:default-font t)
  (let* ((fb (glass:make-framebuffer width height *wm-bg*))
         (inbox (glass::%share (glass::make-rfb-mailbox)))
         (wake (glass:make-wake))
         (compositor (sb-thread:make-thread #'%wm-compositor
                                            :arguments (list fb inbox wake apps open)
                                            :name "glass-wm-compositor")))
    (flet ((post (m) (glass::mb-post inbox m) (glass:wake-signal wake)))
      (unwind-protect
           (glass:serve-multi fb port
                              :address address :clients clients :stop stop
                              :on-key (lambda (down key) (post (list :key down key)))
                              :on-pointer (lambda (mask x y) (post (list :pointer mask x y))))
        (post (list :stop))
        (ignore-errors (sb-thread:join-thread compositor))))))

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
