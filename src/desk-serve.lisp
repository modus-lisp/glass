;;;; desk-serve.lisp — the desk as a served desktop, one thread per concern.
;;;;
;;;; THREADS BY ROLE, NOT BY VIEWER.  The calling thread is the NETWORK thread: SERVE-MULTI,
;;;; every viewer on it.  A second thread is the COMPOSITOR: it owns the DESK -- the windows,
;;;; the apps, the menu -- and draws the screen.  Between them is one mailbox of input events
;;;; and the screen framebuffer, which the compositor writes and the network thread reads.
;;;; Viewers come and go without threads coming and going.  A later HID thread (local
;;;; keyboard, touch) posts to the same mailbox.
;;;;
;;;; WHY IT HOLDS ON MODUS, where threads share nothing: everything both threads touch is made
;;;; here, on the calling thread, before the compositor starts -- the framebuffer, the mailbox
;;;; (shared from birth), the wake -- and is handed over as :ARGUMENTS.  Input crosses as
;;;; messages (copied in), pixels as fixnums written into the shared framebuffer, damage
;;;; through FB-MARK-FRAME (which shares it).  The DESK and every app are made on the
;;;; compositor thread and never leave it, so an app's state is that thread's own.  Hence the
;;;; application contract: an app is a NAME (a symbol or a global function) the compositor
;;;; calls, never a closure made here -- a closure cannot cross threads on modus.

(in-package #:glass.desk)

(export '(serve-desk))

(defun serve-desk (port &key (width 800) (height 600) (apps *standard-apps*) (open '("Clock")) (address "127.0.0.1")
                             clients stop)
  "Serve a desk WIDTH x HEIGHT on PORT: this thread is the network (every viewer), a second
   thread composites.  APPS is a list of (LABEL MAKE-FN &key WIDTH HEIGHT), MAKE-FN a symbol
   or global function (the DESK-REGISTER-APP contract); OPEN lists labels to open at start.
   CLIENTS and STOP are SERVE-MULTI's.  Returns when the network side does."
  ;; The faces, loaded HERE: the compositor draws text, and a font it loaded itself could not
  ;; go in the shared cache on modus.
  (glass:default-font) (glass:default-font t)
  (let* ((fb (glass:make-framebuffer width height *bg*))
         (inbox (glass::%share (glass::make-rfb-mailbox)))
         (wake (glass:make-wake))
         (compositor (sb-thread:make-thread #'%desk-compositor
                                            :arguments (list fb inbox wake apps open)
                                            :name "glass-compositor")))
    (flet ((post (m) (glass::mb-post inbox m) (glass:wake-signal wake)))
      (unwind-protect
           (glass:serve-multi fb port
                              :address address :clients clients :stop stop
                              :on-key (lambda (down key) (post (list :key down key)))
                              :on-pointer (lambda (mask x y) (post (list :pointer mask x y))))
        (post (list :stop))
        (ignore-errors (sb-thread:join-thread compositor))))))

(defun %desk-compositor (fb inbox wake apps open)
  "The compositor thread: make the desk, then apply input and tick it until told to stop."
  (glass:with-own-glyph-cache () (%desk-compositor-1 fb inbox wake apps open)))

(defun %desk-compositor-1 (fb inbox wake apps open)
  (let ((desk (make-desk fb)))
    (dolist (a apps)
      (destructuring-bind (label make-fn &key (width 400) (height 300)) a
        (desk-register-app desk label make-fn :width width :height height)))
    (dolist (label open) (desk-open desk label))
    (loop
      (dolist (m (glass::mb-take inbox))
        (case (first m)
          (:key (desk-key desk (second m) (third m)))
          (:pointer (destructuring-bind (mask x y) (rest m) (desk-pointer desk mask x y)))
          (:stop (return-from %desk-compositor-1 nil))))
      ;; Draw under the pixel lock, so a sender diffing the screen never sees half a frame,
      ;; and tell the senders where (FB-MARK-FRAME) so they diff only that.
      (multiple-value-bind (changed x y w h)
          (glass:with-fb-locked (fb) (desk-tick desk))
        (when changed
          (glass:fb-mark-frame fb (list x y (+ x w) (+ y h)))))
      (glass::wake-wait wake 1/60))))
