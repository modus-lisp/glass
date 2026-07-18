;;;; compositor.lisp — the compositor-actor: the shared display for a desktop of
;;;; app-actors.  It owns the screen (a glass-port in WM mode: framebuffer + RFB
;;;; server + input) and multiplexes windows from many isolated app-actors, each
;;;; talking to it over a mailbox with the SAME protocol the message-port speaks:
;;;;
;;;;   app -> compositor   (:create-window w h title reply owner)   ; owner = app's mbox
;;;;                       (:damage id x y w h argb-array)
;;;;                       (:destroy id)
;;;;   compositor -> app   (:key down keysym) / (:pointer mask lx ly) / (:resize w h)
;;;;
;;;; The trick is that the WM's wm-surface (fb + on-key/on-pointer/dirty-p/
;;;; resize-fn/close-fn) IS the actor boundary in disguise.  An ACTOR-SURFACE is a
;;;; wm-surface whose fb is filled by incoming :damage and whose input thunks SEND
;;;; to the owner actor instead of calling a local renderer.  So the WM decorates,
;;;; composites, drags, raises, focuses, resizes and closes an app-actor's window
;;;; exactly like a terminal — it can't tell the difference, and neither can the
;;;; app.  Same protocol on modus (message = shared-immutable region) as here
;;;; (sb-concurrency mailbox).

(in-package #:clim-glass)

(defstruct (compositor (:constructor %make-compositor))
  port                                       ; the glass-port (WM mode) that owns the screen
  (mbox (sb-concurrency:make-mailbox))       ; protocol inbox — app-actors send here
  (windows (make-hash-table))                ; window-id -> awin
  (next 0)                                   ; window-id counter
  (running t))

(defstruct (awin (:constructor %make-awin))  ; one app-actor window on the compositor
  surface owner)

(defun compositor-connect (comp)
  "The mailbox an app-actor's message-port ships to — pass to MAKE-MESSAGE-PORT."
  (compositor-mbox comp))

;;; ---- pixels across the boundary --------------------------------------------

(defun argb->fb (arr w h)
  "A fresh glass framebuffer holding the ARGB image ARR (2D h x w, 0xAARRGGBB) as
   opaque RRGGBB — the surface content for one :damage.  Whole-image for now, so a
   damage is an atomic fb swap (no in-place write racing the compositor's read)."
  (let* ((fb (glass:make-framebuffer (max 1 w) (max 1 h))) (px (glass:fb-pixels fb)))
    (dotimes (y h)
      (let ((row (* y w)))
        (dotimes (x w) (setf (aref px (+ row x)) (logand (aref arr y x) #xffffff)))))
    fb))

;;; ---- protocol handlers (run on the compositor-actor thread) ----------------

(defun comp-create (comp msg)
  (destructuring-bind (w h title reply owner) (cdr msg)
    (let* ((port (compositor-port comp))
           (id (incf (compositor-next comp)))
           (surf (flet ((tell (&rest m) (sb-concurrency:send-message owner m)))
                   (make-wm-surface
                    :fb (glass:make-framebuffer (max 1 w) (max 1 h))
                    :title (or title "app")
                    :on-key     (lambda (down k)      (tell :key down k))
                    :on-pointer (lambda (mask lx ly)  (tell :pointer mask lx ly))
                    :dirty-p    (constantly nil)       ; the compositor drives our repaint on :damage
                    :resize-fn  (lambda (pw ph)       (tell :resize pw ph))
                    :close-fn   (lambda ()            (tell :destroy))))))
      (setf (gethash id (compositor-windows comp)) (%make-awin :surface surf :owner owner))
      (wm-add-surface* port surf)
      (composite-all port)                             ; first paint places the window
      (sb-concurrency:send-message reply id))))

(defun comp-damage (comp msg)
  (destructuring-bind (id x y w h arr) (cdr msg)
    (declare (ignore x y))                             ; whole-image: window content starts at 0,0
    (when-let ((aw (gethash id (compositor-windows comp))))
      (let ((surf (awin-surface aw)))
        (setf (wm-surface-fb surf) (argb->fb arr w h)) ; atomic swap (also handles relayout/resize)
        (composite-all (compositor-port comp) (wm-window-box surf))))))   ; damage just this window

(defun comp-destroy (comp msg)
  (let ((id (second msg)))
    (when-let ((aw (gethash id (compositor-windows comp))))
      (remhash id (compositor-windows comp))
      (wm-close (compositor-port comp) (awin-surface aw)))))   ; drops surface + recomposites

(defun compositor-run (comp)
  "The compositor-actor loop: fold protocol messages into WM surface operations.
   Fault-contained (a bad message can't take down the display)."
  (loop while (compositor-running comp) do
    (let ((msg (sb-concurrency:receive-message (compositor-mbox comp))))
      (handler-case
          (case (car msg)
            (:create-window (comp-create comp msg))
            (:damage        (comp-damage comp msg))
            (:destroy       (comp-destroy comp msg))
            (:stop          (setf (compositor-running comp) nil))
            (t (format *trace-output* "~&[compositor] ignoring ~s~%" (car msg))))
        (serious-condition (e)
          (format *trace-output* "~&[compositor] ~a: ~a~%" (type-of e) e))))))

;;; ---- spawning app-actors + running a desktop of them -----------------------

(defun start-compositor (port)
  "Wrap a WM-mode PORT in a compositor-actor and start its dispatch thread."
  (let ((comp (%make-compositor :port port)))
    (sb-thread:make-thread (lambda () (compositor-run comp)) :name "compositor-actor")
    comp))

(defun spawn-app-actor (comp frame-class &key (width 400) (height 300) name)
  "Spawn FRAME-CLASS as an ISOLATED app-actor: its own thread, its own message-port
   and McCLIM frame, drawing to COMP over messages.  The frame is unmodified — it
   never learns it's an actor.  Returns the actor thread."
  (sb-thread:make-thread
   (lambda ()
     (handler-case
         (let* ((port (make-message-port (compositor-connect comp)))
                (fm (find-frame-manager :port port)))
           (climi::restart-port port)                 ; the app-actor's own event loop
           (run-frame-top-level
            (make-application-frame frame-class :frame-manager fm :width width :height height)))
       (serious-condition (e) (format *trace-output* "~&[app-actor ~a] ~a~%" frame-class e))))
   :name (or name (format nil "actor-~a" frame-class))))
