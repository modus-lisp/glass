;;;; message-port.lisp — a glass McCLIM backend that draws ACROSS an actor
;;;; boundary.  A message-port is a glass-port with the display cut off: its
;;;; top-level mirrors still render through mcclim-render into their own image
;;;; (in this actor's heap), but on force-output the pixels are SHIPPED as a
;;;; :damage message to a compositor over a mailbox instead of being blitted into
;;;; a shared framebuffer.  Input arrives the other way as NEUTRAL data tuples —
;;;; (:key down keysym) / (:pointer mask x y) / (:resize w h) — and is translated
;;;; to CLIM events locally (no CLIM object crosses the boundary; the sheets live
;;;; in this heap).  So an unmodified CLIM app runs as an isolated actor: same
;;;; frame, same medium, same event queue — only the display transport crosses.
;;;;
;;;; The same protocol runs on modus (a message is a copy into the shared-immutable
;;;; region) as on SBCL now (a message is an sb-concurrency mailbox item).

(in-package #:clim-glass)

(defclass message-port (glass-port)
  ((compositor :initarg :compositor :accessor mp-compositor)   ; mailbox TO the compositor-actor
   (reply      :initform (sb-concurrency:make-mailbox) :reader mp-reply)  ; create-window id replies
   (win-ids    :initform (make-hash-table :test 'eq) :accessor mp-win-ids)))  ; mirror -> window-id

(defun make-message-port (compositor)
  "A message-port whose display ships to the COMPOSITOR mailbox.  Each app-actor
   owns its own, so it is made directly (not registered with find-port)."
  (make-instance 'message-port :compositor compositor))

;;; ---- app -> compositor: create-window / damage / destroy -------------------

(defun copy-argb (arr)
  "A fresh copy of an mcclim-render ARGB image array — the immutable pixel payload
   handed across the actor boundary (the sender must not mutate it after send;
   frames render into a new image each paint, so this copy is that discipline)."
  (let* ((h (array-dimension arr 0)) (w (array-dimension arr 1))
         (c (make-array (list h w) :element-type '(unsigned-byte 32))))
    (dotimes (y h) (dotimes (x w) (setf (aref c y x) (aref arr y x))))
    c))

(defun ensure-window-id (port mirror image)
  "The window-id the compositor assigned this mirror — created on first paint via
   a synchronous ask (send :create-window, await the id on our reply mailbox)."
  (or (gethash mirror (mp-win-ids port))
      (multiple-value-bind (w h) (image-wh image)
        (sb-concurrency:send-message (mp-compositor port)
          (list :create-window w h (glass-mirror-title mirror) (mp-reply port)))
        (setf (gethash mirror (mp-win-ids port))
              (sb-concurrency:receive-message (mp-reply port))))))

(defun send-damage (port mirror)
  "Ship MIRROR's rendered pixels to the compositor as a :damage message.  Whole
   image for now — dirty-tile diffing is the compositor's job (it already does
   TRLE/ZRLE over what actually changed)."
  (when-let ((image (mcclim-render::image-mirror-image mirror)))
    (mcclim-render::with-image-locked (mirror)
      (let ((id (ensure-window-id port mirror image)))
        (multiple-value-bind (w h) (image-wh image)
          (sb-concurrency:send-message (mp-compositor port)
            (list :damage id (glass-mirror-x mirror) (glass-mirror-y mirror)
                  w h (copy-argb (climi::pattern-array image)))))))))

(defmethod present-mirror ((port message-port) mirror)
  ;; the actor-boundary seam: no local fb / RFB server — ship pixels instead.
  (send-damage port mirror))

(defmethod destroy-mirror ((port message-port) (sheet climi::mirrored-sheet-mixin))
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (when-let ((id (gethash mirror (mp-win-ids port))))
      (sb-concurrency:send-message (mp-compositor port) (list :destroy id))
      (remhash mirror (mp-win-ids port)))
    (setf (mcclim-render::image-mirror-image mirror) nil)))

;;; ---- compositor -> app: neutral input tuples, translated locally -----------

(defmethod process-next-event ((port message-port) &key wait-function timeout)
  "Drain the port mailbox.  A NEUTRAL input tuple from the compositor is translated
   here (reusing the very RFB->CLIM path the shared-fb port uses: glass-on-* enqueue
   CLIM events onto this same mailbox, which the next drain distributes).  A CLIM
   event or a marshalled closure is handled as usual."
  (let ((deadline (and timeout (+ (get-internal-real-time)
                                  (* timeout internal-time-units-per-second)))))
    (loop
      (when (maybe-funcall wait-function)
        (return (values nil :wait-function)))
      (multiple-value-bind (msg ok)
          (sb-concurrency:receive-message-no-hang (glass-port-mailbox port))
        (when ok
          (cond
            ((and (consp msg) (keywordp (car msg)))         ; neutral input from the compositor
             (case (car msg)
               (:key     (glass-on-key     port (second msg) (third msg)))
               (:pointer (glass-on-pointer port (second msg) (third msg) (fourth msg)))
               (:resize  (glass-on-resize  port (second msg) (third msg)))))
            ((functionp msg) (funcall msg))                 ; closure marshalled onto our thread
            (t (distribute-event port msg)))                ; a CLIM event (from glass-on-*)
          (return t)))
      (when (and deadline (>= (get-internal-real-time) deadline))
        (return (values nil :timeout)))
      (sleep 1/200))))
