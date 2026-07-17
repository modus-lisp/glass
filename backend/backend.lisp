;;;; backend.lisp — a McCLIM backend that renders into a glass framebuffer and
;;;; serves it over VNC (RFB), with keyboard/pointer coming back from the client.
;;;;
;;;; McCLIM's software renderer (mcclim-render) already rasterizes every CLIM
;;;; drawing op into an (unsigned-byte 32) 0xAARRGGBB image.  The stock CLX-fb
;;;; backend blits that image to an X window; we instead copy the dirty region
;;;; into a glass framebuffer and let glass ship it over RFB.  So this file is
;;;; almost entirely glue: reuse RENDER-PORT-MIXIN / RENDER-MEDIUM-MIXIN for the
;;;; drawing, add a mirror that owns a glass fb, and translate RFB input events
;;;; into CLIM events.  No X server, no FFI — the whole path is Lisp.

(in-package #:clim-glass)

;;; ---- port ------------------------------------------------------------------

(defclass glass-port (mcclim-render::render-port-mixin)
  ((server   :initform nil :accessor glass-port-server)      ; RFB server thread
   (port-num :initarg :port :initform 5900 :accessor glass-port-num)
   (mailbox  :initform (sb-concurrency:make-mailbox) :reader glass-port-mailbox)
   (fb       :initform nil :accessor glass-port-fb)
   (top      :initform nil :accessor glass-port-top)          ; top-level sheet
   (mods     :initform 0   :accessor glass-port-mods)         ; CLIM modifier state
   (buttons  :initform 0   :accessor glass-port-buttons)      ; RFB button mask
   (px       :initform 0   :accessor glass-port-px)           ; last pointer x
   (py       :initform 0   :accessor glass-port-py)
   (clock    :initform 0   :accessor glass-port-clock))       ; monotonic timestamps
  (:default-initargs :pointer (make-instance 'climi::standard-pointer)))

(defun parse-glass-server-path (path) path)     ; plist tail becomes initargs

(defmethod find-port-type ((type (eql :glass)))
  (values 'glass-port 'parse-glass-server-path))

(defmethod initialize-instance :after ((port glass-port) &key server-path &allow-other-keys)
  (when server-path                                ; (:glass :port N) -> RFB port number
    (when-let ((n (getf (rest server-path) :port)))
      (setf (glass-port-num port) n)))
  (push (make-instance 'glass-frame-manager :port port)
        (slot-value port 'climi::frame-managers)))

(defun next-timestamp (port) (incf (glass-port-clock port)))

;;; ---- graft + frame manager -------------------------------------------------

(defclass glass-graft (graft) ())

(defparameter +glass-dpi+ 96 "Assumed pixel density; makes point/pixel sizing (fonts!) come out right.")

(defun graft-extent-in (px units)
  "Convert a device pixel count PX into UNITS (:device/:inches/:millimeters)."
  (ecase units
    ((:device :screen-sized) px)
    (:inches (/ px +glass-dpi+))
    (:millimeters (/ px (/ +glass-dpi+ 25.4)))))

(defmethod graft-width ((graft glass-graft) &key (units :device))
  (let ((fb (glass-port-fb (port graft))))
    (graft-extent-in (if fb (glass:fb-width fb) 1280) units)))

(defmethod graft-height ((graft glass-graft) &key (units :device))
  (let ((fb (glass-port-fb (port graft))))
    (graft-extent-in (if fb (glass:fb-height fb) 1024) units)))

(defmethod make-graft ((port glass-port) &key (orientation :default) (units :device))
  (make-instance 'glass-graft :port port :mirror t :orientation orientation :units units))

(defclass glass-frame-manager (climi::standard-frame-manager) ())

;;; ---- mirror ----------------------------------------------------------------
;;; One mirror per top-level sheet; in single-mirror McCLIM that is the whole UI.
;;; It carries mcclim-render's image plus the glass framebuffer we copy into.

(defclass glass-mirror (mcclim-render::image-mirror-mixin)
  ((fb :initform nil :accessor glass-mirror-fb)))

(defmethod realize-mirror ((port glass-port) (sheet climi::mirrored-sheet-mixin))
  (let ((mirror (make-instance 'glass-mirror)))
    (setf (sheet-direct-mirror sheet) mirror)
    (climi::update-mirror-geometry sheet)          ; creates the render image (via set-mirror-geometry)
    (when (typep sheet 'climi::top-level-sheet-mixin)
      (setf (glass-port-top port) sheet))                    ; server starts lazily on first force-output
    (dispatch-repaint sheet climi::+everywhere+)
    mirror))

(defmethod destroy-mirror ((port glass-port) (sheet climi::mirrored-sheet-mixin))
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (setf (mcclim-render::image-mirror-image mirror) nil)))

(defmethod enable-mirror ((port glass-port) (sheet climi::mirrored-sheet-mixin)) nil)
(defmethod disable-mirror ((port glass-port) (sheet climi::mirrored-sheet-mixin)) nil)

;;; ---- push rendered pixels into the framebuffer -----------------------------

(defun image-wh (image)
  (let ((a (climi::pattern-array image)))
    (values (array-dimension a 1) (array-dimension a 0))))

(defun ensure-fb-and-server (port mirror)
  "Once the render image exists (so we know the size), allocate the glass fb and
   start the RFB server thread.  Idempotent."
  (unless (glass-mirror-fb mirror)
    (when-let ((image (mcclim-render::image-mirror-image mirror)))
      (multiple-value-bind (w h) (image-wh image)
        (let ((fb (glass:make-framebuffer w h)))
          (setf (glass-mirror-fb mirror) fb
                (glass-port-fb port) fb)
          (unless (glass-port-server port)
            (setf (glass-port-server port)
                  (sb-thread:make-thread
                   (lambda ()
                     (glass:serve fb (glass-port-num port)
                                  :on-key     (lambda (down k) (glass-on-key port down k))
                                  :on-pointer (lambda (b x y) (glass-on-pointer port b x y))
                                  :name "glass-mcclim"))
                   :name "glass-server"))))))))

(defun blit-dirty (port mirror)
  "Copy the mirror image's dirty region into the glass framebuffer."
  (let ((fb (glass-mirror-fb mirror)))
    (when fb
      (mcclim-render::with-image-locked (mirror)
        (let ((dirty (mcclim-render::image-dirty-region mirror)))
          (unless (region-equal dirty climi::+nowhere+)
            (setf (mcclim-render::image-dirty-region mirror) climi::+nowhere+)
            (let* ((image (mcclim-render::image-mirror-image mirror))
                   (arr (climi::pattern-array image))
                   (dpx (glass:fb-pixels fb))
                   (fw (glass:fb-width fb)) (fh (glass:fb-height fb))
                   (clip (region-intersection dirty (make-rectangle* 0 0 fw fh))))
              (declare (ignore port))
              (unless (region-equal clip climi::+nowhere+)
                (with-bounding-rectangle* (x1 y1 x2 y2) clip
                  (let ((x1 (max 0 (floor x1))) (y1 (max 0 (floor y1)))
                        (x2 (min fw (ceiling x2))) (y2 (min fh (ceiling y2))))
                    (loop for y from y1 below y2 for row = (* y fw) do
                      (loop for x from x1 below x2 do
                        (setf (aref dpx (+ row x)) (logand (aref arr y x) #x00ffffff))))))))))))))

(defun %mirror-force-output (port mirror)
  (ensure-fb-and-server port mirror)
  (blit-dirty port mirror))

(defmethod port-force-output ((port glass-port))
  (when-let* ((sheet (glass-port-top port))
              (mirror (sheet-direct-mirror sheet)))
    (%mirror-force-output port mirror)))

;;; ---- medium ----------------------------------------------------------------

(defclass glass-medium (mcclim-render::render-medium-mixin climi::basic-medium) ())

(defmethod make-medium ((port glass-port) sheet)
  (make-instance 'glass-medium :port port :sheet sheet))

(defmethod medium-finish-output :after ((medium glass-medium))
  (when-let ((mirror (medium-drawable medium)))
    (%mirror-force-output (port medium) mirror)))

(defmethod medium-force-output :after ((medium glass-medium))
  (when-let ((mirror (medium-drawable medium)))
    (%mirror-force-output (port medium) mirror)))

;;; ---- event injection (RFB callbacks -> CLIM events) ------------------------

(defparameter *modifier-keysyms*
  `((#xffe1 . ,+shift-key+)   (#xffe2 . ,+shift-key+)      ; Shift L/R
    (#xffe3 . ,+control-key+) (#xffe4 . ,+control-key+)    ; Control L/R
    (#xffe9 . ,+meta-key+)    (#xffea . ,+meta-key+)       ; Alt L/R
    (#xffe7 . ,+meta-key+)    (#xffe8 . ,+meta-key+)       ; Meta L/R
    (#xffeb . ,+super-key+)   (#xffec . ,+super-key+)))    ; Super L/R

(defparameter *special-keysyms*
  '((#xff08 :backspace #\Backspace) (#xff09 :tab #\Tab) (#xff0d :return #\Return)
    (#xff1b :escape #\Escape) (#xffff :delete #\Delete) (#xff8d :return #\Return)
    (#xff50 :home) (#xff51 :left) (#xff52 :up) (#xff53 :right) (#xff54 :down)
    (#xff55 :prior) (#xff56 :next) (#xff57 :end) (#xff63 :insert)
    (#xffbe :f1) (#xffbf :f2) (#xffc0 :f3) (#xffc1 :f4) (#xffc2 :f5) (#xffc3 :f6)
    (#xffc4 :f7) (#xffc5 :f8) (#xffc6 :f9) (#xffc7 :f10) (#xffc8 :f11) (#xffc9 :f12)))

(defun keysym->clim (k)
  "Translate an X/RFB keysym into (values key-name key-character)."
  (cond
    ((or (<= #x20 k #x7e) (<= #xa0 k #xff))          ; Latin-1 printables: keysym = codepoint
     (values (intern (string (code-char k)) :keyword) (code-char k)))
    (t (let ((e (assoc k *special-keysyms*)))
         (if e (values (second e) (third e)) (values nil nil))))))

(defun enqueue (port event)
  (sb-concurrency:send-message (glass-port-mailbox port) event))

(defmacro with-reported-errors (&body body)
  `(handler-case (progn ,@body)
     (error (e) (format *trace-output* "~&[glass] event error: ~a: ~a~%" (type-of e) e)
       (force-output *trace-output*))))

(defun glass-on-key (port down-p keysym)
  (with-reported-errors
  (let ((mod (cdr (assoc keysym *modifier-keysyms*))))
    (cond
      (mod (setf (glass-port-mods port)
                 (if down-p (logior (glass-port-mods port) mod)
                     (logandc2 (glass-port-mods port) mod))))
      ((glass-port-top port)
       (multiple-value-bind (name char) (keysym->clim keysym)
         (enqueue port
                  (make-instance (if down-p 'key-press-event 'key-release-event)
                                 :key-name name
                                 :key-character (and down-p char)
                                 :sheet (glass-port-top port)
                                 :x (glass-port-px port) :y (glass-port-py port)
                                 :modifier-state (glass-port-mods port)
                                 :timestamp (next-timestamp port)))))))))

(defparameter *button-bits*
  `((1 . ,+pointer-left-button+) (2 . ,+pointer-middle-button+) (4 . ,+pointer-right-button+)))

(defun glass-on-pointer (port mask x y)
  (with-reported-errors
  (let ((sheet (glass-port-top port)))
    (when sheet
      (setf (glass-port-px port) x (glass-port-py port) y)
      ;; wheel (RFB buttons 4/5 = bits 8/16) arrives as a transient press
      (loop for (bit . delta) in '((8 . -1) (16 . 1))
            when (logtest mask bit)
            do (enqueue port (make-instance 'climi::pointer-scroll-event
                                            :pointer (climi::port-pointer port) :sheet sheet
                                            :x x :y y :delta-x 0 :delta-y delta
                                            :modifier-state (glass-port-mods port)
                                            :timestamp (next-timestamp port))))
      (let ((real (logand mask 7)))
        ;; motion
        (enqueue port (make-instance 'pointer-motion-event
                                     :pointer (climi::port-pointer port) :sheet sheet
                                     :x x :y y
                                     :modifier-state (glass-port-mods port)
                                     :timestamp (next-timestamp port)))
        ;; button transitions
        (let ((changed (logxor real (logand (glass-port-buttons port) 7))))
          (loop for (rbit . cbtn) in *button-bits*
                when (logtest changed rbit)
                do (enqueue port
                            (make-instance (if (logtest real rbit)
                                               'pointer-button-press-event
                                               'pointer-button-release-event)
                                           :pointer (climi::port-pointer port) :sheet sheet
                                           :button cbtn :x x :y y
                                           :modifier-state (glass-port-mods port)
                                           :timestamp (next-timestamp port)))))
        (setf (glass-port-buttons port) real))))))

;;; ---- event loop ------------------------------------------------------------

(defmethod process-next-event ((port glass-port) &key wait-function timeout)
  (let ((deadline (and timeout (+ (get-internal-real-time)
                                  (* timeout internal-time-units-per-second)))))
    (loop
      (when (maybe-funcall wait-function)
        (return (values nil :wait-function)))
      (multiple-value-bind (event ok)
          (sb-concurrency:receive-message-no-hang (glass-port-mailbox port))
        (when ok
          (distribute-event port event)
          (return t)))
      (when (and deadline (>= (get-internal-real-time) deadline))
        (return (values nil :timeout)))
      (sleep 1/200))))

;;; misc no-ops the frame machinery expects
(defmethod set-mirror-geometry ((port glass-port) sheet region)
  (declare (ignore sheet))
  (bounding-rectangle* region))         ; render-port-mixin's :after resizes the image
(defmethod port-modifier-state ((port glass-port)) (glass-port-mods port))
(defmethod (setf port-keyboard-input-focus) (focus (port glass-port)) focus)
(defmethod port-keyboard-input-focus ((port glass-port)) nil)
(defmethod set-sheet-pointer-cursor ((port glass-port) sheet cursor)
  (declare (ignore sheet cursor)) nil)

;;; ---- convenience: run a frame ----------------------------------------------

(defun find-glass-port (&key (port 5900))
  (find-port :server-path (list :glass :port port)))

(defun run-frame (frame-class &key (port 5900) (width 800) (height 600))
  "Make an application frame of FRAME-CLASS on a glass port serving on PORT and
   run its top-level loop (blocks).  Point any VNC client at localhost:PORT."
  (let* ((p (find-glass-port :port port))
         (fm (find-frame-manager :port p)))
    (climi::restart-port p)               ; start the port-io-loop thread that drives process-next-event
    (let ((frame (make-application-frame frame-class
                                         :frame-manager fm :width width :height height)))
      (run-frame-top-level frame))))
