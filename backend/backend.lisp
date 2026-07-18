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
   (top      :initform nil :accessor glass-port-top)          ; the MAIN top-level sheet
   (mirrors  :initform '() :accessor glass-port-mirrors)      ; all top-level mirrors, newest-first
   (mods     :initform 0   :accessor glass-port-mods)         ; CLIM modifier state
   (buttons  :initform 0   :accessor glass-port-buttons)      ; RFB button mask
   (px       :initform 0   :accessor glass-port-px)           ; last pointer x
   (py       :initform 0   :accessor glass-port-py)
   (clock    :initform 0   :accessor glass-port-clock)        ; monotonic timestamps
   ;; --- window-manager mode (OPEN LOOK) ---
   (wm-p     :initform nil :accessor glass-port-wm-p)         ; decorate + manage windows?
   (screen-w :initform 1000 :accessor glass-port-screen-w)
   (screen-h :initform 720  :accessor glass-port-screen-h)
   (drag     :initform nil :accessor glass-port-drag)         ; (window off-x off-y) while moving a window
   (cascade  :initform 0   :accessor glass-port-cascade)      ; next window placement offset
   (surfaces :initform '() :accessor glass-port-surfaces)     ; non-McCLIM windows (e.g. terminals)
   (focus-surface :initform nil :accessor glass-port-focus-surface)  ; surface grabbing the keyboard
   (menu     :initform nil :accessor glass-port-menu)         ; open workspace root menu, or nil
   (menu-items :initform '() :accessor glass-port-menu-items)  ; (label . thunk) list for the root menu
   (bg       :initform nil :accessor glass-port-bg)           ; desktop background framebuffer, or nil (flat teal)
   (wake     :initform (glass:make-wake) :accessor glass-port-wake))  ; nudges RFB senders after compositing
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
;;; One mirror per top-level sheet.  The MAIN one (the application frame) owns the
;;; framebuffer and drives its size; secondary ones (menus, dialogs, tooltips) are
;;; composited on top of it at their screen position — glass serves one screen, so
;;; the backend is a tiny compositor over all the top-level mirrors.

(defclass glass-mirror (mcclim-render::image-mirror-mixin)
  ((x    :initform 0   :accessor glass-mirror-x)             ; content screen position
   (y    :initform 0   :accessor glass-mirror-y)
   (main :initform nil :accessor glass-mirror-main)          ; owns the fb + the RFB server?
   ;; --- window-manager mode ---
   (managed :initform nil :accessor glass-mirror-managed)    ; gets a title bar + border?
   (title   :initform "" :accessor glass-mirror-title)
   (sheet   :initform nil :accessor glass-mirror-sheet)      ; backref (WM pointer routing)
   (deco    :initform nil :accessor glass-mirror-deco)       ; cached (image . width) title bar
   (deco-w  :initform -1 :accessor glass-mirror-deco-w)))

(defconstant +wm-titleh+ 22 "OPEN LOOK title-bar height (px).")
(defconstant +wm-border+ 1  "Window border thickness (px).")

(defmethod realize-mirror ((port glass-port) (sheet climi::mirrored-sheet-mixin))
  (let ((mirror (make-instance 'glass-mirror)))
    (setf (sheet-direct-mirror sheet) mirror
          (glass-mirror-sheet mirror) sheet)
    (when (typep sheet 'climi::top-level-sheet-mixin)
      (when (null (glass-port-top port))                     ; first top-level = the main frame
        (setf (glass-port-top port) sheet (glass-mirror-main mirror) t))
      (push mirror (glass-port-mirrors port))
      ;; window-manager mode: decorate managed frames + give them a cascaded slot
      (when (and (glass-port-wm-p port)
                 (not (typep sheet 'climi::unmanaged-sheet-mixin)))
        (setf (glass-mirror-managed mirror) t
              (glass-mirror-title mirror) (wm-sheet-title sheet))
        (let ((c (glass-port-cascade port)))
          (setf (glass-mirror-x mirror) (+ 40 c) (glass-mirror-y mirror) (+ 40 c +wm-titleh+)
                (glass-port-cascade port) (mod (+ c 28) 200)))))
    (climi::update-mirror-geometry sheet)          ; creates the render image (via set-mirror-geometry)
    (dispatch-repaint sheet climi::+everywhere+)
    mirror))

(defmethod destroy-mirror ((port glass-port) (sheet climi::mirrored-sheet-mixin))
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (setf (glass-port-mirrors port) (remove mirror (glass-port-mirrors port))
          (mcclim-render::image-mirror-image mirror) nil)
    (composite-all port)))                          ; erase a closed menu/dialog

(defmethod enable-mirror ((port glass-port) (sheet climi::mirrored-sheet-mixin)) nil)
(defmethod disable-mirror ((port glass-port) (sheet climi::mirrored-sheet-mixin)) nil)

;;; ---- push rendered pixels into the framebuffer -----------------------------

(defun image-wh (image)
  (let ((a (climi::pattern-array image)))
    (values (array-dimension a 1) (array-dimension a 0))))

(defun start-glass-server (port)
  "Start the RFB server thread for PORT (serving its framebuffer).  Idempotent."
  (unless (glass-port-server port)
    (let ((fb (glass-port-fb port)))
      (setf (glass-port-server port)
            (sb-thread:make-thread
             (lambda ()
               (glass:serve fb (glass-port-num port)
                            :on-key     (lambda (down k) (glass-on-key port down k))
                            :on-pointer (lambda (b x y) (glass-on-pointer port b x y))
                            :on-resize  (lambda (w h) (glass-on-resize port w h))
                            :wake       (glass-port-wake port)
                            :name "glass-mcclim"))
             :name "glass-server")))))

(defun ensure-fb-and-server (port mirror)
  "The MAIN mirror allocates the framebuffer (sized to its image) and starts the
   RFB server, once its image exists.  In WM mode RUN-WM owns the screen fb."
  (when (and (not (glass-port-wm-p port)) (not (glass-port-fb port)))
    (when-let ((image (mcclim-render::image-mirror-image mirror)))
      (multiple-value-bind (w h) (image-wh image)
        (setf (glass-port-fb port) (glass:make-framebuffer w h))
        (start-glass-server port)))))

(defun blit-mirror (mirror fb)
  "Composite one mirror's image into FB at the mirror's screen position (opaque)."
  (when-let ((image (mcclim-render::image-mirror-image mirror)))
    (mcclim-render::with-image-locked (mirror)
      (let* ((arr (climi::pattern-array image))
             (ih (array-dimension arr 0)) (iw (array-dimension arr 1))
             (ox (glass-mirror-x mirror)) (oy (glass-mirror-y mirror))
             (dpx (glass:fb-pixels fb)) (fw (glass:fb-width fb)) (fh (glass:fb-height fb)))
        (dotimes (iy ih)
          (let ((fy (+ oy iy)))
            (when (< -1 fy fh)
              (let ((frow (* fy fw)))
                (dotimes (ix iw)
                  (let ((fx (+ ox ix)))
                    (when (< -1 fx fw)
                      (setf (aref dpx (+ frow fx)) (logand (aref arr iy ix) #x00ffffff)))))))))))))

(defun composite-all (port &optional damage copy)
  "Redraw the desktop.  DAMAGE = (x y w h) confines the redraw (and the RFB
   sender's diff) to that rectangle — the compositor already knows what changed,
   so an idle move/blink doesn't rebuild + re-diff the whole 1280x800.  NIL means
   the whole screen (menus, resize, McCLIM updates, first paint).  COPY = (sx sy
   dx dy w h) marks a window MOVE so the sender can CopyRect it (near-free drag)."
  (when-let ((fb (glass-port-fb port)))
    (glass:with-fb-locked (fb)
      (flet ((paint ()
               (if (glass-port-wm-p port)
                   (wm-composite port fb)
                   ;; mirrors is newest-first; composite oldest (main) first so newer are on top
                   (dolist (mirror (reverse (glass-port-mirrors port))) (blit-mirror mirror fb)))))
        (if (and damage (glass-port-wm-p port))
            (destructuring-bind (dx dy dw dh) damage
              (glass:with-fb-clip (fb dx dy dw dh) (paint))
              (glass:fb-mark-frame fb (list dx dy (+ dx dw) (+ dy dh)) copy))
            (progn (paint) (glass:fb-mark-frame fb :full))))
      (glass:fb-touch fb))                ; content changed -> the sender should re-scan
    (glass:wake-signal (glass-port-wake port))))   ; …and wake it now, don't wait for its poll

(defun sync-fb-size (port mirror)
  "Keep the framebuffer the same size as the MAIN frame's image; on a change the
   RFB client is told the new size via DesktopSize."
  (let ((fb (glass-port-fb port))
        (image (mcclim-render::image-mirror-image mirror)))
    (when (and fb image (glass-mirror-main mirror) (not (glass-port-wm-p port)))
      (multiple-value-bind (w h) (image-wh image)
        (unless (and (= w (glass:fb-width fb)) (= h (glass:fb-height fb)))
          (glass:fb-resize fb w h))))))

(defgeneric present-mirror (port mirror)
  (:documentation
   "Push MIRROR's freshly rendered image to the display — the ONE seam between
    rendering (mcclim-render, per app) and the display (glass, shared).  The
    default composites into the local framebuffer and serves it over RFB; a
    MESSAGE-PORT overrides this to ship the pixels to a remote compositor over a
    mailbox, which is the entire actor boundary.")
  (:method ((port glass-port) mirror)
    (when (glass-mirror-main mirror)
      (ensure-fb-and-server port mirror)       ; main mirror creates the fb + starts the server
      (sync-fb-size port mirror))
    (composite-all port)))

(defun %mirror-force-output (port mirror)
  (present-mirror port mirror))

(defmethod port-force-output ((port glass-port))
  (when-let* ((sheet (glass-port-top port))     ; drive through the main mirror so the server starts
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
  ;; a focused surface window (e.g. a terminal) grabs the keyboard entirely
  (when (and (glass-port-wm-p port) (glass-port-focus-surface port))
    (funcall (wm-surface-on-key (glass-port-focus-surface port)) down-p keysym)
    (return-from glass-on-key))
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
    (setf (glass-port-px port) x (glass-port-py port) y)
    (if (glass-port-wm-p port)
        (wm-on-pointer port mask x y)
        (glass-on-pointer/single port mask x y))))

(defun glass-on-pointer/single (port mask x y)
  (when-let ((sheet (glass-port-top port)))
    (emit-pointer-events port sheet mask x y)))

(defun emit-pointer-events (port sheet mask lx ly)
  "Turn an RFB pointer state (MASK) at sheet-local (LX,LY) into CLIM motion/
   button/scroll events for SHEET.  Button transitions are diffed against the
   port's global button mask (one physical mouse)."
  ;; wheel (RFB buttons 4/5 = bits 8/16) arrives as a transient press
  (loop for (bit . delta) in '((8 . -1) (16 . 1))
        when (logtest mask bit)
        do (enqueue port (make-instance 'climi::pointer-scroll-event
                                        :pointer (climi::port-pointer port) :sheet sheet
                                        :x lx :y ly :delta-x 0 :delta-y delta
                                        :modifier-state (glass-port-mods port)
                                        :timestamp (next-timestamp port))))
  (let ((real (logand mask 7)))
    (enqueue port (make-instance 'pointer-motion-event
                                 :pointer (climi::port-pointer port) :sheet sheet
                                 :x lx :y ly
                                 :modifier-state (glass-port-mods port)
                                 :timestamp (next-timestamp port)))
    (let ((changed (logxor real (logand (glass-port-buttons port) 7))))
      (loop for (rbit . cbtn) in *button-bits*
            when (logtest changed rbit)
            do (enqueue port
                        (make-instance (if (logtest real rbit)
                                           'pointer-button-press-event
                                           'pointer-button-release-event)
                                       :pointer (climi::port-pointer port) :sheet sheet
                                       :button cbtn :x lx :y ly
                                       :modifier-state (glass-port-mods port)
                                       :timestamp (next-timestamp port)))))
    (setf (glass-port-buttons port) real)))

(defun glass-on-resize (port w h)
  "Client asked (by resizing its VNC window) for a W x H desktop.  Relayout the
   frame to that size on the event thread; sync-fb-size then resizes the fb and
   the client is told the actual new size via DesktopSize."
  (with-reported-errors
    (when-let ((sheet (glass-port-top port)))
      (when (and (plusp w) (plusp h))
        ;; the same path the X backend uses for a user-driven window resize: a
        ;; window-configuration-event resizes the sheet (and, via render's
        ;; distribute-event :before, the image) and relays out the frame.
        (enqueue port (make-instance 'window-configuration-event
                                     :sheet sheet
                                     :region (make-bounding-rectangle 0 0 w h)))))))

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
          (if (functionp event)             ; a closure marshalled onto the event thread
              (funcall event)
              (distribute-event port event))
          (return t)))
      (when (and deadline (>= (get-internal-real-time) deadline))
        (return (values nil :timeout)))
      (sleep 1/200))))

;;; misc no-ops the frame machinery expects
(defmethod set-mirror-geometry ((port glass-port) sheet region)
  ;; REGION is the mirror's rectangle in screen coordinates — remember where to
  ;; composite this top-level sheet.  render-port-mixin's :after resizes the image
  ;; (always at a 0,0 origin), so position lives here, size in the image.
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* region)
    (when-let ((mirror (sheet-direct-mirror sheet)))
      (when (and (typep mirror 'glass-mirror)
                 (not (glass-mirror-managed mirror)))   ; WM owns managed-window positions
        (setf (glass-mirror-x mirror) (floor x1)
              (glass-mirror-y mirror) (floor y1))))
    (values x1 y1 x2 y2)))
(defmethod port-modifier-state ((port glass-port)) (glass-port-mods port))
;; NB: keyboard-input-focus is handled by basic-port (it tracks the focused sheet
;; and distribute-event routes key events there) — we must NOT shadow it, or keys
;; never reach an interactor/editor.
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
