;;;; remote-webrtc.lisp — "Remote": another BOX's desktop as a window, reached the way the phone
;;;; reaches it -- Nostr signalling, then WebRTC (ICE, DTLS, SCTP data channels) carrying RFB.
;;;;
;;;; backend/remote-app.lisp did this for the McCLIM desktop over plain TCP to a port on the same
;;;; network.  A box is not on the same network: what it has is an npub, and what a visitor has is
;;;; a login link (or the device key a link once enrolled).  glass-webrtc/dial does the phone's
;;;; whole connect sequence and hands back the `rfb' channel as a byte stream, and glass-client
;;;; reads RFB off that stream through its CONNECTOR exactly as it reads it off a socket -- so the
;;;; window is glass-client's REMOTE again, and everything remote-app.lisp says about input being
;;;; the identity still holds.
;;;;
;;;; THREE FACES, one window:
;;;;   :ASK         a form -- the box (npub) and a login link or code, typed into the window --
;;;;                and a Connect button.  Blank code is fine when this machine already holds
;;;;                a device key for that box (glass-dial keeps them).
;;;;   :CONNECTING  what the dial is doing, said in the window, with the seconds it has taken.
;;;;   :UP          the remote desktop, input passing through.
;;;; A failed dial goes back to :ASK with the reason.  A dropped session redials by itself (the
;;;; device key needs nobody) -- glass-client's reconnect calls the connector again.
;;;;
;;;; THE DESKTOP IS THE SHAPE OF THE WINDOW, as on the phone: once the box has said it takes
;;;; SetDesktopSize (an ExtendedDesktopSize rect), the window's size is asked for, and asked again
;;;; whenever the window is resized.  A box that will not resize is shown at its own size, clipped.
;;;;
;;;; THE REMOTE'S PIXELS LAND IN A FRAMEBUFFER OF ITS OWN, decoded on glass-client's thread, and
;;;; are copied into the window's on the window manager's thread (DIRTY-P), together with the
;;;; translation hint a CopyRect left -- so nothing but the compositor ever writes the window's
;;;; framebuffer, as the WM's one-thread rule asks.  SBCL only for now: on modus the dial's threads
;;;; would need the share-nothing treatment media-folder.lisp gives its worker.
;;;;
;;;; From the environment, for tests and launchers: GLASS_REMOTE_BOX, GLASS_REMOTE_LINK (a link
;;;; or a bare code), GLASS_REMOTE_RELAYS (comma-separated), GLASS_REMOTE_DIR (the credential
;;;; store), GLASS_REMOTE_AUTO=1 (connect when the window opens).

(defpackage #:glass-remote-webrtc
  (:use #:cl)
  (:local-nicknames (#:gc #:glass-client) (#:d #:glass-dial) (#:wd #:webrtc-data))
  (:export #:remote-app #:*remote-apps* #:remote-app-state #:remote-app-remote #:remote-app-status
           #:remote-app-connect))

(in-package #:glass-remote-webrtc)

(defparameter +bg+ #x1e2329)
(defparameter +panel+ #x2a3038)
(defparameter +field+ #x111418)
(defparameter +fg+ #xe6ebf2)
(defparameter +dim+ #x8b949e)
(defparameter +accent+ #x2f6f4f)
(defparameter +err+ #xe07a6b)

(defun %env (k) (let ((v (sb-ext:posix-getenv k))) (and v (plusp (length v)) v)))

;;; ---- the dial's log, as one status line ----------------------------------------------------------

(defclass %status-stream (sb-gray:fundamental-character-output-stream)
  ((line :initform (make-string-output-stream) :reader %ss-line)
   (last :initform "" :accessor %ss-last)
   (lock :initform (sb-thread:make-mutex) :reader %ss-lock))
  (:documentation "The dial's :LOG, kept as its latest complete line (written on the dial's thread,
read on the window manager's)."))

(defmethod sb-gray:stream-write-char ((s %status-stream) ch)
  (sb-thread:with-mutex ((%ss-lock s))
    (if (char= ch #\Newline)
        (let ((l (get-output-stream-string (%ss-line s))))
          (when (plusp (length l))
            (setf (%ss-last s) (string-left-trim " " (if (eql 0 (search "[dial] " l)) (subseq l 7) l)))))
        (write-char ch (%ss-line s))))
  ch)
(defmethod sb-gray:stream-line-column ((s %status-stream)) nil)
(defun %ss-read (s) (sb-thread:with-mutex ((%ss-lock s)) (%ss-last s)))

;;; ---- the app ---------------------------------------------------------------------------------

(defclass remote-app ()
  ((fb :initarg :fb :reader %fb)                         ; the window's content (WM thread only)
   (state :initform :ask :accessor remote-app-state)    ; :ask :connecting :up
   (fields :initform (vector "" "") :reader %fields)    ; box, link-or-code
   (focus :initform 0 :accessor %focus)
   (status :initform "" :accessor remote-app-status)   ; the form's message (an error, a hint)
   (remote :initform nil :accessor remote-app-remote)  ; glass-client REMOTE, once connecting
   (dialled :initform nil :accessor %dialled)          ; the live DIAL-RESULT
   (failure :initform nil :accessor %failure)          ; set by the dial thread: why it gave up
   (log :initform (make-instance '%status-stream) :reader %log)
   (started :initform 0 :accessor %started)
   (drawn :initform nil :accessor %drawn)              ; what the face last drew (redraw on change)
   (asked-size :initform nil :accessor %asked-size)    ; the SetDesktopSize last sent
   (full :initform t :accessor %full)                  ; the next copy must be the whole remote
   (targets :initform '() :accessor %targets))         ; ((x y w h action) ...) on the form
  (:documentation "One Remote window."))

(defvar *remote-apps* '() "Every open Remote window, newest first (for a REPL or a test to find).")

(defun %field (app i) (aref (%fields app) i))
(defun (setf %field) (v app i) (setf (aref (%fields app) i) v))

;;; ---- drawing the form and the connecting face ------------------------------------------------

(defun %text (fb x y s &key (size 13) (color +fg+)) (glass:fb-text fb x y s :size size :color color))

(defun %clip-text (s width size)
  "S, cut from the LEFT with an ellipsis until it fits WIDTH (the end of a long npub or link is
the part being typed)."
  (if (<= (glass:text-width s :size size) width)
      s
      (loop for i from 1 below (length s)
            for c = (concatenate 'string "…" (subseq s i))
            when (<= (glass:text-width c :size size) width) return c
            finally (return ""))))

(defun %draw-form (app)
  (let* ((fb (%fb app)) (w (glass:fb-width fb))
         (fw (max 120 (- (min w 720) 48))) (x 24) (y 24))
    (glass:fb-fill fb +bg+)
    (setf (%targets app) '())
    (%text fb x y "Remote desktop" :size 20)
    (%text fb x (+ y 30) "A glass box over Nostr + WebRTC, as the phone reaches it." :color +dim+)
    (loop for (label hint) in '(("Box" "npub1…")
                                ("Login link or code" "blank: this machine's device key for that box"))
          for i from 0
          for fy = (+ y 70 (* i 70))
          do (%text fb x fy label :color +dim+)
             (glass:fb-rect fb x (+ fy 20) fw 30 (if (= i (%focus app)) #x3d6a8a +panel+))
             (glass:fb-rect fb (1+ x) (+ fy 21) (- fw 2) 28 +field+)
             (let ((v (%field app i)))
               (if (plusp (length v))
                   (let ((shown (%clip-text (if (= i 1) (%mask-code v) v) (- fw 20) 13)))
                     (%text fb (+ x 8) (+ fy 27) shown)
                     (when (= i (%focus app))
                       (glass:fb-rect fb (+ x 9 (glass:text-width shown :size 13)) (+ fy 26) 2 18 +fg+)))
                   (progn (%text fb (+ x 8) (+ fy 27) hint :color #x59616b)
                          (when (= i (%focus app)) (glass:fb-rect fb (+ x 8) (+ fy 26) 2 18 +fg+)))))
             (push (list x (+ fy 20) fw 30 (let ((i i)) (lambda () (setf (%focus app) i)))) (%targets app)))
    (let ((by (+ y 220)))
      (glass:fb-rect fb x by 120 34 +accent+)
      (%text fb (+ x 30) (+ by 9) "Connect" :size 14)
      (push (list x by 120 34 (lambda () (remote-app-connect app))) (%targets app))
      (%text fb (+ x 140) (+ by 10) "Tab: next field   Return: connect" :color #x59616b)
      (when (plusp (length (remote-app-status app)))
        (%text fb x (+ by 54) (%clip-text (remote-app-status app) (- w 48) 13)
               :color (if (%failure app) +err+ +dim+))))
    (glass:fb-touch fb)))

(defun %mask-code (v)
  "A login link or code as the form shows it: the box and relays, never the code itself."
  (let ((p (search "code=" v)))
    (cond (p (concatenate 'string (subseq v 0 (+ p 5)) "•••••"))
          ((and (plusp (length v)) (not (search "://" v))) (make-string (min 24 (length v)) :initial-element #\•))
          (t v))))

(defun %draw-connecting (app)
  (let* ((fb (%fb app)) (w (glass:fb-width fb)) (h (glass:fb-height fb))
         (secs (floor (- (get-internal-real-time) (%started app)) internal-time-units-per-second))
         (box (%field app 0)))
    (glass:fb-fill fb +bg+)
    (let ((y (max 24 (- (floor h 2) 50))))
      (%text fb 24 y (format nil "Connecting… ~ds" secs) :size 20)
      (%text fb 24 (+ y 34) (%clip-text (format nil "to ~a" box) (- w 48) 13) :color +dim+)
      (%text fb 24 (+ y 58) (%clip-text (%ss-read (%log app)) (- w 48) 13) :color +dim+))
    (glass:fb-touch fb)
    secs))

;;; ---- connecting ------------------------------------------------------------------------------

(defun %credential-args (app)
  "dial-box's credential arguments from the form's second field: a link, a bare code, or nothing."
  (let ((v (string-trim " " (%field app 1))))
    (cond ((zerop (length v)) '())
          ((or (search "code=" v) (search "box=" v)) (list :link v))
          (t (list :code v)))))

(defun remote-app-connect (app)
  "Dial the box the form names.  Returns at once: the dial runs on glass-client's thread (its
CONNECTOR), and this window shows :CONNECTING until pixels arrive or the dial gives up."
  (let* ((box (string-trim " " (%field app 0)))
         (cred (%credential-args app))
         (lk (and (getf cred :link) (d:parse-login-link (getf cred :link)))))
    (when (and (zerop (length box)) lk (getf lk :box))
      (setf box (getf lk :box) (%field app 0) box))
    (cond
      ((zerop (length box))
       (setf (remote-app-status app) "Which box?  Type its npub, or paste a login link below."
             (%failure app) t))
      (t
       (setf (remote-app-status app) "" (%failure app) nil
             (remote-app-state app) :connecting (%started app) (get-internal-real-time)
             (%drawn app) nil (%asked-size app) nil (%full app) t)
       (let* ((relays (let ((e (%env "GLASS_REMOTE_RELAYS")))
                        (and e (remove "" (uiop:split-string e :separator ",") :test #'string=))))
              (dir (%env "GLASS_REMOTE_DIR"))
              (first t)
              (log (%log app))
              (fb (%fb app))
              (r nil))
         (setf r (gc:connect-remote
                  box 0 :width (glass:fb-width fb) :height (glass:fb-height fb) :resizable t
                  :connector
                  (lambda (remote)
                    ;; the first dial spends the form's code; a REdial (a dropped session) uses
                    ;; what that one left: the device key, and the renewal code
                    (let ((args (append (list :box box :log log)
                                        (and relays (list :relays relays))
                                        (and dir (list :dir (pathname (concatenate 'string (string-right-trim "/" dir) "/"))))
                                        (and first cred))))
                      (setf first nil)
                      (handler-case
                          (let ((dialled (apply #'d:dial-box args)))
                            (setf (%dialled app) dialled)
                            (values (lambda () (d:dial-close dialled)) (d:dial-stream dialled)))
                        (error (e)
                          ;; never dialled at all: back to the form with the reason, and stop
                          ;; glass-client retrying a credential that is not going to work
                          (unless (gc:remote-connected-p remote)
                            (when (null (%dialled app))
                              (setf (gc::remote-running remote) nil
                                    (%failure app) (princ-to-string e))))
                          (error e)))))))
         ;; glass-client wrote "connecting…" on its framebuffer; this window has its own face
         (setf (remote-app-remote app) r))))))

(defun %close-remote (app)
  (let ((r (remote-app-remote app)))
    (setf (remote-app-remote app) nil (%dialled app) nil)
    (when r (sb-thread:make-thread (lambda () (ignore-errors (gc:remote-stop r)))
                                   :name "glass-remote-webrtc-close"))))

;;; ---- the window's contract -------------------------------------------------------------------

(defun %copy-remote (app)
  "The remote's pixels into the window (WM thread), with the CopyRect hint that came with them.
Returns the hint, or NIL."
  (let* ((fb (%fb app)) (rfb (gc:remote-fb (remote-app-remote app))) (hint nil))
    (glass:with-fb-locked (rfb)
      (let ((rw (glass:fb-width rfb)) (rh (glass:fb-height rfb))
            (w (glass:fb-width fb)) (h (glass:fb-height fb)))
        (setf hint (glass:fb-take-copy rfb))
        (when (%full app)
          (setf hint nil (%full app) nil)
          (when (< rw w) (glass:fb-rect fb rw 0 (- w rw) h +bg+))
          (when (< rh h) (glass:fb-rect fb 0 rh (min w rw) (- h rh) +bg+)))
        (glass:fb-blit-clipped fb rfb 0 0)
        (when (and hint (or (> (+ (max (first hint) (third hint)) (fifth hint)) w)
                            (> (+ (max (second hint) (fourth hint)) (sixth hint)) h)))
          (setf hint nil))))
    hint))

(defun %tick (app)
  "DIRTY-P: draw whichever face is showing; T when the window changed."
  (let ((r (remote-app-remote app)))
    (case (remote-app-state app)
      (:ask
       (unless (eq (%drawn app) :ask) (%draw-form app) (setf (%drawn app) :ask) t))
      (:connecting
       (cond ((%failure app)
              (setf (remote-app-status app) (format nil "~a" (%failure app))
                    (remote-app-state app) :ask (%drawn app) nil)
              (%close-remote app)
              (%draw-form app) (setf (%drawn app) :ask) t)
             ((and r (gc:remote-connected-p r) (plusp (getf (gc:remote-stats r) :frames)))
              (setf (remote-app-state app) :up (%drawn app) nil (%full app) t)
              (%tick app))
             (t (let ((secs (%draw-connecting app)))
                  (prog1 (not (eql secs (%drawn app))) (setf (%drawn app) secs))))))
      (:up
       (let ((fb (%fb app)))
         ;; the desktop is the shape of the window, once the box says it can be
         (let ((want (list (glass:fb-width fb) (glass:fb-height fb))))
           (when (and (not (equal want (%asked-size app)))
                      (gc:remote-set-desktop-size r (first want) (second want)))
             (setf (%asked-size app) want)))
         (when (or (gc:remote-take-dirty r) (%full app) (not (eq (%drawn app) :up)))
           (unless (eq (%drawn app) :up) (setf (%full app) t (%drawn app) :up))
           (%copy-remote app)
           t))))))

(defun %copy-p (app)
  "COPY-P: the remote's scroll, when this tick copied one.  Kept off for now (NIL): the window is
re-blitted whole from the remote's framebuffer, and the compositor diffs it, which is correct
whatever arrived; passing the hint through is the nested-CopyRect optimisation remote-app.lisp has."
  (declare (ignore app))
  nil)

(defun %key (app down keysym)
  (case (remote-app-state app)
    (:up (let ((r (remote-app-remote app))) (when r (gc:remote-key r down keysym))))
    (:ask
     (when down
       (let ((i (%focus app)))
         (cond ((= keysym #xff0d) (remote-app-connect app))                      ; Return
               ((= keysym #xff09) (setf (%focus app) (mod (1+ i) 2)))           ; Tab
               ((= keysym #xff08)                                               ; BackSpace
                (let ((v (%field app i))) (when (plusp (length v)) (setf (%field app i) (subseq v 0 (1- (length v)))))))
               ((= keysym #xff1b) (setf (%field app i) ""))                     ; Escape: clear
               ((<= #x20 keysym #x7e)
                (setf (%field app i) (concatenate 'string (%field app i) (string (code-char keysym))))))
         (setf (%drawn app) nil))))
    (t nil)))

(defun %pointer (app mask x y)
  (case (remote-app-state app)
    (:up (let ((r (remote-app-remote app))) (when r (gc:remote-pointer r mask x y))))
    (:ask
     (when (logbitp 0 mask)
       (loop for (tx ty tw th action) in (%targets app)
             when (and (<= tx x (+ tx tw -1)) (<= ty y (+ ty th -1)))
               do (funcall action) (setf (%drawn app) nil) (return))))
    (t nil)))

(defun %resize (app w h)
  (glass:fb-resize (%fb app) w h +bg+)
  (setf (%drawn app) nil (%full app) t))

(defun remote-app (fb)
  "The Remote window's MAKE-FN: (values ON-KEY ON-POINTER DIRTY-P COPY-P CLOSE-FN RESIZE-FN
TEXT-P).  GLASS_REMOTE_BOX / GLASS_REMOTE_LINK fill the form; GLASS_REMOTE_AUTO=1 connects at once."
  (let ((app (make-instance 'remote-app :fb fb)))
    (setf (%field app 0) (or (%env "GLASS_REMOTE_BOX") "")
          (%field app 1) (or (%env "GLASS_REMOTE_LINK") ""))
    (when (and (zerop (length (%field app 0))) (plusp (length (%field app 1))))
      (setf (%focus app) 1))
    (push app *remote-apps*)
    (when (equal (%env "GLASS_REMOTE_AUTO") "1") (remote-app-connect app))
    (values (lambda (down keysym) (%key app down keysym))
            (lambda (mask x y) (%pointer app mask x y))
            (lambda () (%tick app))
            (lambda () (%copy-p app))
            (lambda ()
              (setf *remote-apps* (remove app *remote-apps*))
              (%close-remote app))
            (lambda (w h) (%resize app w h))
            t)))
