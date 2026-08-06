;;;; speak-app-gate.lisp — the speak window, driven the way a person drives it.
;;;;
;;;; backend/speak-app.lisp is a window whose entire job is to turn typing into speech, so the
;;;; only test worth having is one that TYPES.  This gate runs the real frame on a real glass
;;;; port, connects a real RFB client, and sends real keysyms and clicks at coordinates it works
;;;; out from the panes' own geometry (so it keeps working when the layout changes).  Nothing is
;;;; called directly that a finger could not have caused:
;;;;
;;;;   * the box is typeable the moment it opens — no click first (the window is often opened to
;;;;     say one thing, and a first sentence that silently goes nowhere is the worst kind of bug).
;;;;   * clicking the box and typing appends, which is the ordinary path and proves Drei took the
;;;;     keyboard focus from the click rather than from our startup command.
;;;;   * Speak on an EMPTY box says so and does not even create a voice — a status line must not
;;;;     be what puts a source on the session's mixer.
;;;;   * Speak on a typed sentence is heard ON THE SESSION MIX (loud frames pulled from a sink,
;;;;     not "the function returned"), and counted as said with nothing failed.
;;;;   * Hush stops a paragraph that is still being spoken.
;;;;
;;;;   sbcl --control-stack-size 256 --dynamic-space-size 4096 --non-interactive \
;;;;        --load backend/inspect/speak-app-gate.lisp
;;;;
;;;; The voice comes from GLASS_VOICE, or the lessac medium export if that is unset; with no
;;;; voice on the box at all the speaking checks cannot be run, and the gate says that rather
;;;; than passing quietly.  It serves on 5947 to stay clear of the live desktops.

(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:mcclim :mcclim-render :sb-concurrency))
    (asdf:load-asd "/home/claude/glass/backend/mcclim-glass.asd")
    (asdf:load-system :mcclim-glass)
    (asdf:load-system :mcclim-glass/speak)))

(defpackage #:glass-speak-gate (:use #:cl)) (in-package #:glass-speak-gate)

(defvar *pass* 0) (defvar *fail* 0)
(defun check-that (name ok &optional detail)
  (if ok (progn (incf *pass*) (format t "  ok   ~a~@[ — ~a~]~%" name detail))
      (progn (incf *fail*) (format t "  FAIL ~a~@[ — ~a~]~%" name detail)))
  (finish-output))

(defparameter *port* 5947)

(unless glass:*speech-voice*
  (let ((here "/mnt/lisp/chord/export/en_US-lessac-medium.graph"))
    (when (probe-file here) (setf glass:*speech-voice* here))))

;;; ---- an RFB client with no screen ------------------------------------------
;;;
;;; It never decodes a framebuffer: what this gate reads is the frame's own state, in this same
;;; image.  The client exists to be the thing that pressed the key.

(defun w8 (s v) (write-byte (logand v #xff) s))
(defun w16 (s v) (w8 s (ash v -8)) (w8 s v))
(defun w32 (s v) (w16 s (ash v -16)) (w16 s v))
(defun rn (s n) (let ((b (make-array n :element-type '(unsigned-byte 8)))) (read-sequence b s) b))
(defun r8 (s) (read-byte s))
(defun r16 (s) (logior (ash (r8 s) 8) (r8 s)))
(defun r32 (s) (logior (ash (r16 s) 16) (r16 s)))

(defun connect (port)
  (loop repeat 600
        do (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
             (handler-case
                 (progn (sb-bsd-sockets:socket-connect
                         sock (sb-bsd-sockets:make-inet-address "127.0.0.1") port)
                        (return-from connect
                          (sb-bsd-sockets:socket-make-stream
                           sock :input t :output t :element-type '(unsigned-byte 8)
                                :buffering :full)))
               (error () (ignore-errors (sb-bsd-sockets:socket-close sock)) (sleep 0.05))))
        finally (error "speak gate: nothing listening on ~d" port)))

(defun handshake (s)
  (rn s 12) (write-sequence (map 'vector #'char-code "RFB 003.008") s) (w8 s 10) (force-output s)
  (let ((n (r8 s))) (rn s n)) (w8 s 1) (force-output s) (r32 s)
  (w8 s 1) (force-output s)                                   ; ClientInit (shared)
  (let ((w (r16 s)) (h (r16 s)))
    (rn s 16) (let ((nl (r32 s))) (rn s nl))
    (w8 s 2) (w8 s 0) (w16 s 1) (w32 s 0)                     ; SetEncodings: Raw only
    (force-output s)
    (values w h)))

(defun send-key (s keysym)
  (w8 s 4) (w8 s 1) (w16 s 0) (w32 s keysym) (force-output s)  ; press
  (w8 s 4) (w8 s 0) (w16 s 0) (w32 s keysym) (force-output s)) ; release

(defun type-text (s text)
  "Type TEXT one keysym at a time.  ASCII keysyms ARE the character codes, which is the whole
mapping this needs; the pauses are for the frame's event thread, not for the wire."
  (loop for ch across text do (send-key s (char-code ch)) (sleep 0.02)))

(defun click (s x y)
  (let ((x (round x)) (y (round y)))
    (w8 s 5) (w8 s 0) (w16 s x) (w16 s y)                      ; move there first
    (w8 s 5) (w8 s 1) (w16 s x) (w16 s y)                      ; button 1 down
    (w8 s 5) (w8 s 0) (w16 s x) (w16 s y)                      ; up
    (force-output s)
    (sleep 0.35)))

;;; ---- where the panes actually are ------------------------------------------

(defun pane (frame name)
  "Pane NAME of the frame.  Interned in the APP's package, not this one: a frame's pane names
are symbols from the file that defined it, and CLIM:FIND-PANE-NAMED compares with EQ."
  (or (clim:find-pane-named frame (intern (string name) '#:glass-speak))
      (error "speak gate: the window has no pane named ~a" name)))

(defun pane-point (frame name)
  "The middle of pane NAME in screen coordinates, asked of the pane rather than assumed: a gate
that hardcodes click points passes until someone moves a button by six pixels."
  (let* ((pane (pane frame name))
         (tr (clim:sheet-native-transformation pane)))
    (multiple-value-bind (x1 y1 x2 y2) (clim:bounding-rectangle* (clim:sheet-region pane))
      (clim:transform-position tr (/ (+ x1 x2) 2) (/ (+ y1 y2) 2)))))

(defun await (test &key (timeout 90) (what "condition"))
  (let ((start (get-internal-real-time)))
    (loop until (funcall test)
          do (when (> (/ (- (get-internal-real-time) start) internal-time-units-per-second) timeout)
               (return-from await nil))
             (sleep 0.05))
    t))

(defun rms (pcm)
  (if (zerop (length pcm))
      0d0
      (let ((sum 0d0))
        (dotimes (i (length pcm)) (let ((v (float (aref pcm i) 1d0))) (incf sum (* v v))))
        (sqrt (/ sum (length pcm))))))

(defun listen-for-sound (seconds)
  "Pull the session mix on a sink for SECONDS and report how many frames had sound in them.
This is a LISTENER, subscribed exactly like a VNC session's audio stream: it hears what anything
else on the session would hear, not what the speaker was asked to do."
  (let* ((mixer (glass:session-mixer))
         (sink (glass:mixer-subscribe mixer :name "speak-gate" :lead 2))
         (loud 0) (got 0))
    (unwind-protect
         (dotimes (i (round (* seconds 50)))
           (let ((f (glass:sink-next-frame sink)))
             (when f (incf got) (when (> (rms f) 20d0) (incf loud))))
           (sleep 0.02))
      (glass:mixer-unsubscribe mixer sink))
    (values loud got)))

;;; ---- the window ------------------------------------------------------------

(format t "~&speak window gate — voice ~:[NONE~;~:*~a~]~%~%" glass:*speech-voice*)

(defvar *frame*
  (let* ((p (clim-glass::find-glass-port :port *port*))
         (fm (clim:find-frame-manager :port p)))
    (climi::restart-port p)
    (clim:make-application-frame 'glass-speak:speak-box :width 520 :height 300
                                                       :frame-manager fm)))

(sb-thread:make-thread
 (lambda ()
   (handler-case (clim:run-frame-top-level *frame*)
     (serious-condition (e) (format t "~&FRAME DIED: ~a~%" e) (finish-output))))
 :name "speak-box")

(defvar *client* (connect *port*))
(handshake *client*)
(sleep 2.5)                             ; let the frame lay out before asking where its panes are

(defun box-text () (string (clim:gadget-value (pane *frame* 'input))))

(format t "~&-- 1. typing ----------------------------------------------------------~%")

(check-that "the box has the keyboard as soon as the window is up"
            (eq (pane *frame* 'input)
                (clim:port-keyboard-input-focus (clim:port (pane *frame* 'input))))
            (format nil "focus is ~a"
                    (clim:port-keyboard-input-focus (clim:port (pane *frame* 'input)))))

(type-text *client* "Hello")
(sleep 0.6)
(check-that "the box takes typing with no click first" (search "Hello" (box-text))
            (format nil "box reads ~s" (box-text)))

(multiple-value-bind (x y) (pane-point *frame* 'input)
  (format t "     (text box at ~d,~d)~%" (round x) (round y))
  (click *client* x y))
(type-text *client* " there.")
(sleep 0.6)
(check-that "clicking the box and typing appends" (search "Hello there." (box-text))
            (format nil "box reads ~s" (box-text)))

(multiple-value-bind (x y) (pane-point *frame* 'clear)
  (format t "     (Clear at ~d,~d)~%" (round x) (round y))
  (click *client* x y))
(sleep 0.6)
(check-that "Clear empties the box" (zerop (length (box-text)))
            (format nil "box reads ~s" (box-text)))

(format t "~&-- 2. Speak with nothing to say ---------------------------------------~%")

(multiple-value-bind (x y) (pane-point *frame* 'say)
  (format t "     (Speak at ~d,~d)~%" (round x) (round y))
  (click *client* x y))
(sleep 0.6)
(check-that "an empty Speak explains itself" (glass-speak::app-note *frame*)
            (glass-speak::app-note *frame*))
(check-that "an empty Speak does not create a voice on the mixer"
            (null glass:*session-speaker*))

(cond
  ((null glass:*speech-voice*)
   (format t "~&-- no voice installed: the speaking checks CANNOT be run ---------------~%")
   (incf *fail*))
  (t
   (format t "~&-- 3. Speak ------------------------------------------------------------~%")
   (multiple-value-bind (x y) (pane-point *frame* 'input) (click *client* x y))
   (type-text *client* "Type here and the desktop says it.")
   (sleep 0.5)
   (let ((typed (box-text))
         (sound nil))
     (check-that "the sentence is in the box" (search "desktop says it" typed) typed)
     ;; start listening BEFORE the click: the first frames of an utterance are the ones a
     ;; late listener misses, and they are the ones that prove it started when we asked
     (let ((ear (sb-thread:make-thread
                 (lambda () (multiple-value-list (listen-for-sound 25)))
                 :name "speak-gate ear")))
       (multiple-value-bind (x y) (pane-point *frame* 'say) (click *client* x y))
       (check-that "Speak makes the window's voice the session's"
                   (await (lambda () glass:*session-speaker*) :timeout 10 :what "a speaker"))
       (check-that "it says it, once, with nothing failed"
                   (await (lambda () (and glass:*session-speaker*
                                          (plusp (glass::spk-said glass:*session-speaker*))))
                          :timeout 90 :what "the sentence to be said")
                   (glass:speech-report))
       (check-that "nothing failed" (and glass:*session-speaker*
                                         (zerop (glass::spk-failed glass:*session-speaker*)))
                   (glass::spk-last-error glass:*session-speaker*))
       (setf sound (sb-thread:join-thread ear)))
     (destructuring-bind (loud got) sound
       (check-that "a listener on the session mix HEARD it" (plusp loud)
                   (format nil "~d of ~d frames had sound in them" loud got))))

   (format t "~&-- 4. Hush -------------------------------------------------------------~%")
   (multiple-value-bind (x y) (pane-point *frame* 'clear) (click *client* x y))
   (multiple-value-bind (x y) (pane-point *frame* 'input) (click *client* x y))
   (type-text *client*
              "This is a long paragraph. It has several sentences in it. \
               The point of it is to still be going when the button is pressed. \
               There is more of it after that.")
   (sleep 0.5)
   (multiple-value-bind (x y) (pane-point *frame* 'say) (click *client* x y))
   (check-that "it starts speaking the paragraph"
               (await (lambda () (and glass:*session-speaker*
                                      (glass:speaking-p glass:*session-speaker*)))
                      :timeout 60 :what "the paragraph to start"))
   (multiple-value-bind (x y) (pane-point *frame* 'hush) (click *client* x y))
   (check-that "Hush stops it"
               (await (lambda () (not (glass:speaking-p glass:*session-speaker*)))
                      :timeout 15 :what "silence")
               (glass:speech-report))))

(format t "~&~%speak window gate: ~d passed, ~d failed~%" *pass* *fail*)
(finish-output)
(sb-ext:exit :code (if (zerop *fail*) 0 1))
