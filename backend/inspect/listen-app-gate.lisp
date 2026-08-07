;;;; listen-app-gate.lisp — the listen window, driven the way a person drives it.
;;;;
;;;; backend/listen-app.lisp is speak-app run backwards: press Listen and the box fills up.  So
;;;; this is speak-app-gate run backwards too — the real frame on a real glass port, a real RFB
;;;; client, real clicks at coordinates worked out from the panes' own geometry.  Nothing is
;;;; called that a finger could not have caused, and nothing is handed to the recognizer: the
;;;; audio goes into the session mix the way an application plays it, and comes back out through
;;;; the sink, the level gate and the decoder before any of it reaches the box.
;;;;
;;;;   * a window that has only been OPENED has not created an ear.  Drawing a status line must
;;;;     not be what reads a quarter of a gigabyte of weights and puts a sink on the mixer.
;;;;   * Listen creates the session's ear, and the window says LOADING until it is really
;;;;     collecting audio — the half-minute where a `Listening' would be a lie.
;;;;   * THE BOX FILLS UP WITH WORDS NOBODY TYPED.  This is the whole feature.
;;;;   * Clear empties the box AND the transcript behind it, and it stays empty — clearing only
;;;;     the gadget would let the ear's own copy reappear on the next tick.
;;;;   * Stop stops the ear and takes the sink off the mix with it.
;;;;
;;;;   sbcl --control-stack-size 256 --dynamic-space-size 8192 --non-interactive \
;;;;        --load backend/inspect/listen-app-gate.lisp
;;;;
;;;; The models come from GLASS_EARS, or stave's export directory if that is unset; with no
;;;; recognizer on the box the hearing checks cannot be run, and the gate says so rather than
;;;; passing quietly.  It serves on 5948 to stay clear of the live desktops.

(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:mcclim :mcclim-render :sb-concurrency))
    (asdf:load-asd (merge-pathnames "../mcclim-glass.asd" *load-truename*))
    (asdf:load-system :mcclim-glass)
    (asdf:load-system :mcclim-glass/listen)))

(defpackage #:glass-listen-gate (:use #:cl)) (in-package #:glass-listen-gate)

(defvar *pass* 0) (defvar *fail* 0)
(defun check-that (name ok &optional detail)
  (if ok (progn (incf *pass*) (format t "  ok   ~a~@[ — ~a~]~%" name detail))
      (progn (incf *fail*) (format t "  FAIL ~a~@[ — ~a~]~%" name detail)))
  (finish-output))

(defparameter *port* 5948)
(defparameter *wav* "/mnt/lisp/stave/models/zipformer-en/test_wavs/0.wav")

(unless glass:*hearing-models*
  (let ((here "/mnt/lisp/stave/export/"))
    (when (probe-file here) (setf glass:*hearing-models* here))))

;;; ---- an RFB client with no screen ------------------------------------------
;;;
;;; It never decodes a framebuffer: what this gate reads is the frame's own state, in this same
;;; image.  The client exists to be the thing that pressed the button.

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
        finally (error "listen gate: nothing listening on ~d" port)))

(defun handshake (s)
  (rn s 12) (write-sequence (map 'vector #'char-code "RFB 003.008") s) (w8 s 10) (force-output s)
  (let ((n (r8 s))) (rn s n)) (w8 s 1) (force-output s) (r32 s)
  (w8 s 1) (force-output s)                                   ; ClientInit (shared)
  (let ((w (r16 s)) (h (r16 s)))
    (rn s 16) (let ((nl (r32 s))) (rn s nl))
    (w8 s 2) (w8 s 0) (w16 s 1) (w32 s 0)                     ; SetEncodings: Raw only
    (force-output s)
    (values w h)))

(defun click (s x y)
  (let ((x (round x)) (y (round y)))
    (w8 s 5) (w8 s 0) (w16 s x) (w16 s y)                      ; move there first
    (w8 s 5) (w8 s 1) (w16 s x) (w16 s y)                      ; button 1 down
    (w8 s 5) (w8 s 0) (w16 s x) (w16 s y)                      ; up
    (force-output s)
    (sleep 0.35)))

;;; ---- where the panes actually are ------------------------------------------

(defun pane (frame name)
  "Pane NAME of the frame.  Interned in the APP's package, not this one: a frame's pane names are
symbols from the file that defined it, and CLIM:FIND-PANE-NAMED compares with EQ."
  (or (clim:find-pane-named frame (intern (string name) '#:glass-listen))
      (error "listen gate: the window has no pane named ~a" name)))

(defun pane-point (frame name)
  "The middle of pane NAME in screen coordinates, asked of the pane rather than assumed: a gate
that hardcodes click points passes until someone moves a button by six pixels."
  (let* ((pane (pane frame name))
         (tr (clim:sheet-native-transformation pane)))
    (multiple-value-bind (x1 y1 x2 y2) (clim:bounding-rectangle* (clim:sheet-region pane))
      (clim:transform-position tr (/ (+ x1 x2) 2) (/ (+ y1 y2) 2)))))

(defun buttons-inside-p (frame width)
  "Is every button still ON the window?  Asked more than once, because the answer CHANGES: the
status line is an application pane, an application pane asks for the room the last thing drawn
into it needed, and the ear's report gets longer the moment there is something to report.  Left
unpinned that pushes the last button past the right edge — where this gate's client can still
click it by coordinate and a person cannot reach it at all."
  (every (lambda (name)
           (multiple-value-bind (x y) (pane-point frame name)
             (declare (ignore y))
             (< 0 x width)))
         '(listen stop clear dictate)))

(defun await (test &key (timeout 90))
  (let ((start (get-internal-real-time)))
    (loop until (funcall test)
          do (when (> (/ (- (get-internal-real-time) start) internal-time-units-per-second) timeout)
               (return-from await nil))
             (sleep 0.1))
    t))

(defun words (text) (let ((s (string-trim " " text)))
                      (if (zerop (length s))
                          '()
                          (loop for a = 0 then (1+ b)
                                for b = (position #\Space s :start a)
                                collect (subseq s a b) while b))))

(defun to-pcm16 (samples)
  (let ((pcm (reed:make-pcm16 (length samples))))
    (dotimes (i (length samples) pcm)
      (setf (aref pcm i) (reed:clamp16 (round (* 32767 (aref samples i))))))))

;;; ---- the window ------------------------------------------------------------

(format t "~&listen window gate — ears ~:[NONE~;~:*~a~]~%~%" glass:*hearing-models*)

(defvar *frame*
  (let* ((p (clim-glass::find-glass-port :port *port*))
         (fm (clim:find-frame-manager :port p)))
    (climi::restart-port p)
    (clim:make-application-frame 'glass-listen:listen-box :width 560 :height 360
                                                         :frame-manager fm)))

(sb-thread:make-thread
 (lambda ()
   (handler-case (clim:run-frame-top-level *frame*)
     (serious-condition (e) (format t "~&FRAME DIED: ~a~%" e) (finish-output))))
 :name "listen-box")

(defvar *client* (connect *port*))
(handshake *client*)
(sleep 2.5)                             ; let the frame lay out before asking where its panes are

(defun box-text () (string (clim:gadget-value (pane *frame* 'transcript))))

(format t "~&-- 1. a window that has only been opened -------------------------------~%")

(check-that "the box starts empty" (zerop (length (box-text)))
            (format nil "box reads ~s" (box-text)))
(check-that "opening the window does not create an ear" (null glass:*session-ears*)
            (glass:hearing-report))
(check-that "and it says so" (string= "Idle" (glass-listen::%state))
            (glass-listen::%state))
(check-that "every button is on the window" (buttons-inside-p *frame* 560)
            (format nil "Listen ~d, Stop ~d, Clear ~d, Dictate ~d"
                    (round (pane-point *frame* 'listen)) (round (pane-point *frame* 'stop))
                    (round (pane-point *frame* 'clear)) (round (pane-point *frame* 'dictate))))

(cond
  ((null glass:*hearing-models*)
   (format t "~&-- no recognizer installed: the listening checks CANNOT be run ---------~%")
   (incf *fail*))
  ((not (probe-file *wav*))
   (format t "~&-- no ~a: the listening checks CANNOT be run --~%" *wav*)
   (incf *fail*))
  (t
   (format t "~&-- 2. Listen -----------------------------------------------------------~%")

   (multiple-value-bind (x y) (pane-point *frame* 'listen)
     (format t "     (Listen at ~d,~d)~%" (round x) (round y))
     (click *client* x y))
   (check-that "Listen makes the window's ear the session's"
               (await (lambda () glass:*session-ears*) :timeout 15)
               (glass:hearing-report))
   ;; the model is read on the ear's own thread — the window must not have gone quiet meanwhile
   (check-that "it says LOADING while the weights are being read, not `Listening'"
               (member (glass-listen::%state) '("Loading model..." "Listening" "Hearing...")
                       :test #'string=)
               (glass-listen::%state))
   (check-that "and it becomes ready"
               (await #'glass:hearing-ready-p :timeout 300)
               (glass:hearing-report))
   ;; asked again, now that the status line has a full report in it — this is the state the
   ;; window spends its life in, and it is the one that used to push Clear off the edge
   (check-that "every button is STILL on the window with the ear reporting"
               (buttons-inside-p *frame* 560)
               (format nil "Listen ~d, Stop ~d, Clear ~d, Dictate ~d"
                       (round (pane-point *frame* 'listen)) (round (pane-point *frame* 'stop))
                       (round (pane-point *frame* 'clear)) (round (pane-point *frame* 'dictate))))

   (format t "~&-- 3. the box fills up -------------------------------------------------~%")

   (multiple-value-bind (samples rate)
       (funcall (find-symbol "READ-WAV" :stave) *wav*)
     (let* ((rec (funcall (find-symbol "LOAD-RECOGNIZER" :stave) glass:*hearing-models*))
            (want (funcall (find-symbol "RECOGNIZE-SAMPLES" :stave) rec samples))
            (seconds (/ (length samples) (float rate 1d0))))
       (format t "     (~,1f s of speech into the session mix — nothing typed)~%" seconds)
       (glass:mixer-play (glass:session-mixer) (to-pcm16 samples) :name "listen-gate" :rate rate)
       (check-that "words nobody typed appear in the box"
                   (await (lambda () (>= (length (words (box-text))) 3))
                          :timeout (+ 120 (* 4 seconds)))
                   (format nil "box reads ~s" (box-text)))
       ;; and then the whole utterance, once the silence after it has ended it
       (await (lambda () (and (plusp (length (glass:hearing-heard)))
                              (string= "" (glass:hearing-partial))
                              (equal (box-text) (glass:hearing-text))))
              :timeout (+ 120 (* 4 seconds)))
       (let* ((got (box-text))
              (wer (* 100 (funcall (find-symbol "WORD-ERROR-RATE" :stave) want got))))
         (format t "     want: ~a~%     box:  ~a~%" want got)
         (check-that "and it is what was said" (< wer 15d0)
                     (format nil "~,1f% word error against the same file decoded directly" wer)))))

   (format t "~&-- 4. Clear ------------------------------------------------------------~%")

   (multiple-value-bind (x y) (pane-point *frame* 'clear)
     (format t "     (Clear at ~d,~d)~%" (round x) (round y))
     (click *client* x y))
   (sleep 1.5)                          ; several ticks: a box the ear refilled would show by now
   (check-that "Clear empties the box and it stays empty"
               (zerop (length (box-text)))
               (format nil "box reads ~s" (box-text)))
   (check-that "and the transcript behind it is gone too"
               (zerop (length (glass:hearing-text)))
               (format nil "the ear still holds ~s" (glass:hearing-text)))

   ;; The toggle only, not what it switches on — where the words GO is dictation-gate's job, and
   ;; it takes a whole second window to answer honestly.  What is checked here is the half that
   ;; belongs to this window: that a click reaches the mode, and that clicking it again leaves.
   (format t "~&-- 5. Dictate ----------------------------------------------------------~%")

   (multiple-value-bind (x y) (pane-point *frame* 'dictate)
     (format t "     (Dictate at ~d,~d)~%" (round x) (round y))
     (check-that "the desktop is not dictating until asked" (not (glass:dictating-p))
                 (glass:dictation-report))
     (click *client* x y)
     (check-that "clicking Dictate puts the desktop in dictation mode"
                 (await #'glass:dictating-p :timeout 20)
                 (glass:dictation-report))
     (check-that "and the window says where the words are going"
                 (member (glass-listen::%state) '("Dictating" "Dictating...") :test #'string=)
                 (format nil "~a — ~a" (glass-listen::%state) (glass-listen::%detail *frame*)))
     (click *client* x y)
     (check-that "clicking it again stops dictating, leaving the ear listening"
                 (and (await (lambda () (not (glass:dictating-p))) :timeout 20)
                      glass:*session-ears*
                      (glass:listening-p))
                 (format nil "~a | ~a" (glass:dictation-report) (glass:hearing-report))))

   (format t "~&-- 6. Stop -------------------------------------------------------------~%")

   (let ((mixer (glass:session-mixer)))
     (multiple-value-bind (x y) (pane-point *frame* 'stop)
       (format t "     (Stop at ~d,~d)~%" (round x) (round y))
       (click *client* x y))
     (check-that "Stop stops the ear" (await (lambda () (null glass:*session-ears*)) :timeout 40)
                 (glass:hearing-report))
     (check-that "and takes its sink off the mix with it"
                 (notany (lambda (s) (string= "ears" (glass::sink-name s)))
                         (glass::mixer-sinks mixer))
                 "no sink named ears remains"))))

(format t "~&~%listen window gate: ~d passed, ~d failed~%" *pass* *fail*)
(finish-output)
(sb-ext:exit :code (if (zerop *fail*) 0 1))
