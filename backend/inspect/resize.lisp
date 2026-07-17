;;;; resize.lisp — exercise desktop resize both ways over the glass McCLIM backend.
;;;;
;;;;   sbcl --dynamic-space-size 4096 --non-interactive --load backend/inspect/resize.lisp
;;;;
;;;; A frame whose canvas size is a slot; pressing +/- relays it out.  Headless
;;;; harness: (1) server->client — press '+', confirm the client is told the new
;;;; size via DesktopSize; (2) client->server — send SetDesktopSize (as a VNC
;;;; viewer would when its window is dragged), confirm the frame relaid out and
;;;; the client is told the resulting size.

(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:mcclim :mcclim-render :glass :chipz))
    (require :sb-concurrency)
    (asdf:load-asd "/home/claude/glass/backend/mcclim-glass.asd")
    (asdf:load-system :mcclim-glass)))

(defpackage #:glass-resize (:use #:cl))
(in-package #:glass-resize)

;;; ---- a frame that resizes its canvas on +/- --------------------------------

(clim:define-application-frame board ()
  ((w :initform 320 :accessor board-w)
   (h :initform 200 :accessor board-h))
  (:menu-bar nil)
  (:panes (canvas :application :display-function 'draw :scroll-bars nil
                  :width 320 :height 200))
  (:layouts (default canvas)))

(defun draw (frame pane)
  (clim:draw-rectangle* pane 0 0 (board-w frame) (board-h frame)
                        :ink (clim:make-rgb-color 0.12 0.14 0.20))
  (clim:draw-text* pane (format nil "~dx~d" (board-w frame) (board-h frame)) 20 40
                   :text-size 24 :ink clim:+white+))

(defun relayout (frame w h)
  (setf (board-w frame) w (board-h frame) h)
  (clim:layout-frame frame w h))

(defmethod clim:handle-event :after ((sheet climi::top-level-sheet-mixin)
                                     (event clim:key-press-event))
  (let ((frame (clim:pane-frame sheet)) (ch (clim:keyboard-event-character event)))
    (when (typep frame 'board)
      (case ch
        (#\+ (relayout frame 520 360))
        (#\- (relayout frame 240 160))
        (#\q (clim:frame-exit frame))))))

;;; ---- minimal RFB client that tracks desktop size ---------------------------

(defun r8 (s) (read-byte s)) (defun r16 (s) (logior (ash (r8 s) 8) (r8 s)))
(defun r32 (s) (logior (ash (r16 s) 16) (r16 s)))
(defun rn (s n) (let ((b (make-array n :element-type '(unsigned-byte 8)))) (read-sequence b s) b))
(defun w8 (s v) (write-byte (logand v #xff) s))
(defun w16 (s v) (w8 s (ash v -8)) (w8 s v)) (defun w32 (s v) (w16 s (ash v -16)) (w16 s v))

(defun connect (port)
  (loop repeat 300
        do (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
             (handler-case
                 (progn (sb-bsd-sockets:socket-connect sock (sb-bsd-sockets:make-inet-address "127.0.0.1") port)
                        (return-from connect (sb-bsd-sockets:socket-make-stream
                                              sock :input t :output t :element-type '(unsigned-byte 8) :buffering :full)))
               (error () (ignore-errors (sb-bsd-sockets:socket-close sock)) (sleep 0.05))))
        finally (error "no connect")))

(defun handshake (s)
  (rn s 12) (write-sequence (map 'vector #'char-code "RFB 003.008") s) (w8 s 10) (force-output s)
  (let ((n (r8 s))) (rn s n)) (w8 s 1) (force-output s) (r32 s)
  (w8 s 1) (force-output s)
  (let ((w (r16 s)) (h (r16 s))) (rn s 16) (let ((nl (r32 s))) (rn s nl))
    ;; SetEncodings: ZRLE, Raw, DesktopSize(-223), ExtendedDesktopSize(-308)
    (w8 s 2) (w8 s 0) (w16 s 4) (w32 s 16) (w32 s 0) (w32 s #xFFFFFF21) (w32 s #xFFFFFECC) (force-output s)
    (values w h)))

;; Read one FramebufferUpdate; return :size (W . H) if it's a DesktopSize rect,
;; else :pixels.  (We don't need to decode pixels for this test.)
(defun read-update (s)
  (w8 s 3) (w8 s 1) (w16 s 0) (w16 s 0) (w16 s 9999) (w16 s 9999) (force-output s)  ; incremental
  (r8 s) (r8 s)
  (let ((n (r16 s)) (result :pixels))
    (dotimes (i n)
      (let ((x (r16 s)) (y (r16 s)) (w (r16 s)) (h (r16 s)) (enc (r32 s)))
        (declare (ignore x y))
        (cond
          ((= enc #xFFFFFF21) (setf result (cons w h)))                 ; DesktopSize
          ((= enc 16) (let ((len (r32 s))) (rn s len)))                 ; ZRLE payload (skip)
          ((= enc 0) (rn s (* w h 4)))                                  ; Raw
          (t (error "enc ~x" enc)))))
    result))

(defun send-key (s ch)
  (let ((k (char-code ch)))
    (w8 s 4) (w8 s 1) (w16 s 0) (w32 s k) (force-output s)
    (w8 s 4) (w8 s 0) (w16 s 0) (w32 s k) (force-output s)))

(defun send-set-desktop-size (s w h)
  (w8 s 251) (w8 s 0) (w16 s w) (w16 s h) (w8 s 1) (w8 s 0)             ; 1 screen
  (w32 s 1) (w16 s 0) (w16 s 0) (w16 s w) (w16 s h) (w32 s 0) (force-output s))

(defun wait-for-size (s &optional (tries 8))
  "Poll updates until a DesktopSize rect arrives; return (w . h) or nil."
  (dotimes (i tries)
    (let ((u (read-update s)))
      (when (consp u) (return-from wait-for-size u))))
  nil)

;;; ---- drive it --------------------------------------------------------------

(let ((port 5936))
  (sb-thread:make-thread
   (lambda () (handler-case (clim-glass:run-frame 'board :port port :width 320 :height 200)
                (error (e) (format t "~&FRAME ERROR ~a~%" e))))
   :name "board")
  (let ((s (connect port)))
    (multiple-value-bind (w h) (handshake s)
      (format t "~&initial desktop: ~dx~d~%" w h)
      (sleep 1.0) (read-update s)                                        ; prime
      ;; (1) server-initiated: press '+', frame grows to 520x360
      (send-key s #\+)
      (let ((sz (wait-for-size s)))
        (format t "after '+' keystroke -> client told: ~a  (~a)~%" sz
                (if (and sz (= (car sz) 520) (= (cdr sz) 360)) "SERVER->CLIENT OK" "unexpected")))
      ;; (2) client-initiated: request 700x480 via SetDesktopSize
      (send-set-desktop-size s 700 480)
      (let ((sz (wait-for-size s)))
        (format t "after client SetDesktopSize(700x480) -> client told: ~a  (~a)~%" sz
                (if (and sz (= (car sz) 700) (= (cdr sz) 480)) "CLIENT->SERVER OK" "unexpected")))
      (send-key s #\q)
      (ignore-errors (close s)))))
(finish-output)
(sb-ext:exit)
