;;;; oracle.lisp — drive the scry RFB server with an in-process RFB client and
;;;; check (a) the pixels received equal what was drawn, and (b) dirty-region
;;;; tracking: after a change, an incremental update carries ONLY the changed
;;;; tiles, yet the reconstructed client framebuffer still matches the server.
;;;;
;;;;   (asdf:load-system "scry/test") (scry/test:run-tests)

(defpackage #:scry/test
  (:use #:cl)
  (:export #:run-tests))
(in-package #:scry/test)

(defvar *checks* 0) (defvar *fails* 0)
(defun check (ok fmt &rest args)
  (incf *checks*) (unless ok (incf *fails*) (format t "  FAIL: ~?~%" fmt args)))

;;; ---- a minimal RFB client (big-endian wire) --------------------------------

(defun r8  (s) (read-byte s))
(defun r16 (s) (logior (ash (read-byte s) 8) (read-byte s)))
(defun r32 (s) (logior (ash (r16 s) 16) (r16 s)))
(defun rn  (s n) (let ((b (make-array n :element-type '(unsigned-byte 8)))) (read-sequence b s) b))
(defun w8  (s v) (write-byte (logand v #xff) s))
(defun w16 (s v) (w8 s (ash v -8)) (w8 s v))

(defun rfb-connect (host port)
  (loop repeat 60
        do (handler-case
               (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
                 (sb-bsd-sockets:socket-connect sock (sb-bsd-sockets:make-inet-address host) port)
                 (return (sb-bsd-sockets:socket-make-stream
                          sock :input t :output t :element-type '(unsigned-byte 8) :buffering :full)))
             (error () (sleep 0.05)))))

(defun rfb-open (port)
  "Handshake; return (values stream width height)."
  (let ((s (rfb-connect "127.0.0.1" port)))
    (rn s 12) (write-sequence (map 'vector #'char-code "RFB 003.008") s) (w8 s 10) (force-output s)
    (let ((n (r8 s))) (rn s n)) (w8 s 1) (force-output s) (r32 s)   ; None security
    (w8 s 1) (force-output s)                                       ; ClientInit
    (let ((w (r16 s)) (h (r16 s)))
      (rn s 16) (let ((nl (r32 s))) (rn s nl))                      ; pixfmt + name
      (values s w h))))

(defun request (s inc w h)
  (w8 s 3) (w8 s inc) (w16 s 0) (w16 s 0) (w16 s w) (w16 s h) (force-output s))

(defun read-update (s w cli)
  "Read one FramebufferUpdate, apply its Raw rects into CLI (a width-W u32 array).
   Returns (values n-rects total-pixels)."
  (let ((mt (r8 s))) (check (= mt 0) "update msg-type ~a (want 0)" mt))
  (r8 s)                                                            ; pad
  (let ((nrects (r16 s)) (total 0))
    (dotimes (i nrects)
      (let ((rx (r16 s)) (ry (r16 s)) (rw (r16 s)) (rh (r16 s)) (enc (r32 s)))
        (check (= enc 0) "encoding ~a (want 0 Raw)" enc)
        (let ((raw (rn s (* rw rh 4))) (o 0))
          (incf total (* rw rh))
          (dotimes (yy rh)
            (dotimes (xx rw)
              (setf (aref cli (+ (* (+ ry yy) w) (+ rx xx)))
                    (logior (ash (aref raw (+ o 2)) 16) (ash (aref raw (+ o 1)) 8) (aref raw o)))
              (incf o 4))))))
    (values nrects total)))

;;; ---- the test ---------------------------------------------------------------

(defun run-tests ()
  (setf *checks* 0 *fails* 0)
  (format t "~&[scry RFB oracle]~%")
  (let* ((port 5921) (w 200) (h 150)
         (fb (scry:make-framebuffer w h scry:+blue+)))
    (scry:fb-rect fb 40 20 60 40 scry:+red+)
    (scry:fb-put fb 5 5 scry:+green+)
    (scry:fb-frame fb 0 0 w h scry:+white+ 2)
    (scry:fb-put fb 199 149 (scry:rgb 18 52 86))
    (let ((server (sb-thread:make-thread
                   (lambda () (ignore-errors (scry:serve-one fb port))) :name "scry-server")))
      (multiple-value-bind (s sw sh) (rfb-open port)
        (check (and (= sw w) (= sh h)) "dimensions ~ax~a" sw sh)
        (let ((cli (make-array (* w h) :element-type '(unsigned-byte 32))))
          ;; --- full frame ---
          (request s 0 w h)
          (multiple-value-bind (nr total) (read-update s w cli)
            (declare (ignore nr))
            (check (= total (* w h)) "full frame = all ~a px, got ~a" (* w h) total))
          (check (equalp cli (scry:fb-pixels fb)) "client matches server after full frame")
          (flet ((cpx (x y) (aref cli (+ (* y w) x))))
            (check (= (cpx 100 100) scry:+blue+) "background blue")
            (check (= (cpx 60 40) scry:+red+)    "red rect interior")
            (check (= (cpx 5 5) scry:+green+)    "green pixel")
            (check (= (cpx 0 0) scry:+white+)    "white frame")
            (check (= (cpx 199 149) #x123456)    "corner sentinel"))
          ;; --- dirty-region tracking: change one small area, request incremental ---
          (scry:fb-rect fb 150 100 20 20 scry:+green+)          ; touches ~2 tiles
          (request s 1 w h)
          (multiple-value-bind (nr total) (read-update s w cli)
            (check (< total (* w h)) "incremental (~a px) << full (~a)" total (* w h))
            (check (plusp total) "incremental sent something (~a rects)" nr)
            (check (<= total 4096) "incremental is small: ~a px in ~a rects (want <= 4096)" total nr))
          (check (equalp cli (scry:fb-pixels fb)) "client matches server after incremental")
          (check (= (aref cli (+ (* 105 w) 155)) scry:+green+) "the change landed on the client")
          (close s)
          (ignore-errors (sb-thread:join-thread server))))))
  (format t "----------------------------------~%")
  (format t "checks: ~d   failures: ~d   => ~a~%" *checks* *fails* (if (zerop *fails*) "PASS" "FAIL"))
  (zerop *fails*))
