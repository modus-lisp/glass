;;;; oracle.lisp — drive the scry RFB server with an in-process RFB client and
;;;; check that the pixels the client receives are exactly what was drawn.
;;;;
;;;;   sbcl --non-interactive --load .../run-tests or (scry/test:run-tests)

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

(defstruct clientfb w h pixels)          ; pixels: (unsigned-byte 32) row-major, 0x00RRGGBB

(defun rfb-session (port)
  "Handshake, request the full framebuffer, return a CLIENTFB of what arrived."
  (let ((s (rfb-connect "127.0.0.1" port)))
    (rn s 12) (write-sequence (map 'vector #'char-code "RFB 003.008") s) (w8 s 10) (force-output s)
    (let ((n (r8 s))) (rn s n))          ; security types
    (w8 s 1) (force-output s)            ; pick None
    (r32 s)                              ; SecurityResult
    (w8 s 1) (force-output s)            ; ClientInit (shared)
    (let ((w (r16 s)) (h (r16 s)))
      (rn s 16)                          ; pixel format
      (let ((nl (r32 s))) (rn s nl))     ; desktop name
      ;; FramebufferUpdateRequest: non-incremental, full screen
      (w8 s 3) (w8 s 0) (w16 s 0) (w16 s 0) (w16 s w) (w16 s h) (force-output s)
      (let ((mt (r8 s)))
        (check (= mt 0) "update msg-type ~a (want 0)" mt)
        (r8 s)                           ; pad
        (let ((nrects (r16 s)) (fb (make-clientfb :w w :h h
                                     :pixels (make-array (* w h) :element-type '(unsigned-byte 32)))))
          (dotimes (i nrects)
            (let ((rx (r16 s)) (ry (r16 s)) (rw (r16 s)) (rh (r16 s)) (enc (r32 s)))
              (check (= enc 0) "encoding ~a (want 0 Raw)" enc)
              (let ((raw (rn s (* rw rh 4))) (o 0))
                (dotimes (yy rh)
                  (dotimes (xx rw)
                    (let ((b (aref raw o)) (g (aref raw (+ o 1))) (rr (aref raw (+ o 2))))
                      (setf (aref (clientfb-pixels fb) (+ (* (+ ry yy) w) (+ rx xx)))
                            (logior (ash rr 16) (ash g 8) b)))
                    (incf o 4))))))
          (close s)
          fb)))))

(defun cpx (fb x y) (aref (clientfb-pixels fb) (+ (* y (clientfb-w fb)) x)))

;;; ---- the test ---------------------------------------------------------------

(defun run-tests ()
  (setf *checks* 0 *fails* 0)
  (format t "~&[scry RFB oracle]~%")
  (let* ((port 5921)
         (fb (scry:make-framebuffer 200 150 scry:+blue+)))
    ;; draw a distinctive, checkable pattern
    (scry:fb-rect fb 40 20 60 40 scry:+red+)
    (scry:fb-put fb 5 5 scry:+green+)
    (scry:fb-frame fb 0 0 200 150 scry:+white+ 2)
    (scry:fb-put fb 199 149 (scry:rgb 18 52 86))       ; 0x123456, a corner sentinel
    ;; serve one client in a thread; connect and read the framebuffer back
    (let ((server (sb-thread:make-thread
                   (lambda () (ignore-errors (scry:serve-one fb port)))
                   :name "scry-server")))
      (let ((got (rfb-session port)))
        (ignore-errors (sb-thread:join-thread server))
        (check (and (= (clientfb-w got) 200) (= (clientfb-h got) 150))
               "dimensions ~ax~a" (clientfb-w got) (clientfb-h got))
        (check (= (cpx got 100 100) scry:+blue+) "background blue at (100,100): ~6,'0x" (cpx got 100 100))
        (check (= (cpx got 60 40) scry:+red+)  "red rect interior (60,40): ~6,'0x" (cpx got 60 40))
        (check (= (cpx got 39 20) scry:+blue+) "just left of red rect is blue")
        (check (= (cpx got 5 5) scry:+green+)  "green pixel (5,5): ~6,'0x" (cpx got 5 5))
        (check (= (cpx got 0 0) scry:+white+)  "white frame top-left: ~6,'0x" (cpx got 0 0))
        (check (= (cpx got 100 1) scry:+white+) "white frame top edge")
        (check (= (cpx got 199 149) #x123456)  "corner sentinel: ~6,'0x" (cpx got 199 149)))))
  (format t "----------------------------------~%")
  (format t "checks: ~d   failures: ~d   => ~a~%" *checks* *fails* (if (zerop *fails*) "PASS" "FAIL"))
  (zerop *fails*))
