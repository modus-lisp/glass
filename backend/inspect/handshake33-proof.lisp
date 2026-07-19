;;;; handshake33-proof.lisp — prove glass completes the RFB 3.3 handshake (what
;;;; macOS Screen Sharing speaks), not just 3.8.  A raw 3.3 client connects, and we
;;;; assert the server dictates a single u32 security type (no type-list, no
;;;; SecurityResult), then reaches ServerInit and delivers a frame.
;;;;   sbcl --non-interactive --load backend/inspect/handshake33-proof.lisp
(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream))) (ql:quickload '(:glass))))
(defpackage #:hs (:use #:cl)) (in-package #:hs)

(defun r8 (s) (read-byte s)) (defun r16 (s) (logior (ash (r8 s) 8) (r8 s)))
(defun r32 (s) (logior (ash (r16 s) 16) (r16 s)))
(defun rn (s n) (let ((b (make-array n :element-type '(unsigned-byte 8)))) (read-sequence b s) b))
(defun w8 (s v) (write-byte (logand v #xff) s))
(defun w16 (s v) (w8 s (ash v -8)) (w8 s v)) (defun w32 (s v) (w16 s (ash v -16)) (w16 s v))
(defun wn (s b) (write-sequence b s))
(defun connect (port)
  (loop repeat 300 do
    (let ((sk (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
      (handler-case
          (progn (sb-bsd-sockets:socket-connect sk (sb-bsd-sockets:make-inet-address "127.0.0.1") port)
                 (return-from connect (sb-bsd-sockets:socket-make-stream sk :input t :output t
                                        :element-type '(unsigned-byte 8) :buffering :full)))
        (error () (ignore-errors (sb-bsd-sockets:socket-close sk)) (sleep 0.03))))))

(let* ((fb (glass:make-framebuffer 100 60 (glass:rgb 10 20 30))) (port 5971) (fail 0))
  (sb-thread:make-thread (lambda () (ignore-errors (glass:serve fb port :name "hs33"))) :name "hs33-server")
  (flet ((check (ok fmt &rest args) (format t "  [~:[FAIL~;pass~]] ~?~%" ok fmt args) (unless ok (incf fail))))
    (let ((s (connect port)))
      (format t "~&[RFB 3.3 handshake — what macOS speaks]~%")
      (let ((sv (map 'string #'code-char (rn s 12))))                  ; server version
        (check (search "RFB 003.008" sv) "server offered version ~s" (string-trim '(#\Newline) sv)))
      (wn s (map '(vector (unsigned-byte 8)) #'char-code (format nil "RFB 003.003~a" #\Newline)))
      (force-output s)
      ;; 3.3: server dictates ONE u32 security type; must be 1 (None), NOT the 3.8 [count][type] list
      (let ((sec (r32 s)))
        (check (= sec 1) "server sent a single u32 security-type = ~d (expect 1=None, no list/result)" sec))
      (w8 s 1) (force-output s)                                        ; ClientInit shared-flag
      (let ((w (r16 s)) (h (r16 s)))                                   ; ServerInit
        (rn s 16) (let ((nl (r32 s))) (rn s nl))
        (check (and (= w 100) (= h 60)) "ServerInit reports ~dx~d (expect 100x60)" w h))
      ;; ask for a frame (Raw) and confirm one arrives -> the session actually opened
      (w8 s 2) (w8 s 0) (w16 s 1) (w32 s 0)                            ; SetEncodings: Raw
      (w8 s 3) (w8 s 0) (w16 s 0) (w16 s 0) (w16 s 100) (w16 s 60) (force-output s)  ; FBUpdateRequest
      (let ((mt (r8 s))) (r8 s) (let ((nr (r16 s)))
        (check (and (= mt 0) (>= nr 1)) "got a FramebufferUpdate (~d rect~:p) — session opened" nr)
        (when (plusp nr)                                              ; drain the rect so we don't leak
          (dotimes (i nr) (r16 s)(r16 s)(let ((rw (r16 s))(rh (r16 s))(e (r32 s)))
                                            (when (= e 0) (rn s (* rw rh 4))))))))
      (ignore-errors (close s))))
  (format t "~%=> ~:[PASS~;FAIL (~d)~]~%" (plusp fail) fail)
  (finish-output) (sb-ext:exit :code (if (plusp fail) 1 0)))
