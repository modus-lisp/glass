;;;; vnc-auth-e2e.lisp — end-to-end VNC authentication against a *vnc-password*-
;;;; protected glass server: the correct password reaches ServerInit; a wrong one
;;;; gets SecurityResult=failed and is dropped.  Exercises the real server handshake
;;;; (3.8 type list -> VNC auth -> DES verify), not just the DES primitive.
;;;;   sbcl --non-interactive --load backend/inspect/vnc-auth-e2e.lisp
(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream))) (ql:quickload '(:glass :glass/vncauth))))
(defpackage #:ae (:use #:cl)) (in-package #:ae)

(defun r8 (s) (read-byte s)) (defun r16 (s) (logior (ash (r8 s) 8) (r8 s)))
(defun r32 (s) (logior (ash (r16 s) 16) (r16 s)))
(defun rn (s n) (let ((b (make-array n :element-type '(unsigned-byte 8)))) (read-sequence b s) b))
(defun w8 (s v) (write-byte (logand v #xff) s)) (defun wn (s b) (write-sequence b s))
(defun connect (port)
  (loop repeat 300 do
    (let ((sk (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
      (handler-case (progn (sb-bsd-sockets:socket-connect sk (sb-bsd-sockets:make-inet-address "127.0.0.1") port)
                      (return-from connect (sb-bsd-sockets:socket-make-stream sk :input t :output t
                                             :element-type '(unsigned-byte 8) :buffering :full)))
        (error () (ignore-errors (sb-bsd-sockets:socket-close sk)) (sleep 0.03))))))

(defun do-auth (s password)
  "Run the client side of the 3.8 VNC-auth handshake with PASSWORD; return the u32
   SecurityResult (0 = OK)."
  (rn s 12) (wn s (map '(vector (unsigned-byte 8)) #'char-code (format nil "RFB 003.008~a" #\Newline)))
  (force-output s)
  (let ((n (r8 s)))                                   ; security-type list: count, then types
    (assert (>= n 1)) (let ((types (rn s n))) (assert (find 2 types))))  ; VNC auth offered
  (w8 s 2) (force-output s)                           ; choose VNC auth
  (let* ((challenge (rn s 16))
         (resp (glass:vnc-auth-response password challenge)))
    (wn s resp) (force-output s)
    (r32 s)))                                          ; SecurityResult

(let ((port 5973) (pw "sekret42") (fail 0))
  (setf glass:*vnc-password* pw)
  (sb-thread:make-thread (lambda () (ignore-errors (glass:serve (glass:make-framebuffer 80 60) port :name "ae")))
                         :name "ae-server")
  (flet ((check (ok fmt &rest args) (format t "  [~:[FAIL~;pass~]] ~?~%" ok fmt args) (unless ok (incf fail))))
    (format t "~&[VNC auth end-to-end — *vnc-password* = ~s]~%" pw)
    ;; correct password -> admitted (SecurityResult 0, ServerInit follows)
    (let ((s (connect port)))
      (let ((res (do-auth s pw)))
        (check (= res 0) "correct password -> SecurityResult OK (~d)" res)
        (when (= res 0)
          (w8 s 1) (force-output s)                    ; ClientInit
          (let ((w (r16 s)) (h (r16 s))) (rn s 16) (let ((nl (r32 s))) (rn s nl))
            (check (and (= w 80) (= h 60)) "reached ServerInit ~dx~d (admitted)" w h))))
      (ignore-errors (close s)))
    ;; wrong password -> rejected (SecurityResult failed)
    (let ((s (connect port)))
      (let ((res (do-auth s "wrongpass")))
        (check (= res 1) "wrong password -> SecurityResult FAILED (~d)" res))
      (ignore-errors (close s))))
  (format t "~%=> ~:[PASS~;FAIL (~d)~]~%" (plusp fail) fail)
  (finish-output) (sb-ext:exit :code (if (plusp fail) 1 0)))
