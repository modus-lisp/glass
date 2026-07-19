;;;; vncauth-test.lisp — validate the from-scratch DES against the FIPS 46-3 test
;;;; vector, and the VNC-auth response round-trip (encrypt == verify).
;;;;   sbcl --non-interactive --load backend/inspect/vncauth-test.lisp
(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream))) (ql:quickload '(:glass))))
(in-package :glass)

(let ((fail 0))
  (flet ((check (ok fmt &rest args) (format t "  [~:[FAIL~;pass~]] ~?~%" ok fmt args) (unless ok (incf fail))))
    (format t "~&[vncauth: DES + VNC challenge/response]~%")
    ;; FIPS 46-3 known-answer: key 133457799BBCDFF1, PT 0123456789ABCDEF -> CT 85E813540F0AB405
    (let ((ct (des-encrypt-block (des-subkeys #x133457799BBCDFF1) #x0123456789ABCDEF)))
      (check (= ct #x85E813540F0AB405) "FIPS DES vector: got ~16,'0X (expect 85E813540F0AB405)" ct))
    ;; a second independent DES vector (all-zero key + plaintext -> 8CA64DE9C1B123A7)
    (let ((ct (des-encrypt-block (des-subkeys 0) 0)))
      (check (= ct #x8CA64DE9C1B123A7) "DES zero vector: got ~16,'0X (expect 8CA64DE9C1B123A7)" ct))
    ;; VNC round-trip: the response we'd compute verifies; a wrong password does not
    (let* ((challenge (make-array 16 :element-type '(unsigned-byte 8)))
           (pw "hunter2"))
      (dotimes (i 16) (setf (aref challenge i) (logand (* i 37) #xff)))
      (let ((resp (vnc-auth-response pw challenge)))
        (check (vnc-auth-verify pw challenge resp) "correct password verifies its own response")
        (check (not (vnc-auth-verify "wrongpw" challenge resp)) "a wrong password does NOT verify")
        (check (= (length resp) 16) "response is 16 bytes"))))
  (format t "~%=> ~:[PASS~;FAIL (~d)~]~%" (plusp fail) fail)
  (finish-output) (sb-ext:exit :code (if (plusp fail) 1 0)))
