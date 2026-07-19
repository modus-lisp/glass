;;;; vncauth-test.lisp — the glass VNC-auth wrapper (RFB key quirk + DES-ECB
;;;; response, DES from seal).  The DES known-answer vectors live in seal's own
;;;; self-test; here we check the RFB-specific wiring: a response verifies against
;;;; its own password and not another, and is 16 bytes.
;;;;   sbcl --non-interactive --load backend/inspect/vncauth-test.lisp
(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream))) (ql:quickload '(:glass :glass/vncauth))))
(in-package :glass)

(let ((fail 0))
  (flet ((check (ok fmt &rest args) (format t "  [~:[FAIL~;pass~]] ~?~%" ok fmt args) (unless ok (incf fail))))
    (format t "~&[vncauth: RFB VNC-auth wrapper over seal's DES]~%")
    (check *vnc-verify-fn* "loading :glass/vncauth installed the verifier hook")
    (let* ((challenge (make-array 16 :element-type '(unsigned-byte 8))))
      (dotimes (i 16) (setf (aref challenge i) (logand (* i 37) #xff)))
      (let ((resp (vnc-auth-response "hunter2" challenge)))
        (check (= (length resp) 16) "response is 16 bytes")
        (check (vnc-auth-verify "hunter2" challenge resp) "correct password verifies its own response")
        (check (not (vnc-auth-verify "wrongpw" challenge resp)) "a wrong password does NOT verify")
        ;; >8-char passwords are truncated to 8 by VNC: 'password' == 'password123'
        (check (equalp (vnc-auth-response "password" challenge) (vnc-auth-response "password123" challenge))
               "password truncated to 8 chars (VNC rule)"))))
  (format t "~%=> ~:[PASS~;FAIL (~d)~]~%" (plusp fail) fail)
  (finish-output) (sb-ext:exit :code (if (plusp fail) 1 0)))
