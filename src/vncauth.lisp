;;;; vncauth.lisp — VNC Authentication response/verify for glass's RFB server.
;;;;
;;;; The DES primitive lives in seal (the stack's crypto floor); this is just the
;;;; RFB-specific wrapper: the VNC key quirk (<=8 password bytes, each byte's bits
;;;; reversed) applied over the two 8-byte halves of the 16-byte challenge.  It is
;;;; the OPTIONAL :glass/vncauth system (depends on seal) so that core :glass stays
;;;; crypto-dependency-free; loading it installs glass::*vnc-verify-fn*, which is
;;;; what makes *vnc-password* actually enforced.

(in-package #:glass)

(defun %reverse-bits (byte)
  (let ((r 0)) (dotimes (i 8 r) (setf r (logior (ash r 1) (logand (ash byte (- i)) 1))))))

(defun vnc-key-bytes (password)
  "The 8-byte DES key from PASSWORD: <=8 chars, zero-padded, each byte's bits reversed."
  (let ((key (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0)))
    (dotimes (i (min 8 (length password)) key)
      (setf (aref key i) (%reverse-bits (char-code (char password i)))))))

(defun vnc-auth-response (password challenge)
  "The 16-byte DES-ECB response a client returns for CHALLENGE (16 bytes) under
   PASSWORD — each 8-byte half encrypted with seal's DES."
  (let ((sched (seal:des-key-schedule (vnc-key-bytes password)))
        (out (make-array 16 :element-type '(unsigned-byte 8))))
    (dotimes (blk 2 out)
      (replace out (seal:des-encrypt-block challenge sched :start (* blk 8)) :start1 (* blk 8)))))

(defun vnc-auth-verify (password challenge response)
  "T iff RESPONSE is the correct VNC-auth reply to CHALLENGE for PASSWORD."
  (equalp (vnc-auth-response password challenge) response))

;; Installing the hook is what turns *vnc-password* from advisory into enforced.
(setf *vnc-verify-fn* 'vnc-auth-verify)
