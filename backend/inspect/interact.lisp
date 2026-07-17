;;;; interact.lisp — click a running stock app and screenshot before/after.
;;;;   sbcl ... --load backend/inspect/interact.lisp -- <frame> <w> <h> <clickx> <clicky>
;;;; Probes real gadget interaction and the multi-window (menu/popup) case.

(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:mcclim :mcclim-render :glass :zpng :chipz :clim-examples))
    (require :sb-concurrency)
    (asdf:load-asd "/home/claude/glass/backend/mcclim-glass.asd")
    (asdf:load-system :mcclim-glass)))

(defpackage #:glass-interact (:use #:cl))
(in-package #:glass-interact)

(defun r8 (s) (read-byte s)) (defun r16 (s) (logior (ash (r8 s) 8) (r8 s)))
(defun r32 (s) (logior (ash (r16 s) 16) (r16 s)))
(defun rn (s n) (let ((b (make-array n :element-type '(unsigned-byte 8)))) (read-sequence b s) b))
(defun w8 (s v) (write-byte (logand v #xff) s))
(defun w16 (s v) (w8 s (ash v -8)) (w8 s v)) (defun w32 (s v) (w16 s (ash v -16)) (w16 s v))

(defun connect (port)
  (loop repeat 400
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
    (w8 s 2) (w8 s 0) (w16 s 2) (w32 s 16) (w32 s 0) (force-output s)
    (values w h)))

(defun read-frame (s w h dstate)     ; dstate persists across reads (one zlib stream)
  (let ((cli (make-array (* w h) :element-type '(unsigned-byte 32) :initial-element 0)))
    (w8 s 3) (w8 s 0) (w16 s 0) (w16 s 0) (w16 s w) (w16 s h) (force-output s)
    (r8 s) (r8 s)
    (dotimes (i (r16 s))
      (let ((rx (r16 s)) (ry (r16 s)) (rw (r16 s)) (rh (r16 s)) (enc (r32 s)))
        (cond
          ((= enc 16)
           (let* ((len (r32 s)) (chunk (rn s len)) (dec (chipz:decompress nil dstate chunk)) (pos 0))
             (loop for ty from 0 below rh by 64 for th = (min 64 (- rh ty)) do
               (loop for tx from 0 below rw by 64 for tw = (min 64 (- rw tx)) do
                 (let ((sub (aref dec pos))) (incf pos)
                   (flet ((cp () (prog1 (logior (ash (aref dec (+ pos 2)) 16) (ash (aref dec (+ pos 1)) 8) (aref dec pos)) (incf pos 3)))
                          (put (lx ly c) (let ((px (+ (* (+ ry ty ly) w) (+ rx tx lx)))) (when (< px (length cli)) (setf (aref cli px) c)))))
                     (cond
                       ((= sub 0) (dotimes (ly th) (dotimes (lx tw) (put lx ly (cp)))))
                       ((= sub 1) (let ((c (cp))) (dotimes (ly th) (dotimes (lx tw) (put lx ly c)))))
                       ((<= 2 sub 16)
                        (let ((pal (make-array sub)) (bpp (cond ((<= sub 2) 1) ((<= sub 4) 2) (t 4))))
                          (dotimes (k sub) (setf (aref pal k) (cp)))
                          (dotimes (ly th)
                            (let ((acc 0) (nb 0))
                              (dotimes (lx tw)
                                (when (< nb bpp) (setf acc (logior (ash acc 8) (aref dec pos)) nb (+ nb 8)) (incf pos))
                                (decf nb bpp)
                                (put lx ly (aref pal (logand (ash acc (- nb)) (1- (ash 1 bpp)))))
                                (setf acc (logand acc (1- (ash 1 nb)))))))))
                       (t (error "subenc ~a" sub)))))))))
          ((= enc 0) (dotimes (yy rh) (dotimes (xx rw)
                       (let ((b (rn s 4))) (setf (aref cli (+ (* (+ ry yy) w) (+ rx xx)))
                                                 (logior (ash (aref b 2) 16) (ash (aref b 1) 8) (aref b 0)))))))
          (t (error "enc ~x" enc)))))
    cli))

(defun save-png (cli w h path)
  (let* ((png (make-instance 'zpng:png :width w :height h :color-type :truecolor)) (d (zpng:data-array png)))
    (dotimes (y h) (dotimes (x w) (let ((p (aref cli (+ (* y w) x))))
      (setf (aref d y x 0) (ldb (byte 8 16) p) (aref d y x 1) (ldb (byte 8 8) p) (aref d y x 2) (ldb (byte 8 0) p)))))
    (zpng:write-png png path) path))

(defun send-ptr (s mask x y) (w8 s 5) (w8 s mask) (w16 s x) (w16 s y) (force-output s))

(let* ((argv (member "--" sb-ext:*posix-argv* :test #'string=))
       (frame-class (let ((*package* (find-package :cl-user))) (read-from-string (second argv))))
       (w (parse-integer (third argv))) (h (parse-integer (fourth argv)))
       (cx (parse-integer (fifth argv))) (cy (parse-integer (sixth argv)))
       (port 5941))
  (sb-thread:make-thread
   (lambda () (handler-case (clim-glass:run-frame frame-class :port port :width w :height h)
                (error (e) (format t "~&FRAME ERROR ~a~%" e))))
   :name "f")
  (let ((s (connect port)))
    (multiple-value-bind (sw sh) (handshake s)
      (sleep 2.5)
      (let ((dstate (chipz:make-dstate 'chipz:zlib)))
      (let ((before (read-frame s sw sh dstate)))
        (save-png before sw sh "/tmp/glass-interact-0.png")
        (send-ptr s 0 cx cy) (send-ptr s 1 cx cy) (send-ptr s 0 cx cy)
        (sleep 1.0)
        (let* ((after (read-frame s sw sh dstate))
               (diff (loop for i below (length before) count (/= (aref before i) (aref after i)))))
          (save-png after sw sh "/tmp/glass-interact-1.png")
          (format t "~&click (~d,~d): ~d px changed -> ~a~%" cx cy diff
                  (if (plusp diff) "REACTED" "no change"))
          ;; dismiss by clicking outside the menu; confirm the popup is erased
          ;; (menu sheet destroyed -> destroy-mirror -> recomposite)
          (send-ptr s 0 500 300) (send-ptr s 1 500 300) (send-ptr s 0 500 300) (sleep 1.0)
          (let* ((dismissed (read-frame s sw sh dstate))
                 (d2 (loop for i below (length after) count (/= (aref after i) (aref dismissed i)))))
            (save-png dismissed sw sh "/tmp/glass-interact-2.png")
            (format t "dismiss (click outside): ~d px changed -> ~a~%" d2
                    (if (plusp d2) "POPUP CLOSED" "no change"))))))
      (ignore-errors (close s)))))
(finish-output)
(sb-ext:exit)
