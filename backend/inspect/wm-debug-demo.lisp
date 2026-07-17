;;;; wm-debug-demo.lisp — the Genera-lineage graphical debugger (McCLIM's
;;;; clim-debugger) as a window on the glass OPEN LOOK desktop.  Evaluates a form
;;;; that errors a few frames deep; the debugger pops up with the condition,
;;;; restarts and an inspectable backtrace.  Clicks a frame to reveal its locals.
;;;;   sbcl --dynamic-space-size 4096 --non-interactive --load backend/inspect/wm-debug-demo.lisp

(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:mcclim :mcclim-render :clim-debugger :glass :zpng :chipz))
    (require :sb-concurrency)
    (asdf:load-asd "/home/claude/glass/backend/mcclim-glass.asd")
    (asdf:load-system :mcclim-glass)))

(defpackage #:wdbg (:use #:cl)) (in-package #:wdbg)
(defun r8 (s) (read-byte s)) (defun r16 (s) (logior (ash (r8 s) 8) (r8 s)))
(defun r32 (s) (logior (ash (r16 s) 16) (r16 s)))
(defun rn (s n) (let ((b (make-array n :element-type '(unsigned-byte 8)))) (read-sequence b s) b))
(defun w8 (s v) (write-byte (logand v #xff) s))
(defun w16 (s v) (w8 s (ash v -8)) (w8 s v)) (defun w32 (s v) (w16 s (ash v -16)) (w16 s v))
(defun connect (port)
  (loop repeat 500 do (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (handler-case (progn (sb-bsd-sockets:socket-connect sock (sb-bsd-sockets:make-inet-address "127.0.0.1") port)
                    (return-from connect (sb-bsd-sockets:socket-make-stream sock :input t :output t :element-type '(unsigned-byte 8) :buffering :full)))
      (error () (ignore-errors (sb-bsd-sockets:socket-close sock)) (sleep 0.05))))))
(defun handshake (s)
  (rn s 12) (write-sequence (map 'vector #'char-code "RFB 003.008") s) (w8 s 10) (force-output s)
  (let ((n (r8 s))) (rn s n)) (w8 s 1) (force-output s) (r32 s) (w8 s 1) (force-output s)
  (let ((w (r16 s)) (h (r16 s))) (rn s 16) (let ((nl (r32 s))) (rn s nl))
    (w8 s 2) (w8 s 0) (w16 s 2) (w32 s 16) (w32 s 0) (force-output s) (values w h)))
(defun read-frame (s w h dstate)
  (let ((cli (make-array (* w h) :element-type '(unsigned-byte 32) :initial-element 0)))
    (w8 s 3) (w8 s 0) (w16 s 0) (w16 s 0) (w16 s w) (w16 s h) (force-output s) (r8 s) (r8 s)
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
                          (dotimes (ly th) (let ((acc 0) (nb 0))
                            (dotimes (lx tw) (when (< nb bpp) (setf acc (logior (ash acc 8) (aref dec pos)) nb (+ nb 8)) (incf pos))
                              (decf nb bpp) (put lx ly (aref pal (logand (ash acc (- nb)) (1- (ash 1 bpp))))) (setf acc (logand acc (1- (ash 1 nb)))))))))
                       (t (error "subenc ~a" sub)))))))))
          ((= enc 0) (dotimes (yy rh) (dotimes (xx rw) (let ((b (rn s 4))) (setf (aref cli (+ (* (+ ry yy) w) (+ rx xx))) (logior (ash (aref b 2) 16) (ash (aref b 1) 8) (aref b 0)))))))
          (t (error "enc ~x" enc)))))
    cli))
(defun save-png (cli w h path)
  (let* ((png (make-instance 'zpng:png :width w :height h :color-type :truecolor)) (d (zpng:data-array png)))
    (dotimes (y h) (dotimes (x w) (let ((p (aref cli (+ (* y w) x))))
      (setf (aref d y x 0) (ldb (byte 8 16) p) (aref d y x 1) (ldb (byte 8 8) p) (aref d y x 2) (ldb (byte 8 0) p)))))
    (zpng:write-png png path) path))
(defun ptr (s mask x y) (w8 s 5) (w8 s mask) (w16 s x) (w16 s y) (force-output s) (sleep 0.05))
(defun lclick (s x y) (ptr s 0 x y) (ptr s 1 x y) (ptr s 0 x y) (sleep 0.5))

;; A few nested calls so the backtrace has real frames with locals.  notinline +
;; (debug 3) keep them as distinct frames whose locals the debugger can show.
(declaim (optimize (debug 3) (speed 0)) (notinline deep-c deep-b deep-a))
(defun deep-c (n divisor) (let ((label "about to divide")) (declare (ignorable label)) (/ n divisor)))
(defun deep-b (n) (let ((doubled (* n 2))) (deep-c doubled 0)))
(defun deep-a (start) (deep-b (1+ start)))

(let ((port 5961))
  (sb-thread:make-thread
   (lambda () (handler-case
                  (clim-glass:run-wm '((:debug (deep-a 20)))       ; errors 3 frames deep
                                     :port port :width 940 :height 640
                                     :menu '(("Debug (deep-a 20)" :debug (deep-a 20))
                                             ("Inspect *features*" :inspect *features*)
                                             ("Terminal"           :terminal)))
                (error (e) (format t "~&WM ERROR ~a~%" e)))))
  (let ((s (connect port)))
    (multiple-value-bind (w h) (handshake s)
      (format t "~&debug desktop: ~dx~d~%" w h)
      (sleep 4.5)                                   ; frame + debugger come up
      (let ((dstate (chipz:make-dstate 'chipz:zlib)))
        (save-png (read-frame s w h dstate) w h "/tmp/glass-debug.png")
        (format t "debugger captured~%")
        ;; click the DEEP-C backtrace frame row to expand its local variables
        (lclick s 120 324)
        (sleep 1.2)
        (save-png (read-frame s w h dstate) w h "/tmp/glass-debug-2.png")
        (format t "frame expanded~%"))
      (ignore-errors (close s)))))
(finish-output) (sb-ext:exit)
