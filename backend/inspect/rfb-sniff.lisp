;;;; rfb-sniff.lisp — a diagnostic RFB server that mirrors glass's handshake, then
;;;; LOGS everything the client sends (decoded) and replies with the simplest
;;;; possible frame: a Raw red|blue split.  Point a picky client (macOS Screen
;;;; Sharing) at <host>:5903 to see (a) exactly what pixel format / encodings it
;;;; demands and (b) whether it can display a plain Raw frame in our format at all.
;;;;   sbcl --non-interactive --load backend/inspect/rfb-sniff.lisp   (logs to stdout)
(require :asdf)
(require :sb-bsd-sockets)
(defpackage #:sniff (:use #:cl)) (in-package #:sniff)

(defvar *w* 400) (defvar *h* 300)
(defun lg (fmt &rest args) (format t "~&[sniff] ~?~%" fmt args) (finish-output))

(defun r8 (s) (read-byte s)) (defun r16 (s) (logior (ash (r8 s) 8) (r8 s)))
(defun r32 (s) (logior (ash (r16 s) 16) (r16 s)))
(defun r32s (s) (let ((v (r32 s))) (if (>= v #x80000000) (- v #x100000000) v)))  ; signed (pseudo-encs)
(defun rn (s n) (let ((b (make-array n :element-type '(unsigned-byte 8)))) (read-sequence b s) b))
(defun w8 (s v) (write-byte (logand v #xff) s))
(defun w16 (s v) (w8 s (ash v -8)) (w8 s v)) (defun w32 (s v) (w16 s (ash v -16)) (w16 s v))
(defun wn (s b) (write-sequence b s))

(defun enc-name (e)
  (case e (0 "Raw") (1 "CopyRect") (5 "Hextile") (15 "TRLE") (16 "ZRLE")
        (-223 "DesktopSize") (-239 "Cursor") (-308 "ExtDesktopSize")
        (t (format nil "~d" e))))

(defun log-pixel-format (pf)
  (flet ((b (i) (aref pf i)) (u16 (i) (logior (ash (aref pf i) 8) (aref pf (1+ i)))))
    (lg "  CLIENT SetPixelFormat: bpp=~d depth=~d big-endian=~d true-color=~d~
         ~%           r-max=~d g-max=~d b-max=~d  r-shift=~d g-shift=~d b-shift=~d"
        (b 0) (b 1) (b 2) (b 3) (u16 4) (u16 6) (u16 8) (b 10) (b 11) (b 12))))

(defun send-raw-frame (s)
  "One FramebufferUpdate: a single Raw rect, left half red, right half blue, in OUR
   server format (32bpp, little-endian, r-shift 16 / g 8 / b 0 -> wire bytes B,G,R,0)."
  (w8 s 0) (w8 s 0) (w16 s 1)                    ; msg 0, pad, 1 rect
  (w16 s 0) (w16 s 0) (w16 s *w*) (w16 s *h*) (w32 s 0)   ; x y w h, enc=Raw
  (let ((row (make-array (* *w* 4) :element-type '(unsigned-byte 8))))
    (dotimes (x *w*)
      (let ((red (< x (floor *w* 2))))            ; red 0xFF0000 -> [00 00 FF 00]; blue 0x0000FF -> [FF 00 00 00]
        (setf (aref row (+ (* x 4) 0)) (if red #x00 #xFF)
              (aref row (+ (* x 4) 1)) #x00
              (aref row (+ (* x 4) 2)) (if red #xFF #x00)
              (aref row (+ (* x 4) 3)) #x00)))
    (dotimes (y *h*) (wn s row)))
  (force-output s)
  (lg "  -> sent Raw ~dx~d red|blue frame" *w* *h*))

(defun serve-client (s)
  ;; handshake, mirroring glass exactly
  (wn s (map '(vector (unsigned-byte 8)) #'char-code "RFB 003.008")) (w8 s 10) (force-output s)
  (let ((ver (rn s 12))) (lg "CLIENT version: ~s" (map 'string #'code-char ver)))
  (w8 s 1) (w8 s 1) (force-output s)              ; 1 security type: None(1)
  (lg "CLIENT security choice: ~d (1=None)" (r8 s))
  (w32 s 0) (force-output s)                      ; SecurityResult OK
  (lg "CLIENT ClientInit shared-flag: ~d" (r8 s))
  (w16 s *w*) (w16 s *h*)                         ; ServerInit
  (w8 s 32) (w8 s 24) (w8 s 0) (w8 s 1)           ; our pixel format (32/24/LE/truecolor)
  (w16 s 255) (w16 s 255) (w16 s 255) (w8 s 16) (w8 s 8) (w8 s 0) (w8 s 0) (w8 s 0) (w8 s 0)
  (let ((nm (map '(vector (unsigned-byte 8)) #'char-code "glass-sniff"))) (w32 s (length nm)) (wn s nm))
  (force-output s)
  (lg "handshake done — awaiting client messages...")
  (loop
    (let ((msg (r8 s)))
      (case msg
        (0 (rn s 3) (log-pixel-format (rn s 16)))
        (2 (r8 s) (let ((n (r16 s)))
             (lg "  CLIENT SetEncodings (~d): ~{~a ~}" n (loop repeat n collect (enc-name (r32s s))))))
        (3 (let ((inc (r8 s)) (x (r16 s)) (y (r16 s)) (w (r16 s)) (h (r16 s)))
             (lg "  CLIENT FBUpdateRequest inc=~d ~d,~d ~dx~d" inc x y w h)
             (send-raw-frame s)))
        (4 (r8 s) (r16 s) (lg "  CLIENT KeyEvent key=~d" (r32 s)))
        (5 (let ((m (r8 s)) (x (r16 s)) (y (r16 s))) (lg "  CLIENT PointerEvent mask=~d ~d,~d" m x y)))
        (t (lg "  CLIENT unknown message type ~d — stopping" msg) (return))))))

(let ((listen (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
  (setf (sb-bsd-sockets:sockopt-reuse-address listen) t)
  (sb-bsd-sockets:socket-bind listen (sb-bsd-sockets:make-inet-address "0.0.0.0") 5903)
  (sb-bsd-sockets:socket-listen listen 8)
  (lg "RFB sniffer listening on 0.0.0.0:5903 — point macOS Screen Sharing here")
  (loop
    (handler-case
        (let ((s (sb-bsd-sockets:socket-make-stream (sb-bsd-sockets:socket-accept listen)
                                                    :input t :output t :element-type '(unsigned-byte 8) :buffering :full)))
          (lg "=== new connection ===")
          (unwind-protect (serve-client s) (ignore-errors (close s))))
      (end-of-file () (lg "  (client closed the connection)"))
      (error (e) (lg "  error: ~a: ~a" (type-of e) e)))))
