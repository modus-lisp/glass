;;;; rfb.lisp — a VNC/RFB server (RFC 6143).
;;;;
;;;; Speaks RFB 3.8 over TCP: version + security handshake (None auth), ServerInit
;;;; advertising our 32-bit X8R8G8B8 pixel format, then the message loop —
;;;; FramebufferUpdateRequest is answered with a Raw-encoded update of the current
;;;; framebuffer; KeyEvent / PointerEvent are dispatched to caller callbacks.
;;;;
;;;; v1 keeps it simple and always correct: every update sends the requested rect
;;;; in full (Raw), with a short throttle on incremental polls so a client doesn't
;;;; spin the CPU.  Dirty-region tracking and compact encodings (Hextile/ZRLE/
;;;; Tight) are the efficiency follow-up; Raw is understood by every VNC client.

(in-package #:scry)

(defvar *desktop-name* "scry")

(defun string->bytes (s)
  (map '(simple-array (unsigned-byte 8) (*)) #'char-code s))

;;; ---- stream byte I/O (big-endian on the wire, per RFB) ----------------------

(declaim (inline w-u8 w-u16 w-u32 r-u8 r-u16 r-u32))
(defun w-u8  (s v) (write-byte (logand v #xff) s))
(defun w-u16 (s v) (w-u8 s (ash v -8)) (w-u8 s v))
(defun w-u32 (s v) (w-u16 s (ash v -16)) (w-u16 s v))
(defun w-bytes (s b) (write-sequence b s))
(defun r-u8  (s) (read-byte s))
(defun r-u16 (s) (logior (ash (read-byte s) 8) (read-byte s)))
(defun r-u32 (s) (logior (ash (r-u16 s) 16) (r-u16 s)))
(defun r-bytes (s n)
  (let ((b (make-array n :element-type '(unsigned-byte 8)))) (read-sequence b s) b))
(defun skip (s n) (dotimes (i n) (read-byte s)))

;;; ---- transport --------------------------------------------------------------

(defun tcp-listen (port &key (backlog 8) (address "0.0.0.0"))
  "A listening TCP socket bound to PORT."
  (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address sock) t)
    (sb-bsd-sockets:socket-bind sock (sb-bsd-sockets:make-inet-address address) port)
    (sb-bsd-sockets:socket-listen sock backlog)
    sock))

(defun accept-stream (listen)
  (let ((sock (sb-bsd-sockets:socket-accept listen)))
    (sb-bsd-sockets:socket-make-stream
     sock :input t :output t :element-type '(unsigned-byte 8) :buffering :full)))

;;; ---- RFB pixel format + handshake ------------------------------------------

;; 16-byte PIXEL_FORMAT: bpp=32 depth=24 big-endian=0 true-colour=1,
;; {r,g,b}-max=255, r-shift=16 g-shift=8 b-shift=0.  Matches fb pixel 0x00RRGGBB.
(defun write-pixel-format (s)
  (w-u8 s 32) (w-u8 s 24) (w-u8 s 0) (w-u8 s 1)
  (w-u16 s 255) (w-u16 s 255) (w-u16 s 255)
  (w-u8 s 16) (w-u8 s 8) (w-u8 s 0)
  (w-u8 s 0) (w-u8 s 0) (w-u8 s 0))

(defun handshake (fb s name)
  "RFB 3.8 handshake through ServerInit.  Returns T on success."
  (w-bytes s (string->bytes "RFB 003.008")) (w-u8 s 10) (force-output s)
  (r-bytes s 12)                              ; client ProtocolVersion
  (w-u8 s 1) (w-u8 s 1) (force-output s)       ; 1 security type: None (1)
  (r-u8 s)                                     ; client's choice
  (w-u32 s 0) (force-output s)                 ; SecurityResult = OK
  (r-u8 s)                                     ; ClientInit (shared-flag)
  (w-u16 s (fb-width fb)) (w-u16 s (fb-height fb))
  (write-pixel-format s)
  (let ((nb (string->bytes name))) (w-u32 s (length nb)) (w-bytes s nb))
  (force-output s)
  t)

;;; ---- framebuffer update (Raw encoding) -------------------------------------

(defun send-update (fb s x y w h)
  "Send one FramebufferUpdate: the rect (X,Y,W,H) clipped to the framebuffer,
   Raw-encoded (little-endian pixels, since big-endian-flag=0)."
  (let* ((fw (fb-width fb))
         (x0 (max 0 x)) (y0 (max 0 y))
         (x1 (min fw (+ x w))) (y1 (min (fb-height fb) (+ y h)))
         (rw (max 0 (- x1 x0))) (rh (max 0 (- y1 y0))))
    (w-u8 s 0) (w-u8 s 0) (w-u16 s 1)          ; msg-type, pad, 1 rectangle
    (w-u16 s x0) (w-u16 s y0) (w-u16 s rw) (w-u16 s rh)
    (w-u32 s 0)                                ; encoding = Raw
    (let ((px (fb-pixels fb))
          (buf (make-array (* rw rh 4) :element-type '(unsigned-byte 8)))
          (o 0))
      (loop for yy from y0 below y1 for row = (* yy fw) do
        (loop for xx from x0 below x1 for p = (aref px (+ row xx)) do
          (setf (aref buf o)       (logand p #xff)          ; B
                (aref buf (+ o 1)) (logand (ash p -8) #xff) ; G
                (aref buf (+ o 2)) (logand (ash p -16) #xff); R
                (aref buf (+ o 3)) 0)                        ; X
          (incf o 4)))
      (w-bytes s buf))
    (force-output s)))

;;; ---- client message loop ----------------------------------------------------

(defun client-loop (fb s on-key on-pointer)
  (loop
    (let ((msg (read-byte s nil :eof)))
      (case msg
        (0 (skip s 3) (r-bytes s 16))                    ; SetPixelFormat (we keep ours)
        (2 (skip s 1) (let ((n (r-u16 s))) (dotimes (i n) (r-u32 s))))  ; SetEncodings
        (3 (let ((inc (r-u8 s)) (x (r-u16 s)) (y (r-u16 s)) (w (r-u16 s)) (h (r-u16 s)))
             (when (plusp inc) (sleep 1/60))             ; throttle incremental polls
             (send-update fb s x y w h)))
        (4 (let ((down (r-u8 s)))                        ; KeyEvent
             (skip s 2)
             (let ((key (r-u32 s))) (when on-key (funcall on-key (plusp down) key)))))
        (5 (let ((buttons (r-u8 s)) (x (r-u16 s)) (y (r-u16 s)))   ; PointerEvent
             (when on-pointer (funcall on-pointer buttons x y))))
        (6 (skip s 3) (let ((n (r-u32 s))) (r-bytes s n)))         ; ClientCutText
        (t (return))))))                                 ; :eof or unknown -> done

;;; ---- server -----------------------------------------------------------------

(defun serve (fb port &key on-key on-pointer (name *desktop-name*) once)
  "Serve framebuffer FB over RFB on PORT.  ON-KEY (down-p keysym) and ON-POINTER
   (button-mask x y) are optional input callbacks.  With :ONCE, handle a single
   client and return; otherwise loop, each client in its own thread."
  (let ((listen (tcp-listen port)))
    (format *error-output* "~&scry: RFB server listening on port ~d (~dx~d)~%"
            port (fb-width fb) (fb-height fb))
    (force-output *error-output*)
    (flet ((run (stream)
             (unwind-protect
                  (progn (handshake fb stream name)
                         (client-loop fb stream on-key on-pointer))
               (ignore-errors (close stream)))))
      (unwind-protect
           (loop
             (let ((stream (accept-stream listen)))
               (if once
                   (progn (run stream) (return))
                   (sb-thread:make-thread (lambda () (ignore-errors (run stream)))
                                          :name "scry-client"))))
        (ignore-errors (sb-bsd-sockets:socket-close listen))))))

(defun serve-one (fb port &rest args)
  "Serve exactly one client, then return (handy for tests)."
  (apply #'serve fb port :once t args))
