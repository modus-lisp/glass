;;;; fps-bench.lisp — measure delivered fps over VNC for two scenarios: dragging a
;;;; window (interactive; composite per pointer event) and a flooding terminal
;;;; (heavy content churn).  Parses FramebufferUpdates by SKIPPING each ZRLE rect
;;;; by its length prefix — no decode, so it can't desync or hang on a live frame.
;;;;   sbcl --control-stack-size 256 --dynamic-space-size 4096 --non-interactive --load backend/inspect/fps-bench.lisp

(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:glass :glass/term :pigment :sb-concurrency))
    (asdf:load-asd "/home/claude/glass/backend/mcclim-glass.asd")
    (asdf:load-system :mcclim-glass)))

(defpackage #:fb (:use #:cl)) (in-package #:fb)
(defun r8 (s) (read-byte s)) (defun r16 (s) (logior (ash (r8 s) 8) (r8 s)))
(defun r32 (s) (logior (ash (r16 s) 16) (r16 s)))
(defun rn (s n) (let ((b (make-array n :element-type '(unsigned-byte 8)))) (read-sequence b s) b))
(defun w8 (s v) (write-byte (logand v #xff) s))
(defun w16 (s v) (w8 s (ash v -8)) (w8 s v)) (defun w32 (s v) (w16 s (ash v -16)) (w16 s v))
(defun connect (port)
  (loop repeat 500 do (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (handler-case (progn (sb-bsd-sockets:socket-connect sock (sb-bsd-sockets:make-inet-address "127.0.0.1") port)
                    (setf (sb-bsd-sockets:sockopt-tcp-nodelay sock) t)   ; like TigerVNC: no Nagle client-side
                    (return-from connect (sb-bsd-sockets:socket-make-stream sock :input t :output t :element-type '(unsigned-byte 8) :buffering :full)))
      (error () (ignore-errors (sb-bsd-sockets:socket-close sock)) (sleep 0.05))))))
(defun handshake (s)
  (rn s 12) (write-sequence (map 'vector #'char-code "RFB 003.008") s) (w8 s 10) (force-output s)
  (let ((n (r8 s))) (rn s n)) (w8 s 1) (force-output s) (r32 s) (w8 s 1) (force-output s)
  (let ((w (r16 s)) (h (r16 s))) (rn s 16) (let ((nl (r32 s))) (rn s nl))
    (w8 s 2) (w8 s 0) (w16 s 3) (w32 s 16) (w32 s 1) (w32 s 0) (force-output s) (values w h)))  ; encodings: ZRLE, Raw
(defun req (s w h inc) (w8 s 3) (w8 s inc) (w16 s 0) (w16 s 0) (w16 s w) (w16 s h) (force-output s))
(defun pull (s)                              ; read one FramebufferUpdate; return bytes of rect data
  (let ((mt (r8 s))) (declare (ignore mt)) (r8 s)
    (let ((n (r16 s)) (bytes 0))
      (dotimes (i n)
        (let ((x (r16 s)) (y (r16 s)) (w (r16 s)) (h (r16 s)) (enc (r32 s)))
          (declare (ignore x y))
          (cond ((= enc 16) (let ((len (r32 s))) (rn s len) (incf bytes len)))     ; ZRLE: skip by length
                ((= enc 1)  (r16 s) (r16 s))  ; CopyRect: src-x src-y, no pixel data
                ((= enc 0)  (rn s (* w h 4)) (incf bytes (* w h 4)))                ; Raw
                (t (error "unexpected enc ~x" enc)))))
      bytes)))
(defun ptr (s m x y) (w8 s 5) (w8 s m) (w16 s x) (w16 s y) (force-output s))
(defun key (s k) (w8 s 4)(w8 s 1)(w16 s 0)(w32 s k)(force-output s)(w8 s 4)(w8 s 0)(w16 s 0)(w32 s k)(force-output s)(sleep 0.02))
(defun typ (s str) (loop for c across str do (key s (char-code c))))
(defun secs () (/ (get-internal-real-time) internal-time-units-per-second))

(let* ((wp (namestring (merge-pathnames "assets/wallpaper.svg" (asdf:system-source-directory :mcclim-glass))))
       (port 5987))
  (sb-thread:make-thread
   (lambda () (handler-case (clim-glass:run-wm '((:terminal :cols 90 :rows 26 :ppem 14))
                                               :port port :width 1280 :height 800 :background wp)
                (error (e) (format t "~&WM ERROR ~a~%" e)))))
  (let ((s (connect port)) (p nil) (surf nil))
    (multiple-value-bind (w h) (handshake s)
      (format t "~&fps-bench on ~dx~d (SVG wallpaper + 90x26 terminal)~%" w h)
      (sleep 2.5)
      (setf p (clim-glass::find-glass-port :port port) surf (first (clim-glass::glass-port-surfaces p)))
      (req s w h 0) (pull s)                  ; baseline full frame

      ;; --- Scenario 1: drag the window back and forth (interactive) ---
      (let* ((tx (+ (clim-glass::wm-surface-x surf) 200))       ; a point on the title bar
             (ty (- (clim-glass::wm-surface-y surf) 11))
             (t0 (secs)) (frames 0) (dir 1) (px tx))
        (ptr s 1 tx ty) (force-output s)       ; grab the title bar
        (let ((bytes 0))
          (loop while (< (- (secs) t0) 3.0) do
            (incf px (* dir 6)) (when (or (> px (+ tx 240)) (< px tx)) (setf dir (- dir)))
            (ptr s 1 px ty)                      ; a drag step -> composite
            (req s w h 1) (incf bytes (pull s)) (incf frames))
          (ptr s 0 px ty) (force-output s)
          (format t "DRAG (window move):     ~,1f fps  (~,2f KB/frame)~%" (/ frames (- (secs) t0)) (/ bytes (max 1 frames) 1024.0))))

      ;; --- Scenario 2: a flooding terminal (heavy content churn) ---
      (ptr s 0 300 300) (ptr s 1 300 300) (ptr s 0 300 300) (sleep 0.1)   ; focus the terminal
      (typ s "while :; do echo \"the quick brown fox jumps $((i++))\"; done") (key s #xff0d) (sleep 0.5)
      (let ((t0 (secs)) (frames 0) (bytes 0))
        (loop while (< (- (secs) t0) 3.0) do (req s w h 1) (incf bytes (pull s)) (incf frames))
        (let ((el (- (secs) t0)))
          (format t "FLOOD (scrolling term): ~,1f fps  (~,1f KB/frame)~%" (/ frames el) (/ bytes frames 1024.0))))
      ;; stop the flood: Ctrl-C
      (w8 s 4)(w8 s 1)(w16 s 0)(w32 s #xffe3)(force-output s)   ; Ctrl down
      (key s (char-code #\c))
      (w8 s 4)(w8 s 0)(w16 s 0)(w32 s #xffe3)(force-output s)   ; Ctrl up
      (format t "done~%")
      (ignore-errors (close s)))))
(finish-output) (sb-ext:exit)
