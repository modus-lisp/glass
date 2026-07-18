;;;; copyrect-count.lisp — is CopyRect actually firing on window drags?  Stands up
;;;; a throwaway WM desktop on a spare port (does NOT touch a live :5901), connects
;;;; an RFB client advertising CopyRect, drags a window by its title bar, and counts
;;;; how many rects came back as encoding 1 (CopyRect) vs. re-encoded (ZRLE/Raw).
;;;; CopyRect only fires when the sender is exactly one frame behind the compositor
;;;; (caught-up); if it falls behind it correctly falls back to encoding the
;;;; exposed region — so this measures how often the fast path actually wins.
;;;;   sbcl --control-stack-size 256 --dynamic-space-size 4096 --non-interactive --load backend/inspect/copyrect-count.lisp
(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:glass :glass/term :weft/render :sb-concurrency))
    (asdf:load-asd "/home/claude/glass/backend/mcclim-glass.asd")
    (asdf:load-system :mcclim-glass)))

(defpackage #:cr (:use #:cl)) (in-package #:cr)
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
    ;; SetEncodings: TRLE(15), ZRLE(16), CopyRect(1), Raw(0) — same set TigerVNC offers
    (w8 s 2) (w8 s 0) (w16 s 4) (w32 s 15) (w32 s 16) (w32 s 1) (w32 s 0) (force-output s)
    (values w h)))
(defun req (s w h inc) (w8 s 3) (w8 s inc) (w16 s 0) (w16 s 0) (w16 s w) (w16 s h) (force-output s))
(defun pull (s)                              ; read one FramebufferUpdate; (values n-rects n-copyrect)
  (r8 s) (r8 s)
  (let ((n (r16 s)) (ncopy 0))
    (dotimes (i n)
      (let ((x (r16 s)) (y (r16 s)) (w (r16 s)) (h (r16 s)) (enc (r32 s)))
        (declare (ignore x y))
        (cond ((= enc 1)  (r16 s) (r16 s) (incf ncopy))                 ; CopyRect: src-x src-y
              ((= enc 15) (skip-trle s w h))                            ; TRLE: self-delimiting tiles
              ((= enc 16) (let ((len (r32 s))) (rn s len)))             ; ZRLE: length-prefixed
              ((= enc 0)  (rn s (* w h 4)))                             ; Raw
              (t (error "unexpected enc ~x" enc)))))
    (values n ncopy)))
;; TRLE has no length prefix; walk its 16-px-tile subencodings (RFC 6143 §7.7.5)
;; to consume exactly.  This parser is an INDEPENDENT RFC-compliant client: if it
;; stays in sync with glass's output, glass is emitting spec-correct TRLE.
(defun cpix (s) (r8 s) (r8 s) (r8 s))
(defun skip-trle (s w h)
  (loop for ty from 0 below h by 16 for th = (min 16 (- h ty)) do
    (loop for tx from 0 below w by 16 for tw = (min 16 (- w tx)) do
      (let ((sub (r8 s)))
        (cond
          ((= sub 0) (dotimes (k (* tw th)) (cpix s)))                 ; raw CPIXELs
          ((= sub 1) (cpix s))                                         ; solid
          ((<= 2 sub 16)                                               ; packed palette
           (dotimes (k sub) (cpix s))
           (let ((bpp (cond ((<= sub 2) 1) ((<= sub 4) 2) (t 4))))
             (dotimes (row th) (rn s (ceiling (* tw bpp) 8)))))
          (t (error "unexpected TRLE subenc ~a" sub)))))))
(defun ptr (s m x y) (w8 s 5) (w8 s m) (w16 s x) (w16 s y) (force-output s))
(defun secs () (/ (get-internal-real-time) internal-time-units-per-second))

(let ((wp (namestring (merge-pathnames "assets/wallpaper.svg" (asdf:system-source-directory :mcclim-glass))))
      (port 5988))
  (sb-thread:make-thread
   (lambda () (handler-case (clim-glass:run-wm '((:terminal :cols 90 :rows 26 :ppem 14))
                                               :port port :width 1280 :height 800 :background wp)
                (error (e) (format t "~&WM ERROR ~a~%" e)))))
  (let ((s (connect port)))
    (multiple-value-bind (w h) (handshake s)
      (format t "~&[copyrect-count] ~dx~d, dragging a window by its title bar~%" w h)
      (sleep 2.5)
      (let* ((p (clim-glass::find-glass-port :port port))
             (surf (first (clim-glass::glass-port-surfaces p)))
             (tx (+ (clim-glass::wm-surface-x surf) 200))
             (ty (- (clim-glass::wm-surface-y surf) 11)))
        (req s w h 0) (pull s)                                          ; baseline full frame
        (ptr s 1 tx ty)                                                ; grab the title bar
        (let ((frames 0) (copy-frames 0) (total-rects 0) (total-copy 0)
              (dir 1) (px tx) (t0 (secs)))
          (loop while (< (- (secs) t0) 3.0) do
            (incf px (* dir 6)) (when (or (> px (+ tx 240)) (< px tx)) (setf dir (- dir)))
            (ptr s 1 px ty)                                            ; a drag step -> one composite (with copy)
            (req s w h 1)
            (handler-case
                (multiple-value-bind (nr nc) (pull s)
                  (incf frames) (incf total-rects nr) (incf total-copy nc)
                  (when (plusp nc) (incf copy-frames)))
              (error (e) (format t "  (parse desync at frame ~d: ~a)~%" frames e) (loop-finish))))
          (ptr s 0 px ty) (force-output s)
          (format t "  drag frames:          ~d~%" frames)
          (format t "  frames WITH CopyRect: ~d  (~,0f%)~%" copy-frames (* 100.0 (/ copy-frames (max 1 frames))))
          (format t "  CopyRect rects total: ~d  (of ~d rects)~%" total-copy total-rects)
          (format t "~%=> CopyRect is ~:[NOT firing~;FIRING~] on drags~%" (plusp total-copy))))
      (ignore-errors (close s)))))
(finish-output) (sb-ext:exit)
