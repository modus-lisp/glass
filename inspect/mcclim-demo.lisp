;;;; mcclim-demo.lisp — proof the McCLIM software renderer feeds a glass framebuffer.
;;;;
;;;;   sbcl --dynamic-space-size 4096 --non-interactive --load inspect/mcclim-demo.lisp
;;;;
;;;; McCLIM's raster backend (mcclim-render) draws arbitrary CLIM graphics — lines,
;;;; fills, TrueType text — into a (unsigned-byte 32) 0xAARRGGBB array.  We blit
;;;; that into a glass framebuffer (0x00RRGGBB), dump it to a PNG so it can be
;;;; eyeballed, and — to prove the whole wire path — serve it over RFB and read the
;;;; pixels back with an in-process VNC client, asserting they match what we blit.
;;;; This is the format/orientation bridge; the live interactive port comes next.

(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:glass :mcclim :mcclim-raster-image :zpng :chipz))))

(defpackage #:glass-mcclim-demo (:use #:cl))
(in-package #:glass-mcclim-demo)

;;; ---- draw some CLIM graphics into an mcclim-render image --------------------

(defparameter *w* 480)
(defparameter *h* 320)

(defun render-clim ()
  "Return an mcclim-render image (0xAARRGGBB pattern-array) with a demo drawing."
  (clime:with-output-to-drawing-stream (s :raster :pattern :width *w* :height *h*)
    (clim:draw-rectangle* s 0 0 *w* *h* :ink (clim:make-rgb-color 0.09 0.11 0.16))
    (clim:draw-rectangle* s 24 24 220 150 :ink clim:+firebrick+)
    (clim:draw-circle*    s 340 110 68     :ink (clim:make-rgb-color 0.20 0.55 0.95))
    (clim:draw-polygon*   s (list 60 300  180 210  300 300) :ink clim:+goldenrod+ :closed t)
    (loop for i from 0 to 10
          do (clim:draw-line* s 20 (+ 190 (* i 6)) 460 (+ 190 (* i 6))
                              :ink (clim:make-gray-color (/ i 14.0)) :line-thickness 1))
    ;; ASCII only: McCLIM's TTF kerning path errors on some non-ASCII glyphs
    ;; (e.g. U+2192 mixed among latin) — an McCLIM quirk, unrelated to glass.
    (clim:draw-text* s "McCLIM -> glass -> VNC" 30 60
                     :ink clim:+white+ :text-size 26 :text-family :sans-serif)
    (clim:draw-text* s "pure-CL software render, no X server" 30 285
                     :ink clim:+white+ :text-size 14)))

;;; ---- blit an mcclim image into a glass framebuffer -------------------------

(defun blit-to-glass (image)
  "Copy IMAGE's 0xAARRGGBB pattern-array into a fresh glass framebuffer."
  (let* ((arr (climi::pattern-array image))
         (h (array-dimension arr 0)) (w (array-dimension arr 1))
         (fb (glass:make-framebuffer w h))
         (px (glass:fb-pixels fb)))
    (dotimes (y h)
      (let ((row (* y w)))
        (dotimes (x w)
          (setf (aref px (+ row x)) (logand (aref arr y x) #x00ffffff)))))  ; drop alpha
    fb))

;;; ---- dump a glass framebuffer to a truecolor PNG ---------------------------

(defun fb->png (fb path)
  (let* ((w (glass:fb-width fb)) (h (glass:fb-height fb)) (px (glass:fb-pixels fb))
         (png (make-instance 'zpng:png :width w :height h :color-type :truecolor))
         (data (zpng:data-array png)))
    (dotimes (y h)
      (let ((row (* y w)))
        (dotimes (x w)
          (let ((p (aref px (+ row x))))
            (setf (aref data y x 0) (ldb (byte 8 16) p)
                  (aref data y x 1) (ldb (byte 8 8) p)
                  (aref data y x 2) (ldb (byte 8 0) p))))))
    (zpng:write-png png path)
    path))

;;; ---- read the framebuffer back over RFB (proves the wire path) -------------
;;; A minimal ZRLE-decoding RFB client: connect, negotiate ZRLE, request the
;;; frame, inflate + parse tiles, return the client-side pixel array.

(defun r8 (s) (read-byte s))
(defun r16 (s) (logior (ash (r8 s) 8) (r8 s)))
(defun r32 (s) (logior (ash (r16 s) 16) (r16 s)))
(defun rn (s n) (let ((b (make-array n :element-type '(unsigned-byte 8)))) (read-sequence b s) b))
(defun w8 (s v) (write-byte (logand v #xff) s))
(defun w16 (s v) (w8 s (ash v -8)) (w8 s v))
(defun w32 (s v) (w16 s (ash v -16)) (w16 s v))

(defun rfb-readback (port w h)
  (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (loop repeat 100 do (handler-case
                            (progn (sb-bsd-sockets:socket-connect
                                    sock (sb-bsd-sockets:make-inet-address "127.0.0.1") port)
                                   (return))
                          (error () (sleep 0.05))))
    (let ((s (sb-bsd-sockets:socket-make-stream sock :input t :output t
                                                :element-type '(unsigned-byte 8) :buffering :full))
          (dstate (chipz:make-dstate 'chipz:zlib))
          (cli (make-array (* w h) :element-type '(unsigned-byte 32))))
      (rn s 12) (write-sequence (map 'vector #'char-code "RFB 003.008") s) (w8 s 10) (force-output s)
      (let ((n (r8 s))) (rn s n)) (w8 s 1) (force-output s) (r32 s)          ; None security
      (w8 s 1) (force-output s)                                              ; ClientInit
      (r16 s) (r16 s) (rn s 16) (let ((nl (r32 s))) (rn s nl))               ; ServerInit
      (w8 s 2) (w8 s 0) (w16 s 2) (w32 s 16) (w32 s 0) (force-output s)      ; SetEncodings ZRLE,Raw
      (w8 s 3) (w8 s 0) (w16 s 0) (w16 s 0) (w16 s w) (w16 s h) (force-output s)  ; request full
      (r8 s) (r8 s)                                                          ; FBUpdate hdr
      (dotimes (i (r16 s))
        (let ((rx (r16 s)) (ry (r16 s)) (rw (r16 s)) (rh (r16 s)) (enc (r32 s)))
          (assert (= enc 16) () "expected ZRLE, got ~a" enc)
          (let* ((len (r32 s)) (chunk (rn s len)) (dec (chipz:decompress nil dstate chunk)) (pos 0))
            (loop for ty from 0 below rh by 64 for th = (min 64 (- rh ty)) do
              (loop for tx from 0 below rw by 64 for tw = (min 64 (- rw tx)) do
                (let ((sub (aref dec pos))) (incf pos)
                  (flet ((cp () (prog1 (logior (ash (aref dec (+ pos 2)) 16) (ash (aref dec (+ pos 1)) 8)
                                               (aref dec pos)) (incf pos 3)))
                         (put (lx ly c) (setf (aref cli (+ (* (+ ry ty ly) w) (+ rx tx lx))) c)))
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
                      (t (error "bad ZRLE subenc ~a" sub))))))))))
      (close s)
      cli)))

;;; ---- run it ----------------------------------------------------------------

(let* ((image (render-clim))
       (fb (blit-to-glass image))
       (png (fb->png fb "/tmp/glass-mcclim.png"))
       (port 5931)
       (server (sb-thread:make-thread (lambda () (ignore-errors (glass:serve-one fb port))))))
  (format t "~&rendered ~dx~d, PNG -> ~a~%" (glass:fb-width fb) (glass:fb-height fb) png)
  (let ((cli (rfb-readback port (glass:fb-width fb) (glass:fb-height fb))))
    (ignore-errors (sb-thread:join-thread server))
    (let ((mismatch 0) (px (glass:fb-pixels fb)))
      (dotimes (i (length px)) (unless (= (aref cli i) (aref px i)) (incf mismatch)))
      (format t "RFB read-back: ~d/~d pixels mismatched -> ~a~%"
              mismatch (length px) (if (zerop mismatch) "EXACT (mcclim->glass->RFB->client verified)" "MISMATCH"))))
  (finish-output))
