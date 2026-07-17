;;;; framebuffer.lisp — an in-memory 32-bit framebuffer + drawing primitives.
;;;;
;;;; Pixels are 0x00RRGGBB (X8R8G8B8) in a flat row-major (unsigned-byte 32)
;;;; array — the same value the RFB pixel format advertises, so serving a rect is
;;;; just writing the pixels out little-endian.  All drawing is clipped to the
;;;; framebuffer bounds, so callers never have to bounds-check.

(in-package #:glass)

(declaim (inline rgb))
(defun rgb (r g b)
  "An X8R8G8B8 pixel from 8-bit R, G, B."
  (logior (ash (logand r #xff) 16) (ash (logand g #xff) 8) (logand b #xff)))

(defconstant +black+ #x000000)
(defconstant +white+ #xffffff)
(defconstant +red+   #xff0000)
(defconstant +green+ #x00ff00)
(defconstant +blue+  #x0000ff)

(defstruct (framebuffer (:conc-name fb-) (:constructor %make-framebuffer))
  (width  0 :type fixnum)
  (height 0 :type fixnum)
  (pixels #() :type (simple-array (unsigned-byte 32) (*)))
  ;; Guards the (width height pixels) triple against a RESIZE racing a reader.
  ;; Per-pixel content races are benign (a stale read is re-sent next update);
  ;; only the array swap needs protecting, so readers grab this only to snapshot.
  (lock (sb-thread:make-mutex :name "framebuffer") :type sb-thread:mutex))

(defmacro with-fb-locked ((fb) &body body)
  `(sb-thread:with-mutex ((fb-lock ,fb)) ,@body))

(defun make-framebuffer (width height &optional (fill +black+))
  "A WIDTH x HEIGHT framebuffer, cleared to FILL."
  (let ((px (make-array (* width height) :element-type '(unsigned-byte 32)
                                         :initial-element (logand fill #xffffff))))
    (%make-framebuffer :width width :height height :pixels px)))

(defun fb-resize (fb width height &optional (fill +black+))
  "Resize FB in place to WIDTH x HEIGHT (contents reset to FILL).  Atomic against
   readers that snapshot under WITH-FB-LOCKED.  No-op if the size is unchanged."
  (with-fb-locked (fb)
    (unless (and (= width (fb-width fb)) (= height (fb-height fb)))
      (setf (fb-pixels fb) (make-array (* width height) :element-type '(unsigned-byte 32)
                                                        :initial-element (logand fill #xffffff))
            (fb-width fb) width
            (fb-height fb) height)))
  fb)

(declaim (inline %in-bounds fb-put fb-get))
(defun %in-bounds (fb x y)
  (and (>= x 0) (>= y 0) (< x (fb-width fb)) (< y (fb-height fb))))

(defun fb-put (fb x y color)
  "Set pixel (X,Y) to COLOR (no-op if out of bounds)."
  (when (%in-bounds fb x y)
    (setf (aref (fb-pixels fb) (+ (* y (fb-width fb)) x)) (logand color #xffffff)))
  color)

(defun fb-get (fb x y)
  "Pixel (X,Y), or 0 if out of bounds."
  (if (%in-bounds fb x y) (aref (fb-pixels fb) (+ (* y (fb-width fb)) x)) 0))

(defun fb-fill (fb color)
  "Clear the whole framebuffer to COLOR."
  (fill (fb-pixels fb) (logand color #xffffff))
  color)

(defun fb-rect (fb x y w h color)
  "Filled rectangle at (X,Y), W x H, in COLOR (clipped)."
  (let* ((fw (fb-width fb)) (fh (fb-height fb))
         (x0 (max 0 x)) (y0 (max 0 y))
         (x1 (min fw (+ x w))) (y1 (min fh (+ y h)))
         (px (fb-pixels fb)) (c (logand color #xffffff)))
    (loop for yy from y0 below y1
          for row = (* yy fw)
          do (loop for xx from x0 below x1 do (setf (aref px (+ row xx)) c))))
  fb)

(defun fb-hline (fb x y w color) (fb-rect fb x y w 1 color))
(defun fb-vline (fb x y h color) (fb-rect fb x y 1 h color))

(defun fb-frame (fb x y w h color &optional (thickness 1))
  "Rectangle outline (border only) of THICKNESS pixels."
  (fb-rect fb x y w thickness color)                       ; top
  (fb-rect fb x (+ y (- h thickness)) w thickness color)   ; bottom
  (fb-rect fb x y thickness h color)                       ; left
  (fb-rect fb (+ x (- w thickness)) y thickness h color)   ; right
  fb)

(defun fb-blit (dst src dx dy)
  "Copy the whole framebuffer SRC into DST with its top-left at (DX,DY) (clipped)."
  (dotimes (sy (fb-height src) dst)
    (dotimes (sx (fb-width src))
      (fb-put dst (+ dx sx) (+ dy sy) (fb-get src sx sy)))))
