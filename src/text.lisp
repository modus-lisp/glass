;;;; text.lisp — first-class text on a glass framebuffer, via scribe.
;;;;
;;;; glass draws pixels; scribe turns font bytes + a string into anti-aliased,
;;;; gamma-correct coverage.  Here we marry them: FB-TEXT rasterizes each glyph
;;;; with scribe and composites its coverage straight into a glass framebuffer in
;;;; linear light (the way scribe does onto its own canvas), so glass gets real
;;;; typography without depending on McCLIM.  Pure CL, no FFI — the sovereign
;;;; text path for the WM chrome now, and for modus on bare metal later.
;;;;
;;;; This lives in the :glass/text system (adds a scribe dependency); the core
;;;; framebuffer + RFB server stay dependency-light.

(in-package #:glass)

(defun read-file-bytes (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s) v)))

(defun load-font (path)
  "A scribe font from the TrueType/OpenType file at PATH."
  (scribe:open-font (read-file-bytes path)))

;;; Default UI faces: Liberation Sans (metric-compatible with Helvetica) from
;;; scribe's bundled fonts — loaded lazily, cached.
(defvar *font-regular* nil)
(defvar *font-bold* nil)

(defun %scribe-font (name)
  (asdf:system-relative-pathname :scribe (format nil "fonts/~a" name)))

(defun default-font (&optional bold)
  "The default UI font (Liberation Sans), regular or BOLD."
  (if bold
      (or *font-bold*    (setf *font-bold*    (load-font (%scribe-font "LiberationSans-Bold.ttf"))))
      (or *font-regular* (setf *font-regular* (load-font (%scribe-font "LiberationSans-Regular.ttf"))))))

(defun text-width (string &key (size 13) (font (default-font)))
  "The advance width of STRING at SIZE px in FONT (for centring/layout)."
  (let ((w 0d0))
    (loop for ch across string do
      (multiple-value-bind (cov gw gh left top adv)
          (scribe:rasterize-glyph font (scribe:font-glyph-index font (char-code ch)) size)
        (declare (ignore cov gw gh left top))
        (incf w (or adv (float size 1d0)))))
    (ceiling w)))

(declaim (inline %over))
(defun %over (dst8 fg8 a ia)
  "Composite an 8-bit sRGB FG channel over DST at coverage A (ia = 1-A), linear."
  (scribe:linear->srgb (+ (* ia (scribe:srgb->linear dst8)) (* a (scribe:srgb->linear fg8)))))

(defun fb-text (fb x y string &key (size 13) (color +black+) (font (default-font)))
  "Draw STRING onto FB with its top-left near (X,Y), at SIZE px, in COLOR
   (0xRRGGBB), anti-aliased + gamma-correct.  Returns the ending pen x."
  (let* ((upem (scribe:font-units-per-em font))
         (baseline (+ y (round (* (scribe:font-ascent font) size) upem)))
         (fr (ldb (byte 8 16) color)) (fg (ldb (byte 8 8) color)) (fbb (ldb (byte 8 0) color))
         (px (fb-pixels fb)) (fw (fb-width fb)) (fh (fb-height fb))
         (penx (float x 1d0)))
    (loop for ch across string do
      (let* ((gid (scribe:font-glyph-index font (char-code ch)))
             (sub (- penx (ffloor penx))))
        (multiple-value-bind (cov w h left top adv)
            (scribe:rasterize-glyph font gid size :subpixel sub)
          (when cov
            (let ((ox (+ (floor penx) left)) (oy (+ baseline top)))
              (dotimes (gy h)
                (let ((fy (+ oy gy)))
                  (when (< -1 fy fh)
                    (let ((frow (* fy fw)))
                      (dotimes (gx w)
                        (let ((c (aref cov (+ (* gy w) gx))) (fx (+ ox gx)))
                          (when (and (> c 0d0) (< -1 fx fw))
                            (let* ((idx (+ frow fx)) (dst (aref px idx))
                                   (a (min 1d0 c)) (ia (- 1d0 a)))
                              (setf (aref px idx)
                                    (logior (ash (%over (ldb (byte 8 16) dst) fr a ia) 16)
                                            (ash (%over (ldb (byte 8 8) dst) fg a ia) 8)
                                            (%over (ldb (byte 8 0) dst) fbb a ia)))))))))))))
          (incf penx (or adv (float size 1d0))))))
    (floor penx)))
