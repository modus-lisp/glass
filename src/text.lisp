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
  "The advance width of STRING at SIZE px in FONT (for centring/layout).  Through the glyph
   cache, as FB-TEXT is: measuring rasterized every glyph afresh, which made laying out a line of
   text (word wrap measures every word) cost more than drawing it."
  (let ((w 0d0))
    (loop for ch across string do
      (multiple-value-bind (cov gw gh left top adv)
          (%cached-glyph font (scribe:font-glyph-index font (char-code ch)) size 0d0)
        (declare (ignore cov gw gh left top))
        (incf w (or adv (float size 1d0)))))
    (ceiling w)))

(declaim (inline %over))
(defun %over (dst8 lfg a ia)
  "Composite a FG channel, already in LINEAR light (LFG), over the 8-bit sRGB DST8 at coverage
   A (ia = 1-A).  Back to sRGB through scribe's 4096-entry table rather than LINEAR->SRGB's
   EXPT: three EXPTs a pixel were most of what a line of text cost on modus.  The table can
   land a level away from the exact curve; that is scribe's own compositing path too."
  (let ((l (+ (* ia (scribe:srgb->linear dst8)) (* a lfg))))
    (aref scribe:*linear->srgb* (min 4096 (max 0 (round (* l 4096d0)))))))

;;; THE GLYPH CACHE.  Rasterising a glyph from its outline is the expensive half of drawing
;;; text (0.7 ms a glyph on modus), and a UI draws the same few glyphs every frame.  Keyed by
;;; font, glyph, size and the pen's subpixel offset ROUNDED TO A QUARTER PIXEL -- an exact
;;; offset would make nearly every glyph a miss -- so a glyph may sit up to an eighth of a pixel
;;; from where the exact offset would put it.  On modus, whose threads share no memory, only the
;;; main thread fills it (a worker storing into the shared table is refused); other threads
;;; rasterise as before.
(defvar *glyph-cache* (make-hash-table :test 'equal #+sbcl :synchronized #+sbcl t))

(defun %glyph-entry (font gid size sub)
  "GID's cache entry: (coverage w h left top advance icoverage).  ICOVERAGE is COVERAGE in
   4096ths, for FB-TEXT's integer compositing."
  (let* ((q (/ (round (* sub 4)) 4d0))
         (key (list font gid size q)))
    (or (gethash key *glyph-cache*)
        (multiple-value-bind (cov w h left top adv)
            (scribe:rasterize-glyph font gid size :subpixel q)
          (let ((r (list cov w h left top adv
                         (and cov (map 'simple-vector (lambda (c) (min 4096 (max 0 (round (* c 4096))))) cov)))))
            (when #+modus (sb-thread:main-thread-p) #-modus t
              (setf (gethash key *glyph-cache*) r))
            r)))))

(defun %cached-glyph (font gid size sub)
  "(values coverage w h left top advance) for GID, through *GLYPH-CACHE*."
  (destructuring-bind (cov w h left top adv &rest more) (%glyph-entry font gid size sub)
    (declare (ignore more))
    (values cov w h left top adv)))

;;; INTEGER COMPOSITING.  FB-TEXT blended in double floats, and on modus every intermediate
;;; double is a heap object: a cached 16-px glyph cost ~4 ms to composite, and a title bar's two
;;; strings were most of a window-drag frame on a Pi Zero (55 ms; 15 fps).  Coverage and linear light
;;; are now in 4096ths -- *SRGB->LIN12* below, back through scribe's own
;;; 4096-entry *LINEAR->SRGB* -- so a pixel is a few fixnum multiplies.  Full coverage still
;;; writes the colour itself; a partial pixel can land a level from the double-float result.
(defvar *srgb->lin12*
  (let ((v (make-array 256)))
    (dotimes (i 256 v) (setf (aref v i) (round (* 4096 (scribe:srgb->linear i)))))))

(declaim (inline %over12))
(defun %over12 (dst8 lfg a ia)
  "FG (linear, 4096ths) over the sRGB byte DST8 at coverage A in 4096ths (IA = 4096 - A)."
  (aref scribe:*linear->srgb* (ash (+ (* ia (aref *srgb->lin12* dst8)) (* a lfg) 2048) -12)))

(defun fb-text (fb x y string &key (size 13) (color +black+) (font (default-font))
                                   (alpha 1d0))
  "Draw STRING onto FB with its top-left near (X,Y), at SIZE px, in COLOR
   (0xRRGGBB), anti-aliased + gamma-correct.  Returns the ending pen x.

   ALPHA (0..1) scales the whole run's opacity.  It costs nothing to have: the glyph
   coverage is already an alpha and already blended per pixel, so this multiplies the
   number that was there rather than adding a compositing path.  1.0 is what every
   caller had before and is bit-identical to it."
  (let* ((upem (scribe:font-units-per-em font))
         (baseline (+ y (round (* (scribe:font-ascent font) size) upem)))
         (fr (aref *srgb->lin12* (ldb (byte 8 16) color)))
         (fg (aref *srgb->lin12* (ldb (byte 8 8) color)))
         (fbb (aref *srgb->lin12* (ldb (byte 8 0) color)))
         (a12 (min 4096 (max 0 (round (* alpha 4096)))))
         (solid (logand color #xffffff))
         (px (fb-pixels fb)) (fw (fb-width fb)) (fh (fb-height fb))
         ;; :I420: luma blended per pixel; each pixel nudges its 2x2 chroma block a quarter
         ;; of its coverage toward the text's chroma, so a block the glyph fills becomes the
         ;; text's colour and a block it grazes barely moves
         (i420 (eq (fb-format fb) :i420))
         (yp (fb-y fb)) (up (fb-u fb)) (vp (fb-v fb)) (cw (and i420 (fb-chroma-width fb)))
         (ty (and i420 (%rgb-y color))) (tu (and i420 (%rgb-u color))) (tv (and i420 (%rgb-v color)))
         ;; THE CLIP BOX, like FB-RECT: a compositor redrawing one damaged rectangle draws a title
         ;; that crosses its edge, and glyphs outside it must not land on what was not redrawn
         (clip (fb-clip fb))
         (cx0 (if clip (max 0 (first clip)) 0)) (cy0 (if clip (max 0 (second clip)) 0))
         (cx1 (if clip (min fw (third clip)) fw)) (cy1 (if clip (min fh (fourth clip)) fh))
         (penx (float x 1d0)))
    (loop for ch across string do
      (let* ((gid (scribe:font-glyph-index font (char-code ch)))
             (sub (- penx (ffloor penx))))
        (destructuring-bind (cov w h left top adv icov) (%glyph-entry font gid size sub)
          (declare (ignore cov))
          (when icov
            (let ((ox (+ (floor penx) left)) (oy (+ baseline top)))
              (dotimes (gy h)
                (let ((fy (+ oy gy)))
                  (when (and (<= cy0 fy) (< fy cy1))
                    (let ((frow (* fy fw)) (crow (* gy w)))
                      (dotimes (gx w)
                        (let ((c (aref icov (+ crow gx))) (fx (+ ox gx)))
                          (when (and (> c 0) (<= cx0 fx) (< fx cx1))
                            (let* ((idx (+ frow fx))
                                   (a (if (= a12 4096) c (ash (* a12 c) -12)))
                                   (ia (- 4096 a)))
                              (if i420
                                  (let* ((ci (+ (ash fx -1) (* (ash fy -1) cw)))
                                         (q (ash a -2)) (iq (- 4096 q)))
                                    (setf (aref yp idx) (ash (+ (* (aref yp idx) ia) (* ty a) 2048) -12)
                                          (aref up ci) (ash (+ (* (aref up ci) iq) (* tu q) 2048) -12)
                                          (aref vp ci) (ash (+ (* (aref vp ci) iq) (* tv q) 2048) -12)))
                                  (setf (aref px idx)
                                        (if (>= a 4096)
                                            solid   ; full coverage: the colour, no blend
                                            (let ((dst (aref px idx)))
                                              (logior (ash (%over12 (ldb (byte 8 16) dst) fr a ia) 16)
                                                      (ash (%over12 (ldb (byte 8 8) dst) fg a ia) 8)
                                                      (%over12 (ldb (byte 8 0) dst) fbb a ia))))))))))))))))
          (incf penx (or adv (float size 1d0))))))
    (floor penx)))
