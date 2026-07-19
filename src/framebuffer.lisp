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

;;; The framebuffer's ONE platform seam: a lock guarding the RESIZE array-swap
;;; against a concurrent reader (the RFB server thread).  Real on SBCL; a no-op
;;; where sb-thread is absent (e.g. modus, which will supply its own concurrency
;;; model).  Everything else in this file — and in the text primitive — is pure
;;; Common Lisp with no FFI, so the drawing path drops onto any CL.
#+sb-thread (defun %fb-make-lock () (sb-thread:make-mutex :name "framebuffer"))
#-sb-thread (defun %fb-make-lock () nil)

;; Short critical section around the (damage copy frameno) frame triple — NOT the pixel
;; FB-LOCK, which the sender holds for a whole (long) encode.  Guards FB-MARK-FRAME's
;; compose against the sender's FB-TAKE-FRAME (read + reset).
#+sb-thread (defmacro %with-frame-lock ((fb) &body body)
              `(sb-thread:with-mutex ((fb-frame-lock ,fb)) ,@body))
#-sb-thread (defmacro %with-frame-lock ((fb) &body body)
              (declare (ignore fb)) `(progn ,@body))

(defstruct (framebuffer (:conc-name fb-) (:constructor %make-framebuffer))
  (width  0 :type fixnum)
  (height 0 :type fixnum)
  (pixels #() :type (simple-array (unsigned-byte 32) (*)))
  ;; Guards the (width height pixels) triple against a RESIZE racing a reader.
  ;; Per-pixel content races are benign (a stale read is re-sent next update);
  ;; only the array swap needs protecting, so readers grab this only to snapshot.
  (lock (%fb-make-lock))
  ;; Content version: a writer bumps it via FB-TOUCH so a reader (the RFB sender)
  ;; can cheaply tell "nothing changed since I last looked" and skip a full diff.
  (generation 0)
  ;; Clip rectangle (x0 y0 x1 y1, exclusive) confining drawing to a region, or NIL
  ;; for none — lets the compositor redraw only a damaged rectangle (WITH-FB-CLIP).
  (clip nil)
  ;; Frame counter + last damage box: bumped once per COMPOSITE (FB-MARK-FRAME),
  ;; so the RFB sender can diff only the region that just changed, not the whole
  ;; screen — the "we're the compositor, we already know what changed" shortcut.
  ;; (Named FRAMENO, not FRAME: FB-FRAME is already the rectangle-outline drawer.)
  (frameno 0)
  (damage nil)                          ; accumulated (x0 y0 x1 y1), or :FULL, since last take
  (copy nil)                            ; composed move: (src-x src-y dst-x dst-y w h) -> CopyRect
  (mark-time 0 :type fixnum)            ; when the oldest unsent change was marked (backlog clock)
  (frame-lock (%fb-make-lock)))         ; guards the (damage copy frameno) triple

(defun fb-touch (fb)
  "Mark FB's contents as changed (bumps its generation).  Writers call this after
   drawing so the RFB sender knows to re-scan; an untouched fb is diff-free."
  (setf (fb-generation fb) (logand (1+ (fb-generation fb)) most-positive-fixnum)))

(defun %fb-damage-union (a b)
  "Union two damage marks: :FULL dominates, NIL is identity, else the bounding box."
  (cond ((or (eq a :full) (eq b :full)) :full)
        ((null a) b)
        ((null b) a)
        (t (destructuring-bind (ax0 ay0 ax1 ay1) a
             (destructuring-bind (bx0 by0 bx1 by1) b
               (list (min ax0 bx0) (min ay0 by0) (max ax1 bx1) (max ay1 by1)))))))

(defun %fb-copy-compose (old new)
  "Fold two successive window-move copies (sx sy dx dy w h) into ONE, or NIL if they can't
   be one CopyRect.  A drag is one window translating, so a chain (OLD's dst == NEW's src,
   same size) folds to (OLD-src -> NEW-dst) — this is 'CopyRect farther': the hint spans
   however many frames the sender fell behind.  A non-move composite (NEW NIL) keeps OLD (the
   window hasn't moved; the extra damage rides the diff).  Two independent moves -> NIL."
  (cond
    ((null new) old)
    ((null old) new)
    (t (destructuring-bind (osx osy odx ody ow oh) old
         (declare (ignore osx osy))
         (destructuring-bind (nsx nsy ndx ndy nw nh) new
           (if (and (= odx nsx) (= ody nsy) (= ow nw) (= oh nh))
               (list (first old) (second old) ndx ndy ow oh)
               nil))))))

(defun fb-mark-frame (fb damage &optional copy)
  "Record that a composite changed region DAMAGE ((x0 y0 x1 y1) or :FULL) and, if it was a
   window MOVE, COPY = (src-x src-y dst-x dst-y w h) so the sender can CopyRect the moved
   pixels.  ACCUMULATES onto whatever the sender has not taken yet: damage is unioned and
   successive same-window moves are composed (%FB-COPY-COMPOSE), so a sender several frames
   behind still gets ONE snapshot->current CopyRect instead of re-encoding.  Advances the
   frame counter."
  (%with-frame-lock (fb)
    (when (null (fb-damage fb))           ; first change since the sender's last take: start the
      (setf (fb-mark-time fb) (get-internal-real-time)))  ; backlog clock for this batch
    (setf (fb-damage fb) (%fb-damage-union (fb-damage fb) damage)
          (fb-copy fb)   (%fb-copy-compose (fb-copy fb) copy)
          (fb-frameno fb) (logand (1+ (fb-frameno fb)) most-positive-fixnum))))

(defun fb-take-frame (fb)
  "Atomically read the accumulated (FRAMENO DAMAGE COPY MARK-TIME) and clear DAMAGE/COPY, so the
   next composite starts a fresh accumulation from where the client now is.  MARK-TIME is when
   the oldest change in this batch was marked, so the sender can measure its backlog (now -
   MARK-TIME = how long the change waited).  The sender calls this once per update it sends."
  (%with-frame-lock (fb)
    (multiple-value-prog1 (values (fb-frameno fb) (fb-damage fb) (fb-copy fb) (fb-mark-time fb))
      (setf (fb-damage fb) nil (fb-copy fb) nil))))

(defun %clip-intersect (clip x0 y0 x1 y1)
  "Intersect the (possibly NIL) CLIP box with (x0 y0 x1 y1)."
  (if clip
      (list (max (first clip) x0) (max (second clip) y0)
            (min (third clip) x1) (min (fourth clip) y1))
      (list x0 y0 x1 y1)))

(defmacro with-fb-clip ((fb x y w h) &body body)
  "Confine drawing in BODY to the rectangle (X,Y,W,H) intersected with any current
   clip.  fb-fill/fb-rect/blit-fb honour it; used for damage-limited compositing."
  (let ((g (gensym)) (old (gensym)))
    `(let* ((,g ,fb) (,old (fb-clip ,g)))
       (setf (fb-clip ,g) (%clip-intersect ,old ,x ,y (+ ,x ,w) (+ ,y ,h)))
       (unwind-protect (progn ,@body) (setf (fb-clip ,g) ,old)))))

#+sb-thread (defmacro with-fb-locked ((fb) &body body)
              ;; recursive: fb-resize (which locks) may be called inside a held
              ;; with-fb-locked (e.g. a terminal re-grids under its render lock).
              `(sb-thread:with-recursive-lock ((fb-lock ,fb)) ,@body))
#-sb-thread (defmacro with-fb-locked ((fb) &body body)
              (declare (ignore fb)) `(progn ,@body))

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
            (fb-height fb) height)
      (fb-touch fb)))
  fb)

(declaim (inline %in-bounds fb-put fb-get))
(defun %in-bounds (fb x y)
  (and (>= x 0) (>= y 0) (< x (fb-width fb)) (< y (fb-height fb))))

(defun fb-put (fb x y color)
  "Set pixel (X,Y) to COLOR (no-op if out of bounds)."
  (when (%in-bounds fb x y)
    (setf (aref (fb-pixels fb) (+ (* y (fb-width fb)) x)) (logand color #xffffff))
    (fb-touch fb))
  color)

(defun fb-get (fb x y)
  "Pixel (X,Y), or 0 if out of bounds."
  (if (%in-bounds fb x y) (aref (fb-pixels fb) (+ (* y (fb-width fb)) x)) 0))

(defun fb-fill (fb color)
  "Clear the framebuffer to COLOR (only the clip region, if a clip is set)."
  (let ((clip (fb-clip fb)))
    (if clip
        (fb-rect fb (first clip) (second clip) (- (third clip) (first clip)) (- (fourth clip) (second clip)) color)
        (progn (fill (fb-pixels fb) (logand color #xffffff)) (fb-touch fb))))
  color)

(defun fb-rect (fb x y w h color)
  "Filled rectangle at (X,Y), W x H, in COLOR (clipped to the fb and any clip box)."
  (let* ((fw (fb-width fb)) (fh (fb-height fb)) (clip (fb-clip fb))
         (x0 (max 0 x (if clip (first clip) 0)))  (y0 (max 0 y (if clip (second clip) 0)))
         (x1 (min fw (+ x w) (if clip (third clip) fw))) (y1 (min fh (+ y h) (if clip (fourth clip) fh)))
         (px (fb-pixels fb)) (c (logand color #xffffff)))
    (loop for yy from y0 below y1
          for row = (* yy fw)
          do (loop for xx from x0 below x1 do (setf (aref px (+ row xx)) c))))
  (fb-touch fb)
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
