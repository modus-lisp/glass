;;;; framebuffer.lisp — an in-memory 32-bit framebuffer + drawing primitives.
;;;;
;;;; Pixels are 0x00RRGGBB (X8R8G8B8) in a flat row-major (unsigned-byte 32)
;;;; array — the same value the RFB pixel format advertises, so serving a rect is
;;;; just writing the pixels out little-endian.  All drawing is clipped to the
;;;; framebuffer bounds, so callers never have to bounds-check.
;;;;
;;;; AND THEY ARE sRGB, which until now was true and unwritten.  See
;;;; +PIXEL-COLOUR-SPACE+ below: the numbers meant something specific all along, and
;;;; the only cost of never saying so is that nothing can reason about it.

(in-package #:glass)

;;; ---- what a pixel MEANS -----------------------------------------------------
;;; Distinct from what a pixel IS, which the header above describes.  Every channel
;;; here is sRGB: 8 bits, non-linear, the IEC 61966-2-1 transfer curve, Rec.709
;;; primaries, D65 white.  That is what a browser assumes of an untagged image, what
;;; RFB clients assume of a framebuffer, and what these numbers have always been —
;;; the change is only that it is now stated.
;;;
;;; It is stated because a colour space you have not named cannot be converted FROM.
;;; A wider one (10-bit, Rec.2020, PQ) is four layers of work away — see
;;; docs/density-and-colour.md — and none of that work can even be described while
;;; the current space is an assumption living in nobody's head in particular.
;;;
;;; NOT a promise that anything is colour-managed.  Nothing converts, and nothing
;;; should start converting on the strength of this constant existing; it records
;;; what is true so that a future second answer has a first one to differ from.
;;;
;;; The distinction that matters most is already made elsewhere and deliberately:
;;; gesso composites coverage in LINEAR light (via scribe) while blitting images in
;;; device space, because those are genuinely different operations on these values.
;;; That is the hard half of colour management and it predates this note.

(defconstant +pixel-colour-space+ :srgb
  "The colour space of every pixel in a FRAMEBUFFER: sRGB, 8 bits per channel,
non-linear, Rec.709 primaries, D65 white.

Recorded rather than enforced.  Read it when code needs to state an assumption it is
already making — an encoder describing its output, an image decoder deciding whether
to convert — and do not read it as a claim that any conversion happens.")

(declaim (inline rgb))
(defun rgb (r g b)
  "An X8R8G8B8 pixel from 8-bit sRGB R, G, B.

The channels are sRGB-encoded, NOT linear light: 128 is the middle of the encoding and
about 21% of the light.  Anything averaging or interpolating these values — a blend, a
gradient, a downscale — is wrong in the dark unless it converts first, which is why
gesso does its coverage blending in linear space and says so.  See
+PIXEL-COLOUR-SPACE+.

Keep constructing pixels through here rather than packing the integer by hand: it is
the one place the representation is decided, and the only reason a future widening
would not have to visit every call site in the tree."
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
#+sb-thread (defun %fb-make-lock (&optional (name "framebuffer"))
              (sb-thread:make-mutex :name name))
#-sb-thread (defun %fb-make-lock (&optional name) (declare (ignore name)) nil)

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
  (lock (%fb-make-lock "fb-pixels"))
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
  ;; NAMED APART, because a deadlock report is only as useful as the names in it.  Every
  ;; framebuffer has TWO of these and both used to be called "framebuffer", so a cycle
  ;; between them read as "framebuffer waited for framebuffer" — true, unhelpful, and
  ;; ambiguous about whether the two were different framebuffers or the two locks of one.
  (frame-lock (%fb-make-lock "fb-frame")))   ; guards the (damage copy frameno) triple

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
  "Fold two successive move copies (sx sy dx dy w h) into ONE, or NIL if nothing survives.
   Both are translations, so the pixels a single CopyRect can still carry are exactly those
   NEW moved AND OLD had already put there: intersect OLD's destination with NEW's source,
   then map that patch back to OLD's source and forward to NEW's destination.  This is
   'CopyRect farther' — the hint spans however many frames the sender fell behind.

   A dragged window (OLD's dst == NEW's src, same size) survives whole, which is the case
   this started as; a SCROLL, where each frame moves the same full-width block by a
   different amount, keeps the sub-block common to both (total offset, shorter block) —
   the previous exact-chain test rejected that and threw the hint away.  A non-move
   composite (NEW NIL) keeps OLD (nothing moved; the extra damage rides the diff).

   Dropping to NIL is always SAFE, never wrong: a copy is only ever a shortcut past pixels
   the diff would otherwise re-encode, and the sender applies the same move to the client's
   snapshot before diffing, so whatever the copy does not carry is simply sent."
  (cond
    ((null new) old)
    ((null old) new)
    (t (destructuring-bind (osx osy odx ody ow oh) old
         (destructuring-bind (nsx nsy ndx ndy nw nh) new
           (let* ((x0 (max odx nsx)) (y0 (max ody nsy))                  ; overlap, in OLD's
                  (x1 (min (+ odx ow) (+ nsx nw)))                       ; destination space
                  (y1 (min (+ ody oh) (+ nsy nh)))
                  (w (- x1 x0)) (h (- y1 y0)))
             (when (and (plusp w) (plusp h))
               (list (+ osx (- x0 odx)) (+ osy (- y0 ody))               ; back to OLD's source
                     (+ ndx (- x0 nsx)) (+ ndy (- y0 nsy))               ; on to NEW's destination
                     w h))))))))

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

(defun fb-take-copy (fb)
  "Atomically read and clear FB's accumulated COPY hint, leaving DAMAGE alone.

   For a framebuffer that no RFB sender serves directly — a window-manager SURFACE,
   which somebody paints into and a compositor then blits onto the screen — this is
   how the compositor asks the one question it needs: \"how did your content
   translate since I last looked?\".  The answer is in the surface's own coordinates
   and, like FB-TAKE-FRAME, is CONSUMED, so a hint is never replayed against pixels
   it no longer describes.  The damage mark is left for whoever owns it."
  (%with-frame-lock (fb) (prog1 (fb-copy fb) (setf (fb-copy fb) nil))))

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

#+sb-thread
(defmacro with-fb-locked-or ((fb &key (seconds 0.05)) give-up &body body)
  "BODY under FB's lock, or GIVE-UP if it cannot be had within SECONDS.

   FOR A CALLER THAT MUST NOT BLOCK.  The compositor holds one framebuffer's lock while
   it reaches for another's, and a thread that walks those two in the other order is a
   deadlock — one that froze an entire desktop, because the thread that blocks here is
   the one drawing the screen.

   This does not fix an inversion; it makes the compositor decline to participate in
   one.  The worst case becomes a surface that is one frame stale instead of a session
   that never paints again, and staleness is self-correcting: the next composite tries
   again."
  (let ((g (gensym "FB")))
    `(let ((,g ,fb))
       ;; WITH-FB-LOCKED is recursive and callers rely on that, so a thread may well
       ;; already hold this one.  GRAB-MUTEX is not recursive: asking for a mutex we own
       ;; would block on ourselves, which is the exact failure this macro exists to
       ;; avoid, arrived at from the other side.
       (if (sb-thread:holding-mutex-p (fb-lock ,g))
           (progn ,@body)
           (if (sb-thread:grab-mutex (fb-lock ,g) :timeout ,seconds)
               (unwind-protect (progn ,@body) (sb-thread:release-mutex (fb-lock ,g)))
               ,give-up)))))

#-sb-thread (defmacro with-fb-locked-or ((fb &key seconds) give-up &body body)
              (declare (ignore fb seconds give-up)) `(progn ,@body))
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
  "Filled rectangle at (X,Y), W x H, in COLOR (clipped to the fb and any clip box).

   One FILL per row, not a pixel loop: this is also the CLIPPED path of FB-FILL, so a
   compositor clearing a damage box before redrawing it lands here, and the pixel loop
   made that ~35x dearer than the unclipped whole-array FILL it stands in for —
   measured 3.71 ms against 0.11 ms to cover a 900x620 box."
  (let* ((fw (fb-width fb)) (fh (fb-height fb)) (clip (fb-clip fb))
         (x0 (max 0 x (if clip (first clip) 0)))  (y0 (max 0 y (if clip (second clip) 0)))
         (x1 (min fw (+ x w) (if clip (third clip) fw))) (y1 (min fh (+ y h) (if clip (fourth clip) fh)))
         (px (fb-pixels fb)) (c (logand color #xffffff)))
    (when (< x0 x1)
      (loop for yy from y0 below y1
            for row = (* yy fw)
            do (fill px c :start (+ row x0) :end (+ row x1)))))
  (fb-touch fb)
  fb)

(defun fb-move-rect (fb sx sy dx dy w h)
  "Translate the W x H block of FB's OWN pixels at (SX,SY) to (DX,DY) — in RAM, exactly
   what RFB's CopyRect does on the wire.  Ignores the clip box (the caller decides what
   may move, and a half-moved block is not a thing); a block that would read or write
   outside FB is refused whole rather than trimmed, so callers clip first.

   A compositor that knows a window merely SCROLLED can move the pixels the screen
   already holds instead of blitting the window again, and then redraw only the strip
   the move exposed.  Rows are copied in the order that keeps an overlapping move from
   eating its own source: downwards moves bottom-up, upwards moves top-down.  A purely
   HORIZONTAL move has one row as both source and destination, so it goes through a
   scratch row rather than trusting REPLACE's same-sequence overlap rule."
  (declare (optimize (speed 3) (safety 0))
           (fixnum sx sy dx dy w h))
  (let* ((px (fb-pixels fb)) (fw (fb-width fb)) (fh (fb-height fb)))
    (declare (type (simple-array (unsigned-byte 32) (*)) px) (fixnum fw fh))
    (when (and (plusp w) (plusp h)
               (<= 0 sx) (<= 0 sy) (<= (+ sx w) fw) (<= (+ sy h) fh)
               (<= 0 dx) (<= 0 dy) (<= (+ dx w) fw) (<= (+ dy h) fh)
               (or (/= sx dx) (/= sy dy)))
      (flet ((row (k)                                  ; move row K of the block
               (declare (fixnum k))
               (let ((s (+ (* (+ sy k) fw) sx)) (d (+ (* (+ dy k) fw) dx)))
                 (declare (fixnum s d))
                 (replace px px :start1 d :end1 (+ d w) :start2 s :end2 (+ s w)))))
        (cond
          ((= sy dy)                                   ; same rows: stage through a scratch row
           (let ((tmp (make-array w :element-type '(unsigned-byte 32))))
             (dotimes (k h)
               (let ((s (+ (* (+ sy k) fw) sx)) (d (+ (* (+ dy k) fw) dx)))
                 (replace tmp px :start2 s :end2 (+ s w))
                 (replace px tmp :start1 d :end1 (+ d w))))))
          ((> dy sy) (loop for k fixnum from (1- h) downto 0 do (row k)))
          (t         (loop for k fixnum from 0 below h do (row k)))))
      (fb-touch fb)))
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

(defun fb-blit-scaled (dst src dx dy scale)
  "Copy SRC into DST at (DX,DY), magnified by SCALE (a positive rational).

   FOR CONTENT THAT DOES NOT KNOW ABOUT DENSITY, which is nearly all of it.  An application
   drawing at a fixed number of pixels per character is drawing for one density, and on a
   screen with twice as many pixels it comes out half the size — a browser and a file manager
   that are legible on one machine and tiny on another, with nothing wrong in either of them.
   Magnifying their output is what every desktop does for such a program, and it is the only
   thing that CAN be done without the program's cooperation.

   NEAREST NEIGHBOUR, deliberately.  At an integer scale it is exact — every source pixel
   becomes a square block and nothing is invented — which is what text drawn as pixels wants;
   interpolating would blur glyph edges that were already sharp.  At a fractional scale it is
   uneven rather than soft, and that is the honest trade for a magnifier whose input is not a
   photograph.

   Written as a walk over the DESTINATION so every destination pixel is written exactly once:
   walking the source and painting blocks writes the overlaps repeatedly and leaves seams
   wherever the block size lands between pixels.

   THE REFERENCE VERSION, NOT THE ONE THE COMPOSITOR USES.  This goes through FB-GET and
   FB-PUT, which is a bounds check, a multiply and a generation bump per pixel: 68 ms for one
   1000x640 window magnified to 2x, against 0.7 ms for the row-wise CLIM-GLASS::BLIT-FB-SCALED
   that the WM actually calls.  Correct and obvious and a hundred times slower, which is the
   right trade for a primitive that exists to say what the operation IS — and the wrong one
   for a compositor, where it cost more than the frame budget and the desktop crawled."
  (if (= scale 1)
      (fb-blit dst src dx dy)
      (let* ((sw (fb-width src)) (sh (fb-height src))
             (dw (round (* sw scale))) (dh (round (* sh scale))))
        (dotimes (y dh dst)
          (let ((sy (min (1- sh) (floor (* y sh) dh))))
            (dotimes (x dw)
              (fb-put dst (+ dx x) (+ dy y)
                      (fb-get src (min (1- sw) (floor (* x sw) dw)) sy))))))))
