;;;; zrle.lisp — ZRLE encoding (RFC 6143 §7.7.6), the lossless bandwidth win.
;;;;
;;;; ZRLE = Zlib Run-Length Encoding.  A rectangle is split into 64x64 tiles;
;;;; each tile is described compactly (solid colour, a small packed palette, or
;;;; raw pixels), the whole rectangle's tile bytes are run through ONE persistent
;;;; zlib stream (cram), and the sync-flushed output is length-prefixed.  Because
;;;; the zlib state is retained across rectangles for the life of the connection
;;;; (that's why cram needs SYNC-FLUSH), a mostly-static desktop compresses to
;;;; almost nothing while staying pixel-exact — Apple Screen Sharing "Full
;;;; Quality" is the same idea: sharp and vibrant *because* it is lossless.
;;;;
;;;; Pixels travel as CPIXELs: our format is 32bpp true-colour, depth 24, the RGB
;;;; in the low 3 bytes, so a CPIXEL is just those 3 bytes (B,G,R) — the X byte
;;;; that Raw/Hextile carry is dropped, per the spec's compressed-pixel rule.

(in-package #:glass)

;; +enc-zrle+ is defined in rfb.lisp alongside the other encoding constants.
(defparameter *zrle-tile* 64 "ZRLE tile size (pixels); fixed by the spec.")

;;; The tile bytes are packed into a PRE-SIZED simple-array with a fixnum index
;;; (no vector-push-extend, no per-tile hash/palette allocation) — this is the hot
;;; path, and the old adjustable-array pushes dominated a heavy frame (~270ms).

(declaim (inline cpix) (ftype (function ((simple-array (unsigned-byte 8) (*)) fixnum (unsigned-byte 32)) fixnum) cpix))
(defun cpix (buf i p)
  "Write pixel P as a 3-byte CPIXEL (B,G,R) at BUF[i]; return the next index."
  (declare (type (simple-array (unsigned-byte 8) (*)) buf) (type fixnum i) (type (unsigned-byte 32) p)
           (optimize (speed 3) (safety 0)))
  (setf (aref buf i) (logand p #xff)
        (aref buf (+ i 1)) (logand (ash p -8) #xff)
        (aref buf (+ i 2)) (logand (ash p -16) #xff))
  (the fixnum (+ i 3)))

(defun zrle-tile (buf i px fw ax ay tw th index palette)
  "Pack one tile into BUF at index I (returning the next index): solid (subenc 1),
   a <=16-colour packed palette (2..16), or raw (0).  INDEX (hash) and PALETTE
   (16-elt vector) are scratch, reused across tiles."
  (declare (type (simple-array (unsigned-byte 8) (*)) buf)
           (type (simple-array (unsigned-byte 32) (*)) px palette)
           (type fixnum i fw ax ay tw th) (optimize (speed 3) (safety 0)))
  (clrhash index)
  (let ((psize 0) (over nil))
    (declare (fixnum psize))
    (block scan
      (dotimes (ly th)
        (let ((row (* (+ ay ly) fw)))
          (declare (fixnum row))
          (dotimes (lx tw)
            (let ((c (aref px (+ row ax lx))))
              (unless (nth-value 1 (gethash c index))
                (when (>= psize 16) (setf over t) (return-from scan))
                (setf (gethash c index) psize (aref palette psize) c) (incf psize)))))))
    (cond
      (over                                              ; subencoding 0: raw CPIXELs
       (setf (aref buf i) 0) (incf i)
       (dotimes (ly th)
         (let ((row (* (+ ay ly) fw))) (declare (fixnum row))
           (dotimes (lx tw) (setf i (cpix buf i (aref px (+ row ax lx))))))))
      ((= psize 1)                                       ; subencoding 1: solid
       (setf (aref buf i) 1) (setf i (cpix buf (1+ i) (aref palette 0))))
      (t                                                 ; subencoding 2..16: packed palette
       (let ((bpp (cond ((<= psize 2) 1) ((<= psize 4) 2) (t 4))))
         (declare (fixnum bpp))
         (setf (aref buf i) psize) (incf i)
         (dotimes (k psize) (setf i (cpix buf i (aref palette k))))
         (dotimes (ly th)
           (let ((row (* (+ ay ly) fw)) (acc 0) (nbits 0))
             (declare (fixnum row acc nbits))
             (dotimes (lx tw)
               (setf acc (logior (ash acc bpp) (the fixnum (gethash (aref px (+ row ax lx)) index))))
               (incf nbits bpp)
               (loop while (>= nbits 8) do (decf nbits 8)
                 (setf (aref buf i) (logand (ash acc (- nbits)) #xff)) (incf i)
                 (setf acc (logand acc (1- (ash 1 nbits))))))
             (when (plusp nbits)
               (setf (aref buf i) (logand (ash acc (- 8 nbits)) #xff)) (incf i)))))))
    i))

(defun zrle-rect-cap (w h)
  "A byte capacity that safely holds W x H packed as ZRLE tiles (raw worst case)."
  (+ (* w h 3) (* (ceiling w *zrle-tile*) (ceiling h *zrle-tile*) 49) 16))

(defun pack-rect (fb x y w h &optional buf)
  "Pack the ZRLE tile bytes for rect (X,Y,W,H) into BUF (allocated if NIL); return
   (values BUF LEN).  Pure — no shared state, so calls on disjoint rects/rows run
   in parallel."
  (let* ((px (fb-pixels fb)) (fw (fb-width fb))
         (buf (or buf (make-array (zrle-rect-cap w h) :element-type '(unsigned-byte 8))))
         (i 0) (index (make-hash-table :size 32))
         (palette (make-array 16 :element-type '(unsigned-byte 32))))
    (declare (type (simple-array (unsigned-byte 8) (*)) buf) (fixnum i))
    (loop for ty from 0 below h by *zrle-tile* for th = (min *zrle-tile* (- h ty)) do
      (loop for tx from 0 below w by *zrle-tile* for tw = (min *zrle-tile* (- w tx)) do
        (setf i (zrle-tile buf i px fw (+ x tx) (+ y ty) tw th index palette))))
    (values buf i)))

(defun write-rect-zrle (s fb x y w h zs)
  "Write one ZRLE rectangle: header, then [u32 length][zlib data] over the
   per-client stream ZS (sync-flushed so the client can decode it immediately
   while the compression window carries into the next rectangle)."
  (w-u16 s x) (w-u16 s y) (w-u16 s w) (w-u16 s h) (w-u32 s +enc-zrle+)
  (multiple-value-bind (buf len) (pack-rect fb x y w h)
    (cram:compress zs buf :end len)
    (let ((z (cram:sync-flush zs)))
      (w-u32 s (length z))
      (w-bytes s z))))

(defun write-rect-trle (s fb x y w h)
  "Write one TRLE rectangle (RFC 6143 §7.7.5): header, then the SAME packed tile
   bytes as ZRLE but sent raw — no zlib, no length prefix (tiles are self-
   delimiting).  No serial deflate, so encoding a big/incompressible rect is
   ~200x cheaper; the client reads tiles until the rect is filled."
  (w-u16 s x) (w-u16 s y) (w-u16 s w) (w-u16 s h) (w-u32 s +enc-trle+)
  (multiple-value-bind (buf len) (pack-rect fb x y w h)
    (write-sequence buf s :end len)))
