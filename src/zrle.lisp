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

(defmacro %wcpix (buf i p fmt)
  "Emit pixel P as a CPIXEL at BUF[I], advancing I.  FMT NIL = native 3-byte fast
   path (inlined CPIX); non-NIL = the client's format (PUT-CPIX)."
  `(setf ,i (if ,fmt (put-cpix ,buf ,i ,p ,fmt) (cpix ,buf ,i ,p))))

(defun zrle-tile (buf i px fw ax ay tw th index palette &optional fmt)
  "Pack one tile into BUF at index I (returning the next index): solid (subenc 1),
   a <=16-colour packed palette (2..16), or raw (0).  INDEX (hash) and PALETTE
   (16-elt vector) are scratch, reused across tiles.  FMT NIL = native CPIXELs;
   otherwise the client pixel format drives CPIXEL size/layout."
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
           (dotimes (lx tw) (%wcpix buf i (aref px (+ row ax lx)) fmt)))))
      ((= psize 1)                                       ; subencoding 1: solid
       (setf (aref buf i) 1) (incf i) (%wcpix buf i (aref palette 0) fmt))
      (t                                                 ; subencoding 2..16: packed palette
       (let ((bpp (cond ((<= psize 2) 1) ((<= psize 4) 2) (t 4))))
         (declare (fixnum bpp))
         (setf (aref buf i) psize) (incf i)
         (dotimes (k psize) (%wcpix buf i (aref palette k) fmt))
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

(defparameter *trle-tile* 16
  "TRLE tile size (pixels).  RFC 6143 §7.7.5 fixes TRLE at 16 — NOT ZRLE's 64.
   The tile PACKING is identical (solid/palette/raw subencodings); only the tile
   grid and the transport (TRLE = raw self-delimiting tiles, ZRLE = one zlib
   stream) differ.  A compliant client desyncs if TRLE tiles aren't 16.")

(defun zrle-rect-cap (w h &optional (tile *zrle-tile*))
  "A byte capacity that safely holds W x H packed as TILE-sized tiles (raw worst
   case): all pixels raw (up to 4 bytes for a converted client format) + per-tile
   overhead (subenc byte + <=16 palette at 4 bytes each)."
  (+ (* w h 4) (* (ceiling w tile) (ceiling h tile) 68) 16))

(defun pack-rect (fb x y w h &optional buf (tile *zrle-tile*) fmt)
  "Pack the tile bytes for rect (X,Y,W,H) into BUF (allocated if NIL) on a TILE-px
   grid (64 for ZRLE, 16 for TRLE); return (values BUF LEN).  FMT NIL = native
   3-byte CPIXELs; otherwise the client pixel format.  Pure — no shared state, so
   calls on disjoint rects/rows run in parallel."
  (let* ((px (fb-pixels fb)) (fw (fb-width fb))
         (buf (or buf (make-array (zrle-rect-cap w h tile) :element-type '(unsigned-byte 8))))
         (i 0) (index (make-hash-table :size 32))
         (palette (make-array 16 :element-type '(unsigned-byte 32))))
    (declare (type (simple-array (unsigned-byte 8) (*)) buf) (fixnum i tile))
    (loop for ty from 0 below h by tile for th = (min tile (- h ty)) do
      (loop for tx from 0 below w by tile for tw = (min tile (- w tx)) do
        (setf i (zrle-tile buf i px fw (+ x tx) (+ y ty) tw th index palette fmt))))
    (values buf i)))

(defun write-rect-zrle (s fb x y w h zs &optional stored fmt)
  "Write one ZRLE rectangle: header, then [u32 length][zlib data] over the
   per-client stream ZS (sync-flushed so the client can decode it immediately
   while the compression window carries into the next rectangle).  STORED emits
   the zlib as stored DEFLATE blocks (no LZ77) — ~5x cheaper to encode a big
   incompressible rect, at some ratio; still ordinary ZRLE on the wire, so any
   client (e.g. TigerVNC, which won't negotiate TRLE) decodes it unchanged.
   FMT NIL = native CPIXELs; otherwise the client's requested pixel format."
  (w-u16 s x) (w-u16 s y) (w-u16 s w) (w-u16 s h) (w-u32 s +enc-zrle+)
  (multiple-value-bind (buf len) (pack-rect fb x y w h nil *zrle-tile* fmt)
    (cram:compress zs buf :end len)
    (let ((z (if stored (cram:sync-flush-stored zs) (cram:sync-flush zs))))
      (w-u32 s (length z))
      (w-bytes s z))))

(defun write-rect-trle (s fb x y w h &optional fmt)
  "Write one TRLE rectangle (RFC 6143 §7.7.5): header, then the SAME packed tile
   bytes as ZRLE but sent raw — no zlib, no length prefix (tiles are self-
   delimiting).  No serial deflate, so encoding a big/incompressible rect is
   ~200x cheaper; the client reads tiles until the rect is filled."
  (w-u16 s x) (w-u16 s y) (w-u16 s w) (w-u16 s h) (w-u32 s +enc-trle+)
  (multiple-value-bind (buf len) (pack-rect fb x y w h nil *trle-tile* fmt)   ; 16-px tiles per RFC
    (write-sequence buf s :end len)))
