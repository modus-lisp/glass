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

(in-package #:scry)

;; +enc-zrle+ is defined in rfb.lisp alongside the other encoding constants.
(defparameter *zrle-tile* 64 "ZRLE tile size (pixels); fixed by the spec.")

(declaim (inline cpixel))
(defun cpixel (buf p)
  "Append pixel P as a 3-byte CPIXEL (B, G, R — little-endian low 3 bytes)."
  (vector-push-extend (logand p #xff) buf)
  (vector-push-extend (logand (ash p -8) #xff) buf)
  (vector-push-extend (logand (ash p -16) #xff) buf))

(defun zrle-raw-tile (buf px fw ax ay tw th)
  "Subencoding 0: every pixel as a CPIXEL, in reading order."
  (vector-push-extend 0 buf)
  (dotimes (ly th)
    (let ((row (* (+ ay ly) fw)))
      (dotimes (lx tw) (cpixel buf (aref px (+ row ax lx)))))))

(defun zrle-packed-tile (buf px fw ax ay tw th palette index)
  "Subencoding 2..16: the palette, then each row's indices packed MSB-first
   (1/2/4 bits per index by palette size) and padded to a byte."
  (let* ((psize (fill-pointer palette))
         (bpp (cond ((<= psize 2) 1) ((<= psize 4) 2) (t 4))))
    (vector-push-extend psize buf)                       ; subencoding = palette size
    (dotimes (i psize) (cpixel buf (aref palette i)))
    (dotimes (ly th)
      (let ((row (* (+ ay ly) fw)) (acc 0) (nbits 0))
        (dotimes (lx tw)
          (setf acc (logior (ash acc bpp) (gethash (aref px (+ row ax lx)) index)))
          (incf nbits bpp)
          (loop while (>= nbits 8) do
            (decf nbits 8)
            (vector-push-extend (logand (ash acc (- nbits)) #xff) buf)
            (setf acc (logand acc (1- (ash 1 nbits))))))
        (when (plusp nbits)                              ; left-align the last partial byte
          (vector-push-extend (logand (ash acc (- 8 nbits)) #xff) buf))))))

(defun zrle-tile (buf px fw ax ay tw th)
  "Append one tile: solid (subenc 1), a <=16-colour packed palette (2..16), or
   raw (0) when the tile has too many distinct colours to palette."
  (let ((index (make-hash-table))
        (palette (make-array 16 :fill-pointer 0))
        (over nil))
    (block scan
      (dotimes (ly th)
        (let ((row (* (+ ay ly) fw)))
          (dotimes (lx tw)
            (let ((c (aref px (+ row ax lx))))
              (unless (nth-value 1 (gethash c index))
                (when (>= (fill-pointer palette) 16) (setf over t) (return-from scan))
                (setf (gethash c index) (fill-pointer palette))
                (vector-push c palette)))))))
    (cond
      (over (zrle-raw-tile buf px fw ax ay tw th))
      ((= (fill-pointer palette) 1)                      ; solid tile
       (vector-push-extend 1 buf) (cpixel buf (aref palette 0)))
      (t (zrle-packed-tile buf px fw ax ay tw th palette index)))))

(defun write-rect-zrle (s fb x y w h zs)
  "Write one ZRLE rectangle: header, then [u32 length][zlib data] over the
   per-client stream ZS (sync-flushed so the client can decode it immediately
   while the compression window carries into the next rectangle)."
  (w-u16 s x) (w-u16 s y) (w-u16 s w) (w-u16 s h) (w-u32 s +enc-zrle+)
  (let ((px (fb-pixels fb)) (fw (fb-width fb))
        (buf (make-array 4096 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for ty from 0 below h by *zrle-tile* for th = (min *zrle-tile* (- h ty)) do
      (loop for tx from 0 below w by *zrle-tile* for tw = (min *zrle-tile* (- w tx)) do
        (zrle-tile buf px fw (+ x tx) (+ y ty) tw th)))
    (cram:compress zs buf)
    (let ((z (cram:sync-flush zs)))
      (w-u32 s (length z))
      (w-bytes s z))))
