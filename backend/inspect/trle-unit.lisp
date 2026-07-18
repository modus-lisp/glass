;;;; trle-unit.lisp — TRLE (enc 15) wire-format round-trip, in-process (no
;;;; sockets/threads, so host load can't skew it).  TRLE reuses ZRLE's exact tile
;;;; bytes (pack-rect) but sends them RAW: no zlib, no length prefix, tiles self-
;;;; delimiting.  So the decode is the oracle's ZRLE tile parser run straight on
;;;; the packed bytes — if pixels match, the wire framing is correct.
;;;;   sbcl --non-interactive --load backend/inspect/trle-unit.lisp
(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream))) (ql:quickload '(:glass))))
(in-package :glass)

;;; the oracle's tile decoder, inlined (subenc 0 raw / 1 solid / 2-16 palette).
(defun dec-cpixel (dec p)
  (values (logior (ash (aref dec (+ p 2)) 16) (ash (aref dec (+ p 1)) 8) (aref dec p)) (+ p 3)))

(defun dec-tiles (dec rx ry rw rh w cli)
  (let ((pos 0))
    (loop for ty from 0 below rh by 16 for th = (min 16 (- rh ty)) do   ; RFC 6143 TRLE = 16-px tiles
      (loop for tx from 0 below rw by 16 for tw = (min 16 (- rw tx)) do
        (let ((sub (aref dec pos)))
          (incf pos)
          (flet ((put (lx ly c) (setf (aref cli (+ (* (+ ry ty ly) w) (+ rx tx lx))) c)))
            (cond
              ((= sub 0)
               (dotimes (ly th) (dotimes (lx tw)
                 (multiple-value-bind (c np) (dec-cpixel dec pos) (put lx ly c) (setf pos np)))))
              ((= sub 1)
               (multiple-value-bind (c np) (dec-cpixel dec pos)
                 (setf pos np) (dotimes (ly th) (dotimes (lx tw) (put lx ly c)))))
              ((<= 2 sub 16)
               (let ((pal (make-array sub)))
                 (dotimes (i sub)
                   (multiple-value-bind (c np) (dec-cpixel dec pos) (setf (aref pal i) c pos np)))
                 (let ((bpp (cond ((<= sub 2) 1) ((<= sub 4) 2) (t 4))))
                   (dotimes (ly th)
                     (let ((acc 0) (nbits 0))
                       (dotimes (lx tw)
                         (when (< nbits bpp)
                           (setf acc (logior (ash acc 8) (aref dec pos)) nbits (+ nbits 8)) (incf pos))
                         (decf nbits bpp)
                         (put lx ly (aref pal (logand (ash acc (- nbits)) (1- (ash 1 bpp)))))
                         (setf acc (logand acc (1- (ash 1 nbits))))))))))
              (t (error "bad subenc ~a" sub)))))))
    pos))

(defun run-case (name w h rx ry rw rh fill)
  (let* ((fb (make-framebuffer w h)) (px (fb-pixels fb))
         (cli (make-array (* w h) :element-type '(unsigned-byte 32) :initial-element 0)))
    (dotimes (y h) (dotimes (x w) (setf (aref px (+ (* y w) x)) (funcall fill x y))))
    (multiple-value-bind (buf len) (pack-rect fb rx ry rw rh nil *trle-tile*)
      (let ((bytes (subseq buf 0 len)))
        (let ((consumed (dec-tiles bytes rx ry rw rh w cli)))
          (let ((mism 0))
            (loop for ty below rh do
              (loop for tx below rw do
                (let ((sp (aref px (+ (* (+ ry ty) w) (+ rx tx))))
                      (cp (aref cli (+ (* (+ ry ty) w) (+ rx tx)))))
                  (unless (= (logand sp #xffffff) (logand cp #xffffff)) (incf mism)))))
            (format t "  ~a: ~d bytes, ~d/~d px consumed=~:[SHORT~;ok~] -> ~:[FAIL ~d mismatch~;PASS~]~%"
                    name len (* rw rh) (= consumed len) (zerop mism) (zerop mism) mism)
            (zerop mism)))))))

(format t "~&[TRLE wire-format round-trip]~%")
(let ((ok 0) (n 0))
  (flet ((c (&rest args) (incf n) (when (apply #'run-case args) (incf ok))))
    ;; solid tiles
    (c "solid" 256 256 0 0 200 150 (lambda (x y) (declare (ignore x y)) #x3366aa))
    ;; 2-colour checkerboard -> 1bpp palette
    (c "checker-2col" 256 256 10 10 128 96 (lambda (x y) (if (evenp (+ x y)) #xffffff #x000000)))
    ;; smooth gradient -> raw tiles (>16 colours)
    (c "gradient-raw" 320 240 0 0 300 220 (lambda (x y) (logior (ash (mod x 256) 16) (ash (mod y 256) 8) 30)))
    ;; small palette (4 colours) -> 2bpp
    (c "pal-4col" 200 200 5 5 130 130 (lambda (x y) (aref #(#x111111 #x222222 #xaa0000 #x00aa00) (mod (+ (floor x 7) (floor y 5)) 4))))
    ;; odd size crossing tile boundary at non-multiple of 64
    (c "odd-size" 300 300 3 7 137 91 (lambda (x y) (logior (ash (mod (* x 3) 256) 16) (mod (* y 5) 256)))))
  (format t "~%checks: ~d   passes: ~d   => ~:[FAIL~;PASS~]~%" n ok (= ok n)))
(finish-output) (sb-ext:exit)
