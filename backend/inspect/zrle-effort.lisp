;;;; zrle-effort.lisp — TigerVNC only speaks ZRLE, so the big-frame win has to come
;;;; from ZRLE itself, not TRLE.  ZRLE's cost is deflate's LZ77 match search; cram's
;;;; max-chain caps how hard it searches.  Measure encode time + size for a heavy
;;;; churning region at several max-chain levels — is low-effort ZRLE ~TRLE-fast at
;;;; acceptable size?  In-process, host-load-immune.
;;;;   sbcl --non-interactive --load backend/inspect/zrle-effort.lisp
(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream))) (ql:quickload '(:glass :cram :chipz))))
(in-package :glass)

(defclass sink (sb-gray:fundamental-binary-output-stream) ((n :initform 0 :accessor sink-n)))
(defmethod sb-gray:stream-write-byte ((s sink) b) (incf (sink-n s)) b)
(defmethod sb-gray:stream-write-sequence ((s sink) seq &optional (start 0) end)
  (incf (sink-n s) (- (or end (length seq)) start)) seq)
(defmethod sb-gray:stream-force-output ((s sink)) nil)
(defun ms (ticks n) (/ (* 1000.0 ticks) n internal-time-units-per-second))

(let* ((w 1280) (h 800) (fb (make-framebuffer w h)) (px (fb-pixels fb))
       (rx 40) (ry 62) (rw 720) (rh 442) (iters 20) (seed 12345)
       (s (make-instance 'sink)))
  (dotimes (y h) (dotimes (x w) (setf (aref px (+ (* y w) x)) (logior (ash (mod x 256) 16) (ash (mod y 256) 8) 30))))
  (flet ((churn () (dotimes (yy rh) (dotimes (xx rw)
                     (setf seed (logand (+ (* seed 1103515245) 12345) #xffffffff))
                     (setf (aref px (+ (* (+ ry yy) w) (+ rx xx))) (logand seed #xffffff))))))
    (format t "~&[ZRLE encode effort — heavy 720x442 churn, what TigerVNC would get]~%")
    (format t "  ~10@a ~14@a ~12@a~%" "max-chain" "ms/frame" "KB/frame")
    (dolist (mc '(128 32 8 4 1))
      (let ((zs (cram:make-zstream :max-chain mc)) (tenc 0) (bytes 0))
        (dotimes (i iters)
          (churn) (setf (sink-n s) 0)
          (let ((a (get-internal-real-time)))
            (write-rect-zrle s fb rx ry rw rh zs)
            (incf tenc (- (get-internal-real-time) a)))
          (setf bytes (sink-n s)))
        (format t "  ~10@a ~11,1f ms ~10,1f~a~%" mc (ms tenc iters) (/ bytes 1024.0)
                (if (= mc 128) "   <- current default" ""))))
    ;; the fix: ZRLE with STORED blocks — pack + adler + copy, NO LZ77
    (let ((zs (cram:make-zstream)) (tenc 0) (bytes 0) (dstate (chipz:make-dstate 'chipz:zlib)) (okp t))
      (dotimes (i iters)
        (churn) (setf (sink-n s) 0)
        (let ((a (get-internal-real-time)))
          ;; inline write-rect-zrle but sync-flush-STORED instead of sync-flush
          (multiple-value-bind (buf len) (pack-rect fb rx ry rw rh)
            (cram:compress zs buf :end len)
            (let ((z (cram:sync-flush-stored zs)))
              (incf tenc (- (get-internal-real-time) a))
              (setf bytes (length z))
              ;; correctness: chipz (a real, independent inflate) must recover the tile bytes
              (let ((dec (chipz:decompress nil dstate z)))
                (unless (and (= (length dec) len) (loop for k below len always (= (aref dec k) (aref buf k))))
                  (setf okp nil)))))))
      (format t "  ~10@a ~11,1f ms ~10,1f   <- STORED blocks, chipz round-trip ~:[FAIL~;ok~]~%"
              "ZRLE-store" (ms tenc iters) (/ bytes 1024.0) okp))
    ;; reference: TRLE pack (no deflate at all)
    (let ((tpack 0) (plen 0))
      (dotimes (i iters)
        (churn)
        (let ((a (get-internal-real-time)))
          (multiple-value-bind (buf len) (pack-rect fb rx ry rw rh) (declare (ignore buf))
            (incf tpack (- (get-internal-real-time) a)) (setf plen len))))
      (format t "  ~10@a ~11,1f ms ~10,1f   <- no deflate (reference)~%" "TRLE" (ms tpack iters) (/ plen 1024.0)))))
(finish-output) (sb-ext:exit)
