;;;; fps-measure.lisp — what frame rate does the sender sustain?  In-process (no
;;;; sockets/threads, so host load can't skew it): drive the REAL sender stages
;;;; (diff -> encode -> snapshot) that rfb-sender-loop runs per frame, for a
;;;; full-screen churn (worst case) and a small dirty region (typical desktop),
;;;; on the TRLE path.  Reports the CPU-only fps ceiling + bytes/frame, then the
;;;; other two caps that bound the REAL number (60Hz pull cadence, link bandwidth).
;;;;   sbcl --non-interactive --load backend/inspect/fps-measure.lisp
(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream))) (ql:quickload '(:glass :cram))))
(in-package :glass)

(defclass sink (sb-gray:fundamental-binary-output-stream) ((n :initform 0 :accessor sink-n)))
(defmethod sb-gray:stream-write-byte ((s sink) b) (incf (sink-n s)) b)
(defmethod sb-gray:stream-write-sequence ((s sink) seq &optional (start 0) end)
  (incf (sink-n s) (- (or end (length seq)) start)) seq)
(defmethod sb-gray:stream-force-output ((s sink)) nil)
(defun ms (ticks) (/ (* 1000.0 ticks) internal-time-units-per-second))

(defun bench (label w h rx ry rw rh iters churn-fn)
  "Time the per-frame sender pipeline for a rect (RX,RY,RW,RH) that CHURN-FN
   mutates each frame.  Big rects go TRLE (matches emit-rect's threshold), small
   rects go ZRLE — exactly what the live sender picks."
  (let* ((fb (make-framebuffer w h)) (px (fb-pixels fb))
         (snap (make-array (* w h) :element-type '(unsigned-byte 32)))
         (region (list rx ry (+ rx rw) (+ ry rh)))
         (zs (cram:make-zstream)) (s (make-instance 'sink))
         (tframe 0) (nbytes 0) (nrects 0))
    (dotimes (y h) (dotimes (x w) (setf (aref px (+ (* y w) x)) (logior (ash (mod x 256) 16) (ash (mod y 256) 8) 30))))
    (replace snap px)
    (dotimes (i iters)
      (funcall churn-fn px w rx ry rw rh i)
      (setf (sink-n s) 0)
      (let ((a (get-internal-real-time)))
        (let ((rects (dirty-rects fb snap region)))
          (send-rects s fb rects +enc-zrle+ zs t)          ; t = TRLE allowed, exactly like the live client
          (update-snapshot fb snap rects)
          (setf nrects (length rects)))
        (incf tframe (- (get-internal-real-time) a)))
      (setf nbytes (sink-n s)))
    (let* ((per (/ (ms tframe) iters)) (fps (/ 1000.0 per)) (kb (/ nbytes 1024.0)))
      (format t "~&~a  (~dx~d dirty, ~d rects)~%" label rw rh nrects)
      (format t "    encode pipeline: ~,2f ms/frame  ->  CPU ceiling ~,0f fps~%" per fps)
      (format t "    bytes/frame:     ~,1f KB~%" kb)
      (format t "    real fps is min(CPU ceiling, 60Hz pull cap, link/frame):~%")
      (dolist (lnk '(("1 Gbps LAN" 125000000) ("100 Mbps" 12500000) ("25 Mbps WAN" 3125000)))
        (destructuring-bind (name bps) lnk
          (let ((bwfps (if (plusp nbytes) (/ bps nbytes) 1e9)))
            (format t "      ~12a -> ~,0f fps~a~%" name (min fps 60.0 bwfps)
                    (if (< bwfps 60.0) " (bandwidth-bound)" (if (< fps 60.0) " (encode-bound)" " (pull-cap 60)"))))))
      per)))

(format t "~&[glass sender frame-rate — TRLE build]~%~%")
;; worst case: a terminal-sized region fully changing every frame (video/scroll)
(bench "FULL-CHURN 720x442 (video-like)" 1280 800 40 62 720 442 40
       (let ((seed 12345))
         (lambda (px w rx ry rw rh i) (declare (ignore i))
           (dotimes (yy rh) (dotimes (xx rw)
             (setf seed (logand (+ (* seed 1103515245) 12345) #xffffffff))
             (setf (aref px (+ (* (+ ry yy) w) (+ rx xx))) (logand seed #xffffff)))))))
(terpri)
;; typical: a line of text repaints (mostly-background with dark glyph pixels).
;; Mix the frame index in so consecutive frames genuinely differ (the low bits of
;; a plain LCG have a short period, which would make synthetic frames identical).
(bench "TYPING one line 720x16" 1280 800 40 62 720 16 60
       (lambda (px w rx ry rw rh i)
         (dotimes (yy rh) (dotimes (xx rw)
           (let ((n (logand (+ (* (+ xx (* yy 131) (* i 977)) 2654435761) 40503) #xffffff)))
             (setf (aref px (+ (* (+ ry yy) w) (+ rx xx))) (if (< (logand n #xff) 30) #x111111 #xcccccc)))))))
(finish-output) (sb-ext:exit)
