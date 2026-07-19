;;;; perf.lisp — standing per-frame performance counters for the RFB path.
;;;;
;;;; The RFB path has two halves that each cost time: COMPOSITE (redraw the fb) and
;;;; SEND (diff + encode + write a FramebufferUpdate).  Rather than write a new
;;;; one-off benchmark for every "it's slow", accumulate the numbers live, always
;;;; on, at near-zero cost (a few incfs guarded by *PERF-ON*), and read a snapshot
;;;; over the control socket:  echo '(glass:perf-report)' | nc 127.0.0.1 4009
;;;;
;;;; PERF-RESET zeroes the window, so the workflow is: reset, do the thing that
;;;; feels slow, report — and see exactly where the frame time and bytes went.

(in-package #:glass)

(defparameter *perf-on* t
  "Whether the per-frame perf counters accumulate (a few incfs on the hot path).")
(defvar *tx* nil
  "When bound to a (count) cons on the send path, the byte writers add to its car,
   so the sender can total the bytes it actually wrote for one frame.")

(declaim (inline tx+))
(defun tx+ (n)
  (when *tx* (incf (the fixnum (car *tx*)) (the fixnum n)))
  n)

(defstruct (perf (:conc-name pf-))
  (lock (sb-thread:make-mutex :name "glass-perf"))
  (t0 (get-internal-real-time))
  ;; SEND side — one per delivered FramebufferUpdate
  (frames 0) (enc 0) (bytes 0) (copyrects 0) (full 0) (damaged 0) (dmg-px 0)
  ;; COMPOSITE side — one per composite-all
  (composites 0) (comp 0) (comp-full 0) (comp-px 0))

(defvar *perf* (make-perf))
(defun perf-reset () "Zero the perf window." (setf *perf* (make-perf)) t)

(defun perf-record-send (ticks region copy bytes)
  "One delivered frame: TICKS to diff+encode+write, REGION (:full or (x0 y0 x1 y1)),
   COPY (a CopyRect hint or nil), BYTES written to the socket."
  (when *perf-on*
    (let ((p *perf*))
      (sb-thread:with-mutex ((pf-lock p))
        (incf (pf-frames p)) (incf (pf-enc p) ticks) (incf (pf-bytes p) bytes)
        (when copy (incf (pf-copyrects p)))
        (if (eq region :full)
            (incf (pf-full p))
            (progn (incf (pf-damaged p))
                   (when (and (consp region) (= 4 (length region)))
                     (destructuring-bind (x0 y0 x1 y1) region
                       (incf (pf-dmg-px p) (* (max 0 (- x1 x0)) (max 0 (- y1 y0))))))))))
    t))

(defun perf-record-composite (ticks damage)
  "One composite-all: TICKS to composite, DAMAGE (x y w h) or nil (whole screen)."
  (when *perf-on*
    (let ((p *perf*))
      (sb-thread:with-mutex ((pf-lock p))
        (incf (pf-composites p)) (incf (pf-comp p) ticks)
        (if (and (consp damage) (= 4 (length damage)))
            (incf (pf-comp-px p) (* (max 0 (third damage)) (max 0 (fourth damage))))
            (incf (pf-comp-full p)))))
    t))

(defun %pf-ms (ticks) (/ (* 1000.0 ticks) internal-time-units-per-second))

(defun perf-report ()
  "A human-readable snapshot of the perf window since the last PERF-RESET."
  (let ((p *perf*))
    (sb-thread:with-mutex ((pf-lock p))
      (let* ((el (max 0.001 (/ (- (get-internal-real-time) (pf-t0 p))
                               (float internal-time-units-per-second))))
             (f (pf-frames p)) (c (pf-composites p)))
        (with-output-to-string (o)
          (format o "glass perf — ~,1fs window~:[~; (perf OFF)~]~%" el (not *perf-on*))
          (format o "  SEND       ~d frames, ~,1f fps~%" f (/ f el))
          (when (plusp f)
            (format o "    encode     ~,2f ms/frame~%" (%pf-ms (/ (pf-enc p) f)))
            (format o "    bytes      ~,1f KB/frame  (~,0f KB/s)~%"
                    (/ (pf-bytes p) f 1024.0) (/ (pf-bytes p) el 1024.0))
            (format o "    region     ~d damage (avg ~,0f px) | ~d FULL-screen | ~d CopyRect~%"
                    (pf-damaged p) (if (plusp (pf-damaged p)) (/ (pf-dmg-px p) (float (pf-damaged p))) 0.0)
                    (pf-full p) (pf-copyrects p)))
          (format o "  COMPOSITE  ~d, ~,1f/s~%" c (/ c el))
          (when (plusp c)
            (format o "    time       ~,2f ms each~%" (%pf-ms (/ (pf-comp p) c)))
            (format o "    region     ~d damage (avg ~,0f px) | ~d FULL-screen~%"
                    (- c (pf-comp-full p))
                    (if (< (pf-comp-full p) c) (/ (pf-comp-px p) (float (- c (pf-comp-full p)))) 0.0)
                    (pf-comp-full p))))))))
