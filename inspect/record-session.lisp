;;;; inspect/record-session.lisp — record a running desktop's session mix to a WAV.
;;;;
;;;; VNC carries no audio (the RFB QEMU-audio pseudo-encoding is not implemented, and would not
;;;; help a listener that is not a VNC client anyway), so the way to hear what a desktop sounds
;;;; like — or to show that it made a sound at all — is the audio stream it already serves:
;;;; src/audio-stream.lisp hands one subscription per connection, and this is a listener that
;;;; writes what it hears to a file.  It is deliberately a SEPARATE PROCESS from the desktop: a
;;;; recorder that ran inside the session would prove the mixer works, not that the transport
;;;; does.
;;;;
;;;;   sbcl --non-interactive --load inspect/record-session.lisp 5913 12 /tmp/desktop.wav
;;;;
;;;; Arguments (all optional): the audio-stream PORT of the desktop (display N serves 5910+N),
;;;; SECONDS to record, and the output path.  It records wall-clock seconds at the desktop's own
;;;; rate, so a silent desktop gives a silent file of the right length rather than an empty one —
;;;; the difference between "nothing was playing" and "nothing was connected" is in the report,
;;;; not in a missing file.

(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (asdf:load-system :glass/audio-stream)))

(defpackage #:glass-record (:use #:cl)) (in-package #:glass-record)

(defun main (&optional (port 5911) (seconds 10) (path "/tmp/glass-session.wav"))
  (let* ((rate 48000)                      ; ask for the mix at its native rate: no resampling
         (frame 960)                       ; and in the mixer's own 20 ms frames
         (tap (glass:make-audio-tap :port port :rate rate :frame-samples frame :prime 2))
         (period (/ frame (float rate 1d0)))
         (units internal-time-units-per-second)
         (frames '()) (got 0) (silent 0)
         (next (+ (get-internal-real-time) (round (* period units))))
         (deadline (+ (get-internal-real-time) (round (* seconds units)))))
    ;; a recorder keeps its own clock, exactly like any other consumer of the stream: TAP-NEXT-FRAME
    ;; never blocks, and NIL means "send silence and keep your timing", not "wait".
    (loop while (< (get-internal-real-time) deadline)
          do (let ((f (glass:tap-next-frame tap)))
               (if f (incf got) (incf silent))
               (push (or f (reed:make-pcm16 frame)) frames))
             (let ((now (get-internal-real-time)))
               (when (> next now) (sleep (/ (- next now) (float units 1d0))))
               (setf next (max (+ next (round (* period units))) now))))
    (let* ((fs (nreverse frames))
           (total (reduce #'+ fs :key #'length))
           (v (reed:make-pcm16 total))
           (at 0) (peak 0))
      (dolist (f fs) (replace v f :start1 at) (incf at (length f)))
      (dotimes (i total) (setf peak (max peak (abs (aref v i)))))
      (reed:write-wav-file (reed:make-pcm :samples v :sample-rate rate :channels 1
                                          :frame-count total)
                           path)
      (format t "~&~a~%" (glass:tap-report tap))
      (format t "~&recorded ~,2f s to ~a — ~d frames from the desktop, ~d filled with silence, ~
                 peak ~d~@[~%  (nothing arrived: is a desktop serving audio on ~d?)~]~%"
              (/ total (float rate 1d0)) path got silent peak
              (and (zerop got) port))
      (glass:tap-stop tap)
      (zerop got))))

(let* ((args (rest sb-ext:*posix-argv*))
       (port (or (and (first args) (parse-integer (first args) :junk-allowed t)) 5911))
       (secs (or (and (second args) (parse-integer (second args) :junk-allowed t)) 10))
       (path (or (third args) (format nil "/tmp/glass-session-~d.wav" port))))
  (sb-ext:exit :code (if (main port secs path) 1 0)))
