;;;; test/wm-sound-test.lisp — glass/wm-sound in process: the compositor's clockless mixer, ticked
;;;; by wall time, posting to the network's half, which serves glass-audio/1 at the rate a listener
;;;; asks for.  Both halves and the listener run on this one thread, as turns.
;;;;   sbcl --script test/wm-sound-test.lisp   (with glass/wm-sound findable by ASDF; exits 0 iff all pass)
;;;; Port 6648 on loopback.

(require :asdf)
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream))) (asdf:load-system :glass/wm-sound)))

(defpackage #:glass-wm-sound-test (:use #:cl))
(in-package #:glass-wm-sound-test)

(defvar *fails* 0)
(defun check (name ok)
  (format t "~&WM-SOUND ~:[FAIL~;ok  ~] ~a~%" ok name)
  (unless ok (incf *fails*)))

(defun now-s () (/ (get-internal-real-time) (float internal-time-units-per-second 1d0)))

(let* ((box (glass::%share (glass::make-rfb-mailbox)))
       (snd (glass.wm::wm-sound-open box))
       (listening '())
       (out (glass.wm::audio-out-open box :port 6648
                                          :notify (lambda (n) (push n listening)
                                                    (glass.wm::wm-sound-listen snd n))))
       ;; a 400 Hz tone, forever, as the app's source
       (tone (glass:audio-tone 400 0.02d0 :amplitude 8000)))
  (glass:mixer-add-source (glass.wm::wm-sound-mixer snd) (lambda () (copy-seq tone)) :name "tone")
  ;; the compositor keeps time alone: 0.2 s of turns mixes about ten frames, and posts none
  ;; whatever came due while we set up, and a first few turns (modus compiles on first call)
  (let ((t0 (now-s))) (loop while (< (- (now-s) t0) 0.3d0) do (glass.wm::wm-sound-turn snd) (sleep 0.005)))
  (let ((t0 (now-s)) (f0 (glass.wm::wm-sound-frames snd)))
    (loop while (< (- (now-s) t0) 0.2d0)
          do (glass.wm::wm-sound-turn snd) (sleep 0.005))
    (let ((n (- (glass.wm::wm-sound-frames snd) f0)))
      (check (format nil "the compositor's mixer ticks by wall time (~d frames in 0.2 s)" n)
             (<= 8 n 12))))
  (check "nobody listening: nothing posted" (zerop (glass.wm::wm-sound-posted snd)))
  ;; a listener, as the WebRTC gateway is one: 8 kHz, 160-sample frames
  (let* ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
         (s (progn (sb-bsd-sockets:socket-connect sock #(127 0 0 1) 6648)
                   (sb-bsd-sockets:socket-make-stream sock :input t :output t
                                                           :element-type '(unsigned-byte 8)
                                                           :buffering :full)))
         (fd (sb-bsd-sockets:socket-file-descriptor sock))
         (got (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
         (t0 (now-s)))
    (loop for ch across (format nil "glass-audio/1 rate=8000 frame=160 name=test~%")
          do (write-byte (char-code ch) s))
    (force-output s)
    (loop while (< (- (now-s) t0) 1.2d0)
          do (glass.wm::wm-sound-turn snd)
             (glass.wm::audio-out-turn out)
             (loop while (or (listen s) (sb-sys:wait-until-fd-usable fd :input 0))
                   do (let ((b (read-byte s nil nil))) (if b (vector-push-extend b got) (return))))
             (sleep 0.005))
    (let* ((nl (position 10 got))
           (header (and nl (map 'string #'code-char (subseq got 0 nl))))
           (body (if nl (subseq got (1+ nl)) #()))
           (samples (floor (length body) 2))
           (peak (loop for i below samples
                       maximize (abs (let ((v (logior (aref body (* 2 i)) (ash (aref body (1+ (* 2 i))) 8))))
                                       (if (>= v 32768) (- v 65536) v))))))
      (check (format nil "header names the rate asked for: ~s" header)
             (equal header "glass-audio/1 rate=8000 channels=1 frame=160 format=s16le"))
      (check "the network told the compositor someone listens" (equal listening '(1)))
      (check (format nil "about a second of audio arrived (~d samples at 8 kHz)" samples)
             (<= 6400 samples 10400))
      (check (format nil "and it is the tone, not silence (peak ~d)" peak) (> peak 4000)))
    (close s)
    ;; the hang-up is noticed on the next turns, and the compositor stops posting
    (let ((t0 (now-s)))
      (loop while (< (- (now-s) t0) 0.2d0)
            do (glass.wm::wm-sound-turn snd) (glass.wm::audio-out-turn out) (sleep 0.005)))
    (check "a listener hanging up is noticed" (eql (first listening) 0))
    (check "and the compositor stops posting" (not (glass.wm::wm-sound-listening snd))))
  (glass.wm::audio-out-close out))

(format t "~&~:[ALL PASS~;~:*~d FAILED~]~%" (and (plusp *fails*) *fails*))
(sb-ext:exit :code (if (zerop *fails*) 0 1))
