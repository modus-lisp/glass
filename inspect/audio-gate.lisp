;;;; inspect/audio-gate.lisp — the session mixer, and the claims it exists to make good on.
;;;;
;;;; The mixer is easy to write in a way that works with one listener and is wrong with two, and
;;;; that is precisely the case it exists for.  So the load-bearing checks here are the ones a
;;;; single-consumer test cannot make:
;;;;
;;;;   * two sinks reading the same mix get SAMPLE-IDENTICAL audio.  A mixer whose clock is
;;;;     driven by a consumer's pull passes every other test in this file and fails this one,
;;;;     because the second consumer only ever sees the frames the first one did not take.
;;;;   * a sink that stops reading does not stop the mix, and rejoins at the present rather than
;;;;     replaying a backlog.
;;;;   * sinks at different rates hear the same sound — the 8 kHz one just hears less of it.
;;;;
;;;; Frequencies are measured, not eyeballed: a tone that survives resampling at the wrong level,
;;;; or one whose alias folds back into the band, is inaudible in a length check and obvious in a
;;;; Goertzel.
;;;;
;;;;   sbcl --dynamic-space-size 3072 --non-interactive --load inspect/audio-gate.lisp

(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (asdf:load-system :glass/audio)))

(defpackage #:glass-audio-gate (:use #:cl)) (in-package #:glass-audio-gate)

(defvar *pass* 0) (defvar *fail* 0)
(defun check (name got want)
  (if (equal got want) (progn (incf *pass*) (format t "  ok   ~a = ~s~%" name got))
      (progn (incf *fail*) (format t "  FAIL ~a: got ~s, want ~s~%" name got want)))
  (finish-output))
(defun check-that (name ok &optional detail)
  (if ok (progn (incf *pass*) (format t "  ok   ~a~@[ — ~a~]~%" name detail))
      (progn (incf *fail*) (format t "  FAIL ~a~@[ — ~a~]~%" name detail)))
  (finish-output))

(defun goertzel (pcm hz rate)
  (let* ((n (length pcm)) (k (round (/ (* n hz) rate))) (w (/ (* 2d0 pi k) n))
         (coeff (* 2d0 (cos w))) (s0 0d0) (s1 0d0) (s2 0d0))
    (dotimes (i n) (setf s0 (+ (aref pcm i) (* coeff s1) (- s2)) s2 s1 s1 s0))
    (/ (sqrt (max 0d0 (+ (* s1 s1) (* s2 s2) (- (* s1 s2 coeff))))) n)))

(defun tone-source (hz rate frame &key (amp 8000))
  "An endless sine, phase-continuous across frames — a source that never ends, like a device."
  (let ((i 0))
    (lambda ()
      (let ((v (reed:make-pcm16 frame)))
        (dotimes (j frame v)
          (setf (aref v j) (round (* amp (sin (/ (* 2 pi hz (+ i j)) rate)))))
          (when (= j (1- frame)) (incf i frame)))))))

(defun collect (sink n)
  "N frames from SINK, driving the mix by hand (no clock thread) so the test is deterministic."
  (let ((out '()))
    (loop while (< (length out) n)
          do (glass:mixer-tick (glass::sink-mixer sink))
             (let ((f (glass:sink-next-frame sink))) (when f (push f out))))
    (let* ((frames (nreverse out))
           (total (reduce #'+ frames :key #'length))
           (v (reed:make-pcm16 total)) (at 0))
      (dolist (f frames v) (replace v f :start1 at) (incf at (length f))))))

;;; ---- 1. the mix advances on its own ---------------------------------------
(format t "~&=== the mix has a clock, not a caller ===~%")
(let ((m (glass:make-mixer)))
  (check "48 kHz, 20 ms frames" (list (glass:mixer-rate m) (glass:mixer-frame-samples m))
         '(48000 960))
  (dotimes (i 3) (glass:mixer-tick m))
  (check "silence still advances the clock" (glass:mixer-seq m) 3)
  (check-that "and silence is silent" (zerop (glass:mixer-level m))))

(let ((m (glass:make-mixer)))
  (glass:mixer-add-source m (tone-source 1000 48000 960) :name "tone")
  (glass:mixer-start m)
  (sleep 0.5)
  (glass:mixer-stop m)
  (let ((seq (glass:mixer-seq m)))
    (check-that "the clock thread runs at ~50 fps" (<= 20 seq 30)
                (format nil "~d frames in 0.5 s, ~d late" seq (glass:mixer-late m)))
    (check-that "and it was actually mixing" (> (glass:mixer-level m) 0.5))))

;;; ---- 2. two listeners, one mix --------------------------------------------
(format t "~&~%=== two listeners hear the same thing ===~%")
(let* ((m (glass:make-mixer))
       (a (glass:mixer-subscribe m :name "a"))
       (b (glass:mixer-subscribe m :name "b")))
  (glass:mixer-add-source m (tone-source 440 48000 960) :name "tone")
  ;; drive both from the same ticks, alternating who asks first
  (let ((fa '()) (fb '()))
    (dotimes (i 12)
      (glass:mixer-tick m)
      (if (evenp i)
          (progn (push (glass:sink-next-frame a) fa) (push (glass:sink-next-frame b) fb))
          (progn (push (glass:sink-next-frame b) fb) (push (glass:sink-next-frame a) fa))))
    (let ((va (remove nil (nreverse fa))) (vb (remove nil (nreverse fb))))
      (check-that "both got frames" (and (plusp (length va)) (= (length va) (length vb)))
                  (format nil "~d frames each" (length va)))
      (check-that "SAMPLE-IDENTICAL — neither consumed the other's audio"
                  (every (lambda (x y) (equalp x y)) va vb))
      (check-that "no drops on either side"
                  (and (zerop (glass:sink-drops a)) (zerop (glass:sink-drops b)))))))

(let* ((m (glass:make-mixer))
       (fast (glass:mixer-subscribe m :name "fast"))
       (slow (glass:mixer-subscribe m :name "slow")))
  (glass:mixer-add-source m (tone-source 440 48000 960) :name "tone")
  (dotimes (i 40) (glass:mixer-tick m) (glass:sink-next-frame fast))
  (check-that "a sink that never read does not hold the mixer back" (= 40 (glass:mixer-seq m)))
  (let ((f (glass:sink-next-frame slow)))
    (check-that "and when it comes back it gets audio, not an error" (and f (= 960 (length f))))
    (check-that "having lost what it slept through rather than accumulating it"
                (< 0 (glass:sink-drops slow) 40)
                (format nil "~d frames dropped" (glass:sink-drops slow)))))

;;; ---- 3. sources sum, with gain --------------------------------------------
(format t "~&~%=== sources sum, and gain is per source ===~%")
(let* ((m (glass:make-mixer))
       (s (glass:mixer-subscribe m :name "s")))
  (glass:mixer-add-source m (tone-source 1000 48000 960 :amp 6000) :name "low")
  (glass:mixer-add-source m (tone-source 4000 48000 960 :amp 6000) :name "high")
  (let* ((v (collect s 8))
         (a (goertzel v 1000 48000))
         (b (goertzel v 4000 48000))
         (q (goertzel v 2500 48000)))
    (check-that "both tones are present at the same level" (< 0.8 (/ a b) 1.25)
                (format nil "1 kHz ~,0f vs 4 kHz ~,0f" a b))
    (check-that "and nothing else is" (> (/ (min a b) (max q 1d-9)) 50d0)
                (format nil "2.5 kHz ~,1f" q))))

(let* ((m (glass:make-mixer))
       (s (glass:mixer-subscribe m :name "s"))
       (loud (glass:mixer-add-source m (tone-source 1000 48000 960 :amp 6000) :name "loud")))
  (glass:mixer-add-source m (tone-source 4000 48000 960 :amp 6000) :name "other" :gain 0.25d0)
  (let* ((v (collect s 8))
         (a (goertzel v 1000 48000))
         (b (goertzel v 4000 48000)))
    (check-that "a source at 0.25 gain is a quarter as loud" (< 3.5 (/ a b) 4.5)
                (format nil "ratio ~,2f" (/ a b))))
  (setf (glass:src-gain loud) 0d0)
  ;; measured on the TAIL: the sink's cushion still holds a couple of frames mixed before the
  ;; mute, and they are supposed to still be there — that is what a cushion is.
  (let* ((all (collect s 12))
         (v (subseq all (- (length all) 3840))))
    (check-that "and gain is live — muting one source leaves the other"
                (and (< (goertzel v 1000 48000) 5d0) (> (goertzel v 4000 48000) 500d0))
                (format nil "1 kHz ~,2f, 4 kHz ~,0f" (goertzel v 1000 48000) (goertzel v 4000 48000)))))

(let* ((m (glass:make-mixer))
       (s (glass:mixer-subscribe m :name "s"))
       (up (tone-source 1000 48000 960 :amp 20000)))
  (glass:mixer-add-source m up :name "a")
  (glass:mixer-add-source m (let ((inner (tone-source 1000 48000 960 :amp 20000)))
                              (lambda () (reed:apply-gain (funcall inner) -1.0d0)))
                          :name "anti")
  (let ((v (collect s 4)))
    (check-that "two sources that cancel come out silent, not clipped twice"
                (< (reduce #'max v :key #'abs) 4)
                (format nil "peak ~d" (reduce #'max v :key #'abs)))))

;;; ---- 4. a source that ends -------------------------------------------------
(format t "~&~%=== a source that ends ===~%")
(let ((m (glass:make-mixer)))
  (glass:mixer-add-source m (reed:make-buffer-source (glass:audio-tone 880 0.06) :frame-samples 960)
                          :name "clip" :finite t)
  (dotimes (i 10) (glass:mixer-tick m))
  (check "a finite source removes itself when it runs out" (length (glass:mixer-sources m)) 0))
(let ((m (glass:make-mixer)))
  (glass:mixer-add-source m (lambda () nil) :name "quiet-device")
  (dotimes (i 10) (glass:mixer-tick m))
  (check-that "but a device that merely has nothing right now stays registered"
              (= 1 (length (glass:mixer-sources m)))))
(let ((m (glass:make-mixer)))
  (glass:mixer-add-source m (lambda () (error "no")) :name "broken")
  (dotimes (i 10) (glass:mixer-tick m))
  (check-that "a throwing source is dropped after three strikes, and does not kill the mix"
              (and (zerop (length (glass:mixer-sources m))) (= 10 (glass:mixer-seq m)))))
(let ((m (glass:make-mixer)))
  (glass:mixer-play m (glass:audio-tone 880 0.04) :name "bell")
  (check "a one-shot registers" (length (glass:mixer-sources m)) 1)
  (dotimes (i 8) (glass:mixer-tick m))
  (check-that "and clears itself" (zerop (length (glass:mixer-sources m)))))

;;; ---- 5. every listener at its own rate ------------------------------------
(format t "~&~%=== every listener at its own rate ===~%")
(let* ((m (glass:make-mixer))
       (wide (glass:mixer-subscribe m :name "vnc"))
       (phone (glass:mixer-subscribe m :name "webrtc" :rate 8000 :frame-samples 160)))
  (glass:mixer-add-source m (tone-source 1000 48000 960 :amp 8000) :name "tone")
  (let ((vw '()) (vp '()))
    (dotimes (i 30)
      (glass:mixer-tick m)
      (let ((a (glass:sink-next-frame wide)) (b (glass:sink-next-frame phone)))
        (when a (push a vw)) (when b (push b vp))))
    (let ((w (apply #'concatenate '(simple-array (signed-byte 16) (*)) (nreverse vw)))
          (p (apply #'concatenate '(simple-array (signed-byte 16) (*)) (nreverse vp))))
      (check-that "the 8 kHz sink gets 160-sample frames" (= 160 (length (first vp))))
      (check-that "and 1/6th as many samples for the same wall time"
                  (< 0.9 (/ (* 6d0 (length p)) (length w)) 1.1)
                  (format nil "~d @48k vs ~d @8k" (length w) (length p)))
      (check-that "the tone survives the conversion at the same level"
                  (< 0.85 (/ (goertzel p 1000 8000) (goertzel w 1000 48000)) 1.15)
                  (format nil "8k ~,0f vs 48k ~,0f"
                          (goertzel p 1000 8000) (goertzel w 1000 48000))))))

(let* ((m (glass:make-mixer))
       (phone (glass:mixer-subscribe m :name "webrtc" :rate 8000 :frame-samples 160)))
  ;; 6 kHz is above the 8 kHz sink's Nyquist; interpolation without a filter folds it to 2 kHz,
  ;; which is squarely audible and is content the desktop never made.
  (glass:mixer-add-source m (tone-source 6000 48000 960 :amp 8000) :name "cymbal")
  (let* ((v (collect phone 25))
         (alias (goertzel v 2000 8000))
         (ref (goertzel (collect (let ((m2 (glass:make-mixer)))
                                   (glass:mixer-add-source m2 (tone-source 2000 48000 960 :amp 8000))
                                   (glass:mixer-subscribe m2 :rate 8000 :frame-samples 160))
                                 25)
                        2000 8000)))
    (check-that "content above the sink's Nyquist does not fold back into the band"
                (> (/ ref (max alias 1d-9)) 100d0)
                (format nil "alias ~,2f vs a real 2 kHz ~,0f (~,0f dB down)"
                        alias ref (* 20 (log (/ ref (max alias 1d-9)) 10))))))

;;; ---- 6. the sink is a source ----------------------------------------------
(format t "~&~%=== the sink is a source ===~%")
(let* ((m (glass:make-mixer))
       (s (glass:mixer-subscribe m :name "webrtc" :rate 8000 :frame-samples 160))
       (thunk (glass:sink-source s)))
  (glass:mixer-add-source m (tone-source 440 48000 960) :name "tone")
  (dotimes (i 6) (glass:mixer-tick m))
  (let ((f (funcall thunk)))
    (check-that "reed's contract, unchanged: a thunk giving the next frame"
                (and f (typep f '(simple-array (signed-byte 16) (*))) (= 160 (length f)))))
  (check-that "and NIL when the mix has not got there yet — not an error"
              (let ((n 0)) (loop while (funcall thunk) do (incf n)) (< n 20))))

;;; ---- 7. a lazy consumer does not accumulate latency -----------------------
(format t "~&~%=== latency does not accumulate ===~%")
(let* ((m (glass:make-mixer))
       (s (glass:mixer-subscribe m :name "lazy" :rate 8000 :frame-samples 160)))
  (glass:mixer-add-source m (tone-source 440 48000 960) :name "tone")
  ;; ten mixer frames per one the consumer takes: without a trim it would fall a second behind
  ;; per second, forever, and still "work".
  (dotimes (i 20)
    (dotimes (j 10) (glass:mixer-tick m))
    (glass:sink-next-frame s))
  (check-that "the sink's backlog stays bounded"
              (<= (glass::sink-fill s) (* 5 160))
              (format nil "~d samples pending, ~d frames dropped"
                      (glass::sink-fill s) (glass:sink-drops s)))
  (check-that "and it says so rather than hiding it" (plusp (glass:sink-drops s))))

(format t "~&~%~a~%" (glass:mixer-report (let ((m (glass:make-mixer)))
                                           (glass:mixer-add-source m (lambda () nil) :name "demo")
                                           (glass:mixer-subscribe m :name "peer" :rate 8000)
                                           (glass:mixer-tick m)
                                           m)))

(format t "~&~%~d passed, ~d failed~%" *pass* *fail*)
(finish-output)
(sb-ext:exit :code (if (plusp *fail*) 1 0))
