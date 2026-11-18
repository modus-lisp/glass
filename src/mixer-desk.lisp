;;;; mixer-desk.lisp -- the Mixer as a desk application, with no McCLIM: what is playing, how loud each thing
;;;; is, who is listening -- and a hand on each: mute, and a gain.
;;;;
;;;; backend/mixer-app.lisp is the same window for the McCLIM desktop, and its header says why the window
;;;; exists: audio that is correct and not VISIBLE is a desktop that can be perfectly healthy and perfectly
;;;; silent with no way to tell which.  This one draws itself (as Speak and Listen do, voice-apps.lisp), so it
;;;; runs where McCLIM does not -- the phone -- and adds what the McCLIM one only showed:
;;;;
;;;;   a source's row: its name, a meter of its last frame (SRC-LEVEL), its gain; tap the box to mute it, - and
;;;;     + to turn it down or up.  Both are THIS MIX's (the speaker's, on the phone): MIX-MUTE and
;;;;     MIX-SOURCE-GAIN, a listener's say over what it hears -- not SRC-GAIN, which is the application's own
;;;;     and turns it down for everyone.
;;;;   the session's level, and the listeners: each sink, its rate, and how it is keeping up (frames, drops,
;;;;     underruns) -- the diagnostic half, as the McCLIM window has it.
;;;;
;;;; It is given its MIXER (a host makes its own: the phone's is vessel's, on the desk's loop), or finds the
;;;; session's.

(in-package #:glass.desk)

(export '(mixer-app))

(defun %mx-mixer (mixer)
  "MIXER, else the desktop's own (*MEDIA-MIXER*, the window manager's, bound where apps open), else
the session's if there is one.  NEVER MAKES one: GLASS:SESSION-MIXER creates a mixer on first ask,
and a Mixer window opened before the window manager adopted its mix as the session's would leave
the session with a second, unserved mixer -- everything that plays into it then silent."
  (or mixer *media-mixer* glass::*session-mixer*))

(defun %mx-sources (m)
  (and m (ignore-errors (copy-list (glass:mixer-sources m)))))

(defun %mx-meter (fb x y w level)
  "A meter W wide at X,Y: LEVEL (0-1, as the mixer measures RMS) on a scale that shows quiet sound."
  (let ((fill (round (* w (min 1d0 (sqrt (* 4d0 (or level 0d0))))))))
    (glass:fb-rect fb x y w 8 +va-paper+)
    (when (plusp fill) (glass:fb-rect fb x y fill 8 (if (> (or level 0) 0.85d0) #xd05050 +va-accent+)))))

(defun mixer-app (fb &optional mixer)
  "The Mixer, for the desk: FB its window's framebuffer, MIXER the bus to show (else the session's).  A
   desk's make-fn takes only FB: register (lambda (fb) (mixer-app fb mixer))."
  (let ((shown nil) (dirty t) (hits '()) (was nil) (next 0)
        (before (make-hash-table)))       ; source id -> its gain when muted here, to come back to
    (labels ((mix () (let ((m (%mx-mixer mixer))) (and m (glass:as-mix m))))
             (status ()
               ;; what is drawn, coarsely: levels rounded to the pixel, so silence does not repaint
               (let ((m (%mx-mixer mixer)))
                 (and m
                      (list (round (* 40 (min 1d0 (sqrt (* 4d0 (glass:mixer-level m))))))
                            (mapcar (lambda (s)
                                      (list (glass:src-id s) (glass:src-name s)
                                            (round (* 40 (min 1d0 (sqrt (* 4d0 (glass:src-level s))))))
                                            (glass:mix-source-gain (mix) s) (glass:mix-hears-p (mix) s)))
                                    (%mx-sources m))
                            (mapcar (lambda (k) (list (glass:sink-frames k) (glass:sink-drops k)
                                                      (glass:sink-underruns k)))
                                    (glass:mixer-sinks m))))))
             (text (x y s &key (size 14) (color +va-ink+) bold)
               (glass:fb-text fb x y s :size size :color color :font (glass:default-font bold)))
             (redraw ()
               (let* ((w (glass:fb-width fb)) (m (%mx-mixer mixer)) (y 12))
                 (setf hits '())
                 (glass:fb-fill fb +va-bg+)
                 (unless m
                   (text 12 y "No mixer" :size 15 :bold t)
                   (text 12 (+ y 24) "This image has no sound output, so nothing plays." :size 12 :color +va-dim+)
                   (return-from redraw))
                 ;; the session: what comes out
                 (text 12 y "Out" :size 15 :bold t)
                 (%mx-meter fb 60 (+ y 6) (- w 180) (glass:mixer-level m))
                 (text (- w 110) (+ y 1) (format nil "~D Hz" (glass:mixer-rate m)) :size 12 :color +va-dim+)
                 (incf y 34)
                 ;; the sources: a row each, a hand on each
                 (text 12 y "Sources" :size 15 :bold t)
                 (incf y 26)
                 (let ((sources (%mx-sources m)))
                   (if (null sources)
                       (progn (text 20 y "Nothing is playing." :size 13 :color +va-dim+)
                              (incf y 22)
                              (text 20 y "A film, a podcast or a voice shows here while it plays." :size 12 :color +va-dim+)
                              (incf y 26))
                       (dolist (s sources)
                         (let* ((hears (glass:mix-hears-p (mix) s))
                                (gain (glass:mix-source-gain (mix) s))
                                (bx 12) (by y))
                           ;; the box: heard (filled) or muted (empty)
                           (glass:fb-rect fb bx by 22 22 +va-button+)
                           (when hears (glass:fb-rect fb (+ bx 5) (+ by 5) 12 12 +va-accent+))
                           (push (list :mute s bx by 22 22) hits)
                           (text 44 (+ by 2) (%va-clip (glass:src-name s) (- w 220) 14)
                                 :color (if hears +va-ink+ +va-dim+))
                           (%mx-meter fb 44 (+ by 24) (- w 200) (if hears (glass:src-level s) 0d0))
                           ;; the gain: - value +
                           (let ((gx (- w 148)))
                             (glass:fb-rect fb gx by 34 30 +va-button+)
                             (text (+ gx 12) (+ by 5) "-" :size 16 :bold t)
                             (push (list :down s gx by 34 30) hits)
                             (text (+ gx 44) (+ by 7) (format nil "~D%" (round (* 100 gain))) :size 13)
                             (glass:fb-rect fb (+ gx 100) by 34 30 +va-button+)
                             (text (+ gx 111) (+ by 5) "+" :size 16 :bold t)
                             (push (list :up s (+ gx 100) by 34 30) hits))
                           (incf y 44)))))
                 ;; who is listening
                 (incf y 8)
                 (text 12 y "Listeners" :size 15 :bold t)
                 (incf y 26)
                 (let ((sinks (glass:mixer-sinks m)))
                   (if (null sinks)
                       (text 20 y "No one: nothing reads the mix." :size 13 :color +va-dim+)
                       (dolist (k sinks)
                         (text 20 y (format nil "~A  ~D Hz" (glass::sink-name k) (glass:sink-rate k)) :size 13)
                         (text 20 (+ y 18)
                               (format nil "~:D frames  ·  ~:D dropped  ·  ~:D short" (glass:sink-frames k)
                                       (glass:sink-drops k) (glass:sink-underruns k))
                               :size 12 :color +va-dim+)
                         (incf y 42))))))
             (press (x y)
               (let ((h (find-if (lambda (h) (destructuring-bind (kind s hx hy hw hh) h
                                                (declare (ignore kind s))
                                                (and (<= hx x (+ hx hw)) (<= hy y (+ hy hh)))))
                                 hits)))
                 (when h
                   (destructuring-bind (kind s &rest box) h
                     (declare (ignore box))
                     (let ((mx (mix)))
                       (ecase kind
                         ;; muting is a gain of 0 (MIX-MUTE): the gain it had comes back with it
                         (:mute (if (glass:mix-hears-p mx s)
                                    (progn (setf (gethash (glass:src-id s) before) (glass:mix-source-gain mx s))
                                           (glass:mix-mute mx s))
                                    (setf (glass:mix-source-gain mx s)
                                          (let ((g (gethash (glass:src-id s) before 1d0))) (if (plusp g) g 1d0)))))
                         ;; in steps of a tenth, between silence and twice as loud
                         (:down (setf (glass:mix-source-gain mx s)
                                      (max 0d0 (/ (round (* 10 (- (glass:mix-source-gain mx s) 0.1d0))) 10d0))))
                         (:up (setf (glass:mix-source-gain mx s)
                                    (min 2d0 (/ (round (* 10 (+ (glass:mix-source-gain mx s) 0.1d0))) 10d0)))))))
                   (setf dirty t)))))
      (values nil
              (lambda (mask x y)
                ;; a press, not every move of a drag over a button
                (let ((now (logbitp 0 mask)))
                  (when (and now (not was)) (press x y))
                  (setf was now)))
              (lambda ()
                ;; looked at ten times a second: a meter that moves at the frame rate is a repaint per frame
                (let ((t0 (get-internal-real-time)))
                  (when (>= t0 next)
                    (setf next (+ t0 (floor internal-time-units-per-second 10)))
                    (let ((now (status)))
                      (unless (equal now shown) (setf shown now dirty t)))))
                (when dirty (setf dirty nil) (redraw) t))
              nil nil
              (lambda (w h) (glass:fb-resize fb w h +va-bg+) (setf dirty t))
              nil))))
