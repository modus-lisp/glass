;;;; mixer-app.lisp — what the session is playing, who is being heard, and what is silent.
;;;;
;;;; src/audio.lisp gives the image a mixer: sources go in, one composite comes out, and every
;;;; listener reads it through their own sink.  All of that is correct and none of it is
;;;; VISIBLE, which is how a desktop can be perfectly healthy and perfectly silent with no way
;;;; to tell which.
;;;;
;;;; The WebRTC client has had two RMS meters for a while — mic and box — and they answer the
;;;; question that actually gets asked, which is "is anything reaching this at all".  This is
;;;; that generalised to the machine the mixer is on: every source by name, its level, its
;;;; gain, and whether the session can hear it.
;;;;
;;;; IT IS DIAGNOSTIC BEFORE IT IS A CONTROL.  The reason it exists is a session where Listen
;;;; heard nothing and there was no way to see whether the problem was a missing microphone, a
;;;; muted one, a mis-set gain, or a recognizer that never started.  A mixer with one row in it
;;;; called "speech" answers that in a glance: there is no microphone, and that is not something
;;;; the ear can fix.
;;;;
;;;; A McCLIM frame for the same reason SPEAK-BOX is one: it is a small table with buttons, and
;;;; the parts of that which are tedious — a scroller, hit-testing rows, a font — already exist.

(defpackage #:glass-mixer
  (:use #:cl)
  (:export #:mixer-box #:run #:register))

(in-package #:glass-mixer)

(defun ui-font (&optional (size 13)) (clim:make-text-style :sans-serif :roman size))
(defun ui-bold (&optional (size 13)) (clim:make-text-style :sans-serif :bold size))

;;; ---- what we can see of the mixer ------------------------------------------
;;; BY NAME, all of it.  This file is compiled into the backend, which does not depend on
;;; :glass/audio -- a desktop built without the mixer should still build this window, and the
;;; window should then say so rather than failing to exist.

(defun %fn (name &optional (pkg "GLASS"))
  (let ((s (and (find-package pkg) (find-symbol name pkg))))
    (and s (fboundp s) s)))

(defun %mixer ()
  "The session mixer, or NIL when this image has no audio at all."
  (let ((f (%fn "SESSION-MIXER")))
    (and f (ignore-errors (funcall f :start nil)))))

(defun %rows ()
  "One row per source: (NAME LEVEL GAIN HEARD-P SOURCE).

   LEVEL is the mixer's own per-source meter where there is one and NIL otherwise; a row with
   no level is not a broken row, it is a source that has never been asked how loud it is."
  (let ((m (%mixer)))
    (when m
      (let* ((mix   (funcall (or (%fn "MIXER-DEFAULT-MIX") (%fn "AS-MIX")) m))
             (srcs  (ignore-errors (funcall (%fn "MIXER-SOURCES") m)))
             (gain  (%fn "MIX-SOURCE-GAIN"))
             (hears (%fn "MIX-HEARS-P"))
             (name  (%fn "SRC-NAME")))
        (loop for s in srcs
              collect (list (or (and name (ignore-errors (funcall name s))) "?")
                            nil
                            (and gain (ignore-errors (funcall gain mix s)))
                            (if hears (ignore-errors (funcall hears mix s)) t)
                            s))))))

(defun %session-level ()
  (let ((m (%mixer)) (f (%fn "MIXER-LEVEL")))
    (and m f (ignore-errors (funcall f m)))))

(defun %sinks ()
  "Who is listening: a sink per viewer, per transport, per ear."
  (let ((m (%mixer)) (f (%fn "MIXER-SINKS")))
    (and m f (ignore-errors (funcall f m)))))

;;; ---- the frame -------------------------------------------------------------

(clim:define-application-frame mixer-box ()
  ((shown :initform nil :accessor app-shown)
   (ticker :initform nil :accessor app-ticker))
  (:menu-bar nil)
  (:panes
   (board :application
          :display-function 'draw-board
          :scroll-bars :vertical
          :text-style (ui-font)
          :background clim:+white+))
  (:layouts (default (clim:vertically () board))))

(defun %meter (stream level &key (width 90))
  "A level as a bar, because a number that changes ten times a second is not readable and a
   bar is.  Drawn even at zero: an empty meter beside a name is the difference between a
   source that is silent and a source that is not there, and telling those apart is the whole
   job of this window."
  (let* ((l (max 0.0 (min 1.0 (float (or level 0) 1.0))))
         (w (round (* width l))))
    (multiple-value-bind (x y) (clim:stream-cursor-position stream)
      (clim:draw-rectangle* stream x (+ y 3) (+ x width) (+ y 12)
                            :filled t :ink (clim:make-gray-color 0.92))
      (when (plusp w)
        (clim:draw-rectangle* stream x (+ y 3) (+ x w) (+ y 12)
                              :filled t :ink (if (> l 0.85) clim:+dark-red+
                                                 (clim:make-rgb-color 0.35 0.65 0.45))))
      (clim:draw-rectangle* stream x (+ y 3) (+ x width) (+ y 12)
                            :filled nil :ink (clim:make-gray-color 0.6))
      (setf (clim:stream-cursor-position stream) (values (+ x width 10) y)))))

(defun draw-board (frame stream)
  (declare (ignore frame))
  (let ((m (%mixer)))
    (cond
      ((null m)
       (clim:with-text-style (stream (ui-bold 14))
         (format stream "~&No mixer in this image.~%"))
       (format stream "~&This desktop was built without :glass/audio, so there is nothing~%~
                         playing and nothing to show.~%"))
      (t
       (clim:with-text-style (stream (ui-bold 14))
         (format stream "~&Session~%"))
       (format stream "~&  out  ")
       (%meter stream (%session-level))
       (format stream "~a listener~:p~%~%" (length (%sinks)))

       (clim:with-text-style (stream (ui-bold 14))
         (format stream "~&Sources~%"))
       (let ((rows (%rows)))
         (if (null rows)
             ;; THE CASE THIS WINDOW WAS WRITTEN FOR.  Nothing is playing and nothing is being
             ;; heard, which is a state the desktop otherwise reports as healthy silence.
             (progn
               (format stream "~&  nothing is playing into the session.~%")
               (format stream "~&  A voice or a podcast appears here while it plays.  A~%~
                                 microphone appears when one is connected — and there is no~%~
                                 microphone on a local viewer yet, which is why Listen can~%~
                                 hear the desktop but not you.~%"))
             (dolist (r rows)
               (destructuring-bind (name level gain heard s) r
                 (declare (ignore s))
                 (format stream "~&  ")
                 (%meter stream level :width 70)
                 (clim:with-text-style (stream (if heard (ui-font) (ui-font 12)))
                   (format stream "~:[(muted) ~;~]~a" heard name))
                 (when gain (format stream "   gain ~,2f" gain))
                 (format stream "~%")))))))))

(defun %status (frame)
  (declare (ignore frame))
  (let ((rows (%rows)))
    (list (length rows) (length (%sinks))
          ;; the level, coarsely: a meter that redraws on every flicker of a float is the
          ;; idle repaint SPEAK-BOX's ticker exists to avoid
          (round (* 20 (or (%session-level) 0))))))

(define-mixer-box-command (com-tick :name nil) ()
  (let ((frame clim:*application-frame*))
    (setf (app-shown frame) (%status frame))
    (clim:redisplay-frame-pane frame (clim:find-pane-named frame 'board) :force-p t)))

(defun %start-ticker (frame)
  "Redraw when the mixer would say something different — the same rule, and for the same
   reason, as SPEAK-BOX's ticker: an idle repaint here is a recomposite and bytes on every
   connection, forever, for a window that has not changed."
  (sb-thread:make-thread
   (lambda ()
     (sleep 0.4)
     (loop
       (sleep 0.25)
       (unless (member (clim:frame-state frame) '(:enabled :shrunk)) (return))
       (handler-case
           (unless (equal (%status frame) (app-shown frame))
             (clim:execute-frame-command frame '(com-tick)))
         (error () (return)))))
   :name "glass-mixer-ticker"))

(defmethod clim:run-frame-top-level :before ((frame mixer-box) &key)
  (setf (app-ticker frame) (%start-ticker frame)))

(defun run (&key (width 460) (height 340))
  (let ((frame (clim:make-application-frame 'mixer-box :width width :height height)))
    (clim:run-frame-top-level frame)))

(defun register (&key (label "Mixer") (width 460) (height 340))
  "Put the mixer in the glass desktop's root menu.  Found by name so that loading this system
in an image without the glass backend is not an error."
  (let ((fn (and (find-package "CLIM-GLASS") (find-symbol "REGISTER-APP" "CLIM-GLASS"))))
    (when (and fn (fboundp fn))
      (funcall fn label (list 'mixer-box :width width :height height :title label))
      label)))
