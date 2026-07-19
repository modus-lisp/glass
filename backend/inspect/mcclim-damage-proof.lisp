;;;; mcclim-damage-proof.lisp — a McCLIM app's incremental repaint should now
;;;; recomposite only its DIRTY region, not the whole 1280x800 (the gadget-test
;;;; slowness).  Run a frame in WM mode, draw a small rectangle from its display
;;;; loop, and assert the resulting fb-damage box is small — the mirror's dirty
;;;; region, not :full.  In-process, host-load-immune.
;;;;   sbcl --control-stack-size 256 --dynamic-space-size 4096 --non-interactive --load backend/inspect/mcclim-damage-proof.lisp
(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:glass :mcclim :mcclim-render :sb-concurrency))
    (asdf:load-asd "/home/claude/glass/backend/mcclim-glass.asd")
    (asdf:load-system :mcclim-glass)))
(in-package :clim-glass)

(defvar *spot* nil)   ; (x y w h) the app wants to paint, or nil
(define-application-frame dmg-frame ()
  ()
  (:panes (canvas :application :display-function 'draw-dmg :scroll-bars nil :width 300 :height 220))
  (:layouts (default canvas)))
(defun draw-dmg (frame pane)
  (declare (ignore frame))
  (when *spot* (destructuring-bind (x y w h) *spot* (draw-rectangle* pane x y (+ x w) (+ y h) :ink +red+))))

(defun wait-until (pred &optional (secs 8))
  (let ((end (+ (get-internal-real-time) (* secs internal-time-units-per-second))))
    (loop until (funcall pred) do (sleep 1/50) when (> (get-internal-real-time) end) do (return nil) finally (return t))))

(let* ((port (make-instance 'glass-port :port 5960)) (fail 0))
  (setf (glass-port-wm-p port) t (glass-port-screen-w port) 1280 (glass-port-screen-h port) 800
        (glass-port-fb port) (glass:make-framebuffer 1280 800 +wm-teal+))
  (climi::restart-port port)
  (flet ((check (ok fmt &rest args) (format t "  [~:[FAIL~;pass~]] ~?~%" ok fmt args) (unless ok (incf fail))))
    (let* ((fm (find-frame-manager :port port))
           (frame (make-application-frame 'dmg-frame :frame-manager fm)))
      (sb-thread:make-thread (lambda () (ignore-errors (run-frame-top-level frame))) :name "dmg-app")
      (format t "~&[mcclim-damage: repaint touches only the dirty region]~%")
      (check (wait-until (lambda () (glass-port-mirrors port))) "frame realized a mirror")
      (sleep 0.5)
      (let ((mirror (find-if #'glass-mirror-managed (glass-port-mirrors port))))
        ;; (a) a real full-pane redisplay -> a BOUNDED pending box (the window), not
        ;; 1280x800.  In WM mode present-mirror ACCUMULATES to pending (coalesced) —
        ;; the tick loop composites it — so read port-take-pending, not fb-damage.
        (port-take-pending port)                           ; clear any startup accumulation
        (clim:redisplay-frame-pane frame (clim:find-pane-named frame 'canvas) :force-p t)
        (%mirror-force-output port mirror)
        (let ((d (port-take-pending port)))                ; (x y w h), accumulated not yet composited
          (check (consp d) "real repaint -> bounded pending damage box, not :full (~a)" d)
          (when (consp d)
            (destructuring-bind (x y w h) d (declare (ignore x y))
              (check (and (< w 1280) (< h 800))
                     "damage bounded to the window (~dx~d), not the whole screen" w h))))
        ;; (b) mirror-damage-box itself: a small dirty region -> a small screen box
        (setf (mcclim-render::image-dirty-region mirror) (clim:make-rectangle* 10 12 34 40))
        (let ((box (mirror-damage-box mirror)))
          (check (and (consp box) (= (third box) 24) (= (fourth box) 28))
                 "small dirty region -> small box ~a (expect 24x28 at mirror origin)" box)
          (check (clim:region-equal (mcclim-render::image-dirty-region mirror) clim:+nowhere+)
                 "dirty region is consumed (reset to +nowhere+) after reading"))
        ;; (c) COALESCING: a burst of McCLIM repaints accumulates -> ONE composite
        (port-take-pending port)
        (glass:perf-reset)
        (dotimes (i 15)
          (setf (mcclim-render::image-dirty-region mirror) (clim:make-rectangle* 5 5 40 40))
          (%mirror-force-output port mirror))
        (check (= (glass::pf-composites glass::*perf*) 0)
               "15 McCLIM repaints -> 0 eager composites (all accumulated), composites=~d"
               (glass::pf-composites glass::*perf*))
        (check (consp (glass-port-pending port)) "the burst is one coalesced pending box: ~a"
               (glass-port-pending port))
        (let ((pend (port-take-pending port))) (composite-all port pend))
        (check (= (glass::pf-composites glass::*perf*) 1)
               "the tick composites the whole burst ONCE, composites=~d"
               (glass::pf-composites glass::*perf*))))
    (sb-concurrency:send-message (glass-port-mailbox port) (lambda () nil)))
  (format t "~%=> ~:[PASS~;FAIL (~d)~]~%" (plusp fail) fail)
  (finish-output) (sb-ext:exit :code (if (plusp fail) 1 0)))
