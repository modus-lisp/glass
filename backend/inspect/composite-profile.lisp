;;;; composite-profile.lisp — interactive fps is tiny-data (1-5 KB/frame) yet only
;;;; 10-18 fps, so the cost is NOT encoding.  Time the SERVER-side per-frame work
;;;; directly, in-process (no socket): compositing (wm-composite: blit wallpaper +
;;;; all windows) for a drag-sized damage box vs the whole screen, and the sender's
;;;; diff+encode over the damage.  Whatever dominates here is the jank.
;;;;   sbcl --control-stack-size 256 --dynamic-space-size 4096 --non-interactive --load backend/inspect/composite-profile.lisp
(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:glass :glass/term :weft/render :sb-concurrency))
    (asdf:load-asd "/home/claude/glass/backend/mcclim-glass.asd")
    (asdf:load-system :mcclim-glass)))
(in-package :clim-glass)

(defun ms (ticks n) (/ (* 1000.0 ticks) n internal-time-units-per-second))
(defmacro timed (n &body body)
  `(let ((a (get-internal-real-time))) (dotimes (,(gensym) ,n) ,@body) (ms (- (get-internal-real-time) a) ,n)))

(let* ((port (make-instance 'glass-port :port 5959))
       (wp (namestring (merge-pathnames "assets/wallpaper.svg" (asdf:system-source-directory :mcclim-glass)))))
  (setf (glass-port-wm-p port) t (glass-port-screen-w port) 1280 (glass-port-screen-h port) 800
        (glass-port-fb port) (glass:make-framebuffer 1280 800 +wm-teal+))
  (wm-set-background port wp :mode :cover)
  ;; a terminal window, like the live desktop
  (let* ((tm (glass-term:make-terminal :cols 90 :rows 26 :ppem 14)))
    (glass-term:start-pump tm)
    (wm-add-surface* port (make-wm-surface :fb (glass-term:terminal-fb tm) :x 60 :y 80 :title "terminal"
                                           :dirty-p (lambda () (glass-term:terminal-take-dirty tm)))))
  (sleep 1.0)
  (let* ((surf (first (glass-port-surfaces port)))
         (box (wm-window-box surf))                       ; a window-sized damage box (a drag step)
         (union (wm-box-union (list box (list (+ (first box) 60) (second box) (third box) (fourth box))))))
    (format t "~&[composite-profile] 1280x800, SVG wallpaper + 90x26 terminal~%")
    (format t "  window box ~a, drag-union ~a~%" box union)
    ;; 1. compositing only (what wm-on-pointer does per drag step)
    (format t "  composite-all FULL screen:      ~,2f ms~%" (timed 60 (composite-all port)))
    (format t "  composite-all drag-union box:   ~,2f ms~%" (timed 200 (composite-all port union)))
    (format t "  composite-all window box:       ~,2f ms~%" (timed 200 (composite-all port box)))
    ;; 2. break composite into wallpaper-blit vs window-draw, over the damage clip
    (let ((fb (glass-port-fb port)))
      (destructuring-bind (dx dy dw dh) union
        (format t "  ├ wallpaper blit-fb (clipped): ~,2f ms~%"
                (timed 200 (glass:with-fb-clip (fb dx dy dw dh) (blit-fb (glass-port-bg port) 0 0 fb))))
        (format t "  └ full-screen wallpaper blit:  ~,2f ms~%"
                (timed 200 (blit-fb (glass-port-bg port) 0 0 fb)))))))
(finish-output) (sb-ext:exit)
