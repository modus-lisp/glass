;;;; zorder-gate.lisp — one stacking order for both kinds of window.
;;;;
;;;; The WM hosts two species: McCLIM windows (mirrors) and native glass surface
;;;; windows (terminals, browser, image viewer).  They used to be composited in two
;;;; passes, mirrors first and surfaces after, which is not a z-order but two of them
;;;; plus a rule — and the rule said a McCLIM window can never be in front, however
;;;; recently you clicked it.  This gate holds a real CLIM frame and a real surface
;;;; overlapping on one screen and asks, of the same overlap pixel, the three
;;;; questions that have to agree: what is DRAWN there, what a CLICK there hits, and
;;;; whether the occlusion guard thinks that pixel is covered.
;;;;
;;;; The load-bearing check is "raise the McCLIM window -> the overlap turns its
;;;; colour": that is the one that fails before the fix.  In-process, no VNC.
;;;;   sbcl --control-stack-size 256 --dynamic-space-size 4096 --non-interactive --load backend/inspect/zorder-gate.lisp
(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:glass :mcclim :mcclim-render :sb-concurrency))
    (asdf:load-asd (merge-pathnames "../mcclim-glass.asd" *load-truename*))
    (asdf:load-system :mcclim-glass)))
(in-package :clim-glass)

(defconstant +mirror-ink+ #x0000ff)   ; the CLIM window paints its pane blue
(defconstant +surface-ink+ #xff0000)  ; the surface window's fb is red

(define-application-frame zframe ()
  ()
  (:panes (canvas :application :display-function 'draw-zframe :scroll-bars nil
                              :width 300 :height 220))
  (:layouts (default canvas)))
(defun draw-zframe (frame pane)
  (declare (ignore frame))
  (draw-rectangle* pane 0 0 300 220 :ink +blue+))

(defun pixel-at (port x y)
  (let ((fb (glass-port-fb port)))
    (logand (aref (glass:fb-pixels fb) (+ (* y (glass:fb-width fb)) x)) #xffffff)))

(defun wait-until (pred &optional (secs 8))
  (let ((end (+ (get-internal-real-time) (* secs internal-time-units-per-second))))
    (loop until (funcall pred) do (sleep 1/50)
          when (> (get-internal-real-time) end) do (return nil)
          finally (return t))))

(let* ((port (make-instance 'glass-port :port 5961)) (fail 0))
  (setf (glass-port-wm-p port) t (glass-port-screen-w port) 800 (glass-port-screen-h port) 600
        (glass-port-fb port) (glass:make-framebuffer 800 600 +wm-teal+))
  (climi::restart-port port)
  (flet ((check (ok fmt &rest args) (format t "  [~:[FAIL~;pass~]] ~?~%" ok fmt args) (unless ok (incf fail))))
    (format t "~&[z-order: McCLIM windows and surface windows in ONE stack]~%")
    ;; --- a real CLIM frame (a mirror) ---
    (let ((frame (make-application-frame 'zframe :frame-manager (find-frame-manager :port port))))
      (sb-thread:make-thread (lambda () (ignore-errors (run-frame-top-level frame))) :name "zframe-app"))
    (check (wait-until (lambda () (find-if #'glass-mirror-managed (glass-port-mirrors port))))
           "the CLIM frame realized a managed mirror")
    (let ((mirror (find-if #'glass-mirror-managed (glass-port-mirrors port))))
      (when mirror
        ;; wait for its ink to actually reach the screen fb before judging pixels
        (check (wait-until (lambda () (progn (composite-all port)
                                            (= (pixel-at port (+ (glass-mirror-x mirror) 40)
                                                             (+ (glass-mirror-y mirror) 40))
                                               +mirror-ink+))))
               "the CLIM window's ink is on the screen")
        ;; --- a native surface window, placed to overlap the mirror ---
        (let* ((surf (add-surface port (lambda (fb)
                                         (glass:fb-fill fb (glass:rgb 255 0 0))
                                         (values nil nil (lambda () nil)))
                                  :title "zsurface" :width 200 :height 150))
               (px (+ (glass-mirror-x mirror) 40))       ; a point inside BOTH windows
               (py (+ (glass-mirror-y mirror) 40))
               (probe (list px py 1 1)))
          (setf (wm-surface-x surf) (+ (glass-mirror-x mirror) 20)
                (wm-surface-y surf) (+ (glass-mirror-y mirror) 20))
          (composite-all port)
          (check (and (= (length (glass-port-surfaces port)) 1) mirror)
                 "one window of each kind on the screen")
          ;; (1) the surface opened last, so it is on top — as it always was
          (check (= (pixel-at port px py) +surface-ink+)
                 "the newer window (the surface) covers the older one")
          (check (eq (wm-hit port px py) surf) "the click there hits the surface")
          (check (wm-obstructed-p port mirror probe)
                 "the occlusion guard: the mirror IS obstructed at that pixel")
          (check (not (wm-obstructed-p port surf probe))
                 "...and the surface, on top, is not")
          ;; (2) THE FIX: raising the McCLIM window puts it in front of the surface.
          ;;     Before the unification this was unreachable at any z.
          (wm-raise port mirror)
          (composite-all port)
          (check (= (pixel-at port px py) +mirror-ink+)
                 "raising the McCLIM window brings it in FRONT of the surface")
          (check (eq (wm-hit port px py) mirror)
                 "the pointer agrees with the pixels: the click now hits the mirror")
          (check (eq (wm-topmost port) mirror) "the McCLIM window is the topmost window")
          (check (wm-obstructed-p port surf probe)
                 "the occlusion guard flipped too: the SURFACE is now obstructed there")
          (check (not (wm-obstructed-p port mirror probe))
                 "...and the mirror, on top, is not (no CopyRect smear of its pixels)")
          ;; (3) a content click on a buried McCLIM window brings it forward.  The old
          ;;     test was `already frontmost among mirrors' — always true, so the click
          ;;     raised nothing and the window stayed under the surface.
          (wm-raise port surf)
          (composite-all port)
          (check (= (pixel-at port px py) +surface-ink+) "surface raised back over the mirror")
          (let ((cx (+ (glass-mirror-x mirror) 5))       ; mirror content, clear of the surface
                (cy (+ (glass-mirror-y mirror) 5)))
            (check (eq (wm-hit port cx cy) mirror) "the exposed corner of the mirror is clickable")
            (wm-on-pointer port 1 cx cy)
            (wm-on-pointer port 0 cx cy)
            (check (= (pixel-at port px py) +mirror-ink+)
                   "a content click on the buried McCLIM window raises it over the surface"))
          ;; (4) Back (wm-lower) sends it behind the other kind of window too
          (wm-lower port mirror)
          (check (= (pixel-at port px py) +surface-ink+) "Back sends the McCLIM window behind the surface")
          ;; (5) the pop-up tier is not in the shared order: an unmanaged mirror (a
          ;;     pull-down, a tooltip) is above everything whatever its z says.
          (let ((popup (make-instance 'glass-mirror)))
            (setf (wm-window-z popup) -9999)             ; as stale as a z can be
            (push popup (glass-port-mirrors port))
            (check (eq (first (wm-stacking-order port)) popup)
                   "an unmanaged mirror sits on top of the order regardless of z")
            (check (not (eq (wm-topmost port) popup))
                   "...but it is not what a click raises: WM-TOPMOST skips the pop-up tier")
            (setf (glass-port-mirrors port) (remove popup (glass-port-mirrors port))))
          ;; (6) and the order is one list, with both species in it
          (let ((order (wm-stacking-order port)))
            (check (and (= (length order) 2) (member mirror order) (member surf order))
                   "WM-STACKING-ORDER is ONE list holding both kinds of window")))))
    (sb-concurrency:send-message (glass-port-mailbox port) (lambda () nil)))
  (format t "~%=> ~:[PASS~;FAIL (~d)~]~%" (plusp fail) fail)
  (finish-output) (sb-ext:exit :code (if (plusp fail) 1 0)))
