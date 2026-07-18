;;;; serve-desktop.lisp — a PERSISTENT OPEN LOOK glass desktop over VNC (blocks).
;;;; SVG wallpaper + the full Apps menu (Calculator, Browser, Inspector, Debugger,
;;;; Image Viewer, Listener, ...).  Point any VNC client at <host>:5901.
;;;;   sbcl --control-stack-size 256 --dynamic-space-size 4096 --load backend/inspect/serve-desktop.lisp
(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:glass :mcclim :mcclim-render :sb-concurrency
                    :weft/render :clim-examples :clim-listener))
    (ignore-errors (asdf:load-system :loom/glass))            ; the browser (optional)
    (asdf:load-asd "/home/claude/glass/backend/mcclim-glass.asd")
    (asdf:load-system :mcclim-glass)))

(setf glass:*desktop-name* "modus-lisp :: glass desktop")
(let ((wp (namestring (merge-pathnames "assets/wallpaper.svg"
                                       (asdf:system-source-directory :mcclim-glass)))))
  (format *error-output* "~&@@ glass desktop serving on 0.0.0.0:5901 (~a)~%" wp)
  (finish-output *error-output*)
  (clim-glass:run-wm '((:terminal :cols 80 :rows 24 :ppem 14))
                     :port 5901 :width 1280 :height 800
                     :background wp :background-mode :cover))
