;;;; demo/wm-run.lisp — the window manager (glass/wm): the desk's apps, a Lisp listener and a
;;;; Terminal, in a native window where this Lisp has one (glass:native-display-p), else served
;;;; over VNC.  Run from demo/wm-world.lisp's core on modus.
;;;;   env WM_DISPLAY   native or rfb (default: native when there is one)
;;;;       WM_SIZE      WxH (default 1024x700)
;;;;       WM_PORT      the RFB port, for rfb (default 5910; 127.0.0.1 only)
(unless (find-package "GLASS.WM")
  (error "wm-run: glass/wm is not loaded -- run from demo/wm-world.lisp's core"))
(defun cl-user::wm-env (name default)
  (let ((v (uiop:getenv name))) (if (and v (plusp (length v))) v default)))
(let* ((size (cl-user::wm-env "WM_SIZE" "1024x700"))
       (x (position #\x size))
       (width (parse-integer size :end x))
       (height (parse-integer size :start (1+ x)))
       (display (if (string-equal (cl-user::wm-env "WM_DISPLAY"
                                                   (if (glass:native-display-p) "native" "rfb"))
                                  "native")
                    :native :rfb))
       (port (parse-integer (cl-user::wm-env "WM_PORT" "5910"))))
  (format t "~&wm: ~dx~d, ~a~%" width height
          (if (eq display :native) "in a window" (format nil "VNC on 127.0.0.1:~d" port)))
  (finish-output)
  (glass.wm:serve-wm port :width width :height height :display display))
