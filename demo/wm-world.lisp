;;;; demo/wm-world.lisp — load the window manager (glass/wm) and save a modus core of it,
;;;; for demo/wm-run.lisp.  modus only (SAVE-AND-DIE).
;;;;   env WM_CORE        where to save the core
;;;;       DESK_QL_ADAPTER a file to load after Quicklisp's setup (modus's adapter, and
;;;;                       wherever ASDF should find glass and its friends)
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(let ((adapter (uiop:getenv "DESK_QL_ADAPTER")))
  (when (and adapter (probe-file adapter)) (load adapter)))
(let ((*standard-output* (make-broadcast-stream)))
  (handler-bind ((warning #'muffle-warning))
    (funcall (read-from-string "ql:quickload") "glass/wm" :silent t)))
;; Every face SERVE-WM loads, cached in the core: the run may be where the font files are not.
(glass:default-font)
(glass:default-font t)
(glass-term:term-fonts)
(format t "~&wm: saving ~a~%" (uiop:getenv "WM_CORE"))
(finish-output)
(save-and-die (uiop:getenv "WM_CORE"))
