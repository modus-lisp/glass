;;;; demo/desk-world.lisp — modus: load the desk and save it as a core, so the desk starts in
;;;; a second instead of the minute or two the load takes.  Run by demo/desk.sh.
;;;;   env DESK_CORE    where to save the core
;;;;       DESK_QL_ADAPTER  modus-quicklisp/real-quicklisp.lisp (Quicklisp's network on modus)

(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(let ((adapter (uiop:getenv "DESK_QL_ADAPTER")))
  (when (and adapter (probe-file adapter)) (load adapter)))
(load (merge-pathnames "desk-load.lisp" (or *load-truename* *load-pathname*
                                            (uiop:getenv "DESK_DEMO_DIR"))))
(format t "~&desk: saving ~a~%" (uiop:getenv "DESK_CORE"))
(finish-output)
(save-and-die (uiop:getenv "DESK_CORE"))   ; modus runtime (this file is modus-only)
