;;;; demo/desk-load.lisp — load the desk (and warp's Media, when it is there).
;;;;
;;;; Shared by desk-world.lisp (modus: load, then save a core) and desk-run.lisp (SBCL: load,
;;;; then serve).  Quicklisp must already be loaded; local projects (glass, scribe, warp, ...)
;;;; come from its local-projects directory as usual.

(let ((*standard-output* (make-broadcast-stream)))
  (handler-bind ((warning #'muffle-warning))
    (funcall (read-from-string "ql:quickload") "glass/desk-serve" :silent t)))

;;; Media is warp's player.  Optional: without it the Media window says so.  On modus it needs
;;; warp's message-passing engine (warp master since 2026-10-14); an older warp loads, and its
;;; tracks fail to start -- the desk logs that and carries on.
(defvar cl-user::*desk-media*
  (handler-case
      (let ((*standard-output* (make-broadcast-stream)))
        (handler-bind ((warning #'muffle-warning))
          (funcall (read-from-string "ql:quickload") "warp-media/glass" :silent t))
        t)
    (serious-condition (e)
      (format *error-output* "~&desk: no Media (warp-media/glass did not load: ~a)~%" e)
      nil)))

;;; The faces, loaded now: in a saved core they are already there when the desk starts.
(glass:default-font)
(glass:default-font t)
