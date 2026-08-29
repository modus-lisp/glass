;;;; inspect/focus-history-gate.lisp — a control surface hands the keyboard back.
;;;;
;;;; THE BUG: dictation went into the Listen box almost every time.  Turning it on means
;;;; clicking a button in the Listen window, and clicking a window focuses it — so the words
;;;; went to the thing that started them, and dictating anywhere else meant knowing you had to
;;;; go and click the other window first.  It worked once, which is worse than never: the
;;;; behaviour looked intermittent when it was exactly deterministic.
;;;;
;;;; A control is not a target.  The seat therefore keeps one step of focus history, and Listen
;;;; gives the keyboard back before the words start.
;;;;
;;;; One step and not a stack, because that is the whole of what "give it back" needs and a
;;;; stack raises a question with no answer — how far back.  The two cases that decide whether
;;;; one step is enough are both here: re-focusing the window that already has the keyboard must
;;;; not make it its own predecessor, and a window that has since closed is not somewhere to
;;;; send keys.
;;;;
;;;;   sbcl --script inspect/focus-history-gate.lisp

(require :asdf)
(unless (find-package :quicklisp)
  (let ((setup (find-if #'probe-file
                        (remove nil (list (let ((e (sb-ext:posix-getenv "QUICKLISP_SETUP")))
                                            (and e (pathname e)))
                                          #p"/opt/quicklisp/setup.lisp"
                                          (merge-pathnames "quicklisp/setup.lisp"
                                                           (user-homedir-pathname)))))))
    (unless setup
      (format *error-output* "~&focus-history-gate: no Quicklisp.~%") (sb-ext:exit :code 1))
    (load setup)))
(let* ((here (or *load-truename* *default-pathname-defaults*))
       (root (truename (make-pathname :name nil :type nil
                                      :defaults (merge-pathnames "../../" here)))))
  (asdf:initialize-source-registry
   `(:source-registry (:tree ,root) (:exclude "vendor") (:exclude "deps") :inherit-configuration))
  (handler-bind ((warning #'muffle-warning))
    (let ((*standard-output* (make-broadcast-stream)))
      (ignore-errors (asdf:load-system :pigment))
      (asdf:load-asd (merge-pathnames "../backend/mcclim-glass.asd" here))
      (asdf:load-system :mcclim-glass))))

(defvar *fail* 0)
(defun ok (n g &optional d)
  (if g (format t "  [pass] ~a~@[ — ~a~]~%" n d)
      (progn (incf *fail*) (format t "  [FAIL] ~a~@[ — ~a~]~%" n d))))

(let ((p (clim-glass:make-wm-session :width 1000 :height 700)))
  (clim-glass:start-wm-session p '())
  (sb-thread:make-thread (lambda () (clim-glass:run-wm-loop p)) :name "f")
  (sleep 1.5)
  (let* ((seat (clim-glass:port-seat p))
         (a (clim-glass::wm-add-terminal p :cols 30 :rows 8))
         (b (progn (sleep 0.6) (clim-glass::wm-add-terminal p :cols 30 :rows 8))))
    (sleep 0.6)
    (clim-glass:seat-focus seat a)
    (ok "focusing a window gives it the keyboard" (eq a (clim-glass::seat-focus-surface seat)))
    (clim-glass:seat-focus seat b)
    (ok "focusing another remembers the first" (eq a (clim-glass:seat-prev-focus seat)))
    (ok "...and the second has it now" (eq b (clim-glass::seat-focus-surface seat)))
    ;; the control-surface case: a window takes focus, then hands it back
    (ok "handing it back returns to the first" (eq a (clim-glass:seat-focus-back seat)))
    (ok "...and the keyboard is really there" (eq a (clim-glass::seat-focus-surface seat)))
    ;; focusing what is already focused must not make it its own predecessor
    (let ((prev (clim-glass:seat-prev-focus seat)))
      (clim-glass:seat-focus seat a)
      (ok "re-focusing the same window does not shift the history"
          (eq prev (clim-glass:seat-prev-focus seat))
          "otherwise giving it back gives it to where it already is"))
    ;; a closed window is not somewhere to send keys
    (clim-glass:seat-focus seat b)
    (ignore-errors (clim-glass::wm-close p a))
    (sleep 0.5)
    (ok "a window that has closed is not gone back to"
        (null (clim-glass:seat-focus-back seat))
        "NIL leaves the keyboard where it is, which is honest rather than a failure")))
(format t "~&=> ~:[FAIL~;PASS~]~%" (zerop *fail*))

(finish-output)
(sb-ext:exit :code (if (plusp *fail*) 1 0))
