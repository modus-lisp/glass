;;;; introspect-gate.lisp — a running image can show its own source.
;;;;
;;;;   sbcl --script backend/inspect/introspect-gate.lisp
;;;;
;;;; The claim is narrow and worth pinning: SOURCE-OF returns the TEXT in the file,
;;;; comments and all, for the several different things a name can name — and says so
;;;; plainly when there is nothing to show, rather than printing a confident wrong
;;;; answer.  That last one is not hypothetical: defaulting a class's missing character
;;;; offset to zero printed the (in-package) line at the top of the file as though it
;;;; were the definition asked for.

(require :asdf)
(let ((ql (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file ql) (load ql)))
(asdf:initialize-source-registry
 (let ((here (make-pathname :name nil :type nil :defaults *load-truename*)))
   `(:source-registry (:tree ,(merge-pathnames "../../../" here))
                      (:exclude "vendor") (:exclude "deps") :inherit-configuration)))
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (asdf:load-asd (merge-pathnames "../mcclim-glass.asd"
                                    (make-pathname :name nil :type nil
                                                   :defaults *load-truename*)))
    (asdf:load-system :mcclim-glass)))

(defpackage #:introspect-gate (:use #:cl)) (in-package #:introspect-gate)

(defvar *pass* 0) (defvar *fail* 0)
(defun ok (name got &optional detail)
  (if got (progn (incf *pass*) (format t "  [pass] ~a~@[ — ~a~]~%" name detail))
      (progn (incf *fail*) (format t "  [FAIL] ~a~@[ — ~a~]~%" name detail)))
  (finish-output) got)
(defun head (s) (format t "~%== ~a ==~%" s) (finish-output))

(defun text-of (thing)
  (with-output-to-string (o) (clim-glass:source-of thing :stream o)))

(head "a function")
(let ((txt (text-of #'clim-glass::%form-text)))
  (ok "the source comes back" (search "defun %form-text" txt))
  ;; The reason to read the file instead of FUNCTION-LAMBDA-EXPRESSION.
  (ok "…with its comments, which is the half worth having"
      (search "the comments are the point" txt))
  (ok "…and its docstring" (search "balanced form beginning" txt))
  ;; #\( and #\) inside the body must not confuse the paren counting — this very
  ;; function is full of them, which makes it its own worst case.
  (ok "…balanced past character literals"
      (and (search "(char= ch #\\()" txt) (search "get-output-stream-string" txt))))

(head "a macro, by name")
(let ((txt (text-of 'clim-glass::with-seat-scale)))
  (ok "a macro is found by name, not as an object"
      (or (search "defmacro with-seat-scale" txt) (search "no source" txt))
      (subseq txt 0 (min 90 (length txt)))))

(head "a class, which records no character offset at all")
(let ((txt (text-of 'clim-glass::wm-surface)))
  (ok "a class is found via its top-level form path"
      (search "wm-surface" txt))
  ;; The bug this gate exists for: the file's first form is an IN-PACKAGE, and a class
  ;; whose offset defaulted to 0 printed that instead of the definition.
  (ok "…and not the (in-package) at the top of its file"
      (not (search "(in-package" txt))
      (subseq txt 0 (min 90 (length txt)))))

(head "and it admits when it cannot")
(let ((txt (text-of 'no-such-symbol-anywhere-at-all)))
  (ok "an unknown name says so rather than guessing"
      (search "no source on file" txt) (string-trim '(#\Newline) txt)))
(ok "WHERE-IS names the file"
    (search "control.lisp"
            (with-output-to-string (o) (clim-glass:where-is #'clim-glass::control-answer :stream o))))

(format t "~%~d passed, ~d failed~%~%=> ~:[FAIL~;PASS~]~%" *pass* *fail* (zerop *fail*))
(finish-output)
(sb-ext:exit :code (if (plusp *fail*) 1 0))
