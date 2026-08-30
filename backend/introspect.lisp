;;;; introspect.lisp — asking a running image about its own code.
;;;;
;;;; The control socket reads forms in CLIM-GLASS, so everything here is callable
;;;; unqualified from `kiln eval' — which is the point.  A machine you can interrogate
;;;; while it runs should answer "what does this function actually say?" without being
;;;; restarted, rebuilt, or grepped from outside.
;;;;
;;;;   kiln eval "(source-of #'cl-transport.gate:connections)"
;;;;   kiln eval "(disassemble #'cl-transport.gate::loopback-p)"
;;;;
;;;; SBCL keeps the file and character offset of every definition compiled from a file,
;;;; which is everything in this workspace.  What it does NOT keep is the text: a
;;;; compiled function has no source string in it, and FUNCTION-LAMBDA-EXPRESSION gives
;;;; back a re-printed form with the macros expanded, the comments gone, and the
;;;; formatting invented.  In a codebase where the comments carry the reasoning, that is
;;;; the half worth having and the half that is lost.  So the text is read back OUT of
;;;; the file it was compiled from.
;;;;
;;;; Which means this tells the truth about the FILE, and the file may have moved on
;;;; from the image.  SOURCE-OF says so when it can tell.

(in-package #:clim-glass)

(export '(source-of where-is))

(defparameter *definition-kinds*
  '(:function :macro :generic-function :method :class :structure :condition
    :variable :constant :type :setf-expander :symbol-macro)
  "What a NAME might name, in the order worth trying.  A symbol is not a function:
   asking SB-INTROSPECT about `with-principal' as an object finds nothing, because a
   macro has no function object to hand it, and the same is true of a class or a
   special variable.  Named things are looked up BY NAME instead, which is also how
   somebody types them.")

(defun %source-of-ds (ds)
  "(values pathname character-offset form-index) — any of the last two may be NIL.

   NOT (OR OFFSET 0).  A class records no character offset at all, only which
   top-level form it is; defaulting the missing number to zero produced a confident
   answer that was the first form in the file — an (in-package) line, printed as
   though it were the definition asked for.  A missing position has to stay missing so
   the caller can go and find the form the other way."
  (let ((path (find-symbol "DEFINITION-SOURCE-PATHNAME" "SB-INTROSPECT"))
        (off  (find-symbol "DEFINITION-SOURCE-CHARACTER-OFFSET" "SB-INTROSPECT"))
        (fp   (find-symbol "DEFINITION-SOURCE-FORM-PATH" "SB-INTROSPECT")))
    (let* ((p (and path (funcall path ds)))
           (o (and off (funcall off ds)))
           (f (and fp (funcall fp ds))))
      (and p (values p o (and (consp f) (integerp (first f)) (first f)))))))

(defun %toplevel-form-start (stream n)
  "The file position where top-level form N (0-based) begins, or NIL.

   *READ-SUPPRESS* is what makes this safe on arbitrary source: the reader walks the
   structure — balancing parens, honouring strings, character literals, #| |# and
   dispatch macros — while interning nothing and evaluating nothing.  Reading these
   files for real would need every package to exist and would run any #. it met."
  (file-position stream 0)
  (let ((*read-suppress* t))
    ;; Skip N forms; where the reader stops is where form N begins.  Any whitespace or
    ;; comment sitting between them is in front of that position, and %FORM-TEXT skips
    ;; forward to the opening paren anyway.
    (dotimes (i n)
      (when (eq :eof (handler-case (read stream nil :eof) (error () :eof)))
        (return-from %toplevel-form-start nil)))
    (file-position stream)))

(defun %definition-source (thing)
  "(values pathname character-offset kind) for THING, or NIL if it has no file.
   THING may be a function object or a name."
  (require :sb-introspect)
  (if (symbolp thing)
      (let ((by-name (find-symbol "FIND-DEFINITION-SOURCES-BY-NAME" "SB-INTROSPECT")))
        (when by-name
          (dolist (kind *definition-kinds* nil)
            (let ((hits (ignore-errors (funcall by-name thing kind))))
              (when hits
                (multiple-value-bind (p o f) (%source-of-ds (first hits))
                  (when p (return (values p o kind f)))))))))
      (let* ((find (find-symbol "FIND-DEFINITION-SOURCE" "SB-INTROSPECT"))
             (ds   (and find (ignore-errors (funcall find thing)))))
        (when ds
          (multiple-value-bind (p o f) (%source-of-ds ds)
            (and p (values p o :function f)))))))

(defun %form-text (stream start)
  "The text of ONE balanced form beginning at or after START in STREAM.

   Read as CHARACTERS and not with READ, because the comments are the point.  The
   parenthesis counting has to know about strings, character literals and semicolon
   comments, since a paren inside any of them does not close anything -- #\\( is the
   case that looks like a bug in the counter until you meet it."
  (file-position stream start)
  ;; The offset points at the start of the form, but a leading newline or the tail of a
  ;; preceding comment can sit in front of it; skip to the first open paren.
  (let ((out (make-string-output-stream))
        (depth 0) (started nil) (in-string nil) (in-comment nil) (escaped nil))
    (loop for ch = (read-char stream nil nil)
          while ch do
            (when started (write-char ch out))
            (cond
              (escaped   (setf escaped nil))
              (in-comment (when (char= ch #\Newline) (setf in-comment nil)))
              (in-string (cond ((char= ch #\\) (setf escaped t))
                               ((char= ch #\") (setf in-string nil))))
              ((char= ch #\\) (setf escaped t))     ; #\( and friends
              ((char= ch #\") (setf in-string t))
              ((char= ch #\;) (setf in-comment t))
              ((char= ch #\()
               (unless started (setf started t) (write-char ch out))
               (incf depth))
              ((char= ch #\))
               (when started
                 (decf depth)
                 (when (zerop depth) (return))))))
    (let ((text (get-output-stream-string out)))
      (and (plusp (length text)) text))))

(defun source-of (thing &key (stream *standard-output*))
  "Print the source text of THING — a function, or a name — as it appears in the file
   it was compiled from.  Returns the pathname, or NIL if there is no source to find."
  (multiple-value-bind (path offset kind form-index) (%definition-source thing)
    (declare (ignorable kind))
    (cond
      ((null path)
       (format stream "~&no source on file for ~a — not defined here, or compiled ~
                       from a stream rather than a file~%" thing)
       nil)
      ((not (probe-file path))
       ;; The image remembers where it was built from; that is not a promise the file
       ;; is still there, and saying which path is missing beats saying nothing.
       (format stream "~&source file is gone: ~a~%" path)
       nil)
      (t
       (with-open-file (in path :external-format :utf-8)
         (let* ((start (or offset
                           (and form-index (%toplevel-form-start in form-index))))
                (text (and start (%form-text in start))))
           (cond (text (format stream "~&;;; ~a~@[  (~(~a~))~]~%~a~%" path kind text))
                 (start (format stream "~&~a: nothing readable at ~d~%" path start))
                 (t (format stream "~&~a: no position recorded for ~a~%" path thing)))))
       path))))

(defun where-is (thing &key (stream *standard-output*))
  "Say which file THING came from, and where in it.  The cheap half of SOURCE-OF."
  (multiple-value-bind (path offset kind form-index) (%definition-source thing)
    (if path
        (progn (format stream "~&~a~@[  offset ~d~]~@[  form ~d~]~@[  (~(~a~))~]~%"
                       path offset form-index kind)
               path)
        (progn (format stream "~&no source on file for ~a~%" thing) nil))))
