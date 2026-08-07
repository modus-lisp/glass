;;;; record.lisp — DEFINE-RECORD: a defstruct that can be redefined.
;;;;
;;;; Glass is meant to be loaded into a desktop that is already running.  That is not a
;;;; convenience — it is the point of the control socket, and it is how nearly every change in
;;;; this repo gets tried: recompile one file, pipe a LOAD into the live image, watch the change
;;;; appear on the screen without losing the session.
;;;;
;;;; DEFSTRUCT breaks that, in one specific and very common case.  Adding a slot to a struct the
;;;; running image is holding an instance of is an INCOMPATIBLE redefinition of a
;;;; STRUCTURE-OBJECT class, and SBCL will not do it: the condition is signalled during
;;;; COMPILE-FILE, so under --disable-debugger it takes the load, and the only way out is to
;;;; restart the desktop.  Which means the one change you cannot hot-load is "the ear needs to
;;;; remember one more thing" — exactly the change you make while a feature is being built.
;;;;
;;;; A STANDARD-OBJECT has no such rule.  Redefining a class updates its existing instances
;;;; lazily, through UPDATE-INSTANCE-FOR-REDEFINED-CLASS: an added slot takes its :INITFORM the
;;;; first time the instance is touched, a removed slot is dropped, and the live ear keeps
;;;; decoding through it.  So the long-lived records here are classes.
;;;;
;;;; This macro exists so that saying so costs one token per record instead of a hand-written
;;;; DEFCLASS per record and a rewrite of every accessor call.  It takes DEFSTRUCT's syntax —
;;;; the same slot forms, the same (:CONSTRUCTOR ...) and (:CONC-NAME ...) options — and emits
;;;; the class, a keyword constructor of the named function, and a predicate.  The call sites do
;;;; not change at all.
;;;;
;;;; What it deliberately does NOT cover, because nothing here needs it and each would be a
;;;; silent behaviour change: :INCLUDE (inheritance), BOA constructors (positional arglists),
;;;; COPY-<name> (a class copy is not a shallow struct copy), and :PRINT-FUNCTION.  A record
;;;; that wants one of those should stay a DEFSTRUCT or grow a real DEFCLASS.
;;;;
;;;; And this is NOT for every struct in glass.  The pixel path — FRAMEBUFFER, PXFMT,
;;;; RFB-CLIENT — stays DEFSTRUCT: those accessors sit inside per-pixel and per-rectangle loops
;;;; where an inlined slot read and a generic-function call are not the same thing, and none of
;;;; them is the kind of record that grows a slot mid-session.  The rule is: convert what is
;;;; long-lived and evolving, leave what is hot.

(in-package #:glass)

(defun %record-slot-spec (spec conc-name)
  "One DEFSTRUCT slot form -> one DEFCLASS slot form.

Accepts NAME, (NAME INITFORM) and (NAME INITFORM :type T :read-only BOOL).  :TYPE is carried
through: it documents the intent that made the struct declare it, and on a STANDARD-OBJECT it is
advice rather than a hard gate, which is the trade that buys the redefinition.  :READ-ONLY
becomes a :READER, so a slot that could not be written stays one.

The accessor is interned in *PACKAGE* and not in the slot symbol's home package, which is what
DEFSTRUCT does and is not a detail: SINK has a slot called FILL, and that symbol is CL:FILL —
interning SINK-FILL next to it is a package-lock violation, while interning it here is just an
accessor named after a slot that happens to share a name with a standard function."
  (destructuring-bind (name &optional initform &key type read-only)
      (if (listp spec) spec (list spec))
    `(,name :initform ,initform
            :initarg ,(intern (symbol-name name) :keyword)
            ,(if read-only :reader :accessor)
            ,(intern (concatenate 'string (string conc-name) (symbol-name name)))
            ,@(when type `(:type ,type)))))

(defmacro define-record (name-and-options &body slots)
  "Like DEFSTRUCT, but the result is a CLASS — so adding a slot can be hot-loaded into a running
desktop instead of costing a restart.  See the head of this file for why.

NAME-AND-OPTIONS is a bare name or (NAME (:CONSTRUCTOR fn) (:CONC-NAME prefix)), and SLOTS are
DEFSTRUCT slot forms, optionally preceded by a docstring.  Defines the class, a keyword
constructor (default MAKE-<name>), and the predicate <name>-P."
  (let* ((name (if (listp name-and-options) (first name-and-options) name-and-options))
         (options (if (listp name-and-options) (rest name-and-options) '()))
         (doc (when (stringp (first slots)) (pop slots)))
         (conc (let ((o (assoc :conc-name options)))
                 (cond ((null o) (concatenate 'string (symbol-name name) "-"))
                       ((null (second o)) "")
                       (t (string (second o))))))
         (ctor (let ((o (assoc :constructor options)))
                 (if o (second o) (intern (concatenate 'string "MAKE-" (symbol-name name))))))
         (pred (intern (concatenate 'string (symbol-name name) "-P"))))
    (dolist (o options)
      (unless (member (first o) '(:constructor :conc-name))
        (error "DEFINE-RECORD ~a: unsupported option ~s — see record.lisp for what is left out ~
                on purpose." name (first o))))
    `(progn
       (defclass ,name ()
         ,(mapcar (lambda (s) (%record-slot-spec s conc)) slots)
         ,@(when doc `((:documentation ,doc))))
       ;; &REST rather than a spelled-out lambda list so that a slot added by a hot-load is
       ;; accepted by the constructor in the same image, without the caller being recompiled.
       (defun ,ctor (&rest initargs) (apply #'make-instance ',name initargs))
       (defun ,pred (x) (typep x ',name))
       ',name)))
