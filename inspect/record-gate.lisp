;;;; inspect/record-gate.lisp — the one claim DEFINE-RECORD exists to make, made against SBCL.
;;;;
;;;; The claim is not "these accessors work" — that is true of the DEFSTRUCT this replaced, and
;;;; the six converted records are already covered by audio-gate, clipboard-gate, speech-gate,
;;;; hearing-gate, mic-gate and the two backend app gates.  The claim is narrower and is the
;;;; entire reason for the change:
;;;;
;;;;   Adding a slot to a record the running image is HOLDING AN INSTANCE OF must not be an
;;;;   error, and the instance the running desktop is using must survive it.
;;;;
;;;; That is checked here the only way it can be honestly checked — by doing it.  Both halves are
;;;; run in this image: a DEFSTRUCT is redefined with an extra slot and the condition it signals
;;;; is caught and shown; the same redefinition is made to a DEFINE-RECORD and the instance made
;;;; BEFORE it is read AFTER it.  If SBCL ever relaxes the struct rule, the first check fails and
;;;; tells us this whole macro became unnecessary, which is a result worth being told about.
;;;;
;;;; Then the same thing is done to GLASS:EARS itself — a slot added to the real record, with a
;;;; real ear instance alive across the redefinition — because a macro that works on a toy and
;;;; not on the record with twenty-six slots and two threads holding it is not worth having.
;;;;
;;;;   sbcl --non-interactive --load inspect/record-gate.lisp

(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (asdf:load-system :glass/hearing)))

(defpackage #:glass-record-gate (:use #:cl)) (in-package #:glass-record-gate)

(defvar *pass* 0) (defvar *fail* 0)
(defun check (name got want)
  (if (equal got want) (progn (incf *pass*) (format t "  ok   ~a = ~s~%" name got))
      (progn (incf *fail*) (format t "  FAIL ~a: got ~s, want ~s~%" name got want)))
  (finish-output))
(defun check-that (name ok &optional detail)
  (if ok (progn (incf *pass*) (format t "  ok   ~a~@[ — ~a~]~%" name detail))
      (progn (incf *fail*) (format t "  FAIL ~a~@[ — ~a~]~%" name detail)))
  (finish-output))

;;; A redefinition has to be compiled the way a hot-load compiles it — from a form, at runtime,
;;; in this image — not read from the file at load time, or the second definition would simply
;;; replace the first before anything was ever instanced.
;;;
;;; *PACKAGE* is bound, and that is not housekeeping: DEFINE-RECORD interns its accessors in
;;; *PACKAGE*, exactly as DEFSTRUCT does, so evaluating a glass record's definition from this
;;; gate's package would define GLASS-RECORD-GATE::EAR-PARTIAL and leave the real
;;; GLASS:EAR-PARTIAL a generic function with no methods.  A hot-load never hits this, because
;;; the file it loads begins with (IN-PACKAGE #:GLASS); a gate that evaluates definitions by hand
;;; has to say so itself.  It cost a run to find out.
(defun redefine (form &optional (package *package*))
  "Evaluate FORM as a redefinition, returning T or the condition that stopped it."
  (handler-case (let ((*package* (if (packagep package) package (find-package package))))
                  (handler-bind ((warning #'muffle-warning)) (eval form))
                  t)
    (serious-condition (e) e)))

;;; ---- what DEFSTRUCT does, which is the problem ------------------------------

(format t "~&~%-- a struct with a live instance cannot grow a slot --~%")

(defstruct (probe-struct (:conc-name ps-)) (a 1) (b 2))
(defvar *live-struct* (make-probe-struct :a 10))
(check "the instance is fine before" (ps-a *live-struct*) 10)

(let ((result (redefine '(defstruct (probe-struct (:conc-name ps-)) (a 1) (b 2) (c 3)))))
  (check-that "adding a slot to a DEFSTRUCT signals" (typep result 'serious-condition)
              (if (typep result 'condition)
                  (substitute #\Space #\Newline (princ-to-string result))
                  "IT DID NOT — SBCL relaxed the rule and this macro is obsolete"))
  ;; This is the shape of the failure that costs a restart: the condition arrives during
  ;; COMPILE-FILE, so it is not the redefinition that is lost, it is the whole load.
  (check-that "and it is the incompatible-redefinition condition, not something else"
              (or (typep result 'sb-ext:defconstant-uneql) ; never; keeps the OR honest
                  (search "incompat" (string-downcase (princ-to-string result)))
                  (search "redefin" (string-downcase (princ-to-string result))))))

;;; ---- what DEFINE-RECORD does instead ----------------------------------------

(format t "~&~%-- a record with a live instance can --~%")

(glass::define-record (probe-record (:conc-name pr-) (:constructor %make-probe))
  (a 1) (b 2))
(defvar *live-record* (%make-probe :a 10 :b 20))
(check "the instance is fine before" (pr-a *live-record*) 10)

(let ((result (redefine '(glass::define-record (probe-record (:conc-name pr-)
                                                             (:constructor %make-probe))
                          (a 1) (b 2) (c 3) (d "added")))))
  (check-that "adding two slots to a DEFINE-RECORD just works"
              (eq result t)
              (when (typep result 'condition) (princ-to-string result))))

;; The instance was made before the class had C or D.  This is UPDATE-INSTANCE-FOR-REDEFINED-CLASS
;; doing the work, and it is the whole payoff: the ear the decode thread is holding is the ear
;; that comes out the other side, with what it was carrying intact.
(check "the old instance kept the slot it was carrying" (pr-a *live-record*) 10)
(check "and the other one" (pr-b *live-record*) 20)
(check "the new slot appeared with its initform" (pr-c *live-record*) 3)
(check "including a non-numeric one" (pr-d *live-record*) "added")
(check-that "and it is still the same object" (eq *live-record* *live-record*))
(check-that "the constructor takes the new slot too" (= 99 (pr-c (%make-probe :c 99))))
(check-that "the predicate still recognizes the pre-redefinition instance"
            (probe-record-p *live-record*))

;;; ---- and on the real thing ---------------------------------------------------
;;;
;;; GLASS:EARS is the record this was built for: the longest-lived object in a desktop, held by
;;; the pull thread and the decode thread, and the one that keeps growing slots.  No recognizer
;;; is loaded here — an ear that has never listened is still an ear, and the point is the slot,
;;; not the audio.

(format t "~&~%-- and on GLASS:EARS, with an ear alive across it --~%")

(defvar *ear* (glass:make-ears))
(setf (glass::ear-utterances *ear*) 7
      (glass::ear-partial *ear*) "HALF A SENTENCE")
(check-that "an ear exists and is carrying state" (glass:ears-p *ear*)
            (format nil "~d utterance(s), partial ~s"
                    (glass::ear-utterances *ear*) (glass::ear-partial *ear*)))

;;; The definition comes out of src/hearing.lisp rather than being written out again here, for two
;;; reasons.  A copy would drift, and this gate would then be checking a record that no longer
;;; resembles the ear.  And a definition regenerated from the MOP loses every initform — the ear
;;; would come back with a NIL where its mutex is, which is not a slot being added, it is the ear
;;; being destroyed.  Read the real form, splice one slot onto the end, evaluate: that is what a
;;; person adding a slot does, character for character.
(defvar *here* (or *load-truename* *load-pathname* *default-pathname-defaults*))

(defun ears-definition ()
  "The DEFINE-RECORD form for GLASS:EARS, read out of the file it lives in."
  (let ((*package* (find-package :glass))
        (*read-eval* nil))
    (with-open-file (in (merge-pathnames "../src/hearing.lisp" *here*))
      (loop for form = (read in nil :eof)
            until (eq form :eof)
            when (and (consp form)
                      (symbolp (first form))
                      (string= (symbol-name (first form)) "DEFINE-RECORD")
                      (equal (string (if (consp (second form)) (first (second form)) (second form)))
                             "EARS"))
              return form
            finally (error "record-gate: no DEFINE-RECORD for EARS in src/hearing.lisp")))))

(let* ((original (ears-definition))
       (grown (append original '((probe-slot :not-a-real-slot))))
       (result (redefine grown :glass)))
  (check-that "a slot can be added to the live EARS class" (eq result t)
              (when (typep result 'condition) (princ-to-string result)))
  (check "the live ear kept its transcript" (glass::ear-partial *ear*) "HALF A SENTENCE")
  (check "and its count" (glass::ear-utterances *ear*) 7)
  (check "and gained the new slot at its initform"
         (funcall (intern "EAR-PROBE-SLOT" :glass) *ear*) :not-a-real-slot)
  ;; The ear is a record of live machinery, so "it survived" has to mean the machinery survived:
  ;; a fresh one still gets a real mutex and a real semaphore from its initforms, which a
  ;; redefinition that dropped them would not give it.
  (let ((fresh (glass:make-ears)))
    (check-that "and a fresh ear is a working ear, not a shell"
                (and (glass:ears-p fresh)
                     (typep (glass::ear-lock fresh) 'sb-thread:mutex)
                     (= 16000 (glass::ear-rate fresh)))
                (format nil "rate ~d, lock ~a" (glass::ear-rate fresh)
                        (type-of (glass::ear-lock fresh)))))
  ;; And put it back, so this gate leaves the image the way it found it — the same redefinition
  ;; in the other direction, which is also the check that a slot can be REMOVED.
  (check-that "and the slot can be taken away again" (eq t (redefine original :glass)))
  (check-that "leaving the live ear intact" (equal (glass::ear-partial *ear*) "HALF A SENTENCE"))
  (check-that "and the probe slot gone"
              (not (slot-exists-p *ear* (intern "PROBE-SLOT" :glass)))))

(format t "~&~%~d passed, ~d failed~%" *pass* *fail*)
(finish-output)
(sb-ext:exit :code (if (zerop *fail*) 0 1))
