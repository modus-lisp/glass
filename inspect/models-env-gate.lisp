;;;; inspect/models-env-gate.lisp — GLASS_EARS and GLASS_VOICE, set AFTER the image loaded.
;;;;
;;;; THE BUG THIS EXISTS FOR: Listen said "No ear installed" next to a --ears flag that had
;;;; plainly worked — models downloaded, path right, environment carrying it — and the one
;;;; thing that never happened was anybody looking.
;;;;
;;;; *HEARING-MODELS* was a DEFVAR initialised from (POSIX-GETENV "GLASS_EARS").  A DEFVAR's
;;;; init form runs when the file is LOADED; for this image that is while the core is being
;;;; BUILT, and resuming a saved core does not run it again.  So the value was whatever the
;;;; environment said at build time — nothing — and every GLASS_EARS set afterwards, by a
;;;; launcher or by docker -e, wrote to a variable nobody would read again.
;;;;
;;;; *SPEECH-VOICE* had the identical shape and appeared to work, which is worse: kiln's
;;;; --voice writes the symbol directly instead of exporting a variable, so the path people
;;;; used was fine while the documented environment variable quietly was not.  That is the
;;;; reason this gate tests the ENVIRONMENT and not the flag — the flag was never broken.
;;;;
;;;; A saved core is a snapshot of load time, and the environment is a runtime thing.  Any
;;;; DEFVAR here that reads POSIX-GETENV at load is the same bug waiting; the fix is to read
;;;; at USE, which is what HEARING-MODELS and SPEECH-VOICE do.
;;;;
;;;;   sbcl --script inspect/models-env-gate.lisp

(require :asdf)
(unless (find-package :quicklisp)
  (let ((setup (find-if #'probe-file
                        (remove nil (list (let ((e (sb-ext:posix-getenv "QUICKLISP_SETUP")))
                                            (and e (pathname e)))
                                          #p"/opt/quicklisp/setup.lisp"
                                          (merge-pathnames "quicklisp/setup.lisp"
                                                           (user-homedir-pathname)))))))
    (unless setup
      (format *error-output* "~&models-env-gate: no Quicklisp — McCLIM comes from there.~%")
      (sb-ext:exit :code 1))
    (load setup)))
(let* ((here (or *load-truename* *default-pathname-defaults*))
       (root (truename (make-pathname :name nil :type nil
                                      :defaults (merge-pathnames "../../" here)))))
  (asdf:initialize-source-registry
   `(:source-registry (:tree ,root) (:exclude "vendor") (:exclude "deps")
                      :inherit-configuration))
  (handler-bind ((warning #'muffle-warning))
    (let ((*standard-output* (make-broadcast-stream)))
      (ignore-errors (asdf:load-system :pigment))
      (asdf:load-asd (merge-pathnames "../backend/mcclim-glass.asd" here))
      (asdf:load-system :mcclim-glass)
      (asdf:load-system :glass/hearing) (asdf:load-system :glass/speech)
      (asdf:load-system :mcclim-glass/listen) (asdf:load-system :mcclim-glass/speak))))

(defvar *fail* 0)
(defun ok (n g &optional d)
  (if g (format t "  [pass] ~a~@[ — ~a~]~%" n d)
      (progn (incf *fail*) (format t "  [FAIL] ~a~@[ — ~a~]~%" n d))))
(let ((models (format nil "~a/.stave/models/zipformer-en-2023-06-26"
                      (sb-ext:posix-getenv "HOME")))
      (voice  (format nil "~a/.chord/voices/en_US-lessac-medium.graph"
                      (sb-ext:posix-getenv "HOME"))))
  ;; THE CASE THAT WAS BROKEN: nothing set in the image, the environment set AFTER load.
  (sb-posix:unsetenv "GLASS_EARS") (sb-posix:unsetenv "GLASS_VOICE")
  (setf glass:*hearing-models* nil glass:*speech-voice* nil)
  (ok "with nothing set, there is no ear" (null (glass:hearing-models)))
  (ok "...and no voice" (null (glass:speech-voice)))
  (sb-posix:setenv "GLASS_EARS" models 1)
  (sb-posix:setenv "GLASS_VOICE" voice 1)
  (ok "GLASS_EARS set AFTER load is seen" (equal models (glass:hearing-models))
      (glass:hearing-models))
  (ok "GLASS_VOICE set AFTER load is seen" (equal voice (glass:speech-voice)))
  ;; an empty variable is what an unset docker -e looks like
  (sb-posix:setenv "GLASS_EARS" "" 1)
  (ok "an empty GLASS_EARS counts as unset" (null (glass:hearing-models)))
  ;; a launcher setting the symbol still wins
  (setf glass:*hearing-models* models)
  (ok "a launcher's symbol still wins over the environment"
      (equal models (glass:hearing-models)))
  ;; and the app agrees
  (sb-posix:setenv "GLASS_EARS" models 1)
  (setf glass:*hearing-models* nil)
  ;; the headline the window actually shows -- the string the report was about
  (let ((state (funcall (find-symbol "%STATE" "GLASS-LISTEN"))))
    (ok "Listen no longer says the ear is missing"
        (not (string= "No ear installed" state)) state))
  (setf glass:*speech-voice* nil)
  (let ((state (funcall (find-symbol "%STATE" "GLASS-SPEAK"))))
    (ok "...and Speak sees its voice too"
        (not (string= "No voice installed" state)) state)))
(format t "~&=> ~:[FAIL~;PASS~]~%" (zerop *fail*))

(finish-output)
(sb-ext:exit :code (if (plusp *fail*) 1 0))
