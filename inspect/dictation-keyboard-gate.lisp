;;;; inspect/dictation-keyboard-gate.lisp — dictation needs a keyboard, not a VNC server.
;;;;
;;;; THE BUG: on a welded desktop — one process, its own window, no wire — pressing Listen's
;;;; dictate toggle refused with "Nothing to type into — no VNC server is running on this
;;;; session".  A true statement about a transport, and the wrong answer to the question: there
;;;; was a window open, a keyboard working, and somebody typing into it by hand.
;;;;
;;;; GLASS:SERVE installs its client's :ON-KEY as *KEY-INJECTOR* and gives the reason — the only
;;;; path that knows where focus IS, is the one a real keystroke takes.  That reason has nothing
;;;; to do with sockets.  A viewer in this image takes the identical path, and ATTACH-SEAT-LOCAL
;;;; already built the injector and stored it on the seat; nobody made it the SESSION's, so
;;;; everything that types for you — dictation, paste, a script — had nowhere to type.
;;;;
;;;; It is the same shape as the microphone one commit earlier: a capability that belonged to a
;;;; seat, reachable only through a transport, on a desktop that no longer has one.
;;;;
;;;; The last check types for real rather than asserting on a variable: a variable being set is
;;;; what the fix DID, and keys arriving is what it was FOR.
;;;;
;;;;   sbcl --script inspect/dictation-keyboard-gate.lisp

(require :asdf)
(unless (find-package :quicklisp)
  (let ((setup (find-if #'probe-file
                        (remove nil (list (let ((e (sb-ext:posix-getenv "QUICKLISP_SETUP")))
                                            (and e (pathname e)))
                                          #p"/opt/quicklisp/setup.lisp"
                                          (merge-pathnames "quicklisp/setup.lisp"
                                                           (user-homedir-pathname)))))))
    (unless setup
      (format *error-output* "~&dictation-keyboard-gate: no Quicklisp.~%") (sb-ext:exit :code 1))
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
      (asdf:load-system :mcclim-glass)
      (asdf:load-system :mcclim-glass/listen)
      (asdf:load-system :glass/dictation))))

(defvar *fail* 0)
(defun ok (n g &optional d)
  (if g (format t "  [pass] ~a~@[ — ~a~]~%" n d)
      (progn (incf *fail*) (format t "  [FAIL] ~a~@[ — ~a~]~%" n d))))

(sb-posix:setenv "GLASS_EARS" (format nil "~a/.stave/models/zipformer-en-2023-06-26"
                                      (sb-ext:posix-getenv "HOME")) 1)
(setf glass:*key-injector* nil)
(let ((p (clim-glass:make-wm-session :width 800 :height 600)))
  (clim-glass:start-wm-session p '())
  (sb-thread:make-thread (lambda () (clim-glass:run-wm-loop p)) :name "d")
  (sleep 1.5)
  (ok "a session with nobody attached has no keyboard" (null glass:*key-injector*)
      "which is the state Listen must refuse in")
  (let ((seat (clim-glass:port-seat p)))
    ;; the welded viewer's attachment, with no wire anywhere
    (clim-glass:attach-seat-local seat)
    (ok "attaching a local viewer gives the session a keyboard"
        (not (null glass:*key-injector*)))
    (ok "...and it is that seat's own injector"
        (eq glass:*key-injector* (clim-glass::seat-injector seat)))
    ;; which is what Listen was checking, so dictation is now offered
    (ok "Listen would no longer refuse" (not (null glass:*key-injector*))
        "the check it makes is exactly this")
    ;; and the keyboard actually delivers: type into the desktop and see the key arrive
    (let ((seen '()))
      (let ((orig (symbol-function 'clim-glass::glass-on-key)))
        (unwind-protect
             (progn
               (setf (symbol-function 'clim-glass::glass-on-key)
                     (lambda (port down k &optional s)
                       (push (list down k) seen) (funcall orig port down k s)))
               (glass:paste-text "hi")
               (sleep 0.5))
          (setf (symbol-function 'clim-glass::glass-on-key) orig)))
      (ok "typed text reaches the desktop's key path" (plusp (length seen))
          (format nil "~a key events" (length seen))))))
(format t "~&=> ~:[FAIL~;PASS~]~%" (zerop *fail*))

(finish-output)
(sb-ext:exit :code (if (plusp *fail*) 1 0))
