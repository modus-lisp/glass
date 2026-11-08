;;;; test/term-apps-test.lisp — the WM's Lisp and Terminal apps, share-nothing, on SBCL and modus.
;;;;   sbcl --script test/term-apps-test.lisp      modus --script test/term-apps-test.lisp
;;;; (with glass/wm findable by ASDF; prints a verdict, exits 0 iff all pass)
;;;;
;;;; Laid out as SERVE-WM lays it out: THIS thread (main) runs the listener, and a second
;;;; thread plays the compositor -- it opens the windows, types into them and reads what
;;;; comes back.  On modus that is the arrangement that has to work: a thread other than
;;;; main can store nothing where main can see it, so a DEFUN typed into a window must be
;;;; evaluated here, and the shell must be read by the thread that owns the terminal.

(require :asdf)
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream))) (asdf:load-system :glass/wm)))

(defpackage #:glass-term-apps-test (:use #:cl))
(in-package #:glass-term-apps-test)

(defvar *fails* 0)
(defun check (name ok)
  (format t "~&TERM ~:[FAIL~;ok  ~] ~a~%" ok name)
  (unless ok (incf *fails*)))

;;; ---- the pieces, on this thread --------------------------------------------------------

(check "form-complete-p: whole forms, open brackets, strings, comments, characters"
       (equal (mapcar #'glass-repl:form-complete-p
                      '("(+ 1 2)" "(+ 1" "foo" "" "  " "\"a (b\"" "\"a (b" "(list #\\( 1)"
                        "; (x" "#| ( |# 7" "(a) (b" "'x" "(f \"\\\"\" )"))
               '(t nil t nil nil t nil t nil t nil t t)))

(flet ((ev (text &optional (pkg "COMMON-LISP-USER"))
         (let ((s (make-string-output-stream)))
           (let ((p (glass-repl:evaluate-text text pkg s)))
             (list (get-output-stream-string s) p)))))
  (check "evaluate-text prints values" (equal (ev "(+ 1 2) (values 4 5)")
                                              (list (format nil "3~%4~%5~%") "COMMON-LISP-USER")))
  (check "an error is printed, not raised"
         (let ((r (first (ev "(car 5)")))) (and (search "; type-error" r) t)))
  (check "a reader error too" (and (search ";" (first (ev "(+ 1 #<"))) t))
  (check "output and no values" (equal (first (ev "(progn (princ 'hi) (values))"))
                                       (format nil "HI~%; No values~%")))
  (check "IN-PACKAGE sticks" (equal (second (ev "(in-package :glass-repl)")) "GLASS-REPL")))

;;; ---- the compositor's side, on another thread ------------------------------------------

(defun keys (on-key string)
  (loop for c across string
        do (let ((ks (if (char= c #\Newline) #xff0d (char-code c))))
             (funcall on-key t ks) (funcall on-key nil ks))))

(defun until (pred &optional (secs 20))
  (loop with end = (+ (get-internal-real-time) (* secs internal-time-units-per-second))
        do (when (funcall pred) (return t))
           (when (> (get-internal-real-time) end) (return nil))
           (sleep 1/50)))

(defun compositor (listener)
  "Plays SERVE-WM's compositor (with its own glyph cache, as SERVE-WM's): returns a list of
   (NAME . OK).  An error is reported with its backtrace and ends the run."
  (glass:with-own-glyph-cache ()
    (handler-case
        (handler-bind ((error (lambda (e)
                                (format t "~&compositor error: ~a~%" e)
                                (ignore-errors
                                 (let ((f (and (find-package "MODUS")
                                               (find-symbol "CONDITION-BACKTRACE" "MODUS"))))
                                   (if f (funcall f e *standard-output*)
                                       (sb-debug:print-backtrace :count 30))))
                                (finish-output))))
          (compositor-1 listener))
      (error () (glass-repl:stop-listener listener) :error))))

(defun compositor-1 (listener)
  (let ((results '()))
    (flet ((note (name ok) (push (cons name (and ok t)) results)))
      ;; A REPL window, talking to the listener on main.
      (let ((fb (glass:make-framebuffer 640 400)))
        (multiple-value-bind (on-key on-pointer dirty-p copy-p close-fn resize-fn text-p)
            (glass-repl:repl-app fb)
          (declare (ignore on-pointer copy-p))
          (note "repl-app returns the contract (key, dirty, close, resize, takes text)"
                (and on-key dirty-p close-fn resize-fn text-p))
          (funcall dirty-p)
          (keys on-key (format nil "(defun term-apps-sq (x) (* x x))~%(defvar *term-apps-v* (list 1 2))~%"))
          (keys on-key (format nil "(car 5)~%(term-apps-sq~% 12)~%"))
          ;; the window's own grid is closed over; the listener's work shows in the image
          (note "the forms reached the listener (on main)"
                (until (lambda () (funcall dirty-p)
                         (and (fboundp 'cl-user::term-apps-sq) (boundp 'cl-user::*term-apps-v*)))))
          (funcall resize-fn 500 300)
          (note "resized" (and (<= (glass:fb-width fb) 500) (< 400 (glass:fb-width fb))))
          (funcall close-fn)))
      ;; The listener protocol itself, as a window speaks it: the reply text.
      (let* ((l glass-repl:*listener*)
             (slot (position nil (glass-repl::listener-owner l)))
             (token (incf (glass-repl::listener-next-token l)))
             (out '()) (done nil))
        (setf (svref (glass-repl::listener-owner l) slot) token)
        (glass-repl::listener-post l (list :eval slot token "(term-apps-sq 9) (length *term-apps-v*)"))
        (until (lambda ()
                 (dolist (m (glass::mb-take (svref (glass-repl::listener-replies l) slot)) done)
                   (case (first m)
                     (:out (push (third m) out))
                     (:done (setf done t))))))
        (setf (svref (glass-repl::listener-owner l) slot) nil)
        (note "the listener evaluated a DEFUN'd function and printed 81 and 2"
              (equal (apply #'concatenate 'string (reverse out)) (format nil "81~%2~%"))))
      ;; A shell on a pty, owned by this thread, polled -- no pump thread.
      (let ((tm (glass-term:make-terminal :cols 60 :rows 12 :ppem 12)))
        (until (lambda () (glass-term:terminal-poll tm)
                 (some (lambda (l) (search "$" l)) (glass-term:terminal-text tm))))
        (keys (lambda (d k) (glass-term:on-key tm d k)) (format nil "echo sum=$((6*7)); stty size~%"))
        (note "the shell echoes, runs, and sees its size"
              (until (lambda () (glass-term:terminal-poll tm)
                       (let ((text (glass-term:terminal-text tm)))
                         (and (some (lambda (l) (search "echo sum=$((6*7))" l)) text)
                              (some (lambda (l) (string= l "sum=42")) text)
                              (some (lambda (l) (string= l "12 60")) text))))))
        ;; ctrl and arrows: Left into the line, type there, C-e to the end, type again
        (keys (lambda (d k) (glass-term:on-key tm d k)) "echo b")
        (glass-term:on-key tm t #xff51)                                   ; Left
        (keys (lambda (d k) (glass-term:on-key tm d k)) "a")
        (glass-term:on-key tm t #xffe3) (glass-term:on-key tm t (char-code #\e)) (glass-term:on-key tm nil #xffe3)
        (keys (lambda (d k) (glass-term:on-key tm d k)) (format nil "c~%"))
        (note "arrows and C-e edit the line (abc)"
              (until (lambda () (glass-term:terminal-poll tm)
                       (some (lambda (l) (string= l "abc")) (glass-term:terminal-text tm)))))
        (glass-term:resize-terminal tm 50 10)
        (keys (lambda (d k) (glass-term:on-key tm d k)) (format nil "stty size~%"))
        (note "TIOCSWINSZ on resize"
              (until (lambda () (glass-term:terminal-poll tm)
                       (some (lambda (l) (string= l "10 50")) (glass-term:terminal-text tm)))))
        (glass-term:kill-terminal tm)
        (note "closing hangs the shell up and collects it"
              (not (sb-ext:process-alive-p (glass-term::terminal-proc tm))))))
    (glass-repl:stop-listener glass-repl:*listener*)
    (nreverse results)))

(let* ((l (glass-repl:make-listener)))
  (setf glass-repl:*listener* l)
  (let ((th (sb-thread:make-thread 'compositor :arguments (list l) :name "test-compositor")))
    (glass-repl:run-listener l)
    (let ((results (sb-thread:join-thread th)))
      (if (listp results)
          (dolist (r results) (check (car r) (cdr r)))
          (check "the compositor thread finished" nil)))))

(check "the DEFUN is in the image, callable from main" (eql (funcall 'cl-user::term-apps-sq 7) 49))
(format t "~&TERM-APPS: ~:[FAIL (~d)~;PASS~]~%" (zerop *fails*) *fails*)
(finish-output)
#+sbcl (sb-ext:exit :code (if (zerop *fails*) 0 1))
