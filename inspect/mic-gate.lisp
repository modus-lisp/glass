;;;; inspect/mic-gate.lisp — a microphone with no wire under it.
;;;;
;;;; SOURCES AND SINKS, which is the shape both directions now have:
;;;;
;;;;   out   MIXER-SUBSCRIBE -> sink -> SINK-NEXT-FRAME    audio-stream.lisp carries one away
;;;;   in    ATTACH-MIC      -> mic  -> MIC-NEXT-FRAME     mic-stream.lisp fills one from a socket
;;;;
;;;; Audio OUT has had that split since there were sinks: a sink is an in-image read cursor and
;;;; the socket is a thing that carries one somewhere.  Audio IN did not — the microphone was
;;;; DEFINED INSIDE its transport, so the only way to have one was for something to dial in and
;;;; push frames at a port.  True while the only peer was a browser behind a gateway; false the
;;;; moment the desktop grew a viewer in its own process, which has no connection to accept and
;;;; would have had to open a wire to itself for the object to exist.
;;;;
;;;; So this gate is about the object, deliberately with no socket anywhere near it: make one,
;;;; attach it, push samples in, take frames out.  hearing.lisp is not involved and did not
;;;; change, which is the test of whether the seam was in the right place — the ear asked for a
;;;; microphone all along and never asked where it came from.
;;;;
;;;;   sbcl --script inspect/mic-gate.lisp

(require :asdf)
(unless (find-package :quicklisp)
  (let ((setup (find-if #'probe-file
                        (remove nil (list (let ((e (sb-ext:posix-getenv "QUICKLISP_SETUP")))
                                            (and e (pathname e)))
                                          #p"/opt/quicklisp/setup.lisp"
                                          (merge-pathnames "quicklisp/setup.lisp"
                                                           (user-homedir-pathname)))))))
    (unless setup
      (format *error-output* "~&mic-gate: no Quicklisp.~%") (sb-ext:exit :code 1))
    (load setup)))
(let* ((here (or *load-truename* *default-pathname-defaults*))
       (root (truename (make-pathname :name nil :type nil
                                      :defaults (merge-pathnames "../../" here)))))
  (asdf:initialize-source-registry
   `(:source-registry (:tree ,root) (:exclude "vendor") (:exclude "deps") :inherit-configuration))
  (handler-bind ((warning #'muffle-warning))
    (let ((*standard-output* (make-broadcast-stream)))
      (asdf:load-asd (merge-pathnames "../glass.asd" here))
      (asdf:load-system :glass/mic))))

(defvar *fail* 0)
(defun ok (name got &optional detail)
  (if got (format t "  [pass] ~a~@[ — ~a~]~%" name detail)
      (progn (incf *fail*) (format t "  [FAIL] ~a~@[ — ~a~]~%" name detail)))
  (finish-output) got)

(format t "~&== a microphone, made in this image ==~%")
(let ((m (glass:make-mic :name "probe" :wire-rate 16000 :rate 16000)))
  (ok "one can be made without a socket" (glass:mic-p m))
  (glass:attach-mic m)
  (ok "...and attached to the session" (eq m (glass:session-mic)))

  ;; THE ONE THIS GATE FOUND.  GET-INTERNAL-REAL-TIME counts from process start, so a STAMP of
  ;; 0 — which means "no frame has ever arrived" — reads as a frame that arrived at time zero,
  ;; and that is RECENT for the first *MIC-LIVE-SECONDS* of the process.  A microphone attached
  ;; during startup was live before anything had been said into it, and a listener would have
  ;; waited on it.  Latent while every microphone came from a socket on a box that had been up
  ;; for hours; immediate for one a viewer attaches as the desktop boots.
  (ok "a microphone that has never spoken is not live" (null (glass:mic-live-p m))
      "open is not the same as live, and the clock alone cannot tell them apart at startup")

  (let ((pcm (make-array 320 :element-type '(signed-byte 16) :initial-element 1000)))
    (dotimes (i 4) (glass:mic-push m pcm)))
  (ok "...and is, once something is pushed into it" (glass:mic-live-p m))
  (let ((f (glass:mic-next-frame m)))
    (ok "a whole frame comes back out" (and f (= 320 (length f)))
        (format nil "~a samples" (and f (length f)))))

  ;; the ear's contract, which is reed's: a thunk that never blocks
  (let ((src (glass:mic-source m)))
    (ok "MIC-SOURCE is a thunk — reed's source contract, which is the ear's"
        (functionp src))
    (ok "...and it never blocks when there is nothing" (progn (funcall src) t)))

  (glass:detach-mic m)
  (ok "detaching leaves the session with no microphone" (null (glass:session-mic)))

  ;; the guard that matters when two producers overlap
  (let ((a (glass:make-mic :name "a")) (b (glass:make-mic :name "b")))
    (glass:attach-mic a) (glass:attach-mic b)
    (glass:detach-mic a)
    (ok "a retiring producer cannot unhook a newer microphone" (eq b (glass:session-mic))
        "the socket server relies on this when one connection replaces another")
    (glass:detach-mic b)))

(format t "~&=> ~:[FAIL~;PASS~]~%" (zerop *fail*))
(finish-output)
(sb-ext:exit :code (if (plusp *fail*) 1 0))
