;;;; control.lisp — the wire you poke a RUNNING desktop with.
;;;;
;;;; A desktop runs for weeks.  Everything interesting about it — how many seats it has,
;;;; where a window sits, what the perf counters say — is a form away, and the way to say
;;;; that form to a process that is already running is a socket which reads one, evaluates
;;;; it, and writes back what came out.  Four launchers had grown their own copy of that
;;;; loop (serve-desktop, scroll-desktop, remote-desktop, and warren's desktop-5903), which
;;;; is how all four came to share one bug.
;;;;
;;;; THE BUG, because it is the whole reason this file exists: the READ sat inside the
;;;; accept loop's blanket `(handler-case … (error () nil))' and OUTSIDE the one that
;;;; reports.  Only (EVAL FORM) was wrapped in the reporting handler, so an eval error came
;;;; back as `ERROR: …' and a READ error came back as NOTHING AT ALL — the connection simply
;;;; closed.  A form naming a symbol in the wrong package (`glass:seat-name', when it is
;;;; `clim-glass:seat-name') is a read error, and the answer to it was silence, which reads
;;;; exactly like `this desktop has no seats'.  It cost an operator two wrong conclusions
;;;; about a live desktop before anybody suspected the wire.
;;;;
;;;; So: A CONTROL SOCKET THAT CANNOT READ SOMETHING MUST SAY SO.  Reading is the half most
;;;; likely to fail, because it is the half that meets what a person typed.
;;;;
;;;; The kind of wire is the caller's choice, exactly as a seat's transport is: :PATH gets a
;;;; socket file only its owner can open (and, with GLASS:*PEER-POLICY*, only a process of
;;;; our own uid), :PORT gets a TCP listener on loopback.  This is an UNAUTHENTICATED EVAL
;;;; either way — whoever reaches it runs Lisp in this image — so the file is the one to
;;;; prefer, and the port binds 127.0.0.1 and nothing else, because binding it anywhere else
;;;; would be handing the box away.  src/socket.lisp says why 127.0.0.1 is not itself an
;;;; access control.

(in-package #:clim-glass)

(defun control-answer (stream)
  "Read ONE form from STREAM, evaluate it, and return what to say back as a string — or
   NIL if the stream held nothing at all, which is the one case that deserves silence.

   Separate from the socket so the question can be asked without one: hand it a string
   stream and it is the whole semantics of a control connection, reader included.

   The form is read HERE and not by the caller, because reading is what fails: a symbol
   in a package this image does not have, an unbalanced parenthesis, a stray `#'.  All of
   it comes back as text on the wire."
  (let ((*package* (find-package :clim-glass)))
    (multiple-value-bind (form read-error)
        (handler-case (values (read stream nil nil) nil)
          (error (e) (values nil e)))
      (cond (read-error (format nil "ERROR: unreadable form: ~a" read-error))
            ((null form) nil)
            (t
             ;; WHAT THE FORM PRINTS COMES BACK TOO, which is what a REPL does and what
             ;; anybody typing at this expects.  Without it, every reporting function in
             ;; the image answers a control connection with its return value while the
             ;; interesting part goes to the session log: (cl-transport.gate:report)
             ;; replied `5\' -- a true and useless statement of how many lines it had
             ;; just written somewhere else.
             ;;
             ;; Only *STANDARD-OUTPUT*.  Warnings and backtraces belong in the log
             ;; whether or not somebody is holding this socket open, and folding them
             ;; into the answer would make an unrelated warning look like the reply.
             (let ((printed (make-string-output-stream)))
               (handler-case
                   (let* ((value (let ((*standard-output* printed)) (eval form)))
                          (text (get-output-stream-string printed))
                          (shown (princ-to-string value)))
                     (if (plusp (length text))
                         ;; Value last, as a REPL does -- and separated, so a caller
                         ;; reading only the final line still gets the value.
                         (format nil "~a~:[~;~%~]~a" text
                                 (char/= (char text (1- (length text))) #\Newline)
                                 shown)
                         shown))
                 (error (e)
                   ;; Anything already printed is part of the story of the failure.
                   (let ((text (get-output-stream-string printed)))
                     (format nil "~@[~a~%~]ERROR: ~a"
                             (and (plusp (length text)) text) e))))))))))

(defun start-control-socket (&key port path (name "glass-control"))
  "Serve a read-eval-print wire onto this image: one form per connection, evaluated in the
   CLIM-GLASS package, its printed value written back with a newline.  Returns the thread.

     echo '(length (glass-port-seats (find-glass-port :port 5901)))' | nc -q1 127.0.0.1 4009
     echo '(mapcar (function seat-name) (glass-port-seats *p*))' | nc -q1 -U ~/.glass/run/seat-0.control

   PATH opens a UNIX socket there; PORT opens a TCP listener on 127.0.0.1 (and only there —
   see the header).  One or the other.

   EVERY FAILURE ANSWERS.  A form that will not READ comes back as `ERROR: unreadable form:
   …' and one that will not EVAL as `ERROR: …', because the failure mode this replaces was a
   connection that closed without a byte and an operator who read that silence as an answer
   about the desktop.  Only a connection that sends nothing at all gets nothing back.

   The reply goes out as soon as ONE form has been read, not at end of input: a client that
   holds its half of the connection open (`nc' does, unless told otherwise) must not be made
   to wait for a timeout to hear an answer it has already asked for."
  (let ((listen (if path
                    (glass:open-listener :unix :path path)
                    (glass:open-listener :tcp :port port :address "127.0.0.1"))))
    (values
     (sb-thread:make-thread
      (lambda ()
        (loop
          (handler-case
              (let ((s (glass:accept-stream listen :element-type 'character :buffering :full)))
                (when s
                  (unwind-protect
                       (let ((answer (control-answer s)))
                         (when answer
                           (write-string answer s) (terpri s) (force-output s)))
                    (ignore-errors (close s)))))
            ;; A dead connection is not an event.  Anything unexpected here must not end
            ;; this thread either: a desktop whose control socket has quietly stopped
            ;; accepting is a desktop nobody can ask anything.
            (error () nil))))
      :name name)
     listen)))
