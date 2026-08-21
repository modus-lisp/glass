;;;; repl.lisp — the terminal, with a Lisp listener on the other end.
;;;;
;;;; glass/term's grid, VT parser and scribe rendering are ~780 lines of portable
;;;; Common Lisp; the only Unix in the file is a pty, a shell and two ioctls.
;;;; That shell was always the accident.  A terminal on a Lisp machine has no
;;;; process to talk to — there are no processes — so what it talks to is the
;;;; image it is already inside.
;;;;
;;;; TERMINAL-PTY is a bidirectional character stream and nothing else, so this
;;;; needs no change to the terminal: a stream pair, a listener on the far end,
;;;; and the same grid draws it.
;;;;
;;;;   (glass-repl:make-repl-terminal :cols 80 :rows 24)
;;;;
;;;; No pty, no fork, no shell, no FFI.

(in-package #:glass-repl)

;;; ---- a stream pair, in memory ----------------------------------------------

(defstruct (chanq (:constructor %make-chanq))
  (buf (make-array 256 :element-type 'character :adjustable t :fill-pointer 0))
  (pos 0)
  (open t)
  (lock (sb-thread:make-mutex :name "glass-repl-q"))
  (cv (sb-thread:make-waitqueue :name "glass-repl-q")))

(defun chanq-push (q string)
  (sb-thread:with-mutex ((chanq-lock q))
    (loop for c across string do (vector-push-extend c (chanq-buf q)))
    (sb-thread:condition-broadcast (chanq-cv q))))

(defun chanq-close (q)
  "Stop the queue and wake anyone waiting on it."
  (sb-thread:with-mutex ((chanq-lock q))
    (setf (chanq-open q) nil)
    (sb-thread:condition-broadcast (chanq-cv q))))

(defun chanq-pop (q &key (wait t))
  "One character, or NIL when nothing is buffered and WAIT is false."
  (sb-thread:with-mutex ((chanq-lock q))
    (loop
      (when (< (chanq-pos q) (fill-pointer (chanq-buf q)))
        (let ((c (aref (chanq-buf q) (chanq-pos q))))
          (incf (chanq-pos q))
          ;; Compact once the read cursor has run away, so a long session does
          ;; not keep every character it ever saw.
          (when (and (> (chanq-pos q) 4096)
                     (= (chanq-pos q) (fill-pointer (chanq-buf q))))
            (setf (fill-pointer (chanq-buf q)) 0 (chanq-pos q) 0))
          (return c)))
      (unless (and wait (chanq-open q)) (return nil))
      ;; No :TIMEOUT here, deliberately.  SBCL's CONDITION-WAIT returns from a
      ;; timeout WITHOUT having reacquired the mutex, so the next iteration
      ;; signals "the current thread is not holding..." -- which is exactly how
      ;; the first version of this died.  Closing the queue broadcasts, so a
      ;; waiter has something to be woken by and needs no poll.
      (sb-thread:condition-wait (chanq-cv q) (chanq-lock q)))))

(defclass pipe-end (sb-gray:fundamental-character-input-stream
                    sb-gray:fundamental-character-output-stream)
  ((in :initarg :in :reader pipe-in)
   (out :initarg :out :reader pipe-out)
   (column :initform 0 :accessor pipe-column)
   ;; A terminal has no tty discipline behind it, so nothing turns #\Newline
   ;; into carriage-return + line-feed the way ONLCR would.  Without that the
   ;; cursor drops a line and keeps its column, and the screen stair-steps.  The
   ;; listener's end translates on the way out; the terminal's end must not, or
   ;; a typed Return would arrive as two characters.
   (crlf :initarg :crlf :initform nil :reader pipe-crlf-p)))

(defmethod sb-gray:stream-read-char ((s pipe-end))
  (or (chanq-pop (pipe-in s)) :eof))

(defmethod sb-gray:stream-read-char-no-hang ((s pipe-end))
  ;; PUMP polls with READ-CHAR-NO-HANG, so this is the method that actually
  ;; carries the listener's output to the screen.
  (chanq-pop (pipe-in s) :wait nil))

(defmethod sb-gray:stream-write-char ((s pipe-end) c)
  (chanq-push (pipe-out s)
              (if (and (pipe-crlf-p s) (char= c #\Newline))
                  (format nil "~c~c" #\Return #\Newline)
                  (string c)))
  (setf (pipe-column s) (if (char= c #\Newline) 0 (1+ (pipe-column s))))
  c)

(defmethod sb-gray:stream-write-string ((s pipe-end) string &optional (start 0) end)
  (let ((sub (subseq string start (or end (length string)))))
    (chanq-push (pipe-out s)
                (if (pipe-crlf-p s)
                    (with-output-to-string (o)
                      (loop for c across sub
                            do (when (char= c #\Newline) (write-char #\Return o))
                               (write-char c o)))
                    sub))
    (let ((nl (position #\Newline sub :from-end t)))
      (setf (pipe-column s) (if nl (- (length sub) nl 1) (+ (pipe-column s) (length sub))))))
  string)

(defmethod sb-gray:stream-line-column ((s pipe-end)) (pipe-column s))
(defmethod sb-gray:stream-force-output ((s pipe-end)) nil)
(defmethod sb-gray:stream-finish-output ((s pipe-end)) nil)

(defun make-pipe-pair ()
  "Two ends of one pipe: whatever is written to either is readable from the other."
  (let ((a (%make-chanq)) (b (%make-chanq)))
    (values (make-instance 'pipe-end :in a :out b)                 ; terminal's end
            (make-instance 'pipe-end :in b :out a :crlf t))))      ; listener's end

;;; ---- the listener ------------------------------------------------------------

(defparameter *banner*
  "glass listener — Common Lisp.  No shell here; this is the image you are in.~%~%")

(defun prompt (out package)
  (format out "~a> " (package-name package))
  (force-output out))

(defun incomplete-form-p (text)
  "True when TEXT reads as the START of a form rather than a whole one — an open
   paren or an unterminated string.  A listener should wait for the rest instead
   of complaining, which is what makes it usable for anything longer than a line."
  (handler-case (progn (read-from-string text) nil)
    (end-of-file () t)
    (error () nil)))

(defun eval-and-print (text out package-holder)
  "READ everything in TEXT, evaluate it, print the values.  A reader or evaluator
   error is reported and swallowed: a listener that dies on a typo is not one."
  (handler-case
      (let ((*package* (car package-holder))
            (pos 0))
        (loop
          (multiple-value-bind (form next)
              (read-from-string text nil :eof :start pos)
            (when (eq form :eof) (return))
            (setf pos next)
            (let ((values (multiple-value-list (eval form))))
              (if values
                  (dolist (v values) (format out "~&~s~%" v))
                  (format out "~&; No values~%")))))
        ;; IN-PACKAGE inside the form should stick for the next prompt.
        (setf (car package-holder) *package*))
    (error (e) (format out "~&;; ~a~%" e))
    (sb-sys:interactive-interrupt () (format out "~&;; interrupted~%"))))

(defun listener-loop (stream)
  "Read characters, echo them, evaluate a line at a time.

   The line editing is done here rather than by a tty discipline, because there
   is no tty: characters arrive exactly as the RFB client sent them."
  (let ((line (make-array 64 :element-type 'character :adjustable t :fill-pointer 0))
        (package-holder (list (find-package :cl-user))))
    (format stream *banner*)
    (prompt stream (car package-holder))
    (loop
      (let ((c (read-char stream nil :eof)))
        (when (eq c :eof) (return))
        (cond
          ((or (char= c #\Return) (char= c #\Newline))
           (write-char #\Newline stream)
           (cond
             ((zerop (length line)) (prompt stream (car package-holder)))
             ((incomplete-form-p (coerce line 'string))
              ;; Keep the text and ask for more, the way every listener does.
              (vector-push-extend #\Newline line)
              (write-string "  " stream)
              (force-output stream))
             (t
              (eval-and-print (coerce line 'string) stream package-holder)
              (setf (fill-pointer line) 0)
              (prompt stream (car package-holder)))))
          ;; Backspace / delete: rub the character out on screen as well as in
          ;; the buffer, which is the tty discipline's job on a real terminal.
          ((or (char= c #\Backspace) (char= c (code-char 127)))
           (when (plusp (length line))
             (decf (fill-pointer line))
             (write-string (format nil "~c ~c" #\Backspace #\Backspace) stream)
             (force-output stream)))
          ((char= c (code-char 12))            ; C-l
           (write-string (format nil "~c[2J~c[H" #\Escape #\Escape) stream)
           (setf (fill-pointer line) 0)
           (prompt stream (car package-holder)))
          ((char= c (code-char 3))             ; C-c: abandon the line
           (format stream "~%")
           (setf (fill-pointer line) 0)
           (prompt stream (car package-holder)))
          ((graphic-char-p c)
           (vector-push-extend c line)
           (write-char c stream)
           (force-output stream))
          (t nil))))))

(defun make-repl-terminal (&key (cols 80) (rows 24) (ppem 16) emoji-font)
  "A terminal whose other end is a Lisp listener in this image.

   Returns the terminal; the listener runs on its own thread.  Everything the
   terminal does — the grid, the parser, scribe's rendering, the RFB input path —
   is exactly what the shell version does, because the only thing that changed is
   what is on the far end of the stream."
  (multiple-value-bind (term-end listener-end) (make-pipe-pair)
    (let ((tm (glass-term:make-terminal :cols cols :rows rows :ppem ppem
                                        :emoji-font emoji-font
                                        :pty term-end)))
      (sb-thread:make-thread
       (lambda ()
         (unwind-protect (handler-case (listener-loop listener-end)
                           (error (e)
                             (ignore-errors
                              (format listener-end "~&;; listener stopped: ~a~%" e))))
           (chanq-close (pipe-in listener-end))
           (chanq-close (pipe-out listener-end))))
       :name "glass-listener")
      tm)))
