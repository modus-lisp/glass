;;;; test/term-scrollback-test.lisp — glass/term keeps the rows that scroll off the main screen,
;;;; Shift+PgUp/PgDn and the wheel page through them, output and typing return to the live screen,
;;;; and the alternate screen adds nothing.
;;;;   sbcl --non-interactive --load test/term-scrollback-test.lisp   (exits 0 iff every check passes)
;;;; (with glass and scribe findable by ASDF; modus: modus --script test/term-scrollback-test.lisp)

(require :asdf)
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream))) (asdf:load-system :glass/term)))

(defpackage #:glass-term-scrollback-test (:use #:cl))
(in-package #:glass-term-scrollback-test)

(defvar *fails* 0)
(defun check (name ok)
  (format t "~&TERM-SCROLLBACK ~:[FAIL~;ok  ~] ~a~%" ok name)
  (unless ok (incf *fails*)))

(defun row-text (chars cols y)
  (string-right-trim " " (map 'string (lambda (c) (let ((k (glass-term::ch-code c)))
                                                     (if (< k 32) #\Space (code-char k))))
                              (subseq chars (* y cols) (* (1+ y) cols)))))

(defun screen (tm)
  "The rows as the window shows them now."
  (multiple-value-bind (ch co) (if (plusp (glass-term::terminal-view tm))
                                   (glass-term::view-grid tm)
                                   (values (glass-term::terminal-chars tm) nil))
    (declare (ignore co))
    (loop for y below (glass-term::terminal-rows tm)
          collect (row-text ch (glass-term::terminal-cols tm) y))))

(let* ((sent '())
       (tm (glass-term::make-terminal :cols 20 :rows 5 :ppem 14 :nerd-font nil
                                      :pty (lambda (s) (push s sent)))))
  (flet ((feed (s) (loop for c across s do (glass-term::feed-cp tm (char-code c))))
         (key (sym &optional (down t)) (glass-term::on-key tm down sym))
         (wheel (mask) (glass-term::on-mouse tm mask 10 10) (glass-term::on-mouse tm 0 10 10)))
    (dotimes (i 12) (feed (format nil "line ~d~c~c" i #\Return #\Newline)))
    ;; 12 lines and the cursor's empty line in 5 rows: lines 0..7 scrolled off
    (check "the rows that scrolled off are kept" (= (glass-term::terminal-hist-count tm) 8))
    (check "the live screen shows the last lines" (equal (first (screen tm)) "line 8"))
    (key #xffe1) (key #xff55)                         ; Shift down, PgUp
    (check "Shift+PgUp goes back a page (rows - 1)" (= (glass-term::terminal-view tm) 4))
    (check "and shows the older rows" (equal (first (screen tm)) "line 4"))
    (check "Shift+PgUp sent nothing to the shell" (null sent))
    (key #xff55) (key #xff55)
    (check "it stops at the oldest row" (= (glass-term::terminal-view tm) 8))
    (check "which is line 0" (equal (first (screen tm)) "line 0"))
    (key #xff56)
    (check "Shift+PgDn comes forward" (= (glass-term::terminal-view tm) 4))
    (key #xffe1 nil)                                  ; Shift up
    (key (char-code #\a))
    (check "typing returns to the live screen" (zerop (glass-term::terminal-view tm)))
    (check "and the key went to the shell" (equal sent '("a")))
    (wheel 8) (wheel 8)
    (check "the wheel scrolls back three rows a notch" (= (glass-term::terminal-view tm) 6))
    (wheel 16)
    (check "and forward" (= (glass-term::terminal-view tm) 3))
    ;; rendering a scrolled view is the same as a full redraw of it, and hides the cursor
    (glass-term::render tm)
    (let ((partial (copy-seq (glass:fb-pixels (glass-term::terminal-fb tm)))))
      (setf (glass-term::terminal-drawn-chars tm) nil)
      (glass-term::render tm)
      (check "a scrolled render equals a full redraw"
             (equalp partial (glass:fb-pixels (glass-term::terminal-fb tm)))))
    (feed "x")
    (check "output returns to the live screen" (zerop (glass-term::terminal-view tm)))
    ;; the alternate screen keeps no history and the wheel does not scroll it
    (let ((n (glass-term::terminal-hist-count tm)))
      (feed (format nil "~c[?1049h" (code-char 27)))
      (dotimes (i 10) (feed (format nil "alt ~d~c~c" i #\Return #\Newline)))
      (check "the alternate screen adds no scrollback" (= n (glass-term::terminal-hist-count tm)))
      (wheel 8)
      (check "the wheel does not scroll the alternate screen" (zerop (glass-term::terminal-view tm)))
      (feed (format nil "~c[?1049l" (code-char 27))))
    ;; a narrower terminal still shows the old, wider rows (cut)
    (glass-term::resize-terminal tm 8 5)
    (glass-term::scroll-view tm 20)
    (check "after a resize the old rows still show, cut to the width"
           (every (lambda (r) (<= (length r) 8)) (screen tm)))
    ;; the ring is bounded
    (let ((tm2 (glass-term::make-terminal :cols 10 :rows 3 :ppem 14 :nerd-font nil
                                          :pty (lambda (s) s))))
      (setf (glass-term::terminal-hist-max tm2) 5)
      (dotimes (i 20) (loop for c across (format nil "r~d~c~c" i #\Return #\Newline)
                            do (glass-term::feed-cp tm2 (char-code c))))
      (glass-term::scroll-view tm2 100)
      (check "a full ring keeps the newest HIST-MAX rows"
             (and (= (glass-term::terminal-hist-count tm2) 5)
                  (equal (first (screen tm2)) "r13"))))))

(format t "~&TERM-SCROLLBACK ~:[FAILED (~d)~;all passed~*~]~%" (zerop *fails*) *fails*)
(uiop:quit (if (zerop *fails*) 0 1))
