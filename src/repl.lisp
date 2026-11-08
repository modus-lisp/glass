;;;; repl.lisp — a Lisp listener in a terminal window: the image you are in, not a shell.
;;;;
;;;; glass/term's grid, VT parser and scribe rendering are portable Common Lisp; the pty was
;;;; only ever the far end.  Here the far end is a LISTENER: a thread that READs, EVALs and
;;;; PRINTs, and a terminal window that edits a line and shows what comes back.
;;;;
;;;; SHARE-NOTHING, BECAUSE MODUS IS.  The window -- its grid, its line editor, its history -- is
;;;; owned by whichever thread runs the app (SERVE-WM's compositor) and nothing else touches it.
;;;; The listener is a different thread, the image's MAIN one, and the two speak only by
;;;; messages through mailboxes the main thread made:
;;;;
;;;;     window  --(:eval slot token text)-->  listener      a form per Enter
;;;;     window  <--(:out token string)------  listener      output as it is written
;;;;     window  <--(:done token package)----  listener      finished: prompt again
;;;;
;;;; WHY MAIN.  On modus a thread other than main may not store its own objects anywhere another
;;;; thread can reach, and a DEFUN, a DEFVAR or a SETQ of a global is exactly such a store -- so
;;;; is the compiler's own memo inside EVAL.  A listener on the compositor could evaluate almost
;;;; nothing.  On main everything it makes is shared by construction.  It also means a runaway
;;;; form (an infinite loop) stops the listener, never the desktop: the compositor and the
;;;; network keep running, the window says it is busy, and Ctrl-C interrupts the evaluation
;;;; where the Lisp can interrupt a thread.
;;;;
;;;; With no listener running (*LISTENER* NIL) a window evaluates in place, on its own thread:
;;;; right on SBCL or on modus's main thread, and an honest error elsewhere.

(in-package #:glass-repl)

(defconstant +slots+ 8 "How many REPL windows one listener serves at once.")

(defvar *listener* nil
  "The listener REPL-APP windows evaluate on, or NIL to evaluate in place.  Set (not bound: the
   windows are on another thread) by the host that runs RUN-LISTENER -- SERVE-WM.")

(defstruct (listener (:constructor %make-listener))
  requests                ; mailbox: (:eval slot token text) (:open slot token) (:stop)
  wake                    ; the listener sleeps on it
  replies                 ; per slot: the mailbox its window reads
  owner                   ; per slot: the claiming window's token, or NIL (windows write it)
  taken                   ; per slot: how many :OUT messages the window has consumed
  posted                  ; per slot: how many the listener has sent (listener writes it)
  packages                ; per slot: the session's *PACKAGE* name (listener writes it)
  (next-token 0)          ; the windows' token counter (only the windows' thread writes it)
  thread                  ; the listener's thread: what Ctrl-C interrupts
  (busy nil))             ; the slot being evaluated, or NIL

(defun make-listener ()
  "A listener, on the thread that will run it (RUN-LISTENER) -- on modus, MAIN, which is the
   thread whose objects every other thread may see: every mailbox and vector a window touches
   is made here."
  (flet ((vec (x) (make-array +slots+ :initial-element x)))
    (let ((replies (make-array +slots+)))
      (dotimes (i +slots+) (setf (svref replies i) (glass::make-rfb-mailbox)))
      (%make-listener :requests (glass::make-rfb-mailbox) :wake (glass:make-wake)
                      :replies replies :owner (vec nil) :taken (vec 0) :posted (vec 0)
                      :packages (vec "COMMON-LISP-USER")
                      :thread sb-thread:*current-thread*))))

(defun listener-post (l msg)
  "MSG to the listener, waking it."
  (glass::mb-post (listener-requests l) msg)
  (glass:wake-signal (listener-wake l)))

(defun stop-listener (l) (listener-post l (list :stop)))

;;; ---- evaluation (the listener's side) -----------------------------------------------------

(defvar *in-eval* nil "True while the listener is inside a user's form: an interrupt may unwind it.")

(defun %interrupt-eval ()
  "Run IN the listener thread by INTERRUPT-THREAD: abandon the form being evaluated."
  (when *in-eval* (throw '%interrupt '%interrupted)))

(defun evaluate-text (text package-name out)
  "READ every form in TEXT, EVAL it and PRINT its values to OUT, in the package named
   PACKAGE-NAME (else CL-USER).  Nothing escapes: a reader or evaluation error, a stack
   exhaustion, anything SERIOUS, is printed and the next prompt follows -- a listener that dies
   on a typo is not one.  Warnings are printed and muffled.  Returns the package name after (an
   IN-PACKAGE sticks)."
  (let* ((in (make-string-input-stream ""))
         (io (make-two-way-stream in out))
         (*standard-output* out) (*error-output* out) (*trace-output* out)
         (*standard-input* in) (*terminal-io* io) (*query-io* io) (*debug-io* io)
         (*package* (or (find-package package-name) (find-package "COMMON-LISP-USER"))))
    (let ((how (catch '%interrupt
                 (let ((*in-eval* t))
                   (handler-case
                       (handler-bind ((warning
                                        (lambda (w)
                                          (ignore-errors (format out "~&; warning: ~a~%" w))
                                          (let ((r (find-restart 'muffle-warning w)))
                                            (when r (invoke-restart r))))))
                         (let ((pos 0))
                           (loop
                             (multiple-value-bind (form next)
                                 (read-from-string text nil '%eof :start pos)
                               (when (eq form '%eof) (return))
                               (setf pos next)
                               (let ((vals (multiple-value-list (eval form))))
                                 (setf +++ ++ ++ + + form
                                       /// // // / / vals
                                       *** ** ** * * (first vals))
                                 (fresh-line out)
                                 (if vals
                                     (dolist (v vals) (format out "~s~%" v))
                                     (format out "; No values~%")))))))
                     (serious-condition (e)
                       (ignore-errors
                        (format out "~&; ~(~a~): ~a~%" (type-of e)
                                (or (ignore-errors (princ-to-string e)) "(unprintable)"))))))
                 nil)))
      (when (eq how '%interrupted) (ignore-errors (format out "~&; interrupted~%"))))
    (package-name *package*)))

;;; Output, as it is written: a character stream whose buffer goes to the window in chunks.

(defclass reply-stream (sb-gray:fundamental-character-output-stream)
  ((listener :initarg :listener :reader rs-listener)
   (slot :initarg :slot :reader rs-slot)
   (token :initarg :token :reader rs-token)
   (buf :initform (make-array 256 :element-type 'character :adjustable t :fill-pointer 0)
        :reader rs-buf)
   (column :initform 0 :accessor rs-column)
   (sent :initform (get-internal-real-time) :accessor rs-sent)))

(defun %rs-flush (s)
  "Send what is buffered to the window -- unless the window has gone (or another has taken its
   slot), when it is dropped.  Holds back while the window is more than a screenful of messages
   behind, so a form printing forever cannot queue the image's memory away."
  (let* ((l (rs-listener s)) (slot (rs-slot s)) (buf (rs-buf s)))
    (when (plusp (length buf))
      (when (eql (svref (listener-owner l) slot) (rs-token s))
        (loop while (and (eql (svref (listener-owner l) slot) (rs-token s))
                         (> (- (svref (listener-posted l) slot) (svref (listener-taken l) slot)) 64))
              do (sleep 1/100))
        (incf (svref (listener-posted l) slot))
        (glass::mb-post (svref (listener-replies l) slot)
                        (list :out (rs-token s) (coerce buf 'simple-string))))
      (setf (fill-pointer buf) 0)
      (setf (rs-sent s) (get-internal-real-time)))))

(defmethod sb-gray:stream-write-char ((s reply-stream) c)
  (vector-push-extend c (rs-buf s))
  (setf (rs-column s) (if (char= c #\Newline) 0 (1+ (rs-column s))))
  (when (or (>= (length (rs-buf s)) 4096)
            (and (char= c #\Newline)
                 (> (- (get-internal-real-time) (rs-sent s))
                    (floor internal-time-units-per-second 30))))
    (%rs-flush s))
  c)

(defmethod sb-gray:stream-line-column ((s reply-stream)) (rs-column s))
(defmethod sb-gray:stream-force-output ((s reply-stream)) (%rs-flush s) nil)
(defmethod sb-gray:stream-finish-output ((s reply-stream)) (%rs-flush s) nil)

(defun %listener-eval (l slot token text)
  (let ((out (make-instance 'reply-stream :listener l :slot slot :token token)))
    (setf (listener-busy l) slot)
    (unwind-protect
         (setf (svref (listener-packages l) slot)
               (evaluate-text text (svref (listener-packages l) slot) out))
      (setf (listener-busy l) nil)
      (%rs-flush out)
      (glass::mb-post (svref (listener-replies l) slot)
                      (list :done token (svref (listener-packages l) slot))))))

(defun run-listener (l)
  "Serve REPL windows from this thread until STOP-LISTENER: evaluate each form a window sends,
   one at a time, in the order sent."
  (loop
    (dolist (m (glass::mb-take (listener-requests l)))
      (case (first m)
        (:stop (return-from run-listener nil))
        (:open (destructuring-bind (slot token) (rest m)
                 (setf (svref (listener-packages l) slot) "COMMON-LISP-USER")
                 (glass::mb-post (svref (listener-replies l) slot)
                                 (list :done token "COMMON-LISP-USER"))))
        (:eval (destructuring-bind (slot token text) (rest m)
                 (when (eql (svref (listener-owner l) slot) token)
                   (%listener-eval l slot token text))))))
    (glass::wake-wait (listener-wake l) 1/4)))

(defun listener-interrupt (l)
  "Abandon the form L is evaluating.  NIL if this Lisp cannot interrupt a thread."
  (handler-case (progn (sb-thread:interrupt-thread (listener-thread l) #'%interrupt-eval) t)
    (error () nil)))

;;; ---- the window (the app's side) ----------------------------------------------------------

(defun form-complete-p (text)
  "Does TEXT hold at least one whole form -- every paren closed, no string or #| comment left
   open?  A scan, not the reader: the window's thread must not intern the symbols it is shown
   (on modus it could not), and an unfinished form is only ever a question of brackets."
  (let ((depth 0) (any nil) (i 0) (n (length text)))
    (loop
      (when (>= i n) (return (and any (<= depth 0))))
      (let ((c (char text i)))
        (cond
          ((char= c #\;) (loop while (and (< i n) (char/= (char text i) #\Newline)) do (incf i)))
          ((and (char= c #\#) (< (1+ i) n) (char= (char text (1+ i)) #\|))
           (let ((nest 1)) (incf i 2)
             (loop (when (>= i n) (return-from form-complete-p nil))
                   (cond ((and (char= (char text i) #\|) (< (1+ i) n) (char= (char text (1+ i)) #\#))
                          (incf i) (when (zerop (decf nest)) (return)))
                         ((and (char= (char text i) #\#) (< (1+ i) n) (char= (char text (1+ i)) #\|))
                          (incf i) (incf nest)))
                   (incf i))))
          ((and (char= c #\#) (< (1+ i) n) (char= (char text (1+ i)) #\\))
           (setf any t) (incf i 2))                         ; #\x: the next character is data
          ((char= c #\") (setf any t)
           (incf i)
           (loop (when (>= i n) (return-from form-complete-p nil))
                 (case (char text i) (#\\ (incf i)) (#\" (return)))
                 (incf i)))
          ((char= c #\|) (setf any t)
           (incf i)
           (loop (when (>= i n) (return-from form-complete-p nil))
                 (case (char text i) (#\\ (incf i)) (#\| (return)))
                 (incf i)))
          ((char= c #\\) (setf any t) (incf i))
          ((char= c #\() (setf any t) (incf depth))
          ((char= c #\)) (decf depth))
          ((not (member c '(#\Space #\Tab #\Newline #\Return))) (setf any t))))
      (incf i))))

(defparameter *banner*
  "glass listener -- Common Lisp, in the image the desktop runs in.  Ctrl-C interrupts.~%")

(defparameter *app-ppem* 14 "REPL-APP's text size.")

(defun %short-package (name)
  (let ((p (find-package name)))
    (or (and p (first (sort (copy-list (cons (package-name p) (package-nicknames p))) #'<
                            :key #'length)))
        name)))

(defun repl-app (fb)
  "Lisp: a listener in a terminal window, as a glass application (the contract glass.wm calls:
   FB in, (values ON-KEY ON-POINTER DIRTY-P COPY-P CLOSE-FN RESIZE-FN TEXT-P) out).  A line
   editor (Left/Right, Home/End, Up/Down history, Backspace/Delete, C-a C-e C-k C-u C-l) and a
   form per Enter -- or more lines, while brackets are open -- evaluated by *LISTENER*."
  (multiple-value-bind (cw ch) (glass-term::%cell-metrics (car (glass-term:term-fonts)) *app-ppem*)
    (let* ((tm (glass-term:make-terminal :cols (max 10 (floor (glass:fb-width fb) cw))
                                         :rows (max 2 (floor (glass:fb-height fb) ch))
                                         :ppem *app-ppem* :fb fb
                                         :pty (lambda (s) (declare (ignore s)) nil)))
           (l *listener*)
           (slot (and l (position nil (listener-owner l))))
           (token (when slot
                    (let ((tk (incf (listener-next-token l))))
                      (setf (svref (listener-owner l) slot) tk)
                      tk)))
           (line (make-array 80 :element-type 'character :adjustable t :fill-pointer 0))
           (cursor 0) (pending "") (history '()) (hpos -1) (saved "")
           (busy nil) (typeahead '()) (ctrl nil) (package "CL-USER") (closed nil))
      (labels ((out (s) (glass-term:terminal-write tm s))
               (prompt-string () (if (plusp (length pending)) "  " (format nil "~a> " package)))
               (redraw ()
                 ;; the line again, from the carriage: prompt, text, clear the rest, cursor back
                 (out (format nil "~c~a~a~c[K" #\Return (prompt-string) line #\Escape))
                 (when (< cursor (length line))
                   (out (format nil "~c[~dD" #\Escape (- (length line) cursor)))))
               (prompt () (out (prompt-string)))
               (set-line (s)
                 (setf (fill-pointer line) 0)
                 (loop for c across s do (vector-push-extend c line))
                 (setf cursor (length line)) (redraw))
               (insert (c)
                 (if (= cursor (length line))
                     (progn (vector-push-extend c line) (incf cursor) (out (string c)))
                     (let ((rest (subseq line cursor)))
                       (setf (fill-pointer line) cursor)
                       (vector-push-extend c line)
                       (loop for r across rest do (vector-push-extend r line))
                       (incf cursor) (redraw))))
               (delete-at (k)
                 (when (< -1 k (length line))
                   (let ((rest (subseq line (1+ k))))
                     (setf (fill-pointer line) k)
                     (loop for r across rest do (vector-push-extend r line))
                     (when (> cursor k) (decf cursor))
                     (redraw))))
               (done (pkg)
                 (setf busy nil package (%short-package pkg))
                 (prompt)
                 (let ((keys (reverse typeahead)))
                   (setf typeahead '())
                   (dolist (k keys) (key t k))))
               (submit ()
                 (let ((text (concatenate 'string pending line)))
                   (setf (fill-pointer line) 0 cursor 0 hpos -1)
                   (out (string #\Newline))
                   (cond
                     ((every (lambda (c) (member c '(#\Space #\Tab #\Newline))) text)
                      (setf pending "") (prompt))
                     ((not (form-complete-p text))
                      (setf pending (concatenate 'string text (string #\Newline))) (prompt))
                     (t
                      (setf pending "")
                      (push (substitute #\Space #\Newline text) history)
                      (setf busy t)
                      (if l
                          (listener-post l (list :eval slot token text))
                          ;; no listener: evaluate here, now
                          (let ((s (make-string-output-stream)))
                            (let ((pkg (evaluate-text text package s)))
                              (out (get-output-stream-string s))
                              (done pkg))))))))
               (interrupt ()
                 (setf typeahead '())
                 (when (and l (eql (listener-busy l) slot))
                   (unless (listener-interrupt l)
                     (out (format nil "~&; this Lisp cannot interrupt the listener thread: ~
                                       the form keeps running there (the desktop does not wait)~%")))))
               (key (down ks)
                 (cond
                   ((member ks '(#xffe3 #xffe4)) (setf ctrl down))
                   ((not down) nil)
                   ((and ctrl (member ks '(#x63 #x43)))                       ; C-c
                    (if busy
                        (interrupt)
                        (progn (out (format nil "^C~%"))
                               (setf pending "" (fill-pointer line) 0 cursor 0)
                               (prompt))))
                   (busy (push ks typeahead))   ; typed while busy: replayed at the next prompt
                   ((and ctrl (member ks '(#x6c #x4c)))                       ; C-l
                    (out (format nil "~c[2J~c[H" #\Escape #\Escape)) (redraw))
                   ((or (= ks #xff0d) (= ks #xff8d)) (submit))                ; Return
                   ((= ks #xff08) (when (plusp cursor) (delete-at (1- cursor))))   ; BackSpace
                   ((or (= ks #xffff) (and ctrl (member ks '(#x64 #x44)))) (delete-at cursor)) ; Delete, C-d
                   ((or (= ks #xff51) (and ctrl (member ks '(#x62 #x42))))    ; Left, C-b
                    (when (plusp cursor) (decf cursor) (redraw)))
                   ((or (= ks #xff53) (and ctrl (member ks '(#x66 #x46))))    ; Right, C-f
                    (when (< cursor (length line)) (incf cursor) (redraw)))
                   ((or (= ks #xff50) (and ctrl (member ks '(#x61 #x41)))) (setf cursor 0) (redraw))  ; Home, C-a
                   ((or (= ks #xff57) (and ctrl (member ks '(#x65 #x45)))) (setf cursor (length line)) (redraw)) ; End, C-e
                   ((and ctrl (member ks '(#x6b #x4b))) (setf (fill-pointer line) cursor) (redraw)) ; C-k
                   ((and ctrl (member ks '(#x75 #x55))) (set-line ""))       ; C-u
                   ((or (= ks #xff52) (and ctrl (member ks '(#x70 #x50))))    ; Up, C-p: older
                    (when (< (1+ hpos) (length history))
                      (when (= hpos -1) (setf saved (copy-seq line)))
                      (incf hpos) (set-line (nth hpos history))))
                   ((or (= ks #xff54) (and ctrl (member ks '(#x6e #x4e))))    ; Down, C-n: newer
                    (when (>= hpos 0)
                      (decf hpos) (set-line (if (= hpos -1) saved (nth hpos history)))))
                   ((= ks #xff09) (insert #\Space) (insert #\Space))          ; Tab
                   (ctrl nil)
                   ((or (<= #x20 ks #x7e) (<= #xa0 ks #xff)) (insert (code-char ks)))
                   ((<= #x1000100 ks #x110ffff) (insert (code-char (- ks #x1000000))))))
               (poll ()
                 (when (and l slot (not closed))
                   (dolist (m (glass::mb-take (svref (listener-replies l) slot)))
                     (when (eql (second m) token)
                       (case (first m)
                         (:out (setf (svref (listener-taken l) slot)
                                     (1+ (svref (listener-taken l) slot)))
                               (out (third m)))
                         (:done (done (third m))))))))
               (close-window ()
                 (setf closed t)
                 (when slot
                   (setf (svref (listener-owner l) slot) nil)
                   (glass::mb-take (svref (listener-replies l) slot)))))
        (out (format nil *banner*))
        (cond ((null l) (prompt))
              (slot (setf busy t) (listener-post l (list :open slot token)))
              (t (setf l nil)
                 (out (format nil "; every listener slot is taken: evaluating on this window's thread~%"))
                 (prompt)))
        (values #'key
                nil
                (lambda () (poll) (glass-term:terminal-take-dirty tm))
                nil
                #'close-window
                (lambda (w h) (glass-term:resize-terminal-px tm w h))
                t)))))
