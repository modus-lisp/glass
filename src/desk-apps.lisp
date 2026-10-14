;;;; desk-apps.lisp — the desk's own applications: Clock, Notes, Paint, and Media from warp.
;;;;
;;;; Each is a desk application (DESK-REGISTER-APP's MAKE-FN: called with the window's content
;;;; framebuffer, returning ON-KEY ON-POINTER DIRTY-P [COPY-P CLOSE-FN TEXT-P]) and a GLOBAL
;;;; FUNCTION, so SERVE-DESK can hand it to its compositor thread by name.  Clock and Notes
;;;; came from kiln's phone desk (kiln-android boot/app.lisp), unchanged but for the names
;;;; and the Clock's second line.

(in-package #:glass.desk)

(export '(clock-app notes-app paint-app media-app *media-root* *standard-apps*))

(defun clock-app (fb)
  "Clock: the time, redrawn once a second, and the date under it."
  (let ((last -1))
    (values nil nil
            (lambda ()
              (let ((now (get-universal-time)))
                (when (/= now last)
                  (setf last now)
                  (multiple-value-bind (s m h) (decode-universal-time now)
                    (glass:fb-fill fb #x10141a)
                    (glass:fb-text fb 16 20 (format nil "~2,'0d:~2,'0d:~2,'0d" h m s)
                                   :size 40 :color #xe6ebf2)
                    (glass:fb-text fb 16 80 (multiple-value-bind (s m h d mo y) (decode-universal-time now) (declare (ignore s m h)) (format nil "~d-~2,'0d-~2,'0d" y mo d)) :size 13 :color #x8b949e))
                  t))))))

(defun %keysym-char (ks)
  "The character an X11 KEYSYM types, or NIL."
  (cond ((= ks #xff0d) #\Newline)
        ((= ks #xff09) #\Space)
        ((<= #x20 ks #xff) (code-char ks))
        ((<= #x1000000 ks #x110ffff) (code-char (- ks #x1000000)))))

(defun %wrap (line width size font)
  "LINE broken into rows no wider than WIDTH at SIZE in FONT: at spaces, and inside a word only
   when the word alone is wider than a row."
  (let ((rows '()) (row ""))
    (flet ((fits (s) (<= (glass:text-width s :size size :font font) width)))
      (dolist (word (let ((acc '()) (start 0))          ; split, keeping each word's trailing space
                      (loop for k from 0 below (length line)
                            when (char= (char line k) #\Space)
                              do (push (subseq line start (1+ k)) acc) (setf start (1+ k)))
                      (push (subseq line start) acc)
                      (nreverse acc)))
        (cond ((fits (concatenate 'string row (string-right-trim " " word)))
               (setf row (concatenate 'string row word)))
              (t
               (when (plusp (length row)) (push row rows))
               (setf row "")
               ;; a word longer than a row: as many characters as fit, then the rest
               (loop for ch across word
                     do (if (fits (concatenate 'string row (string ch)))
                            (setf row (concatenate 'string row (string ch)))
                            (progn (push row rows) (setf row (string ch))))))))
      (push row rows)
      (nreverse rows))))

(defun notes-app (fb)
  "Somewhere to type, looking like the phone's own notes: 17-point text on white, the first line
   a bold title, words wrapped whole, a thin gold caret.  The caret goes where you tap (and follows
   a drag), typing and Backspace work at it, a hardware keyboard's arrows move it, and the view
   scrolls to keep it in sight."
  (let ((lines (vector "")) (cl 0) (cc 0) (top-row 0) (dirty t)
        (body (glass:default-font)) (bold (glass:default-font t))
        (size 17) (lh 23) (tsize 22) (tlh 30) (margin 16) (top 14)
        (ink #x1c1c1e) (paper #xffffff) (caret #xe0a800)
        (layout nil))                     ; the visual rows, rebuilt when the text changes
    (labels ((style (li) (if (zerop li) (values bold tsize tlh) (values body size lh)))
             (rows ()
               ;; #((line start text font size height) ...): every visual row in order; a line's
               ;; rows partition it exactly, so START is the column its first character is at
               (or layout
                   (setf layout
                         (let ((w (- (glass:fb-width fb) (* 2 margin))) (out '()))
                           (dotimes (li (length lines) (coerce (nreverse out) 'vector))
                             (multiple-value-bind (font sz h) (style li)
                               (let ((start 0))
                                 (dolist (r (%wrap (aref lines li) w sz font))
                                   (push (list li start r font sz h) out)
                                   (incf start (length r))))))))))
             (changed () (setf layout nil dirty t))
             (caret-row ()
               ;; the row holding the caret: the last row of its line that starts at or before it
               (let ((rs (rows)) (best 0))
                 (dotimes (k (length rs) best)
                   (destructuring-bind (li start &rest _) (aref rs k)
                     (declare (ignore _))
                     (when (and (= li cl) (<= start cc)) (setf best k))))))
             (row-x (row col)
               ;; the x of column COL of ROW (COL relative to the line)
               (destructuring-bind (li start text font sz h) row
                 (declare (ignore li h))
                 (+ margin (glass:text-width (subseq text 0 (max 0 (min (length text) (- col start))))
                                             :size sz :font font))))
             (col-at (row x)
               ;; the column of ROW nearest the x coordinate X: the closest character boundary
               (destructuring-bind (li start text font sz h) row
                 (declare (ignore li h))
                 (let ((best 0) (bestd most-positive-fixnum) (w 0)
                       ;; a wrapped row's trailing space belongs before the next row, not after
                       (n (if (and (plusp (length text)) (char= (char text (1- (length text))) #\Space))
                              (1- (length text)) (length text))))
                   (loop for k from 0 to n
                         do (let ((d (abs (- (+ margin w) x))))
                              (when (< d bestd) (setf best k bestd d)))
                            (when (< k n)
                              (incf w (glass:text-width (string (char text k)) :size sz :font font))))
                   (+ start best))))
             (place (k x)
               ;; the caret onto visual row K at x X
               (let ((r (aref (rows) k))) (setf cl (first r) cc (col-at r x) dirty t)))
             (visible-count (from)
               (let ((room (- (glass:fb-height fb) top margin)) (used 0) (n 0) (rs (rows)))
                 (loop for k from from below (length rs)
                       do (incf used (sixth (aref rs k)))
                          (when (> used room) (return))
                          (incf n))
                 (max 1 n)))
             (scroll-to-caret ()
               (let ((k (caret-row)))
                 (when (< k top-row) (setf top-row k))
                 (loop while (>= k (+ top-row (visible-count top-row))) do (incf top-row))))
             (redraw ()
               (scroll-to-caret)
               (glass:fb-fill fb paper)
               (let ((rs (rows)) (ck (caret-row)) (y top))
                 (loop for k from top-row below (min (length rs) (+ top-row (visible-count top-row)))
                       do (destructuring-bind (li start text font sz h) (aref rs k)
                            (declare (ignore li start))
                            (glass:fb-text fb margin (+ y (floor (- h (* 1.2 sz)) 2)) text
                                           :size sz :color ink :font font)
                            (when (= k ck)
                              (glass:fb-rect fb (1+ (row-x (aref rs k) cc))
                                             (+ y (floor (- h (* 1.25 sz)) 2)) 2 (round (* 1.25 sz)) caret))
                            (incf y h)))))
             (row-at-y (py)
               ;; the visual row at local y PY, counting from the first shown; past the end, the last
               (let ((rs (rows)) (y top))
                 (loop for k from top-row below (length rs)
                       do (incf y (sixth (aref rs k)))
                          (when (< py y) (return-from row-at-y k)))
                 (1- (length rs))))
             (edit (ks)
               (let* ((line (aref lines cl)) (ch (%keysym-char ks)))
                 (cond
                   ((= ks #xff08)                                  ; BackSpace
                    (cond ((plusp cc)
                           (setf (aref lines cl) (concatenate 'string (subseq line 0 (1- cc)) (subseq line cc)))
                           (decf cc))
                          ((plusp cl)                              ; join with the line above
                           (let ((prev (aref lines (1- cl))))
                             (setf (aref lines (1- cl)) (concatenate 'string prev line)
                                   lines (concatenate 'vector (subseq lines 0 cl) (subseq lines (1+ cl))))
                             (decf cl) (setf cc (length prev))))))
                   ((= ks #xff51)                                  ; Left
                    (cond ((plusp cc) (decf cc))
                          ((plusp cl) (decf cl) (setf cc (length (aref lines cl))))))
                   ((= ks #xff53)                                  ; Right
                    (cond ((< cc (length line)) (incf cc))
                          ((< cl (1- (length lines))) (incf cl) (setf cc 0))))
                   ((or (= ks #xff52) (= ks #xff54))               ; Up, Down: the row above or below
                    (let* ((k (caret-row)) (x (row-x (aref (rows) k) cc))
                           (k2 (+ k (if (= ks #xff52) -1 1))))
                      (when (< -1 k2 (length (rows))) (place k2 x))))
                   ((eql ch #\Newline)                            ; split the line at the caret
                    (setf lines (concatenate 'vector (subseq lines 0 cl)
                                             (vector (subseq line 0 cc) (subseq line cc))
                                             (subseq lines (1+ cl))))
                    (incf cl) (setf cc 0))
                   (ch
                    (setf (aref lines cl) (concatenate 'string (subseq line 0 cc) (string ch) (subseq line cc)))
                    (incf cc))))
               (changed)))
      (values (lambda (down ks) (when down (edit ks)))
              ;; a press puts the caret where it lands, and a drag carries it along
              (lambda (mask x y)
                (when (logbitp 0 mask) (place (row-at-y y) x)))
              (lambda () (when dirty (setf dirty nil) (redraw) t))
              nil nil
              t))))                       ; it takes typing: focusing it brings up the keyboard

(defun paint-app (fb)
  "Paint: press and drag to draw; any key clears."
  (let ((dirty t) (down nil))
    (glass:fb-fill fb #xf4f1ea)
    (values
     (lambda (pressed keysym)
       (declare (ignore keysym))
       (when pressed (glass:fb-fill fb #xf4f1ea) (setf dirty t)))
     (lambda (mask x y)
       (setf down (logtest mask 1))
       (when down
         (glass:fb-rect fb (- x 3) (- y 3) 6 6 #x1d3557)
         (setf dirty t)))
     (lambda () (prog1 dirty (setf dirty nil))))))

;;; ---- Media, from warp --------------------------------------------------------------------------
;;;
;;; warp's player (warp/media) is a desk app already -- kiln's phone desk opens it.  glass does not
;;; depend on warp: MEDIA-APP finds it at run time, and says so in the window when it is not loaded.

(defvar *media-root* nil
  "The folder MEDIA-APP's library plays from (a directory namestring), or NIL for none.")

(defvar *media-library* nil "The library every Media window shares, made on first open.")

(defun media-app (fb)
  "Media: warp's player over *MEDIA-ROOT*.  Needs the warp-media-glass system loaded."
  (let ((make-lib (find-symbol "MAKE-LIBRARY" "WARP-MEDIA"))
        (make-win (and (find-package "WARP-MEDIA-GLASS")
                       (find-symbol "MAKE-MEDIA-WINDOW" "WARP-MEDIA-GLASS"))))
    (if (not (and make-lib make-win))
        (progn (glass:fb-fill fb #x10141a)
               (glass:fb-text fb 16 20 "Media needs warp-media-glass loaded." :size 14 :color #xe6ebf2)
               (values nil nil (let ((once t)) (lambda () (prog1 once (setf once nil))))))
        (let ((lib (or *media-library*
                       (setf *media-library* (funcall make-lib :root (or *media-root* "./"))))))
          ;; FIVE values, then NIL: the player's sixth value is its consumer, which the desk
          ;; would read as TEXT-P and raise a keyboard for (kiln found this).
          (multiple-value-bind (on-key on-pointer dirty-p copy-p close-fn) (funcall make-win fb lib)
            (values on-key on-pointer dirty-p copy-p close-fn nil))))))

(defparameter *standard-apps*
  '(("Clock" clock-app :width 260 :height 110)
    ("Notes" notes-app :width 420 :height 300)
    ("Paint" paint-app :width 320 :height 240)
    ("Media" media-app :width 480 :height 440))
  "The desk's applications, as SERVE-DESK takes them: (LABEL MAKE-FN &key WIDTH HEIGHT).")
