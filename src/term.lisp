;;;; term.lisp — a terminal emulator on glass: a real PTY + shell, an ANSI/VT
;;;; parser, and a character grid rendered with scribe into a glass framebuffer,
;;;; served over VNC.  No xterm, no X — glass owns the pixels, scribe the glyphs,
;;;; and a pseudo-terminal runs the shell.
;;;;
;;;; System :glass/term (depends on glass + glass/text + scribe).  The PTY / shell
;;;; and the winsize ioctl are the platform seam (sb-ext:run-program, one alien
;;;; ioctl); everything above — grid, parser, rendering, input — is portable CL.

(defpackage #:glass-term
  (:use #:cl)
  (:export #:run #:make-terminal #:terminal))
(in-package #:glass-term)

;;; ---- palette (Tango 16-colour) ---------------------------------------------

(defparameter *palette*
  (map 'vector (lambda (rgb) (glass:rgb (first rgb) (second rgb) (third rgb)))
       '((0 0 0) (204 0 0) (78 154 6) (196 160 0) (52 101 164) (117 80 123)
         (6 152 154) (211 215 207) (85 87 83) (239 41 41) (138 226 52) (252 233 79)
         (114 159 207) (173 127 168) (52 226 226) (238 238 236))))
(defun pal (i) (aref *palette* (logand i 15)))
(defconstant +def-fg+ 7)
(defconstant +def-bg+ 0)

;;; ---- cell packing: char | fg<<21 | bg<<25 | bold<<29 -----------------------

(declaim (inline mkcell cell-char cell-fg cell-bg))
(defun mkcell (ch fg bg bold) (logior (logand ch #x1fffff) (ash (logand fg 15) 21)
                                      (ash (logand bg 15) 25) (ash (if bold 1 0) 29)))
(defun cell-char (c) (logand c #x1fffff))
(defun cell-fg (c) (logand (ash c -21) 15))
(defun cell-bg (c) (logand (ash c -25) 15))
(defun blank-cell () (mkcell (char-code #\Space) +def-fg+ +def-bg+ nil))

;;; ---- terminal ---------------------------------------------------------------

(defstruct (terminal (:constructor %make-terminal))
  cols rows cells                       ; grid: (simple-vector rows*cols) of packed cells
  (cx 0) (cy 0)                         ; cursor
  (fg +def-fg+) (bg +def-bg+) (bold nil) (reverse nil)   ; current SGR
  (top 0) (bot 0)                       ; scroll region (inclusive rows)
  (saved-cx 0) (saved-cy 0)
  (pstate :ground) (params nil) (pcur nil) (ppriv nil)   ; ANSI parser state
  pty proc                              ; the shell
  fb font ppem cell-w cell-h ascent     ; rendering
  (glyphs (make-hash-table))            ; char-code -> (cov w h left top)
  (ctrl nil))                           ; keyboard: control held?

(defun cell (tm x y) (aref (terminal-cells tm) (+ (* y (terminal-cols tm)) x)))
(defun (setf cell) (v tm x y) (setf (aref (terminal-cells tm) (+ (* y (terminal-cols tm)) x)) v))

(defun make-grid (cols rows)
  (let ((v (make-array (* cols rows)))) (dotimes (i (length v) v) (setf (aref v i) (blank-cell)))))

;;; ---- ANSI / VT parser -------------------------------------------------------

(defun eff-colors (tm)
  "Current (fg . bg) after reverse-video, with bold brightening the fg."
  (let ((fg (if (and (terminal-bold tm) (< (terminal-fg tm) 8)) (+ (terminal-fg tm) 8) (terminal-fg tm)))
        (bg (terminal-bg tm)))
    (if (terminal-reverse tm) (cons bg fg) (cons fg bg))))

(defun put-char (tm ch)
  (when (>= (terminal-cx tm) (terminal-cols tm))   ; auto-wrap
    (setf (terminal-cx tm) 0) (line-feed tm))
  (destructuring-bind (fg . bg) (eff-colors tm)
    (setf (cell tm (terminal-cx tm) (terminal-cy tm)) (mkcell ch fg bg (terminal-bold tm))))
  (incf (terminal-cx tm)))

(defun scroll-up (tm)
  (let ((cols (terminal-cols tm)) (cells (terminal-cells tm)))
    (loop for y from (terminal-top tm) below (terminal-bot tm) do
      (replace cells cells :start1 (* y cols) :end1 (* (1+ y) cols) :start2 (* (1+ y) cols)))
    (fill cells (blank-cell) :start (* (terminal-bot tm) cols) :end (* (1+ (terminal-bot tm)) cols))))

(defun line-feed (tm)
  (if (>= (terminal-cy tm) (terminal-bot tm)) (scroll-up tm) (incf (terminal-cy tm))))

(defun clampc (tm) (setf (terminal-cx tm) (max 0 (min (1- (terminal-cols tm)) (terminal-cx tm)))
                         (terminal-cy tm) (max 0 (min (1- (terminal-rows tm)) (terminal-cy tm)))))

(defun erase (tm from-x from-y to-x to-y)
  (loop for y from from-y to to-y do
    (loop for x from (if (= y from-y) from-x 0) to (if (= y to-y) to-x (1- (terminal-cols tm)))
          do (setf (cell tm x y) (blank-cell)))))

(defun p (tm n default) (or (nth n (terminal-params tm)) default))

(defun sgr (tm)
  (let ((ps (or (terminal-params tm) '(0))))
    (dolist (n ps)
      (cond ((= n 0) (setf (terminal-fg tm) +def-fg+ (terminal-bg tm) +def-bg+
                           (terminal-bold tm) nil (terminal-reverse tm) nil))
            ((= n 1) (setf (terminal-bold tm) t))
            ((= n 7) (setf (terminal-reverse tm) t))
            ((or (= n 22) (= n 21)) (setf (terminal-bold tm) nil))
            ((= n 27) (setf (terminal-reverse tm) nil))
            ((<= 30 n 37) (setf (terminal-fg tm) (- n 30)))
            ((= n 39) (setf (terminal-fg tm) +def-fg+))
            ((<= 40 n 47) (setf (terminal-bg tm) (- n 40)))
            ((= n 49) (setf (terminal-bg tm) +def-bg+))
            ((<= 90 n 97) (setf (terminal-fg tm) (+ 8 (- n 90))))
            ((<= 100 n 107) (setf (terminal-bg tm) (+ 8 (- n 100))))))))

(defun csi-dispatch (tm final)
  (case final
    (#\m (sgr tm))
    ((#\H #\f) (setf (terminal-cy tm) (1- (p tm 0 1)) (terminal-cx tm) (1- (p tm 1 1))) (clampc tm))
    (#\A (decf (terminal-cy tm) (p tm 0 1)) (clampc tm))
    (#\B (incf (terminal-cy tm) (p tm 0 1)) (clampc tm))
    (#\C (incf (terminal-cx tm) (p tm 0 1)) (clampc tm))
    (#\D (decf (terminal-cx tm) (p tm 0 1)) (clampc tm))
    (#\G (setf (terminal-cx tm) (1- (p tm 0 1))) (clampc tm))
    (#\d (setf (terminal-cy tm) (1- (p tm 0 1))) (clampc tm))
    (#\J (case (p tm 0 0)
           (0 (erase tm (terminal-cx tm) (terminal-cy tm) (1- (terminal-cols tm)) (1- (terminal-rows tm))))
           (1 (erase tm 0 0 (terminal-cx tm) (terminal-cy tm)))
           (t (erase tm 0 0 (1- (terminal-cols tm)) (1- (terminal-rows tm))))))
    (#\K (case (p tm 0 0)
           (0 (erase tm (terminal-cx tm) (terminal-cy tm) (1- (terminal-cols tm)) (terminal-cy tm)))
           (1 (erase tm 0 (terminal-cy tm) (terminal-cx tm) (terminal-cy tm)))
           (t (erase tm 0 (terminal-cy tm) (1- (terminal-cols tm)) (terminal-cy tm)))))
    (#\r (setf (terminal-top tm) (1- (p tm 0 1)) (terminal-bot tm) (1- (p tm 1 (terminal-rows tm)))))
    (#\s (setf (terminal-saved-cx tm) (terminal-cx tm) (terminal-saved-cy tm) (terminal-cy tm)))
    (#\u (setf (terminal-cx tm) (terminal-saved-cx tm) (terminal-cy tm) (terminal-saved-cy tm)))
    (#\P (let ((n (p tm 0 1)) (y (terminal-cy tm)))   ; delete chars: shift line left
           (loop for x from (terminal-cx tm) below (- (terminal-cols tm) n)
                 do (setf (cell tm x y) (cell tm (+ x n) y)))
           (loop for x from (max 0 (- (terminal-cols tm) n)) below (terminal-cols tm)
                 do (setf (cell tm x y) (blank-cell)))))
    (t nil)))                                          ; ignore the rest

(defun feed-char (tm c)
  (let ((code (char-code c)))
    (ecase (terminal-pstate tm)
      (:ground
       (cond
         ((= code 27) (setf (terminal-pstate tm) :esc))
         ((= code 13) (setf (terminal-cx tm) 0))
         ((= code 10) (line-feed tm))
         ((= code 8) (setf (terminal-cx tm) (max 0 (1- (terminal-cx tm)))))
         ((= code 9) (setf (terminal-cx tm) (min (1- (terminal-cols tm)) (* 8 (1+ (floor (terminal-cx tm) 8))))))
         ((= code 7))                                  ; bell
         ((>= code 32) (put-char tm code))))
      (:esc
       (case c
         (#\[ (setf (terminal-pstate tm) :csi (terminal-params tm) nil (terminal-pcur tm) nil (terminal-ppriv tm) nil))
         ((#\] #\P) (setf (terminal-pstate tm) :osc))  ; OSC / DCS: swallow to ST/BEL
         ((#\( #\)) (setf (terminal-pstate tm) :charset))
         (#\M (if (<= (terminal-cy tm) (terminal-top tm)) nil (decf (terminal-cy tm)))  ; reverse index
              (setf (terminal-pstate tm) :ground))
         (t (setf (terminal-pstate tm) :ground))))
      (:charset (setf (terminal-pstate tm) :ground))
      (:osc (when (or (= code 7) (= code 27)) (setf (terminal-pstate tm) :ground)))
      (:csi
       (cond
         ((char= c #\?) (setf (terminal-ppriv tm) t))
         ((digit-char-p c) (setf (terminal-pcur tm) (+ (* (or (terminal-pcur tm) 0) 10) (digit-char-p c))))
         ((char= c #\;) (setf (terminal-params tm) (append (terminal-params tm) (list (or (terminal-pcur tm) 0)))
                              (terminal-pcur tm) nil))
         ((<= 64 code 126)
          (when (terminal-pcur tm) (setf (terminal-params tm) (append (terminal-params tm) (list (terminal-pcur tm)))))
          (unless (terminal-ppriv tm) (csi-dispatch tm c))
          (setf (terminal-pstate tm) :ground)))))))

;;; ---- rendering (glyph-cached scribe) ---------------------------------------

(declaim (inline over))
(defun over (dst8 fg8 a ia)
  (scribe:linear->srgb (+ (* ia (scribe:srgb->linear dst8)) (* a (scribe:srgb->linear fg8)))))

(defun glyph (tm code)
  "Cached (cov w h left top) for CODE at the terminal's ppem."
  (or (gethash code (terminal-glyphs tm))
      (setf (gethash code (terminal-glyphs tm))
            (multiple-value-list
             (scribe:rasterize-glyph (terminal-font tm) (scribe:font-glyph-index (terminal-font tm) code)
                                     (terminal-ppem tm))))))

(defun draw-cell (tm x y)
  (let* ((c (cell tm x y)) (fb (terminal-fb tm))
         (cw (terminal-cell-w tm)) (ch (terminal-cell-h tm))
         (px0 (* x cw)) (py0 (* y ch))
         (cursor (and (= x (terminal-cx tm)) (= y (terminal-cy tm))))
         (fg (pal (if cursor (cell-bg c) (cell-fg c))))
         (bg (pal (if cursor (cell-fg c) (cell-bg c)))))   ; cursor = inverse block
    (glass:fb-rect fb px0 py0 cw ch bg)
    (let (( code (cell-char c)))
      (when (> code 32)
        (destructuring-bind (cov gw gh left top &rest ignore) (glyph tm code)
          (declare (ignore ignore))
          (when cov
            (let* ((ox (+ px0 left)) (oy (+ py0 (terminal-ascent tm) top))
                   (fr (ldb (byte 8 16) fg)) (fgc (ldb (byte 8 8) fg)) (fbc (ldb (byte 8 0) fg))
                   (dpx (glass:fb-pixels fb)) (fw (glass:fb-width fb)) (fh (glass:fb-height fb)))
              (dotimes (yy gh)
                (let ((yv (+ oy yy)))
                  (when (< -1 yv fh)
                    (let ((row (* yv fw)))
                      (dotimes (xx gw)
                        (let ((cvv (aref cov (+ (* yy gw) xx))) (xv (+ ox xx)))
                          (when (and (> cvv 0d0) (< -1 xv fw))
                            (let* ((i (+ row xv)) (d (aref dpx i)) (a (min 1d0 cvv)) (ia (- 1d0 a)))
                              (setf (aref dpx i)
                                    (logior (ash (over (ldb (byte 8 16) d) fr a ia) 16)
                                            (ash (over (ldb (byte 8 8) d) fgc a ia) 8)
                                            (over (ldb (byte 8 0) d) fbc a ia))))))))))))))))))

(defun render (tm)
  (glass:with-fb-locked ((terminal-fb tm))
    (dotimes (y (terminal-rows tm))
      (dotimes (x (terminal-cols tm)) (draw-cell tm x y)))))

;;; ---- keyboard -> PTY --------------------------------------------------------

(defparameter *keymap*
  `((#xff0d . ,(string (code-char 13))) (#xff08 . ,(string (code-char 127)))
    (#xff09 . ,(string (code-char 9)))  (#xff1b . ,(string (code-char 27)))
    (#xffff . ,(format nil "~c[3~~" (code-char 27)))
    (#xff52 . ,(format nil "~c[A" (code-char 27))) (#xff54 . ,(format nil "~c[B" (code-char 27)))
    (#xff53 . ,(format nil "~c[C" (code-char 27))) (#xff51 . ,(format nil "~c[D" (code-char 27)))
    (#xff50 . ,(format nil "~c[H" (code-char 27))) (#xff57 . ,(format nil "~c[F" (code-char 27)))
    (#xff55 . ,(format nil "~c[5~~" (code-char 27))) (#xff56 . ,(format nil "~c[6~~" (code-char 27)))))

(defun on-key (tm down keysym)
  (cond
    ((member keysym '(#xffe3 #xffe4)) (setf (terminal-ctrl tm) down))   ; Control L/R
    ((not down) nil)
    (t (let ((bytes (cond
                      ((cdr (assoc keysym *keymap*)))
                      ((or (<= #x20 keysym #x7e) (<= #xa0 keysym #xff))
                       (if (and (terminal-ctrl tm) (<= #x40 (logand keysym #xdf) #x5f))
                           (string (code-char (logand keysym #x1f)))     ; Ctrl-A..Z
                           (string (code-char keysym))))
                      (t nil))))
         (when bytes
           (write-string bytes (terminal-pty tm)) (force-output (terminal-pty tm)))))))

;;; ---- PTY + run --------------------------------------------------------------

(defun set-winsize (stream rows cols)
  "TIOCSWINSZ on the pty master so programs know the size (best-effort)."
  (ignore-errors
   (sb-alien:with-alien ((ws (sb-alien:array sb-alien:unsigned-short 4)))
     (setf (sb-alien:deref ws 0) rows (sb-alien:deref ws 1) cols
           (sb-alien:deref ws 2) 0 (sb-alien:deref ws 3) 0)
     (sb-alien:alien-funcall
      (sb-alien:extern-alien "ioctl"
        (function sb-alien:int sb-alien:int sb-alien:unsigned-long
                  (* (sb-alien:array sb-alien:unsigned-short 4))))
      (sb-sys:fd-stream-fd stream) #x5414 (sb-alien:addr ws)))))

(defun make-terminal (&key (cols 80) (rows 24) (ppem 16) (shell "/bin/bash"))
  (let* ((font (glass:load-font (asdf:system-relative-pathname :scribe "fonts/LiberationMono-Regular.ttf")))
         (upem (scribe:font-units-per-em font))
         (cell-w (max 1 (ceiling (nth-value 5 (scribe:rasterize-glyph font (scribe:font-glyph-index font (char-code #\M)) ppem)))))
         (asc (round (* (scribe:font-ascent font) ppem) upem))
         (desc (round (* (- (scribe:font-descent font)) ppem) upem))
         (cell-h (+ asc desc 2))
         (proc (sb-ext:run-program shell '("--norc" "-i") :pty t :wait nil
                                   :environment (list "TERM=xterm-256color" "PS1=\\w $ "
                                                      "PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"
                                                      (format nil "COLUMNS=~d" cols) (format nil "LINES=~d" rows)
                                                      (format nil "HOME=~a" (or (sb-ext:posix-getenv "HOME") "/root")))))
         (pty (sb-ext:process-pty proc))
         (tm (%make-terminal :cols cols :rows rows :cells (make-grid cols rows)
                            :bot (1- rows) :pty pty :proc proc :font font :ppem ppem
                            :cell-w cell-w :cell-h cell-h :ascent asc
                            :fb (glass:make-framebuffer (* cols cell-w) (* rows cell-h) glass:+black+))))
    (set-winsize pty rows cols)
    tm))

(defun pump (tm)
  "Read shell output, feed the parser, render.  Returns when the shell exits."
  (loop
    (let ((got nil))
      (handler-case
          (loop repeat 200000 for c = (read-char-no-hang (terminal-pty tm) nil :eof)
                do (cond ((null c) (return)) ((eq c :eof) (return-from pump))
                         (t (feed-char tm c) (setf got t))))
        (stream-error () (return-from pump)))
      (when got (render tm))
      (sleep 1/60))))

(defun run (&key (port 5900) (cols 80) (rows 24) (ppem 16) (shell "/bin/bash"))
  "Open SHELL in a pseudo-terminal and serve it as a terminal over VNC on PORT.
   Point any VNC client at localhost:PORT.  Blocks until the shell exits."
  (let ((tm (make-terminal :cols cols :rows rows :ppem ppem :shell shell)))
    (render tm)
    (sb-thread:make-thread
     (lambda () (ignore-errors
                 (glass:serve (terminal-fb tm) port
                              :on-key (lambda (down k) (ignore-errors (on-key tm down k)))
                              :name "glass-term")))
     :name "glass-term-server")
    (pump tm)
    tm))
