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
  (:export #:run #:make-terminal #:terminal #:terminal-fb #:on-key #:on-mouse #:start-pump
           ;; tabbed terminal (multiple shells, a tab bar)
           #:make-tabbed-terminal #:tabterm-fb #:tabterm-on-key #:tabterm-on-mouse #:tabterm-new))
(in-package #:glass-term)

;;; ---- palette (Tango 16-colour) ---------------------------------------------

(defparameter *palette*
  (let ((v (make-array 256)))
    (loop for i from 0 for rgb in       ; 0-15: the 16 ANSI colours (Tango)
          '((0 0 0) (204 0 0) (78 154 6) (196 160 0) (52 101 164) (117 80 123)
            (6 152 154) (211 215 207) (85 87 83) (239 41 41) (138 226 52) (252 233 79)
            (114 159 207) (173 127 168) (52 226 226) (238 238 236))
          do (setf (aref v i) (glass:rgb (first rgb) (second rgb) (third rgb))))
    (flet ((lvl (x) (if (zerop x) 0 (+ 55 (* x 40)))))         ; 16-231: 6x6x6 cube
      (loop for i from 16 below 232 for n = (- i 16) do
        (setf (aref v i) (glass:rgb (lvl (floor n 36)) (lvl (mod (floor n 6) 6)) (lvl (mod n 6))))))
    (loop for i from 232 below 256 for l = (+ 8 (* (- i 232) 10)) do   ; 232-255: greys
      (setf (aref v i) (glass:rgb l l l)))
    v))
(defun pal (i) (aref *palette* (logand i 255)))
(defun q6 (x) (cond ((< x 48) 0) ((< x 115) 1) ((< x 155) 2) ((< x 195) 3) ((< x 235) 4) (t 5)))
(defun rgb->256 (r g b) (+ 16 (* 36 (q6 r)) (* 6 (q6 g)) (q6 b)))   ; truecolor -> nearest cube
(defconstant +def-fg+ 7)
(defconstant +def-bg+ 0)

;;; ---- cell packing: char | fg<<21 | bg<<25 | bold<<29 -----------------------

(declaim (inline mkcell cell-char cell-fg cell-bg cell-wide-p cell-spacer-p))
(defun mkcell (ch fg bg bold &key wide spacer)   ; char(21) fg(8) bg(8) bold wide spacer -> 40 bits
  (logior (logand ch #x1fffff) (ash (logand fg 255) 21) (ash (logand bg 255) 29)
          (ash (if bold 1 0) 37) (ash (if wide 1 0) 38) (ash (if spacer 1 0) 39)))
(defun cell-char (c) (logand c #x1fffff))
(defun cell-fg (c) (logand (ash c -21) 255))
(defun cell-bg (c) (logand (ash c -29) 255))
(defun cell-wide-p (c) (logbitp 38 c))          ; glyph spans this + the next cell
(defun cell-spacer-p (c) (logbitp 39 c))        ; right half of a wide char (draw nothing)
(defun blank-cell () (mkcell (char-code #\Space) +def-fg+ +def-bg+ nil))

;;; ---- character width (wcwidth-lite: 0 combining, 2 CJK/emoji, else 1) -------

(defun zero-width-p (cp)
  (or (<= #x0300 cp #x036F) (<= #x1AB0 cp #x1AFF) (<= #x1DC0 cp #x1DFF)
      (<= #x20D0 cp #x20FF) (<= #xFE20 cp #xFE2F) (<= #x200B cp #x200F) (= cp #xFEFF)))
(defun wide-p (cp)
  (or (<= #x1100 cp #x115F) (<= #x2E80 cp #x303E) (<= #x3041 cp #x33FF)
      (<= #x3400 cp #x4DBF) (<= #x4E00 cp #x9FFF) (<= #xA000 cp #xA4CF)
      (<= #xAC00 cp #xD7A3) (<= #xF900 cp #xFAFF) (<= #xFE30 cp #xFE4F)
      (<= #xFF00 cp #xFF60) (<= #xFFE0 cp #xFFE6)
      (<= #x1F300 cp #x1FAFF) (<= #x1F000 cp #x1F0FF) (<= #x20000 cp #x3FFFD)))
(defun char-width (cp)
  (cond ((< cp 32) 0) ((zero-width-p cp) 0) ((wide-p cp) 2) (t 1)))

;;; ---- terminal ---------------------------------------------------------------

(defstruct (terminal (:constructor %make-terminal))
  cols rows cells                       ; grid: (simple-vector rows*cols) of packed cells
  (cx 0) (cy 0)                         ; cursor
  (fg +def-fg+) (bg +def-bg+) (bold nil) (reverse nil)   ; current SGR
  (top 0) (bot 0)                       ; scroll region (inclusive rows)
  (saved-cx 0) (saved-cy 0)
  (pstate :ground) (params nil) (pcur nil) (ppriv nil)   ; ANSI parser state
  pty proc                              ; the shell
  fb font emoji-font ppem cell-w cell-h ascent   ; rendering (+ optional colour-emoji font)
  (glyphs (make-hash-table))            ; char-code -> rendered glyph (:mono/:color ...)
  (ctrl nil)                            ; keyboard: control held?
  (u8-need 0) (u8-cp 0)                 ; UTF-8 decoder state (bytes remaining, accumulator)
  (dcs nil)                             ; DCS payload buffer (sixel), while collecting
  (graphics nil)                        ; placed sixel images: list of (px py . framebuffer)
  (cursor-vis t)                        ; DECTCEM (?25): draw the cursor block?
  (alt nil)                             ; saved main screen while on the alternate buffer
  (dec-saved nil)                       ; DECSC cursor+attrs save
  (mouse-mode nil) (mouse-sgr nil)      ; mouse tracking: nil/:normal/:button/:any, SGR (1006)?
  (mouse-buttons 0) (mouse-col 0) (mouse-row 0))   ; last mouse state (for diffing)

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
  (let ((w (char-width ch)))
    (when (zerop w) (return-from put-char))          ; combining/zero-width: v1 drops it
    (when (> (+ (terminal-cx tm) w) (terminal-cols tm))   ; wrap if it won't fit
      (setf (terminal-cx tm) 0) (line-feed tm))
    (destructuring-bind (fg . bg) (eff-colors tm)
      (setf (cell tm (terminal-cx tm) (terminal-cy tm))
            (mkcell ch fg bg (terminal-bold tm) :wide (= w 2)))
      (when (= w 2)
        (setf (cell tm (1+ (terminal-cx tm)) (terminal-cy tm))
              (mkcell 0 fg bg (terminal-bold tm) :spacer t))))
    (incf (terminal-cx tm) w)))

(defun scroll-up (tm)
  (let ((cols (terminal-cols tm)) (cells (terminal-cells tm)))
    (loop for y from (terminal-top tm) below (terminal-bot tm) do
      (replace cells cells :start1 (* y cols) :end1 (* (1+ y) cols) :start2 (* (1+ y) cols)))
    (fill cells (blank-cell) :start (* (terminal-bot tm) cols) :end (* (1+ (terminal-bot tm)) cols)))
  ;; sixel images scroll with the text
  (dolist (g (terminal-graphics tm)) (decf (cadr g) (terminal-cell-h tm)))
  (setf (terminal-graphics tm)
        (remove-if (lambda (g) (< (+ (cadr g) (glass:fb-height (cddr g))) 0)) (terminal-graphics tm))))

(defun line-feed (tm)
  (if (>= (terminal-cy tm) (terminal-bot tm)) (scroll-up tm) (incf (terminal-cy tm))))

(defun clampc (tm) (setf (terminal-cx tm) (max 0 (min (1- (terminal-cols tm)) (terminal-cx tm)))
                         (terminal-cy tm) (max 0 (min (1- (terminal-rows tm)) (terminal-cy tm)))))

(defun erase (tm from-x from-y to-x to-y)
  (loop for y from from-y to to-y do
    (loop for x from (if (= y from-y) from-x 0) to (if (= y to-y) to-x (1- (terminal-cols tm)))
          do (setf (cell tm x y) (blank-cell)))))

(defun scroll-down (tm)
  "Scroll the region down one line (blank line inserted at the top)."
  (let ((cols (terminal-cols tm)) (cells (terminal-cells tm)))
    (loop for y from (terminal-bot tm) above (terminal-top tm) do
      (replace cells cells :start1 (* y cols) :end1 (* (1+ y) cols) :start2 (* (1- y) cols)))
    (fill cells (blank-cell) :start (* (terminal-top tm) cols) :end (* (1+ (terminal-top tm)) cols))))

(defun ins-lines (tm n)                 ; IL: insert N blank lines at the cursor row
  (let ((cols (terminal-cols tm)) (cells (terminal-cells tm))
        (cy (terminal-cy tm)) (bot (terminal-bot tm)))
    (loop for y from bot downto (+ cy n) do
      (replace cells cells :start1 (* y cols) :end1 (* (1+ y) cols) :start2 (* (- y n) cols)))
    (loop for y from cy below (min (+ cy n) (1+ bot)) do
      (fill cells (blank-cell) :start (* y cols) :end (* (1+ y) cols)))))

(defun del-lines (tm n)                 ; DL: delete N lines at the cursor row
  (let ((cols (terminal-cols tm)) (cells (terminal-cells tm))
        (cy (terminal-cy tm)) (bot (terminal-bot tm)))
    (loop for y from cy to (- bot n) do
      (replace cells cells :start1 (* y cols) :end1 (* (1+ y) cols) :start2 (* (+ y n) cols)))
    (loop for y from (max cy (1+ (- bot n))) to bot do
      (fill cells (blank-cell) :start (* y cols) :end (* (1+ y) cols)))))

(defun ins-chars (tm n)                 ; ICH: shift the line right by N at the cursor
  (let ((y (terminal-cy tm)) (cols (terminal-cols tm)) (cx (terminal-cx tm)))
    (loop for x from (1- cols) downto (+ cx n) do (setf (cell tm x y) (cell tm (- x n) y)))
    (loop for x from cx below (min (+ cx n) cols) do (setf (cell tm x y) (blank-cell)))))

(defun ech (tm n)                       ; ECH: erase N chars from the cursor (no shift)
  (let ((y (terminal-cy tm)) (cx (terminal-cx tm)) (cols (terminal-cols tm)))
    (loop for x from cx below (min (+ cx n) cols) do (setf (cell tm x y) (blank-cell)))))

(defun set-alt (tm on)
  "Switch to / from the alternate screen buffer (?1049/?47/?1047)."
  (cond
    ((and on (not (terminal-alt tm)))
     (setf (terminal-alt tm) (list (terminal-cells tm) (terminal-cx tm) (terminal-cy tm)
                                   (terminal-fg tm) (terminal-bg tm) (terminal-bold tm))
           (terminal-cells tm) (make-grid (terminal-cols tm) (terminal-rows tm))
           (terminal-cx tm) 0 (terminal-cy tm) 0))
    ((and (not on) (terminal-alt tm))
     (destructuring-bind (cells cx cy fg bg bold) (terminal-alt tm)
       (setf (terminal-cells tm) cells (terminal-cx tm) cx (terminal-cy tm) cy
             (terminal-fg tm) fg (terminal-bg tm) bg (terminal-bold tm) bold
             (terminal-alt tm) nil)))))

(defun private-mode (tm set-p)
  "DEC private mode set/reset (CSI ? Pn h / l)."
  (dolist (n (or (terminal-params tm) '(0)))
    (case n
      (25 (setf (terminal-cursor-vis tm) set-p))          ; DECTCEM cursor visibility
      ((47 1047 1049) (set-alt tm set-p))                 ; alternate screen
      (1000 (setf (terminal-mouse-mode tm) (and set-p :normal)))  ; mouse: press/release
      (1002 (setf (terminal-mouse-mode tm) (and set-p :button)))  ; + motion while pressed
      (1003 (setf (terminal-mouse-mode tm) (and set-p :any)))     ; + all motion
      (1006 (setf (terminal-mouse-sgr tm) set-p)))))      ; SGR extended encoding

(defun decsc (tm)
  (setf (terminal-dec-saved tm) (list (terminal-cx tm) (terminal-cy tm)
                                      (terminal-fg tm) (terminal-bg tm) (terminal-bold tm))))
(defun decrc (tm)
  (when (terminal-dec-saved tm)
    (destructuring-bind (cx cy fg bg bold) (terminal-dec-saved tm)
      (setf (terminal-cx tm) cx (terminal-cy tm) cy (terminal-fg tm) fg
            (terminal-bg tm) bg (terminal-bold tm) bold))))

(defun p (tm n default) (or (nth n (terminal-params tm)) default))

(defun sgr-ext (ps i)
  "Parse an extended colour after 38/48 at PS[I]: 5;n (256) or 2;r;g;b (truecolor
   -> nearest 256).  Returns (values colour-index params-consumed)."
  (let ((kind (nth i ps)))
    (cond ((eql kind 5) (values (or (nth (1+ i) ps) 0) 2))
          ((eql kind 2) (values (rgb->256 (or (nth (1+ i) ps) 0) (or (nth (+ i 2) ps) 0) (or (nth (+ i 3) ps) 0)) 4))
          (t (values +def-fg+ 1)))))

(defun sgr (tm)
  (let* ((ps (or (terminal-params tm) '(0))) (i 0) (len (length ps)))
    (loop while (< i len) do
      (let ((n (nth i ps)))
        (cond ((= n 0) (setf (terminal-fg tm) +def-fg+ (terminal-bg tm) +def-bg+
                             (terminal-bold tm) nil (terminal-reverse tm) nil))
              ((= n 1) (setf (terminal-bold tm) t))
              ((= n 7) (setf (terminal-reverse tm) t))
              ((or (= n 22) (= n 21)) (setf (terminal-bold tm) nil))
              ((= n 27) (setf (terminal-reverse tm) nil))
              ((<= 30 n 37) (setf (terminal-fg tm) (- n 30)))
              ((= n 38) (multiple-value-bind (col adv) (sgr-ext ps (1+ i)) (setf (terminal-fg tm) col) (incf i adv)))
              ((= n 39) (setf (terminal-fg tm) +def-fg+))
              ((<= 40 n 47) (setf (terminal-bg tm) (- n 40)))
              ((= n 48) (multiple-value-bind (col adv) (sgr-ext ps (1+ i)) (setf (terminal-bg tm) col) (incf i adv)))
              ((= n 49) (setf (terminal-bg tm) +def-bg+))
              ((<= 90 n 97) (setf (terminal-fg tm) (+ 8 (- n 90))))
              ((<= 100 n 107) (setf (terminal-bg tm) (+ 8 (- n 100)))))
        (incf i)))))

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
    (#\P (let ((n (p tm 0 1)) (y (terminal-cy tm)))   ; DCH delete chars: shift line left
           (loop for x from (terminal-cx tm) below (- (terminal-cols tm) n)
                 do (setf (cell tm x y) (cell tm (+ x n) y)))
           (loop for x from (max 0 (- (terminal-cols tm) n)) below (terminal-cols tm)
                 do (setf (cell tm x y) (blank-cell)))))
    (#\@ (ins-chars tm (p tm 0 1)))                    ; ICH
    (#\X (ech tm (p tm 0 1)))                          ; ECH
    (#\L (ins-lines tm (p tm 0 1)))                    ; IL
    (#\M (del-lines tm (p tm 0 1)))                    ; DL
    (#\S (dotimes (k (p tm 0 1)) (scroll-up tm)))      ; SU
    (#\T (dotimes (k (p tm 0 1)) (scroll-down tm)))    ; SD
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
         (#\] (setf (terminal-pstate tm) :osc))         ; OSC (window title etc.): swallow to ST/BEL
         (#\P (setf (terminal-pstate tm) :dcs           ; DCS (sixel graphics): collect to ST
                    (terminal-dcs tm) (make-array 64 :element-type 'character :adjustable t :fill-pointer 0)))
         ((#\( #\)) (setf (terminal-pstate tm) :charset))
         (#\7 (decsc tm) (setf (terminal-pstate tm) :ground))          ; DECSC save cursor
         (#\8 (decrc tm) (setf (terminal-pstate tm) :ground))          ; DECRC restore cursor
         (#\D (line-feed tm) (setf (terminal-pstate tm) :ground))      ; IND index
         (#\E (setf (terminal-cx tm) 0) (line-feed tm) (setf (terminal-pstate tm) :ground))  ; NEL
         (#\M (if (<= (terminal-cy tm) (terminal-top tm)) (scroll-down tm) (decf (terminal-cy tm)))  ; RI
              (setf (terminal-pstate tm) :ground))
         (t (setf (terminal-pstate tm) :ground))))
      (:charset (setf (terminal-pstate tm) :ground))
      (:osc (when (or (= code 7) (= code 27)) (setf (terminal-pstate tm) :ground)))
      (:dcs (if (= code 27) (setf (terminal-pstate tm) :dcs-esc)   ; maybe ST (ESC \)
                (vector-push-extend c (terminal-dcs tm))))
      (:dcs-esc (cond ((char= c #\\) (process-dcs tm) (setf (terminal-pstate tm) :ground))
                      (t (vector-push-extend (code-char 27) (terminal-dcs tm))
                         (vector-push-extend c (terminal-dcs tm)) (setf (terminal-pstate tm) :dcs))))
      (:csi
       (cond
         ((char= c #\?) (setf (terminal-ppriv tm) t))
         ((digit-char-p c) (setf (terminal-pcur tm) (+ (* (or (terminal-pcur tm) 0) 10) (digit-char-p c))))
         ((char= c #\;) (setf (terminal-params tm) (append (terminal-params tm) (list (or (terminal-pcur tm) 0)))
                              (terminal-pcur tm) nil))
         ((<= 64 code 126)
          (when (terminal-pcur tm) (setf (terminal-params tm) (append (terminal-params tm) (list (terminal-pcur tm)))))
          (cond ((and (terminal-ppriv tm) (member c '(#\h #\l))) (private-mode tm (char= c #\h)))
                ((not (terminal-ppriv tm)) (csi-dispatch tm c)))
          (setf (terminal-pstate tm) :ground)))))))

;;; ---- sixel graphics (DCS <params> q <data> ST) -----------------------------

(defun sixel-decode (data start)
  "Decode sixel DATA from index START (just after the 'q') into a glass fb."
  (let ((pal (make-array 256 :initial-element (glass:rgb 0 0 0)))
        (cur 0) (x 0) (band 0) (maxx 0) (maxy 0) (i start) (n (length data))
        (px (make-hash-table :test 'eql)))          ; y*1e6+x -> colour
    (labels ((num () (let ((v 0) (any nil))
                       (loop while (and (< i n) (digit-char-p (char data i)))
                             do (setf v (+ (* v 10) (digit-char-p (char data i))) any t) (incf i))
                       (and any v)))
             (semi () (when (and (< i n) (char= (char data i) #\;)) (incf i)))
             (plot (bits) (dotimes (b 6)
                            (when (logbitp b bits)
                              (let ((yy (+ (* band 6) b)))
                                (setf (gethash (+ (* yy 1000000) x) px) (aref pal cur)
                                      maxx (max maxx (1+ x)) maxy (max maxy (1+ yy))))))
               (incf x)))
      (loop while (< i n) do
        (let ((c (char data i)))
          (cond
            ((char= c #\#) (incf i)
             (let ((pc (or (num) 0)))
               (if (and (< i n) (char= (char data i) #\;))
                   (progn (semi) (num) (semi)         ; colour-space id ignored (assume RGB)
                          (let ((r (or (num) 0))) (semi)
                            (let ((g (or (num) 0))) (semi)
                              (let ((bl (or (num) 0)))
                                (setf (aref pal (logand pc 255))
                                      (glass:rgb (round (* r 255) 100) (round (* g 255) 100) (round (* bl 255) 100)))))))
                   (setf cur (logand pc 255)))))
            ((char= c #\!) (incf i)
             (let ((rep (or (num) 1)))
               (when (< i n) (let ((sc (char data i))) (incf i)
                               (when (<= #x3f (char-code sc) #x7e)
                                 (dotimes (k rep) (plot (- (char-code sc) #x3f))))))))
            ((char= c #\$) (setf x 0) (incf i))
            ((char= c #\-) (setf x 0) (incf band) (incf i))
            ((char= c #\") (incf i) (num) (semi) (num) (semi) (num) (semi) (num))  ; raster attrs
            ((<= #x3f (char-code c) #x7e) (plot (- (char-code c) #x3f)) (incf i))
            (t (incf i)))))
      (when (and (plusp maxx) (plusp maxy))
        (let ((fb (glass:make-framebuffer maxx maxy glass:+black+)))
          (maphash (lambda (k v) (glass:fb-put fb (mod k 1000000) (floor k 1000000) v)) px)
          fb)))))

(defun process-dcs (tm)
  "The collected DCS: if it is a sixel (has a 'q'), decode it and place the image
   at the cursor, then move the cursor below it."
  (let* ((data (terminal-dcs tm)) (q (and data (position #\q data))))
    (setf (terminal-dcs tm) nil)
    (when q
      (let ((img (sixel-decode data (1+ q))))
        (when img
          (push (list* (* (terminal-cx tm) (terminal-cell-w tm))
                       (* (terminal-cy tm) (terminal-cell-h tm)) img)
                (terminal-graphics tm))
          (setf (terminal-cx tm) 0)
          (dotimes (k (ceiling (glass:fb-height img) (terminal-cell-h tm)))
            (line-feed tm)))))))

(defun feed-cp (tm cp)
  (when (and (plusp cp) (<= cp #x10FFFF)) (feed-char tm (code-char cp))))

(defun feed-byte (tm b)
  "Decode a UTF-8 byte stream into codepoints, feeding each to the VT parser."
  (cond
    ((plusp (terminal-u8-need tm))
     (if (<= #x80 b #xBF)
         (progn (setf (terminal-u8-cp tm) (logior (ash (terminal-u8-cp tm) 6) (logand b #x3F)))
                (when (zerop (decf (terminal-u8-need tm))) (feed-cp tm (terminal-u8-cp tm))))
         (progn (setf (terminal-u8-need tm) 0) (feed-byte tm b))))   ; malformed: restart
    ((< b #x80) (feed-cp tm b))
    ((<= #xC0 b #xDF) (setf (terminal-u8-cp tm) (logand b #x1F) (terminal-u8-need tm) 1))
    ((<= #xE0 b #xEF) (setf (terminal-u8-cp tm) (logand b #x0F) (terminal-u8-need tm) 2))
    ((<= #xF0 b #xF7) (setf (terminal-u8-cp tm) (logand b #x07) (terminal-u8-need tm) 3))))

;;; ---- rendering (glyph-cached scribe) ---------------------------------------

(declaim (inline over))
(defun over (dst8 fg8 a ia)
  (scribe:linear->srgb (+ (* ia (scribe:srgb->linear dst8)) (* a (scribe:srgb->linear fg8)))))

(defun blit-mono (fb cov gw gh ox oy fg)
  "Blend monochrome coverage COV (gw x gh) in colour FG at (OX,OY), gamma-correct."
  (let ((dpx (glass:fb-pixels fb)) (fw (glass:fb-width fb)) (fh (glass:fb-height fb))
        (fr (ldb (byte 8 16) fg)) (fgc (ldb (byte 8 8) fg)) (fbc (ldb (byte 8 0) fg)))
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
                                  (over (ldb (byte 8 0) d) fbc a ia)))))))))))))

(defun blit-color (fb rgb alpha gw gh ox oy)
  "Composite a premultiplied-RGB + ALPHA colour glyph (gw x gh) at (OX,OY): the
   straight-over of premultiplied colour on the existing pixel."
  (let ((dpx (glass:fb-pixels fb)) (fw (glass:fb-width fb)) (fh (glass:fb-height fb)))
    (dotimes (yy gh)
      (let ((yv (+ oy yy)))
        (when (< -1 yv fh)
          (let ((row (* yv fw)))
            (dotimes (xx gw)
              (let* ((i (+ (* yy gw) xx)) (a (aref alpha i)) (xv (+ ox xx)))
                (when (and (plusp a) (< -1 xv fw))
                  (let* ((di (+ row xv)) (d (aref dpx di)) (ia (- 255 a)))
                    (flet ((ch (pm dc) (min 255 (+ pm (floor (* dc ia) 255)))))
                      (setf (aref dpx di)
                            (logior (ash (ch (aref rgb (* 3 i))       (ldb (byte 8 16) d)) 16)
                                    (ash (ch (aref rgb (+ (* 3 i) 1)) (ldb (byte 8 8) d)) 8)
                                    (ch (aref rgb (+ (* 3 i) 2))      (ldb (byte 8 0) d)))))))))))))))

(defun render-glyph (tm code)
  "Render CODE: colour (COLR emoji) -> (:color rgb alpha w h left top adv), else
   monochrome coverage -> (:mono cov w h left top adv), picking the primary mono
   font, a colour-emoji font, or a scribe script fallback as appropriate."
  (let ((primary (terminal-font tm)) (ppem (terminal-ppem tm)) (ef (terminal-emoji-font tm)))
    (cond
      ((scribe:font-covers-p primary code)
       (list* :mono (multiple-value-list
                     (scribe:rasterize-glyph primary (scribe:font-glyph-index primary code) ppem))))
      ((and ef (let ((g (scribe:font-glyph-index ef code))) (and g (plusp g) (scribe:color-glyph-p ef g))))
       (list* :color (multiple-value-list
                      (scribe:rasterize-color-glyph ef (scribe:font-glyph-index ef code) ppem))))
      (t (let* ((f (or (ignore-errors (scribe:fallback-font-for-codepoint code)) primary))
                (g (scribe:font-glyph-index f code)))
           (if (and g (plusp g) (scribe:color-glyph-p f g))   ; scribe's emoji fallback is now COLR
               (list* :color (multiple-value-list (scribe:rasterize-color-glyph f g ppem)))
               (list* :mono (multiple-value-list (scribe:rasterize-glyph f g ppem)))))))))

(defun glyph (tm code)
  (or (gethash code (terminal-glyphs tm))
      (setf (gethash code (terminal-glyphs tm)) (render-glyph tm code))))

(defun draw-cell (tm x y)
  (let ((c (cell tm x y)))
    (when (cell-spacer-p c) (return-from draw-cell))   ; right half of a wide char
  (let* ((fb (terminal-fb tm))
         (cw (terminal-cell-w tm)) (ch (terminal-cell-h tm))
         (span (if (cell-wide-p c) 2 1))
         (px0 (* x cw)) (py0 (* y ch))
         (cursor (and (terminal-cursor-vis tm) (= x (terminal-cx tm)) (= y (terminal-cy tm))))
         (fg (pal (if cursor (cell-bg c) (cell-fg c))))
         (bg (pal (if cursor (cell-fg c) (cell-bg c)))))   ; cursor = inverse block
    (glass:fb-rect fb px0 py0 (* cw span) ch bg)
    (let ((code (cell-char c)))
      (when (> code 32)
        (let ((g (glyph tm code)) (asc (terminal-ascent tm)))
          (ecase (car g)
            (:mono (destructuring-bind (cov gw gh left top &rest ign) (cdr g)
                     (declare (ignore ign))
                     (when cov (blit-mono fb cov gw gh (+ px0 left) (+ py0 asc top) fg))))
            (:color (destructuring-bind (rgb alpha gw gh left top &rest ign) (cdr g)
                      (declare (ignore ign))
                      (when rgb (blit-color fb rgb alpha gw gh (+ px0 left) (+ py0 asc top))))))))))))

(defun blit-image (src ox oy dst)
  "Composite glass fb SRC into DST at (OX,OY)."
  (let* ((sw (glass:fb-width src)) (sh (glass:fb-height src)) (sp (glass:fb-pixels src))
         (dp (glass:fb-pixels dst)) (dw (glass:fb-width dst)) (dh (glass:fb-height dst)))
    (dotimes (yy sh)
      (let ((dy (+ oy yy)))
        (when (< -1 dy dh)
          (let ((drow (* dy dw)) (srow (* yy sw)))
            (dotimes (xx sw)
              (let ((dx (+ ox xx)))
                (when (< -1 dx dw) (setf (aref dp (+ drow dx)) (aref sp (+ srow xx))))))))))))

(defun render (tm)
  (glass:with-fb-locked ((terminal-fb tm))
    (dotimes (y (terminal-rows tm))
      (dotimes (x (terminal-cols tm)) (draw-cell tm x y)))
    (dolist (g (reverse (terminal-graphics tm)))       ; sixel images on top, oldest first
      (blit-image (cddr g) (car g) (cadr g) (terminal-fb tm)))))

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

(defun mouse-send (tm cb col row press-p)
  (let ((out (terminal-pty tm)))
    (if (terminal-mouse-sgr tm)
        (format out "~c[<~d;~d;~d~a" (code-char 27) cb col row (if press-p "M" "m"))
        (let ((b (if press-p cb 3)))                       ; legacy X10: release is button 3
          (write-char (code-char 27) out) (write-char #\[ out) (write-char #\M out)
          (write-char (code-char (min 255 (+ b 32))) out)
          (write-char (code-char (min 255 (+ col 32))) out)
          (write-char (code-char (min 255 (+ row 32))) out)))
    (force-output out)))

(defun on-mouse (tm mask lx ly)
  "Feed a pointer event (RFB button MASK; content-pixel LX,LY).  If the app has
   enabled mouse tracking, encode it (SGR or legacy) and write it to the shell."
  (when (terminal-mouse-mode tm)
    (let* ((col (max 1 (1+ (floor lx (terminal-cell-w tm)))))
           (row (max 1 (1+ (floor ly (terminal-cell-h tm)))))
           (real (logand mask 7)) (changed (logxor real (terminal-mouse-buttons tm))))
      (when (logtest mask 8)  (mouse-send tm 64 col row t))    ; wheel up
      (when (logtest mask 16) (mouse-send tm 65 col row t))    ; wheel down
      (cond
        ((plusp changed)                                       ; a button pressed / released
         (loop for (bit . code) in '((1 . 0) (2 . 1) (4 . 2))
               when (logtest changed bit)
               do (mouse-send tm code col row (logtest real bit))))
        ((and (or (eq (terminal-mouse-mode tm) :any)
                  (and (eq (terminal-mouse-mode tm) :button) (plusp real)))
              (or (/= col (terminal-mouse-col tm)) (/= row (terminal-mouse-row tm))))
         (mouse-send tm (+ 32 (cond ((logtest real 1) 0) ((logtest real 2) 1) ((logtest real 4) 2) (t 3)))
                     col row t)))                              ; motion (drag / any)
      (setf (terminal-mouse-buttons tm) real
            (terminal-mouse-col tm) col (terminal-mouse-row tm) row))))

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

;; Colour emoji come from scribe's bundled COLR fallback (Twemoji) by default;
;; :emoji-font is an optional override (a path to another COLR font).
(defun load-emoji-font (&optional path)
  (and path (ignore-errors (glass:load-font path))))

(defun make-terminal (&key (cols 80) (rows 24) (ppem 16) (shell "/bin/bash") emoji-font)
  (let* ((font (glass:load-font (asdf:system-relative-pathname :scribe "fonts/LiberationMono-Regular.ttf")))
         (upem (scribe:font-units-per-em font))
         (cell-w (max 1 (ceiling (nth-value 5 (scribe:rasterize-glyph font (scribe:font-glyph-index font (char-code #\M)) ppem)))))
         (asc (round (* (scribe:font-ascent font) ppem) upem))
         (desc (round (* (- (scribe:font-descent font)) ppem) upem))
         (cell-h (+ asc desc 2))
         ;; setsid -c makes the pty the controlling terminal: real job control,
         ;; no "cannot set process group" warning, and readline echoes normally.
         (proc (sb-ext:run-program "/usr/bin/setsid" (list "-c" shell "--norc" "-i")
                                   :pty t :wait nil
                                   :external-format :latin-1   ; read raw bytes; we UTF-8 decode
                                   :environment (list "TERM=xterm-256color" "PS1=\\w $ "
                                                      "PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"
                                                      (format nil "COLUMNS=~d" cols) (format nil "LINES=~d" rows)
                                                      (format nil "HOME=~a" (or (sb-ext:posix-getenv "HOME") "/root")))))
         (pty (sb-ext:process-pty proc))
         (tm (%make-terminal :cols cols :rows rows :cells (make-grid cols rows)
                            :bot (1- rows) :pty pty :proc proc :font font :ppem ppem
                            :emoji-font (load-emoji-font emoji-font)
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
                         (t (feed-byte tm (char-code c)) (setf got t))))
        (stream-error () (return-from pump)))
      (when got (render tm))
      (sleep 1/60))))

(defun start-pump (tm)
  "Spawn the shell-output pump thread; it renders into (terminal-fb TM).  Use
   when embedding a terminal (e.g. as a WM window) rather than the standalone RUN."
  (sb-thread:make-thread (lambda () (ignore-errors (pump tm))) :name "glass-term-pump"))

;;; ---- tabbed terminal --------------------------------------------------------
;;; One window, several shells: a tab bar across the top, the active shell below.
;;; It is just a glass framebuffer + input handlers — the WM adds it as a surface
;;; window like any other.  A render thread composites the tab bar and the active
;;; terminal's framebuffer; clicking a tab switches, clicking "+" opens a new one.

(defstruct tabterm terminals (active 0) cols rows ppem cell-w cell-h (tab-h 22) fb (last-mask 0))

(defun tabterm-active-term (tt) (nth (tabterm-active tt) (tabterm-terminals tt)))
(defun tabterm-tabw (tt)
  (min 130 (floor (glass:fb-width (tabterm-fb tt)) (1+ (length (tabterm-terminals tt))))))

(defun tabterm-new (tt)
  "Open a new shell tab and make it active."
  (let ((term (make-terminal :cols (tabterm-cols tt) :rows (tabterm-rows tt) :ppem (tabterm-ppem tt))))
    (start-pump term)
    (setf (tabterm-terminals tt) (append (tabterm-terminals tt) (list term))
          (tabterm-active tt) (1- (length (tabterm-terminals tt))))
    term))

(defun tabterm-render (tt)
  (let* ((fb (tabterm-fb tt)) (th (tabterm-tab-h tt)) (n (length (tabterm-terminals tt)))
         (tabw (tabterm-tabw tt)))
    (glass:with-fb-locked (fb)
      (glass:fb-rect fb 0 0 (glass:fb-width fb) th (glass:rgb 38 38 44))
      (dotimes (i n)                                         ; the tabs
        (let* ((x (* i tabw)) (active (= i (tabterm-active tt))))
          (glass:fb-rect fb (1+ x) 2 (- tabw 2) (- th 3) (if active (glass:rgb 20 20 24) (glass:rgb 58 58 66)))
          (when active (glass:fb-rect fb (1+ x) (- th 2) (- tabw 2) 2 (glass:rgb 114 159 207)))
          (glass:fb-text fb (+ x 10) 4 (format nil "sh ~d" (1+ i)) :size 12
                         :color (if active glass:+white+ (glass:rgb 185 185 195)))))
      (let ((x (* n tabw)))                                  ; the "+" new-tab button
        (glass:fb-rect fb (1+ x) 2 (- tabw 2) (- th 3) (glass:rgb 48 48 56))
        (glass:fb-text fb (+ x (floor tabw 2) -4) 3 "+" :size 15 :color (glass:rgb 150 220 150)))
      (blit-image (terminal-fb (tabterm-active-term tt)) 0 th fb))))   ; active shell

(defun tabterm-on-key (tt down keysym) (on-key (tabterm-active-term tt) down keysym))

(defun tabterm-on-mouse (tt mask lx ly)
  (let ((th (tabterm-tab-h tt))
        (press (and (logtest mask 1) (not (logtest (tabterm-last-mask tt) 1)))))
    (setf (tabterm-last-mask tt) mask)
    (if (< ly th)                                            ; on the tab bar
        (when press
          (let ((n (length (tabterm-terminals tt))) (idx (floor lx (tabterm-tabw tt))))
            (cond ((< idx n) (setf (tabterm-active tt) idx))
                  ((= idx n) (tabterm-new tt)))))
        (on-mouse (tabterm-active-term tt) mask lx (- ly th)))))  ; forward to the active shell

(defun make-tabbed-terminal (&key (cols 80) (rows 24) (ppem 14))
  "A tabbed terminal: a glass framebuffer showing a tab bar and one of several
   shells.  Use TABTERM-FB / TABTERM-ON-KEY / TABTERM-ON-MOUSE to embed it."
  (let* ((term (make-terminal :cols cols :rows rows :ppem ppem))
         (cw (terminal-cell-w term)) (ch (terminal-cell-h term)) (tab-h 22)
         (tt (make-tabterm :terminals (list term) :cols cols :rows rows :ppem ppem
                           :cell-w cw :cell-h ch :tab-h tab-h
                           :fb (glass:make-framebuffer (* cols cw) (+ tab-h (* rows ch)) glass:+black+))))
    (start-pump term)
    (sb-thread:make-thread (lambda () (loop (ignore-errors (tabterm-render tt)) (sleep 1/30)))
                           :name "tabterm-render")
    tt))

(defun run (&key (port 5900) (cols 80) (rows 24) (ppem 16) (shell "/bin/bash") emoji-font)
  "Open SHELL in a pseudo-terminal and serve it as a terminal over VNC on PORT.
   Point any VNC client at localhost:PORT.  Blocks until the shell exits.
   EMOJI-FONT is an optional COLR colour-emoji font path (else a bundled one is tried)."
  (let ((tm (make-terminal :cols cols :rows rows :ppem ppem :shell shell :emoji-font emoji-font)))
    (render tm)
    (sb-thread:make-thread
     (lambda () (ignore-errors
                 (glass:serve (terminal-fb tm) port
                              :on-key (lambda (down k) (ignore-errors (on-key tm down k)))
                              :name "glass-term")))
     :name "glass-term-server")
    (pump tm)
    tm))
