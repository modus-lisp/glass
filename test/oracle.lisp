;;;; oracle.lisp — drive the scry RFB server with an in-process RFB client and
;;;; check (a) the pixels received equal what was drawn, under BOTH Raw and
;;;; Hextile; (b) dirty-region tracking (incremental sends only changed tiles);
;;;; (c) Hextile is compact (a mostly-solid frame costs far fewer bytes than Raw).
;;;;
;;;;   (asdf:load-system "scry/test") (scry/test:run-tests)

(defpackage #:scry/test
  (:use #:cl)
  (:export #:run-tests))
(in-package #:scry/test)

(defvar *checks* 0) (defvar *fails* 0)
(defvar *bytes* 0)                       ; wire bytes read (for compactness checks)
(defun check (ok fmt &rest args)
  (incf *checks*) (unless ok (incf *fails*) (format t "  FAIL: ~?~%" fmt args)))

;;; ---- minimal RFB client (big-endian wire, counts bytes) --------------------

(defun r8  (s) (incf *bytes*) (read-byte s))
(defun r16 (s) (logior (ash (r8 s) 8) (r8 s)))
(defun r32 (s) (logior (ash (r16 s) 16) (r16 s)))
(defun rn  (s n) (incf *bytes* n)
  (let ((b (make-array n :element-type '(unsigned-byte 8)))) (read-sequence b s) b))
(defun read-pixel (s) (let ((b (rn s 4))) (logior (ash (aref b 2) 16) (ash (aref b 1) 8) (aref b 0))))
(defun w8  (s v) (write-byte (logand v #xff) s))
(defun w16 (s v) (w8 s (ash v -8)) (w8 s v))
(defun w32 (s v) (w16 s (ash v -16)) (w16 s v))

(defun rfb-connect (host port)
  (loop repeat 60
        do (handler-case
               (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
                 (sb-bsd-sockets:socket-connect sock (sb-bsd-sockets:make-inet-address host) port)
                 (return (sb-bsd-sockets:socket-make-stream
                          sock :input t :output t :element-type '(unsigned-byte 8) :buffering :full)))
             (error () (sleep 0.05)))))

(defun rfb-open (port)
  (let ((s (rfb-connect "127.0.0.1" port)))
    (rn s 12) (write-sequence (map 'vector #'char-code "RFB 003.008") s) (w8 s 10) (force-output s)
    (let ((n (r8 s))) (rn s n)) (w8 s 1) (force-output s) (r32 s)   ; None security
    (w8 s 1) (force-output s)                                       ; ClientInit
    (let ((w (r16 s)) (h (r16 s)))
      (rn s 16) (let ((nl (r32 s))) (rn s nl))
      (values s w h))))

(defun set-encodings (s &rest encs)
  (w8 s 2) (w8 s 0) (w16 s (length encs)) (dolist (e encs) (w32 s e)) (force-output s))

(defun request (s inc w h)
  (w8 s 3) (w8 s inc) (w16 s 0) (w16 s 0) (w16 s w) (w16 s h) (force-output s))

;;; ---- decode ZRLE: chipz inflates the shared stream, we parse the tiles ------
;;;
;;; The ZRLE zlib stream is continuous for the connection (compression state is
;;; retained across rectangles), so *DSTATE* is a persistent chipz inflate state;
;;; each rect's sync-flushed chunk decodes to exactly that rect's tile bytes.

(defvar *dstate* nil)

(defun cpixel-at (dec p)
  "The 3-byte CPIXEL at DEC[p] as a pixel; returns (values pixel next-pos)."
  (values (logior (ash (aref dec (+ p 2)) 16) (ash (aref dec (+ p 1)) 8) (aref dec p))
          (+ p 3)))

(defun apply-zrle-tiles (dec pos rx ry rw rh w cli)
  "Parse 64x64 ZRLE tiles (subencodings 0 raw / 1 solid / 2-16 packed palette)
   from DEC starting at POS, writing pixels into CLI.  Returns the new POS."
  (loop for ty from 0 below rh by 64 for th = (min 64 (- rh ty)) do
    (loop for tx from 0 below rw by 64 for tw = (min 64 (- rw tx)) do
      (let ((sub (aref dec pos)))
        (incf pos)
        (flet ((put (lx ly c) (setf (aref cli (+ (* (+ ry ty ly) w) (+ rx tx lx))) c)))
          (cond
            ((= sub 0)                                      ; raw
             (dotimes (ly th) (dotimes (lx tw)
               (multiple-value-bind (c np) (cpixel-at dec pos) (put lx ly c) (setf pos np)))))
            ((= sub 1)                                      ; solid
             (multiple-value-bind (c np) (cpixel-at dec pos)
               (setf pos np)
               (dotimes (ly th) (dotimes (lx tw) (put lx ly c)))))
            ((<= 2 sub 16)                                  ; packed palette
             (let ((pal (make-array sub)))
               (dotimes (i sub)
                 (multiple-value-bind (c np) (cpixel-at dec pos) (setf (aref pal i) c pos np)))
               (let ((bpp (cond ((<= sub 2) 1) ((<= sub 4) 2) (t 4))))
                 (dotimes (ly th)
                   (let ((acc 0) (nbits 0))                 ; MSB-first, rows byte-padded
                     (dotimes (lx tw)
                       (when (< nbits bpp)
                         (setf acc (logior (ash acc 8) (aref dec pos)) nbits (+ nbits 8))
                         (incf pos))
                       (decf nbits bpp)
                       (put lx ly (aref pal (logand (ash acc (- nbits)) (1- (ash 1 bpp)))))
                       (setf acc (logand acc (1- (ash 1 nbits))))))))))
            (t (check nil "unhandled ZRLE subencoding ~a" sub)))))))
  pos)

(defun apply-zrle (s rx ry rw rh w cli)
  (let* ((len (r32 s)) (chunk (rn s len))
         (dec (chipz:decompress nil *dstate* chunk)))
    (apply-zrle-tiles dec 0 rx ry rw rh w cli))
  (* rw rh))

;;; ---- decode Raw and Hextile into a client framebuffer ----------------------

(defun apply-raw (s rx ry rw rh w cli)
  (dotimes (yy rh) (dotimes (xx rw)
    (setf (aref cli (+ (* (+ ry yy) w) (+ rx xx))) (read-pixel s))))
  (* rw rh))

(defun apply-hextile (s rx ry rw rh w cli)
  (let ((bg 0) (fg 0))
    (loop for ty from 0 below rh by 16 for th = (min 16 (- rh ty)) do
      (loop for tx from 0 below rw by 16 for tw = (min 16 (- rw tx)) do
        (flet ((put (lx ly c) (setf (aref cli (+ (* (+ ry ty ly) w) (+ rx tx lx))) c)))
          (let ((mask (r8 s)))
            (cond
              ((logtest mask 1) (dotimes (ly th) (dotimes (lx tw) (put lx ly (read-pixel s)))))
              (t (when (logtest mask 2) (setf bg (read-pixel s)))
                 (when (logtest mask 4) (setf fg (read-pixel s)))
                 (dotimes (ly th) (dotimes (lx tw) (put lx ly bg)))
                 (when (logtest mask 8)
                   (dotimes (k (r8 s))
                     (let ((color (if (logtest mask 16) (read-pixel s) fg))
                           (xy (r8 s)) (wh (r8 s)))
                       (let ((sx (ash xy -4)) (sy (logand xy 15))
                             (sw (1+ (ash wh -4))) (sh (1+ (logand wh 15))))
                         (dotimes (jy sh) (dotimes (jx sw) (put (+ sx jx) (+ sy jy) color)))))))))))))
    (* rw rh)))

(defun read-update (s w cli)
  "Read one FramebufferUpdate (Raw or Hextile per rect), apply into CLI.
   Returns (values n-rects total-pixels)."
  (let ((mt (r8 s))) (check (= mt 0) "update msg-type ~a (want 0)" mt))
  (r8 s)
  (let ((nrects (r16 s)) (total 0))
    (dotimes (i nrects)
      (let ((rx (r16 s)) (ry (r16 s)) (rw (r16 s)) (rh (r16 s)) (enc (r32 s)))
        (incf total (cond ((= enc 0) (apply-raw s rx ry rw rh w cli))
                          ((= enc 5) (apply-hextile s rx ry rw rh w cli))
                          ((= enc 16) (apply-zrle s rx ry rw rh w cli))
                          (t (check nil "unknown encoding ~a" enc) 0)))))
    (values nrects total)))

;;; ---- the test ---------------------------------------------------------------

(defun scenario (port mode)
  "Run a full-frame + change + incremental round trip; return T if pixels match.
   MODE is :raw, :hextile, or :zrle — advertise it and (for the compressed ones)
   check it is used and compact."
  (setf *dstate* (chipz:make-dstate 'chipz:zlib))          ; fresh ZRLE stream per connection
  (let* ((w 200) (h 150) (fb (scry:make-framebuffer w h scry:+blue+))
         (name (string-downcase (symbol-name mode))))
    (scry:fb-rect fb 40 20 60 40 scry:+red+)
    (scry:fb-put fb 5 5 scry:+green+)
    (scry:fb-frame fb 0 0 w h scry:+white+ 2)
    (scry:fb-put fb 199 149 (scry:rgb 18 52 86))
    (let ((server (sb-thread:make-thread
                   (lambda () (ignore-errors (scry:serve-one fb port))) :name "scry-server")))
      (multiple-value-bind (s sw sh) (rfb-open port)
        (check (and (= sw w) (= sh h)) "~a: dimensions ~ax~a" name sw sh)
        (case mode
          (:hextile (set-encodings s 5 0))
          (:zrle    (set-encodings s 16 0)))
        (let ((cli (make-array (* w h) :element-type '(unsigned-byte 32))))
          (setf *bytes* 0)
          (request s 0 w h)
          (multiple-value-bind (nr total) (read-update s w cli)
            (declare (ignore nr))
            (check (= total (* w h)) "full frame covers all ~a px, got ~a" (* w h) total)
            (unless (eq mode :raw)
              (check (< *bytes* 20000) "~a full frame compact: ~a bytes (raw would be ~a)"
                     name *bytes* (* w h 4))))
          (check (equalp cli (scry:fb-pixels fb)) "client matches server after full frame")
          ;; dirty tracking: change one small area, request incremental
          (scry:fb-rect fb 150 100 20 20 scry:+green+)
          (request s 1 w h)
          (multiple-value-bind (nr total) (read-update s w cli)
            (check (< total (* w h)) "incremental (~a px) << full (~a)" total (* w h))
            (check (and (plusp total) (<= total 4096)) "incremental small: ~a px in ~a rects" total nr))
          (check (equalp cli (scry:fb-pixels fb)) "client matches server after incremental")
          (close s)
          (ignore-errors (sb-thread:join-thread server)))))))

(defun run-tests ()
  (setf *checks* 0 *fails* 0)
  (format t "~&[scry RFB oracle]~%")
  (format t "-- Raw --~%")     (scenario 5921 :raw)
  (format t "-- Hextile --~%") (scenario 5922 :hextile)
  (format t "-- ZRLE --~%")    (scenario 5923 :zrle)
  (format t "----------------------------------~%")
  (format t "checks: ~d   failures: ~d   => ~a~%" *checks* *fails* (if (zerop *fails*) "PASS" "FAIL"))
  (zerop *fails*))
