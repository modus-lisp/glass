;;;; interactive.lisp — a live McCLIM frame served over VNC by the glass backend.
;;;;
;;;;   sbcl --dynamic-space-size 4096 --non-interactive --load backend/inspect/interactive.lisp
;;;;
;;;; Defines a tiny CLIM frame (a click/keystroke counter), runs it on a glass
;;;; port, then drives it headlessly: an in-process VNC client reads the rendered
;;;; UI, injects a keystroke and a pointer click, and checks the framebuffer
;;;; changed in response — proving the full loop render + input over RFB, no X.
;;;; Dumps before/after PNGs so the reaction can be eyeballed.

(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:mcclim :mcclim-render :glass :zpng :chipz))
    (require :sb-concurrency)
    (asdf:load-asd "/home/claude/glass/backend/mcclim-glass.asd")
    (asdf:load-system :mcclim-glass)))

(defpackage #:glass-interactive (:use #:cl))
(in-package #:glass-interactive)

;;; ---- a minimal interactive CLIM frame --------------------------------------

(clim:define-application-frame counter ()
  ((n :initform 0 :accessor counter-n))
  (:menu-bar nil)
  (:panes (canvas :application :display-function 'draw-canvas :scroll-bars nil
                  :width 400 :height 220))
  (:layouts (default canvas)))

(defun draw-canvas (frame pane)
  (handler-case
      (progn
        (clim:draw-rectangle* pane 0 0 400 220 :ink (clim:make-rgb-color 0.10 0.12 0.18))
        (clim:draw-rectangle* pane 20 20 (+ 24 (* 24 (counter-n frame))) 70
                              :ink clim:+orange+)                ; grows with the count
        (clim:draw-text* pane (format nil "count: ~d" (counter-n frame)) 40 130
                         :text-size 30 :ink clim:+white+)
        (clim:draw-text* pane "press SPACE or click" 40 175 :text-size 18
                         :ink (clim:make-rgb-color 0.4 0.8 1.0)))
    (error (e) (format *trace-output* "~&[draw-canvas ERROR] ~a: ~a~%" (type-of e) e)
      (force-output *trace-output*))))

(define-counter-command (com-inc :keystroke #\space) ()
  (incf (counter-n clim:*application-frame*)))

(define-counter-command (com-quit :keystroke #\q) ()
  (clim:frame-exit clim:*application-frame*))

;; a click anywhere on the canvas increments
(defmethod clim:handle-event :after ((pane clim:clim-stream-pane)
                                     (event clim:pointer-button-press-event))
  (let ((frame (clim:pane-frame pane)))
    (when (typep frame 'counter)
      (incf (counter-n frame))
      (clim:redisplay-frame-pane frame pane :force-p t))))

;; keyboard: SPACE increments, q quits.  Key events arrive at the top-level sheet
;; (real apps get command :keystroke accelerators via an interactor pane; this
;; demo has no interactor, so it handles keys directly).
(defmethod clim:handle-event :after ((sheet climi::top-level-sheet-mixin)
                                     (event clim:key-press-event))
  (let ((frame (clim:pane-frame sheet))
        (ch (clim:keyboard-event-character event)))
    (when (typep frame 'counter)
      (cond ((eql ch #\Space) (incf (counter-n frame))
             (clim:redisplay-frame-pane frame 'canvas :force-p t))
            ((eql ch #\q) (clim:frame-exit frame))))))

;;; ---- minimal ZRLE-decoding RFB client --------------------------------------

(defun r8 (s) (read-byte s)) (defun r16 (s) (logior (ash (r8 s) 8) (r8 s)))
(defun r32 (s) (logior (ash (r16 s) 16) (r16 s)))
(defun rn (s n) (let ((b (make-array n :element-type '(unsigned-byte 8)))) (read-sequence b s) b))
(defun w8 (s v) (write-byte (logand v #xff) s))
(defun w16 (s v) (w8 s (ash v -8)) (w8 s v)) (defun w32 (s v) (w16 s (ash v -16)) (w16 s v))

(defun connect (port)
  (loop repeat 200
        do (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
             (handler-case
                 (progn (sb-bsd-sockets:socket-connect
                         sock (sb-bsd-sockets:make-inet-address "127.0.0.1") port)
                        (return-from connect
                          (sb-bsd-sockets:socket-make-stream
                           sock :input t :output t :element-type '(unsigned-byte 8) :buffering :full)))
               (error () (ignore-errors (sb-bsd-sockets:socket-close sock)) (sleep 0.05))))
        finally (error "could not connect to port ~d" port)))

(defun handshake (s)
  (rn s 12) (write-sequence (map 'vector #'char-code "RFB 003.008") s) (w8 s 10) (force-output s)
  (let ((n (r8 s))) (rn s n)) (w8 s 1) (force-output s) (r32 s)
  (w8 s 1) (force-output s)
  (let ((w (r16 s)) (h (r16 s))) (rn s 16) (let ((nl (r32 s))) (rn s nl))
    (w8 s 2) (w8 s 0) (w16 s 2) (w32 s 16) (w32 s 0) (force-output s)   ; ZRLE, Raw
    (values w h)))

(defun read-update (s w h dstate cli)
  (w8 s 3) (w8 s 1) (w16 s 0) (w16 s 0) (w16 s w) (w16 s h) (force-output s)  ; incremental
  (r8 s) (r8 s)
  (dotimes (i (r16 s))
    (let ((rx (r16 s)) (ry (r16 s)) (rw (r16 s)) (rh (r16 s)) (enc (r32 s)))
      (cond
        ((= enc 16)
         (let* ((len (r32 s)) (chunk (rn s len)) (dec (chipz:decompress nil dstate chunk)) (pos 0))
           (loop for ty from 0 below rh by 64 for th = (min 64 (- rh ty)) do
             (loop for tx from 0 below rw by 64 for tw = (min 64 (- rw tx)) do
               (let ((sub (aref dec pos))) (incf pos)
                 (flet ((cp () (prog1 (logior (ash (aref dec (+ pos 2)) 16) (ash (aref dec (+ pos 1)) 8)
                                              (aref dec pos)) (incf pos 3)))
                        (put (lx ly c) (setf (aref cli (+ (* (+ ry ty ly) w) (+ rx tx lx))) c)))
                   (cond
                     ((= sub 0) (dotimes (ly th) (dotimes (lx tw) (put lx ly (cp)))))
                     ((= sub 1) (let ((c (cp))) (dotimes (ly th) (dotimes (lx tw) (put lx ly c)))))
                     ((<= 2 sub 16)
                      (let ((pal (make-array sub)) (bpp (cond ((<= sub 2) 1) ((<= sub 4) 2) (t 4))))
                        (dotimes (k sub) (setf (aref pal k) (cp)))
                        (dotimes (ly th)
                          (let ((acc 0) (nb 0))
                            (dotimes (lx tw)
                              (when (< nb bpp) (setf acc (logior (ash acc 8) (aref dec pos)) nb (+ nb 8)) (incf pos))
                              (decf nb bpp)
                              (put lx ly (aref pal (logand (ash acc (- nb)) (1- (ash 1 bpp)))))
                              (setf acc (logand acc (1- (ash 1 nb)))))))))
                     (t (error "ZRLE subenc ~a" sub)))))))))
        ((= enc 0) (dotimes (yy rh) (dotimes (xx rw)
                     (let ((b (rn s 4)))
                       (setf (aref cli (+ (* (+ ry yy) w) (+ rx xx)))
                             (logior (ash (aref b 2) 16) (ash (aref b 1) 8) (aref b 0)))))))
        (t (error "enc ~a" enc)))))
  cli)

(defun send-key (s down keysym) (w8 s 4) (w8 s (if down 1 0)) (w16 s 0) (w32 s keysym) (force-output s))
(defun send-ptr (s mask x y) (w8 s 5) (w8 s mask) (w16 s x) (w16 s y) (force-output s))

(defun save-png (cli w h path)
  (let* ((png (make-instance 'zpng:png :width w :height h :color-type :truecolor))
         (d (zpng:data-array png)))
    (dotimes (y h) (dotimes (x w) (let ((p (aref cli (+ (* y w) x))))
      (setf (aref d y x 0) (ldb (byte 8 16) p) (aref d y x 1) (ldb (byte 8 8) p) (aref d y x 2) (ldb (byte 8 0) p)))))
    (zpng:write-png png path) path))

(defun diff-count (a b) (let ((n 0)) (dotimes (i (length a)) (unless (= (aref a i) (aref b i)) (incf n))) n))

;;; ---- drive it --------------------------------------------------------------

(let ((port 5934))
  (sb-thread:make-thread
   (lambda () (handler-case (clim-glass:run-frame 'counter :port port :width 400 :height 220)
                (error (e) (format t "~&FRAME-THREAD-ERROR ~a: ~a~%" (type-of e) e) (finish-output))))
   :name "counter-frame")
  (let ((s (connect port)))
    (multiple-value-bind (w h) (handshake s)
      (format t "~&connected: framebuffer ~dx~d~%" w h)
      (let ((dstate (chipz:make-dstate 'chipz:zlib))
            (cli (make-array (* w h) :element-type '(unsigned-byte 32))))
        (sleep 1.0)                                            ; let the frame paint
        (read-update s w h dstate cli)
        (let ((before (copy-seq cli)))
          (save-png cli w h "/tmp/glass-counter-0.png")
          (format t "initial frame captured (nonblank: ~a)~%"
                  (> (diff-count cli (make-array (* w h) :element-type '(unsigned-byte 32))) 1000))
          ;; press SPACE
          (send-key s t #x20) (send-key s nil #x20) (sleep 0.6)
          (read-update s w h dstate cli)
          (let ((d1 (diff-count before cli)))
            (save-png cli w h "/tmp/glass-counter-1.png")
            (format t "after SPACE: ~d pixels changed -> ~a~%" d1 (if (plusp d1) "REACTED" "no change"))
            ;; click in the canvas
            (let ((after-key (copy-seq cli)))
              (send-ptr s 1 200 110) (send-ptr s 0 200 110) (sleep 0.6)
              (read-update s w h dstate cli)
              (let ((d2 (diff-count after-key cli)))
                (save-png cli w h "/tmp/glass-counter-2.png")
                (format t "after CLICK: ~d pixels changed -> ~a~%" d2 (if (plusp d2) "REACTED" "no change"))))))
        (send-key s t #x71) (send-key s nil #x71)              ; 'q' to quit the frame
        (ignore-errors (close s))))))
(finish-output)
(sb-ext:exit)
