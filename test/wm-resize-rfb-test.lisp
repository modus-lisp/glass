;;;; test/wm-resize-rfb-test.lisp — a client's SetDesktopSize, end to end over a real RFB connection
;;;; to START-WM: the desktop becomes the size asked for, the client is told, and EVERY pixel of the
;;;; new screen reaches it.  The client asks for frames at its OLD size, as noVNC does until it hears
;;;; of the new one -- clipped to that request, the rest of a bigger screen was never sent.
;;;;   sbcl --script test/wm-resize-rfb-test.lisp   (with glass/wm findable by ASDF; exits 0 iff all pass)

(require :asdf)
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream))) (asdf:load-system :glass/wm)))
(require :sb-bsd-sockets)

(defpackage #:glass-wm-resize-rfb-test (:use #:cl))
(in-package #:glass-wm-resize-rfb-test)

(defvar *fails* 0)
(defun check (name ok &optional detail)
  (format t "~&RESIZE ~:[FAIL~;ok  ~] ~a~@[   ~a~]~%" ok name detail)
  (unless ok (incf *fails*)))

(defun free-port ()
  (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (sb-bsd-sockets:socket-bind s #(127 0 0 1) 0)
    (prog1 (nth-value 1 (sb-bsd-sockets:socket-name s)) (sb-bsd-sockets:socket-close s))))

(defun run (encodings label)
  "Connect offering ENCODINGS (u32s), ask for 786x1390, keep requesting at 640x480."
  (let* ((port (free-port)) (nw 786) (nh 1390)
         (seen (make-array (* nw nh) :element-type 'bit :initial-element 0))
         (sized nil) (deadline 0)
         (sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (let ((*trace-output* (make-broadcast-stream)) (*error-output* (make-broadcast-stream)))
      (glass.wm:start-wm port :width 640 :height 480))
    (loop repeat 50 until (ignore-errors (sb-bsd-sockets:socket-connect sock #(127 0 0 1) port) t)
          do (sleep 0.1))
    (let ((s (sb-bsd-sockets:socket-make-stream sock :input t :output t
                                                     :element-type '(unsigned-byte 8) :buffering :full)))
      (labels ((rd (n) (let ((b (make-array n :element-type '(unsigned-byte 8)))) (read-sequence b s) b))
               (wr (v) (write-sequence (coerce v '(vector (unsigned-byte 8))) s) (finish-output s))
               (u16 (b i) (+ (* 256 (aref b i)) (aref b (1+ i))))
               (u32 (b i) (+ (ash (aref b i) 24) (ash (aref b (+ i 1)) 16) (ash (aref b (+ i 2)) 8) (aref b (+ i 3))))
               (be16 (n) (list (ldb (byte 8 8) n) (ldb (byte 8 0) n))))
        (rd 12) (wr (map 'list #'char-code (format nil "RFB 003.008~c" (code-char 10))))
        (let ((n (aref (rd 1) 0))) (rd n) (wr (list 1)) (rd 4))
        (wr (list 1))
        (let ((init (rd 24))) (rd (u32 init 20)))
        (wr (append (list 2 0) (be16 (1+ (length encodings)))
                    (list 255 255 254 204)                              ; ExtendedDesktopSize
                    (loop for e in encodings append (list (ldb (byte 8 24) e) (ldb (byte 8 16) e)
                                                          (ldb (byte 8 8) e) (ldb (byte 8 0) e)))))
        (wr (append (list 251 0) (be16 nw) (be16 nh) (list 1 0)
                    (list 0 0 0 1 0 0 0 0) (be16 nw) (be16 nh) (list 0 0 0 0)))
        (setf deadline (+ (get-internal-real-time) (* 10 internal-time-units-per-second)))
        (progn
          (loop until (or (> (get-internal-real-time) deadline)
                          (and sized (not (find 0 seen))))
                do (wr (append (list 3 1 0 0 0 0) (be16 640) (be16 480)))   ; at the OLD size
                   (let ((hdr (rd 4)))
                     (when (= (aref hdr 0) 0)
                       (dotimes (i (u16 hdr 2))
                         (let* ((r (rd 12)) (x (u16 r 0)) (y (u16 r 2)) (w (u16 r 4)) (h (u16 r 6))
                                (enc (u32 r 8)))
                           (cond ((= enc #xFFFFFECC) (rd (* 16 (aref (rd 4) 0)))
                                  (when (and (= w nw) (= h nh)) (setf sized t)))
                                 ((= enc 0) (rd (* 4 w h)))
                                 ((= enc 16) (rd (u32 (rd 4) 0)))
                                 ((= enc 1) (rd 4))
                                 (t (error "unexpected encoding ~x" enc)))
                           (when (and sized (member enc '(0 16)))
                             (loop for yy from y below (min nh (+ y h))
                                   do (fill seen 1 :start (+ (* yy nw) x) :end (+ (* yy nw) (min nw (+ x w)))))))))))
          (check (format nil "~a: the client is told the desktop is ~dx~d" label nw nh) sized)
          (check (format nil "~a: every pixel of the new screen reaches it" label)
                 (and sized (not (find 0 seen)))
                 (format nil "~d of ~d" (count 1 seen) (* nw nh))))
        (ignore-errors (close s))))))

(defun fresh-viewer-lag ()
  "Leave a WM with nobody watching while its Clock ticks, then connect as a browser does -- a full
   request and, at once, an incremental one -- and read the send-lag the WM's drag consults."
  (let* ((port (free-port)) (sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (let ((*trace-output* (make-broadcast-stream)) (*error-output* (make-broadcast-stream)))
      (glass.wm:start-wm port :width 640 :height 480))
    (loop repeat 50 until (ignore-errors (sb-bsd-sockets:socket-connect sock #(127 0 0 1) port) t)
          do (sleep 0.1))
    (sleep 2.5)                                         ; the Clock marks changes nobody takes
    (setf glass:*send-lag* 0d0)
    (let ((s (sb-bsd-sockets:socket-make-stream sock :input t :output t
                                                     :element-type '(unsigned-byte 8) :buffering :full)))
      (labels ((rd (n) (let ((b (make-array n :element-type '(unsigned-byte 8)))) (read-sequence b s) b))
               (wr (v) (write-sequence (coerce v '(vector (unsigned-byte 8))) s) (finish-output s)))
        (rd 12) (wr (map 'list #'char-code (format nil "RFB 003.008~c" (code-char 10))))
        (let ((n (aref (rd 1) 0))) (rd n) (wr (list 1)) (rd 4))
        (wr (list 1))
        (let ((init (rd 24))) (rd (+ (ash (aref init 20) 24) (ash (aref init 21) 16) (ash (aref init 22) 8) (aref init 23))))
        (wr (list 2 0 0 1 0 0 0 0))                      ; Raw
        (wr (append (list 3 0 0 0 0 0 2 128 1 224) (list 3 1 0 0 0 0 2 128 1 224)))
        (sleep 1.5)
        (ignore-errors (close s))
        glass:*send-lag*))))

(let ((lag (fresh-viewer-lag)))
  (check "a viewer connecting to a desktop nobody watched is not read as lagging (drag stays opaque)"
         (< lag glass.wm:*wireframe-lag-ms*) (format nil "send-lag ~,1f ms" lag)))

(run '(0) "Raw")
(run '(16 1 0) "ZRLE+CopyRect")

(format t "~&RESIZE ~d failure~:p~%" *fails*)
(finish-output)
(sb-ext:exit :code (if (zerop *fails*) 0 1) :abort t)
