;;;; nested-copyrect.lisp — does the scroll optimisation survive a desktop boundary?
;;;;
;;;; glass turns a window's scroll into an RFB CopyRect: the client already has the
;;;; pixels, so the update is a 12-byte "move that block" plus the strip the move
;;;; exposed, instead of a re-encode of the window.  A remote desktop hosted as a
;;;; window (backend/remote-app.lisp) receives such a CopyRect from the desktop
;;;; inside it, and can either swallow it — repaint the window and let the outer
;;;; server diff a screenful — or pass it on as its own translation hint, so the
;;;; outer compositor moves the strip and the outer server emits a CopyRect too.
;;;;
;;;; This measures which, and what it is worth, by being an ordinary RFB client on
;;;; the OUTER desktop: it drives the scroll through real PointerEvents (which
;;;; travel outer server -> WM -> remote surface -> inner server -> loom, the whole
;;;; path) and counts what comes back.  The inner hop's cost is read off the outer
;;;; desktop's own client over its control socket, so both hops are measured at
;;;; once.  The two arms differ in ONE parameter, GLASS-CLIENT:*PASS-COPYRECT*, set
;;;; live on the outer desktop between runs — same processes, same page, same
;;;; scroll.
;;;;
;;;; Needs the two fixtures up:
;;;;   backend/inspect/scroll-desktop.lisp  -- 5931 4031      (inner: a tall page)
;;;;   backend/inspect/remote-desktop.lisp  -- 5921 5931 4021 (outer: hosts it)
;;;; then:
;;;;   sbcl --non-interactive --load inspect/nested-copyrect.lisp -- 5921 4021 4031

(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (asdf:load-asd "/home/claude/glass/glass.asd")
    (ql:quickload '(:glass/client :glass/text))))

(defpackage #:glass-nested-copyrect (:use #:cl) (:local-nicknames (#:gc #:glass-client)))
(in-package #:glass-nested-copyrect)

;;; ---- talking to a desktop's control socket ---------------------------------

(defun control (port text)
  "Send the source TEXT to the control socket on PORT and read the reply.  TEXT, not
   a form: this process has no clim-glass in it (it is a plain RFB client, which is
   the whole point of using one as the instrument), so the symbols only exist at the
   far end.  The desktops PRINC their result, so anything we want back structured is
   asked for with PRIN1 over there."
  (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (unwind-protect
         (progn
           (sb-bsd-sockets:socket-connect sock (sb-bsd-sockets:make-inet-address "127.0.0.1") port)
           (let ((s (sb-bsd-sockets:socket-make-stream sock :input t :output t
                                                            :element-type 'character :buffering :full)))
             (write-string text s) (terpri s) (force-output s)
             ;; read to EOF, not one line: the far end PRIN1s and the pretty printer
             ;; wraps a long plist across several of them
             (let ((reply (with-output-to-string (o)
                            (loop for line = (read-line s nil nil) while line
                                  do (write-line line o)))))
               (values (ignore-errors (read-from-string reply)) reply))))
      (ignore-errors (sb-bsd-sockets:socket-close sock)))))

(defun windows (control-port vnc-port)
  "((title x y w h) ...) for the surface windows on the desktop behind CONTROL-PORT."
  (control control-port
           (format nil "(let ((*print-pretty* nil)) (prin1-to-string (mapcar (lambda (s) (list (wm-surface-title s) ~
                        (wm-surface-x s) (wm-surface-y s) (glass:fb-width (wm-surface-fb s)) ~
                        (glass:fb-height (wm-surface-fb s)))) ~
                        (glass-port-surfaces (find-glass-port :port ~d)))))" vnc-port)))

(defun inner-stats (control-port)
  (control control-port
           "(let ((*print-pretty* nil)) (prin1-to-string (glass-client:remote-stats (first glass-remote:*remotes*))))"))

(defun set-pass (control-port on)
  (control control-port (format nil "(setf glass-client:*pass-copyrect* ~:[nil~;t~])" on)))

;;; ---- the measurement --------------------------------------------------------

(defun delta (a b keys)
  (loop for k in keys append (list k (- (getf b k 0) (getf a k 0)))))

(defun arm (m outer-control x y steps cadence pass)
  "One arm: set the pass-through, scroll STEPS notches at (X,Y) on the outer screen,
   and return what each hop cost."
  (set-pass outer-control pass)
  (sleep 0.6)                                    ; let anything in flight land first
  (let ((m0 (gc:remote-stats m))
        (i0 (inner-stats outer-control)))
    (dotimes (i steps)
      (gc:remote-pointer m 16 x y)               ; wheel-down notch
      (gc:remote-pointer m 0 x y)                ; and its release
      (sleep cadence))
    (sleep 1.2)                                  ; drain
    (let ((m1 (gc:remote-stats m))
          (i1 (inner-stats outer-control))
          (keys '(:frames :bytes :rects :zrle :raw :copyrects)))
      (list :pass pass
            :outer (delta m0 m1 keys)
            :inner (delta i0 i1 (append keys '(:hints :hints-trimmed :hints-refused)))))))

(defun show (label d)
  (let ((o (getf d :outer)) (i (getf d :inner)))
    (format t "~&  ~a~%" label)
    (format t "    hop 1  inner :5931 -> the remote window   ~5d frames  ~8,1f KB  ~
               ~4d rects (~d ZRLE, ~d Raw, ~d CopyRect)~%"
            (getf i :frames) (/ (getf i :bytes) 1024.0) (getf i :rects)
            (getf i :zrle) (getf i :raw) (getf i :copyrects))
    (format t "    hop 2  outer :5921 -> this client         ~5d frames  ~8,1f KB  ~
               ~4d rects (~d ZRLE, ~d Raw, ~d CopyRect)~%"
            (getf o :frames) (/ (getf o :bytes) 1024.0) (getf o :rects)
            (getf o :zrle) (getf o :raw) (getf o :copyrects))
    (format t "    hints handed to the compositor ~d (~d of them trimmed past a tile-aligned strip), refused ~d~%"
            (getf i :hints) (getf i :hints-trimmed) (getf i :hints-refused))))

(defun run (&key (outer 5921) (outer-control 4021) (inner-control 4031)
                 (steps 40) (cadence 1/10) (rounds 2))
  (let* ((ow (windows outer-control outer))
         (rw (or (find-if (lambda (w) (search ":5931" (first w))) ow) (first ow)))
         (iw (windows inner-control 5931))
         (bw (or (find-if (lambda (w) (search "browser" (first w))) iw) (first iw))))
    (unless (and rw bw)
      (error "nested-copyrect: need a remote window on the outer desktop and a browser on the inner~
              ~%  outer: ~s~%  inner: ~s" ow iw))
    (destructuring-bind (rt rx ry rww rwh) rw
      (destructuring-bind (bt bx by bww bwh) bw
        (declare (ignore rww rwh))
        ;; a point in the middle of the inner browser's content, expressed on the
        ;; outer screen: the remote window's content origin plus the inner coordinate
        (let ((x (+ rx bx (floor bww 2)))
              (y (+ ry by (floor bwh 2))))
          (format t "~&==================== nested CopyRect ====================~%")
          (format t "outer window ~s at ~d,~d | inner window ~s at ~d,~d ~dx~d~%" rt rx ry bt bx by bww bwh)
          (format t "scrolling ~d notches at outer (~d,~d), ~d round(s) per arm~%~%" steps x y rounds)
          (let ((m (gc:connect-remote "127.0.0.1" outer))
                (on '()) (off '()))
            (sleep 2.5)
            (dotimes (i rounds)
              (push (arm m outer-control x y steps cadence t) on)
              (push (arm m outer-control x y steps cadence nil) off))
            (set-pass outer-control t)           ; leave the desktop as we found it
            (dolist (d (reverse on))  (show "PASS-COPYRECT T  — the hint crosses the boundary" d))
            (dolist (d (reverse off)) (show "PASS-COPYRECT NIL — the hint is swallowed" d))
            (let ((bon  (/ (reduce #'+ on  :key (lambda (d) (getf (getf d :outer) :bytes))) (length on)))
                  (boff (/ (reduce #'+ off :key (lambda (d) (getf (getf d :outer) :bytes))) (length off)))
                  (fon  (/ (reduce #'+ on  :key (lambda (d) (getf (getf d :outer) :frames))) (length on)))
                  (foff (/ (reduce #'+ off :key (lambda (d) (getf (getf d :outer) :frames))) (length off))))
              (format t "~%  outer hop, averaged: ~,1f KB over ~,1f frames WITH the hint, ~
                         ~,1f KB over ~,1f frames without~%"
                      (/ bon 1024.0) (float fon) (/ boff 1024.0) (float foff))
              (format t "  => ~,2fx fewer bytes on the outer hop, ~,2f KB/frame vs ~,2f KB/frame~%"
                      (/ boff (max 1.0 (float bon)))
                      (/ bon 1024.0 (max 1.0 (float fon))) (/ boff 1024.0 (max 1.0 (float foff)))))
            (format t "~%~a~%" (gc:remote-report m))
            (format t "=========================================================~%")
            (gc:remote-stop m)))))))

(let* ((tail (cdr (member "--" sb-ext:*posix-argv* :test #'string=)))
       (outer (if (first tail) (parse-integer (first tail)) 5921))
       (oc (if (second tail) (parse-integer (second tail)) 4021))
       (ic (if (third tail) (parse-integer (third tail)) 4031)))
  (run :outer outer :outer-control oc :inner-control ic))
