;;;; nested-copyrect.lisp — does the CopyRect optimisation survive a desktop boundary?
;;;;
;;;; glass turns a window's scroll — and a window's MOVE — into an RFB CopyRect: the
;;;; client already has the pixels, so the update is a 12-byte "move that block" plus
;;;; whatever the move exposed, instead of a re-encode of the window.  A remote
;;;; desktop hosted as a window (backend/remote-app.lisp) receives such a CopyRect
;;;; from the desktop inside it, and can either swallow it — repaint the window and
;;;; let the outer server diff a screenful — or pass it on as its own translation
;;;; hint, so the outer compositor moves the block and the outer server emits a
;;;; CopyRect too.
;;;;
;;;; This measures which, and what it is worth, by being an ordinary RFB client on
;;;; the OUTER desktop: it drives the gesture through real PointerEvents (which
;;;; travel outer server -> WM -> remote surface -> inner server -> inner WM, the
;;;; whole path) and counts what comes back.  The inner hop's cost is read off the
;;;; outer desktop's own client over its control socket, so both hops are measured at
;;;; once.  The two arms differ in ONE parameter, GLASS-CLIENT:*PASS-COPYRECT*, set
;;;; live on the outer desktop between runs — same processes, same page, same gesture.
;;;;
;;;; FOUR gestures, because they stress different things.  A SCROLL exposes one strip
;;;; along an edge, so the hint survives with a tile's worth of trim.  A window DRAG
;;;; exposes an L (two rectangles) for a diagonal move and a strip for an axis-aligned
;;;; one, and %BOX-MINUS keeps only the largest single untouched sub-rectangle of the
;;;; destination — so :drag-d is where trimming, if it bites, will show.  A drag also
;;;; holds a mouse BUTTON down, which a scroll does not, and that turns out to matter
;;;; more than the geometry does (see the report).
;;;;
;;;;   --gesture scroll   40 wheel notches in the inner browser
;;;;   --gesture drag-h   grab the inner window's title bar, 160 px right and back
;;;;   --gesture drag-v   the same, 160 px down and back
;;;;   --gesture drag-d   the same, 160 px right AND down and back
;;;;
;;;; Needs the two fixtures up:
;;;;   backend/inspect/scroll-desktop.lisp  -- 5931 4031 [W H]  (inner: a tall page)
;;;;   backend/inspect/remote-desktop.lisp  -- 5921 5931 4021   (outer: hosts it)
;;;; then:
;;;;   sbcl --non-interactive --load inspect/nested-copyrect.lisp -- 5921 4021 4031 [gesture...]

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

(defun tally-reset (control-port)
  "Arm the outer WM's per-verdict tally (see CLIM-GLASS::*WM-COPY-TALLY*).  A fresh
   non-NIL plist; NIL would turn it off."
  (control control-port "(setf clim-glass::*wm-copy-tally* (list :armed 1))"))

(defun tally-read (control-port)
  (control control-port
           "(let ((*print-pretty* nil)) (prin1-to-string clim-glass::*wm-copy-tally*))"))

(defun place-window (control-port vnc-port title x y)
  "Put the named window's CONTENT origin at (X,Y) on that desktop, and repaint.  Both
   arms of a drag then start from the same pixel, so the two runs are the same gesture
   and not merely the same shape."
  (control control-port
           (format nil "(let* ((p (find-glass-port :port ~d)) ~
                               (w (find-if (lambda (s) (search ~s (wm-surface-title s))) ~
                                           (glass-port-surfaces p)))) ~
                          (when w (wm-move w ~d ~d) (composite-all p) (list ~d ~d)))"
                   vnc-port title x y x y)))

(defun drag-probe (inner-control vnc-port)
  "(SEND-QUEUE-KB WIREFRAME-P) on the INNER desktop, sampled mid-drag.  The adaptive
   drag switches an opaque move to a wireframe outline once GLASS:*SEND-QUEUE* passes
   *WIREFRAME-QUEUE-KB*, and a wireframe drag emits no CopyRect at all — so a run that
   never trips it and a run that does are measuring different things, and the report
   has to say which happened."
  (control inner-control
           (format nil "(let ((*print-pretty* nil)) ~
                          (prin1-to-string (list glass:*send-queue* ~
                                                 (and (glass-port-drag-wire (find-glass-port :port ~d)) t))))"
                   vnc-port)))

;;; ---- the gestures -----------------------------------------------------------

(defun scroll-gesture (m x y steps cadence)
  "STEPS wheel-down notches at outer (X,Y) — the original measurement's gesture."
  (lambda (&optional probe)
    (declare (ignore probe))
    (dotimes (i steps)
      (gc:remote-pointer m 16 x y)               ; wheel-down notch
      (gc:remote-pointer m 0 x y)                ; and its release
      (sleep cadence))))

(defun drag-gesture (m x0 y0 ddx ddy steps cadence)
  "Grab the title bar at outer (X0,Y0), walk to (+DDX,+DDY) in STEPS and back again,
   and release where it started — so the window ends the arm exactly where it began
   and the next arm is the same gesture from the same pixel.  Real PointerEvents with
   the LEFT BUTTON HELD the whole way, which is what a person's drag is."
  (lambda (&optional probe)
    (flet ((at (i) (gc:remote-pointer m 1 (+ x0 (round (* ddx i) steps))
                                      (+ y0 (round (* ddy i) steps)))))
      (gc:remote-pointer m 0 x0 y0) (sleep cadence)      ; hover onto the title bar
      (gc:remote-pointer m 1 x0 y0) (sleep cadence)      ; press: the WM starts a :move
      (loop for i from 1 to steps
            do (at i) (sleep cadence)
               (when (and probe (zerop (mod i 4))) (funcall probe)))
      (loop for i from (1- steps) downto 0
            do (at i) (sleep cadence)
               (when (and probe (zerop (mod i 4))) (funcall probe)))
      (gc:remote-pointer m 0 x0 y0))))                    ; release: the final opaque move

;;; ---- the measurement --------------------------------------------------------

(defun delta (a b keys)
  (loop for k in keys append (list k (- (getf b k 0) (getf a k 0)))))

(defun arm (m outer-control inner-control inner-vnc gesture pass)
  "One arm: set the pass-through, run GESTURE, and return what each hop cost."
  (set-pass outer-control pass)
  (tally-reset outer-control)
  (sleep 0.6)                                    ; let anything in flight land first
  (let ((m0 (gc:remote-stats m))
        (i0 (inner-stats outer-control))
        (queue 0.0) (wire nil))
    (funcall gesture
             (lambda ()
               (let ((p (drag-probe inner-control inner-vnc)))
                 (when (consp p)
                   (setf queue (max queue (float (first p) 1.0)))
                   (when (second p) (setf wire t))))))
    (sleep 1.2)                                  ; drain
    (let ((m1 (gc:remote-stats m))
          (i1 (inner-stats outer-control))
          (keys '(:frames :bytes :rects :zrle :raw :copyrects)))
      (list :pass pass
            :outer (delta m0 m1 keys)
            :inner (delta i0 i1 (append keys '(:hints :hints-trimmed :hints-refused
                                               :copy-px :trim-px :refuse-px :taken :taken-px)))
            :tally (tally-read outer-control)
            :queue queue :wire wire))))

(defun show (label d)
  (let ((o (getf d :outer)) (i (getf d :inner)) (ta (getf d :tally)))
    (format t "~&  ~a~%" label)
    (format t "    hop 1  inner -> the remote window   ~5d frames  ~8,1f KB  ~
               ~4d rects (~d ZRLE, ~d Raw, ~d CopyRect)~%"
            (getf i :frames) (/ (getf i :bytes) 1024.0) (getf i :rects)
            (getf i :zrle) (getf i :raw) (getf i :copyrects))
    (format t "    hop 2  outer -> this client         ~5d frames  ~8,1f KB  ~
               ~4d rects (~d ZRLE, ~d Raw, ~d CopyRect)~%"
            (getf o :frames) (/ (getf o :bytes) 1024.0) (getf o :rects)
            (getf o :zrle) (getf o :raw) (getf o :copyrects))
    (format t "    client hints: ~d offered, ~d trimmed, ~d refused; ~d taken by the compositor~%"
            (getf i :hints) (getf i :hints-trimmed) (getf i :hints-refused) (getf i :taken))
    (format t "    translated area: ~,2f Mpx arrived, ~,2f Mpx trimmed off, ~,2f Mpx refused, ~
               ~,2f Mpx taken (~,1f%% of what arrived)~%"
            (/ (getf i :copy-px) 1e6) (/ (getf i :trim-px) 1e6) (/ (getf i :refuse-px) 1e6)
            (/ (getf i :taken-px) 1e6)
            (if (plusp (getf i :copy-px))
                (* 100.0 (/ (getf i :taken-px) (getf i :copy-px))) 0.0))
    (format t "    outer WM verdicts: ~a~%" ta)
    (format t "    inner drag: ~a, peak send-queue ~,1f KB (wireframe at 100 KB)~%"
            (if (getf d :wire) "WENT WIREFRAME" "stayed OPAQUE") (getf d :queue))))

(defun summarise (name on off)
  (let ((bon  (/ (reduce #'+ on  :key (lambda (d) (getf (getf d :outer) :bytes))) (length on)))
        (boff (/ (reduce #'+ off :key (lambda (d) (getf (getf d :outer) :bytes))) (length off)))
        (fon  (/ (reduce #'+ on  :key (lambda (d) (getf (getf d :outer) :frames))) (length on)))
        (foff (/ (reduce #'+ off :key (lambda (d) (getf (getf d :outer) :frames))) (length off)))
        (ion  (/ (reduce #'+ on  :key (lambda (d) (getf (getf d :inner) :bytes))) (length on)))
        (ioff (/ (reduce #'+ off :key (lambda (d) (getf (getf d :inner) :bytes))) (length off))))
    (format t "~%  ~a — outer hop, averaged: ~,1f KB over ~,1f frames WITH the hint, ~
               ~,1f KB over ~,1f frames without~%"
            name (/ bon 1024.0) (float fon) (/ boff 1024.0) (float foff))
    (format t "  ~a => ~,2fx fewer bytes on the outer hop, ~,2f KB/frame vs ~,2f KB/frame~%"
            name (/ boff (max 1.0 (float bon)))
            (/ bon 1024.0 (max 1.0 (float fon))) (/ boff 1024.0 (max 1.0 (float foff))))
    (format t "  ~a    inner hop control: ~,1f KB with vs ~,1f KB without (~,2fx — should be ~~1)~%"
            name (/ ion 1024.0) (/ ioff 1024.0) (/ ioff (max 1.0 (float ion))))
    (list name (/ boff (max 1.0 (float bon))) (/ bon 1024.0 (max 1.0 (float fon)))
          (/ boff 1024.0 (max 1.0 (float foff))))))

(defun run (&key (outer 5921) (outer-control 4021) (inner-control 4031) (inner 5931)
                 (gestures '(:scroll :drag-h :drag-v :drag-d))
                 (scroll-steps 40) (scroll-cadence 1/10)
                 (drag-steps 20) (drag-amp 160) (drag-cadence 1/20)
                 (start-x 100) (start-y 60)
                 (rounds 2))
  (let* ((ow (windows outer-control outer))
         (rw (or (find-if (lambda (w) (search (format nil ":~d" inner) (first w))) ow) (first ow))))
    (unless rw (error "nested-copyrect: no remote window on the outer desktop: ~s" ow))
    (destructuring-bind (rt rx ry rww rwh) rw
      (declare (ignore rww rwh))
      (format t "~&==================== nested CopyRect ====================~%")
      (format t "outer window ~s at ~d,~d~%" rt rx ry)
      (let ((m (gc:connect-remote "127.0.0.1" outer))
            (results '()))
        (sleep 2.5)
        (dolist (g gestures)
          ;; Re-place the inner window before each gesture: the drag arms must start
          ;; from a known pixel, and the scroll arm wants the same window in the same
          ;; place so its numbers are comparable with them.
          (place-window inner-control inner "browse" start-x start-y)
          (sleep 1.0)
          (let* ((iw (windows inner-control inner))
                 (bw (or (find-if (lambda (w) (search "browse" (first w))) iw)
                         (find-if (lambda (w) (search "file:" (first w))) iw)
                         (first iw))))
            (unless bw (error "nested-copyrect: no window on the inner desktop: ~s" iw))
            (destructuring-bind (bt bx by bww bwh) bw
              (let* ((title-x (+ rx bx (floor bww 2)))     ; middle of the title bar,
                     (title-y (+ ry by -11))               ; clear of both wedge buttons
                     (mid-x (+ rx bx (floor bww 2)))
                     (mid-y (+ ry by (floor bwh 2)))
                     (ddx (if (member g '(:drag-h :drag-d)) drag-amp 0))
                     (ddy (if (member g '(:drag-v :drag-d)) drag-amp 0))
                     (gesture (if (eq g :scroll)
                                  (scroll-gesture m mid-x mid-y scroll-steps scroll-cadence)
                                  (drag-gesture m title-x title-y ddx ddy drag-steps drag-cadence))))
                (format t "~%---------- ~a ----------~%" g)
                (format t "inner window ~s at ~d,~d ~dx~d | ~a~%" bt bx by bww bwh
                        (if (eq g :scroll)
                            (format nil "~d wheel notches at outer (~d,~d)" scroll-steps mid-x mid-y)
                            (format nil "grab (~d,~d) outer, ~d steps to +~d,+~d and back"
                                    title-x title-y drag-steps ddx ddy)))
                (format t "~d round(s) per arm~%~%" rounds)
                (let ((on '()) (off '()))
                  (dotimes (i rounds)
                    (push (arm m outer-control inner-control inner gesture t) on)
                    (push (arm m outer-control inner-control inner gesture nil) off))
                  (dolist (d (reverse on))  (show "PASS-COPYRECT T  — the hint crosses the boundary" d))
                  (dolist (d (reverse off)) (show "PASS-COPYRECT NIL — the hint is swallowed" d))
                  (push (summarise (string g) on off) results))))))
        (set-pass outer-control t)                 ; leave the desktop as we found it
        (format t "~%~a~%" (gc:remote-report m))
        (format t "~%  gesture      ratio   KB/frame with   KB/frame without~%")
        (dolist (r (reverse results))
          (format t "  ~10a  ~5,2fx  ~14,2f  ~17,2f~%" (first r) (second r) (third r) (fourth r)))
        (format t "=========================================================~%")
        (gc:remote-stop m)))))

(let* ((tail (cdr (member "--" sb-ext:*posix-argv* :test #'string=)))
       (outer (if (first tail) (parse-integer (first tail)) 5921))
       (oc (if (second tail) (parse-integer (second tail)) 4021))
       (ic (if (third tail) (parse-integer (third tail)) 4031))
       (gs (or (mapcar (lambda (s) (intern (string-upcase s) :keyword)) (cdddr tail))
               '(:scroll :drag-h :drag-v :drag-d))))
  (run :outer outer :outer-control oc :inner-control ic :gestures gs))
