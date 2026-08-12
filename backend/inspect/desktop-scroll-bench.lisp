;;;; desktop-scroll-bench.lisp — what it costs to scroll a browser window ON THE
;;;; DESKTOP, with a real RFB client attached, and whether the CopyRect that makes
;;;; it cheap is PIXEL-CORRECT when other windows are in the way.
;;;;
;;;; loom/inspect/scroll-bench.lisp measures a bare page served straight over glass.
;;;; This is the other half: the same page inside a decorated window on the WM, where
;;;; a scroll CopyRect lands on the SHARED screen framebuffer and so has to answer for
;;;; everything else on it.  Each case scrolls the same window under a different
;;;; arrangement and reports frames, bytes, and CopyRect rects; with :DECODE it also
;;;; rebuilds the client's framebuffer from the wire (CopyRect + Raw) and compares it
;;;; to the server's, pixel for pixel — 0 differing is the only acceptable answer.
;;;;
;;;;   A     browser alone, topmost                      -> CopyRect
;;;;   B     terminal stacked over it                    -> refused
;;;;   B*    same, with the occlusion guard disabled      (what the guard costs)
;;;;   B'    same overlap, browser raised above           -> CopyRect (z-order, not overlap)
;;;;   B''   terminal on top but clear of the content    -> CopyRect
;;;;   C     root menu open over it                      -> refused
;;;;   C'    same drive, menu closed                     -> CopyRect
;;;;   D     window hanging off the screen edge          -> CopyRect, clipped
;;;;   E     back to clean, scrolling up                 -> CopyRect
;;;;   F     window DRAG                                  (the pre-existing CopyRect path)
;;;;   G     five idle seconds                            (composites should be 0/s)
;;;;
;;;; Needs loom (the browser and its page generator), which glass does not depend on.
;;;;   sbcl --control-stack-size 256 --dynamic-space-size 6144 --non-interactive \
;;;;        --load backend/inspect/desktop-scroll-bench.lisp [VNCPORT] [decode] [off]
;;;; VNCPORT defaults to 5921; "decode" adds the pixel check (Raw, so slower and
;;;; fatter on the wire); "off" is the control arm with *wm-scroll-copyrect* NIL.
(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:glass :mcclim :mcclim-render :sb-concurrency :pigment))
    (asdf:load-system :loom/glass)
    (asdf:load-asd (merge-pathnames "../mcclim-glass.asd" *load-truename*))
    (asdf:load-system :mcclim-glass)))
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    ;; the tall deterministic page comes from loom's own scroll bench, so both halves
    ;; of the measurement are scrolling literally the same pixels
    (load (merge-pathnames "inspect/scroll-bench.lisp"
                           (asdf:system-source-directory :loom)))))

(defpackage #:glass.desktop-scroll-bench
  (:use #:cl) (:local-nicknames (#:cg #:clim-glass)))
(in-package #:glass.desktop-scroll-bench)

;;; ---------------------------------------------------------------------------
;;; A minimal RFB client that RECONSTRUCTS the framebuffer
;;; ---------------------------------------------------------------------------

(defconstant +zrle+ 16) (defconstant +copy+ 1) (defconstant +raw+ 0)

(defstruct rc stream socket px w h
  (in 0) (frames 0) (rects 0) (zrle 0) (copyrects 0) (raw 0)
  (running t) (last-t 0) (wlock (sb-thread:make-mutex)))

(defun r-u8 (c) (incf (rc-in c) 1) (read-byte (rc-stream c)))
(defun r-u16 (c) (let ((a (r-u8 c)) (b (r-u8 c))) (logior (ash a 8) b)))
(defun r-u32 (c) (let ((a (r-u16 c)) (b (r-u16 c))) (logior (ash a 16) b)))
(defun r-bytes (c n)
  (let ((b (make-array n :element-type '(unsigned-byte 8))))
    (read-sequence b (rc-stream c)) (incf (rc-in c) n) b))
(defun r-skip (c n) (r-bytes c n) nil)
(defun w-u8 (s v) (write-byte v s))
(defun w-u16 (s v) (w-u8 s (ldb (byte 8 8) v)) (w-u8 s (ldb (byte 8 0) v)))
(defun w-u32 (s v) (w-u16 s (ldb (byte 16 16) v)) (w-u16 s (ldb (byte 16 0) v)))

(defun connect (host port)
  (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (sb-bsd-sockets:socket-connect sock (sb-bsd-sockets:make-inet-address host) port)
    (setf (sb-bsd-sockets:sockopt-tcp-nodelay sock) t)
    (let* ((s (sb-bsd-sockets:socket-make-stream sock :input t :output t
                                                 :element-type '(unsigned-byte 8) :buffering :full))
           (c (make-rc :stream s :socket sock)))
      (r-skip c 12)
      (write-sequence (map '(simple-array (unsigned-byte 8) (*)) #'char-code
                           (format nil "RFB 003.008~c" #\Newline)) s)
      (force-output s)
      (let ((n (r-u8 c))) (r-skip c n) (w-u8 s 1) (force-output s)
        (let ((res (r-u32 c)))
          (unless (zerop res)                 ; failure reason string
            (error "RFB security result ~d" res))))
      (w-u8 s 1) (force-output s)
      (let ((w (r-u16 c)) (h (r-u16 c)))
        (r-skip c 16) (r-skip c (r-u32 c))
        (setf (rc-w c) w (rc-h c) h
              (rc-px c) (make-array (* w h) :element-type '(unsigned-byte 32))
              (rc-last-t c) (get-internal-real-time))
        c))))

(defun set-encodings (c encs)
  (let ((s (rc-stream c)))
    (sb-thread:with-mutex ((rc-wlock c))
      (w-u8 s 2) (w-u8 s 0) (w-u16 s (length encs))
      (dolist (e encs) (w-u32 s e)) (force-output s))))

(defun request (c inc) (let ((s (rc-stream c)))
                         (sb-thread:with-mutex ((rc-wlock c))
                           (w-u8 s 3) (w-u8 s inc) (w-u16 s 0) (w-u16 s 0)
                           (w-u16 s (rc-w c)) (w-u16 s (rc-h c)) (force-output s))))
(defun pointer (c mask x y) (let ((s (rc-stream c)))
                              (sb-thread:with-mutex ((rc-wlock c))
                                (w-u8 s 5) (w-u8 s mask) (w-u16 s x) (w-u16 s y) (force-output s))))

(defun apply-copy (c x y w h sx sy)
  "CopyRect exactly as a client does it: read the source block, then write it."
  (let* ((px (rc-px c)) (fw (rc-w c))
         (tmp (make-array (* w h) :element-type '(unsigned-byte 32))))
    (dotimes (yy h) (replace tmp px :start1 (* yy w) :end1 (* (1+ yy) w)
                                    :start2 (+ (* (+ sy yy) fw) sx)))
    (dotimes (yy h) (replace px tmp :start1 (+ (* (+ y yy) fw) x) :end1 (+ (* (+ y yy) fw) x w)
                                    :start2 (* yy w)))))

(defun apply-raw (c x y w h)
  (let ((b (r-bytes c (* w h 4))) (px (rc-px c)) (fw (rc-w c)) (o 0))
    (dotimes (yy h)
      (let ((row (* (+ y yy) fw)))
        (dotimes (xx w)
          (setf (aref px (+ row x xx))
                (logior (ash (aref b (+ o 2)) 16) (ash (aref b (+ o 1)) 8) (aref b o)))
          (incf o 4))))))

(defun read-update (c decode)
  (let ((msg (r-u8 c)))
    (unless (zerop msg) (return-from read-update nil))
    (r-skip c 1)
    (dotimes (i (r-u16 c))
      (let ((x (r-u16 c)) (y (r-u16 c)) (w (r-u16 c)) (h (r-u16 c)) (enc (r-u32 c)))
        (incf (rc-rects c))
        (cond ((= enc +copy+) (incf (rc-copyrects c))
                              (let ((sx (r-u16 c)) (sy (r-u16 c)))
                                (when decode (apply-copy c x y w h sx sy))))
              ((= enc +raw+)  (incf (rc-raw c))
                              (if decode (apply-raw c x y w h) (r-skip c (* w h 4))))
              ((= enc +zrle+) (incf (rc-zrle c)) (r-skip c (r-u32 c)))
              (t (error "unexpected encoding ~d" enc)))))
    (incf (rc-frames c))
    (setf (rc-last-t c) (get-internal-real-time))
    t))

(defun reader-loop (c decode)
  (handler-case
      (progn (request c 0)
             (loop while (rc-running c)
                   do (unless (read-update c decode) (return)) (request c 1)))
    (error (e) (declare (ignore e)) nil))
  (setf (rc-running c) nil))

(defun secs (ticks) (/ ticks (float internal-time-units-per-second)))

(defun quiesce (c &optional (idle 0.6) (limit 8.0))
  "Wait until no update has arrived for IDLE seconds."
  (let ((t0 (get-internal-real-time)))
    (loop (sleep 0.1)
          (let ((now (get-internal-real-time)))
            (when (or (> (secs (- now (rc-last-t c))) idle)
                      (> (secs (- now t0)) limit))
              (return))))))

;;; ---------------------------------------------------------------------------
;;; Desktop under test
;;; ---------------------------------------------------------------------------

(defvar *port* nil) (defvar *fb* nil) (defvar *browser* nil) (defvar *html* nil)

(defun start-desktop (vncport)
  (setf *html* (loom.scroll-bench::make-page-files
                (merge-pathnames "loom-scroll-bench/" (uiop:temporary-directory))
                :sections 24))
  (sb-thread:make-thread
   (lambda () (cg:run-wm '() :port vncport :width 1280 :height 800)) :name "desk")
  (sleep 2.0)
  (setf *port* (cg::find-glass-port :port vncport)
        *fb* (cg::glass-port-fb *port*))
  (cg::wm-spawn-spec *port* (list :browse (namestring *html*) :width 900 :height 620))
  (sleep 12.0)                                    ; page fetch + first render
  (setf *browser* (first (cg::glass-port-surfaces *port*)))
  (cg::composite-all *port*)
  *browser*)

(defun surf-center (s)
  (values (+ (cg::wm-surface-x s) (floor (glass:fb-width (cg::wm-surface-fb s)) 2))
          (+ (cg::wm-surface-y s) 100
             (floor (- (glass:fb-height (cg::wm-surface-fb s)) 100) 2))))

(defun server-pixels ()
  (glass:with-fb-locked (*fb*) (copy-seq (glass:fb-pixels *fb*))))

(defun pixel-diff (a b) (let ((n 0)) (dotimes (i (length a)) (unless (= (aref a i) (aref b i)) (incf n))) n))

(defun recomposite-diff ()
  "Server fb as it stands vs a from-scratch FULL recomposite of the same state."
  (let ((before (server-pixels)))
    (cg::composite-all *port*)
    (let ((after (server-pixels))) (pixel-diff before after))))

(defun scroll (c n &key (cadence 1/30) (dir 16) at direct)
  "N wheel notches at screen point AT (default: a point in the browser's content, well
   left of anything we stack on its right).  DIRECT bypasses the WM's pointer routing
   and pokes the window's own handler — the only way to scroll while a menu holds the
   pointer grab, and still a real paint -> composite -> send."
  (let* ((bx (cg::wm-surface-x *browser*)) (by (cg::wm-surface-y *browser*))
         (p (or at (cons (+ bx 150) (+ by 380)))))
    (dotimes (i n)
      (if direct
          (progn (funcall (cg::wm-surface-on-pointer *browser*) dir (- (car p) bx) (- (cdr p) by))
                 (funcall (cg::wm-surface-on-pointer *browser*) 0 (- (car p) bx) (- (cdr p) by)))
          (progn (pointer c dir (car p) (cdr p)) (pointer c 0 (car p) (cdr p))))
      (sleep cadence))))

(defun snap-counters (c) (list (rc-frames c) (rc-in c) (rc-copyrects c) (rc-rects c)))
(defun delta (a b) (mapcar #'- b a))

;;; The server-side half of the frame budget, straight off glass's standing counters
;;; (src/perf.lisp): how long the COMPOSITE that redrew the screen took, and how long
;;; the diff+encode+write that shipped it took.  The client counters above say what
;;; arrived; these say what it cost to make.  Reset per case, read after it quiesces.
(defun perf-costs ()
  "(values COMPOSITES MS/COMPOSITE SENDS MS/SEND) since the last GLASS:PERF-RESET."
  (let* ((p glass::*perf*)
         (c (glass::pf-composites p)) (f (glass::pf-frames p)))
    (values c (if (plusp c) (glass::%pf-ms (/ (glass::pf-comp p) c)) 0.0)
            f (if (plusp f) (glass::%pf-ms (/ (glass::pf-enc p) f)) 0.0))))

(defun run-case (c label &key (steps 60) (decode nil) (cadence 1/30) at direct (dir 16))
  (quiesce c)
  (let ((c0 (snap-counters c)) (t0 (get-internal-real-time)))
    (setf glass:*perf-on* t) (glass:perf-reset)
    (scroll c steps :cadence cadence :at at :direct direct :dir dir)
    (quiesce c)
    (multiple-value-bind (ncomp comp-ms nsend enc-ms) (perf-costs)
      (let* ((d (delta c0 (snap-counters c)))
             (wall (secs (- (get-internal-real-time) t0)))
             (frames (first d)) (bytes (second d)) (crs (third d)))
        (format t "~&~a~%" label)
        (format t "  frames ~d in ~,1fs = ~,1f fps | ~,1f KB total | ~,1f KB/frame~%"
                frames wall (/ frames (max 0.001 wall)) (/ bytes 1024.0)
                (if (plusp frames) (/ bytes frames 1024.0) 0.0))
        (format t "  rects ~d, CopyRect rects ~d  (CopyRect frames: ~d/~d)~%"
                (fourth d) crs crs frames)
        (format t "  COST: ~d composites at ~,2f ms | ~d sends at ~,2f ms encode~%"
                ncomp comp-ms nsend enc-ms)
        (let ((sd nil) (rd nil))
          (when decode
            (setf sd (pixel-diff (rc-px c) (server-pixels))
                  rd (recomposite-diff))
            (format t "  PIXELS: client-vs-server ~d differing | server-vs-full-recomposite ~d~%" sd rd))
          (list :label label :frames frames :bytes bytes :copyrects crs
                :fps (/ frames (max 0.001 wall)) :client-diff sd :recomposite-diff rd
                :composites ncomp :composite-ms comp-ms :encode-ms enc-ms))))))

(defun main (&key (vncport 5921) (decode nil) (copyrect t) (steps 60))
  (setf cg::*wm-scroll-copyrect* copyrect)
  (start-desktop vncport)
  (let* ((c (connect "127.0.0.1" vncport)))
    (set-encodings c (if decode (list +copy+ +raw+) (list +zrle+ +copy+ +raw+)))
    (sb-thread:make-thread (lambda () (reader-loop c decode)) :name "rfb-client")
    (sleep 2.0)
    (format t "~&==== desktop scroll bench (decode=~a scroll-copyrect=~a steps=~d) ====~%"
            decode copyrect steps)
    (let ((results '()) (bx (cg::wm-surface-x *browser*)) (by (cg::wm-surface-y *browser*)))
      ;; ---- A: browser alone, topmost -------------------------------------
      (push (run-case c "A. browser alone, topmost" :decode decode :steps steps) results)

      ;; ---- B: a terminal stacked partly OVER the browser ------------------
      (cg::wm-spawn-spec *port* '(:terminal :cols 60 :rows 16 :ppem 14))
      (sleep 4.0)
      (let ((term (first (cg::glass-port-surfaces *port*))))
        (cg::wm-move term (+ bx 430) (+ by 220))         ; right half of the browser content
        (cg::wm-raise *port* term)
        (cg::composite-all *port*) (quiesce c)
        (push (run-case c "B. terminal stacked OVER the browser" :decode decode :steps steps) results)
        ;; B* : the SAME occluded scroll with the occlusion guard neutered — what the
        ;; guard is actually buying, measured rather than asserted
        ;; SETF, not a binding: the compositor reads it from the WM's own tick thread
        (setf cg::*wm-copyrect-occlusion-guard* nil)
        (push (run-case c "B*. SAME overlap, occlusion guard DISABLED"
                        :decode decode :steps steps) results)
        (setf cg::*wm-copyrect-occlusion-guard* t)
        (cg::composite-all *port*) (quiesce c)
        ;; B' control: SAME overlap, but the browser raised above the terminal — the
        ;; guard is about z-order, not about any two boxes touching
        (cg::wm-raise *port* *browser*)
        (cg::composite-all *port*) (quiesce c)
        (push (run-case c "B'. control: same overlap, browser raised ABOVE terminal"
                        :decode decode :steps steps) results)
        ;; B'' control: terminal back on top, but moved fully clear of the content
        (cg::wm-raise *port* term)
        (cg::wm-move term 1000 420)
        (cg::composite-all *port*) (quiesce c)
        (push (run-case c "B''. control: terminal on top but clear of the browser"
                        :decode decode :steps steps) results)

        ;; ---- C: root menu open over the browser ---------------------------
        ;; the menu grabs the pointer, so this pair is driven DIRECTLY into the window
        (cg::wm-open-menu *port* (+ bx 420) (+ by 60))
        (cg::composite-all *port*) (quiesce c)
        (push (run-case c "C. WM root menu open OVER the browser (direct-drive)"
                        :decode decode :steps steps :direct t) results)
        (setf (cg::glass-port-menu *port*) nil)
        (cg::composite-all *port*) (quiesce c)
        (push (run-case c "C'. control: same direct-drive, menu closed"
                        :decode decode :steps steps :direct t) results)
        (cg::wm-move term 40 40) (cg::composite-all *port*) (quiesce c))

      ;; ---- D: browser hanging off the screen edge -------------------------
      (cg::wm-move *browser* 700 460)
      (cg::composite-all *port*) (quiesce c)
      (push (run-case c "D. browser partly off the screen edge" :decode decode :steps steps
                      :at (cons 800 620) :dir 8) results)

      ;; ---- E: back to a clean topmost browser (regression of A) ----------
      (cg::wm-move *browser* bx by)
      (cg::wm-raise *port* *browser*)
      (cg::composite-all *port*) (quiesce c)
      (push (run-case c "E. browser alone again (raised, moved back, scrolling up)"
                      :decode decode :steps steps :dir 8) results)

      ;; ---- F: window DRAG still gets its CopyRect -------------------------
      (quiesce c)
      (let ((c0 (snap-counters c)) (t0 (get-internal-real-time)))
        (glass:perf-reset)
        (dotimes (i 40) (cg::wm-drag-move-opaque *port* *browser* (+ bx i 1) (+ by (floor i 2)))
                        (sleep 1/30))
        (quiesce c)
        (multiple-value-bind (ncomp comp-ms nsend enc-ms) (perf-costs)
          (declare (ignore nsend))
          (let* ((d (delta c0 (snap-counters c))) (wall (secs (- (get-internal-real-time) t0))))
            (push (list :label "F. window drag (regression)" :frames (first d) :bytes (second d)
                        :copyrects (third d) :fps (/ (first d) (max 0.001 wall))
                        :client-diff (when decode (pixel-diff (rc-px c) (server-pixels)))
                        :recomposite-diff (when decode (recomposite-diff))
                        :composites ncomp :composite-ms comp-ms :encode-ms enc-ms)
                  results))))
      (cg::wm-move *browser* bx by) (cg::composite-all *port*) (quiesce c)

      ;; ---- G: idle composites with a browser window open ------------------
      (setf glass:*perf-on* t) (glass:perf-reset)
      (sleep 5.0)
      (format t "~&G. idle (5 s, browser + terminal open) — glass perf:~%~a~%" (glass:perf-report))

      (format t "~&~%==== summary ====~%")
      (dolist (r (reverse results))
        (format t "~&~a~%   CopyRect ~d/~d frames, ~,1f KB/frame, ~,1f fps~@[~a~]~a~%"
                (getf r :label) (getf r :copyrects) (getf r :frames)
                (if (plusp (getf r :frames)) (/ (getf r :bytes) (getf r :frames) 1024.0) 0.0)
                (getf r :fps)
                (when (getf r :composites)
                  (format nil "~%   COST: composite ~,2f ms x~d | encode ~,2f ms"
                          (getf r :composite-ms) (getf r :composites) (getf r :encode-ms)))
                (if (getf r :client-diff)
                    (format nil "~%   PIXELS: client-vs-server ~d, fb-vs-full-recomposite ~d"
                            (getf r :client-diff) (getf r :recomposite-diff))
                    "")))
      (setf (rc-running c) nil)
      (force-output))))

(main :vncport (parse-integer (or (second sb-ext:*posix-argv*) "5921"))
      :decode (member "decode" (cddr sb-ext:*posix-argv*) :test #'equal)
      :copyrect (not (member "off" (cddr sb-ext:*posix-argv*) :test #'equal))
      :steps (if (member "decode" (cddr sb-ext:*posix-argv*) :test #'equal) 24 60))
(finish-output)
(sb-ext:exit :abort t)
