;;;; rfb-client.lisp — the OTHER end of rfb.lisp: an RFB *client*.
;;;;
;;;; glass serves a framebuffer over RFB so a viewer somewhere else can see it.
;;;; This is the same protocol read the other way round: connect to a remote RFB
;;;; server, decode its updates into a LOCAL glass framebuffer, and push key and
;;;; pointer events back.  A remote desktop then IS a glass framebuffer plus an
;;;; on-key and an on-pointer — which is exactly a wm-surface, so the window
;;;; manager can host it as a window with no idea that the pixels came off a
;;;; socket (backend/remote-app.lisp does that part; nothing here knows about the
;;;; WM, McCLIM or a window, so it is equally usable headless — as a screen
;;;; scraper, a conformance probe, or the measuring instrument in a bandwidth
;;;; benchmark, which is what it is used as in inspect/nested-copyrect.lisp).
;;;;
;;;; Lifted from loom/inspect/scroll-bench.lisp, which already had a working
;;;; client in CL: the u8/u16/u32 stream readers and writers, the 3.8 handshake
;;;; (version -> security-type None -> ClientInit -> ServerInit), SetEncodings,
;;;; the FramebufferUpdateRequest / read-update / request-again pump, the
;;;; PointerEvent writer, and the per-rect byte tally that made the benchmark a
;;;; real load on the server.  What that client did NOT do is what a viewer has
;;;; to: it counted rects and threw the pixels away.  Everything below the
;;;; handshake — the ZRLE and Raw decoders, CopyRect, the pixel-format
;;;; negotiation, DesktopSize, KeyEvent, the write queue, reconnection, and the
;;;; translation bookkeeping — is new here.
;;;;
;;;; The zlib half of ZRLE is cram's inflate, continued across rectangles: RFB
;;;; runs ONE zlib stream for the life of the connection and sync-flushes it per
;;;; rectangle (rfb.lisp's WRITE-RECT-ZRLE is the encoder for the very same
;;;; stream), so the decoder needs the history to survive a message boundary.
;;;; cram's top-level entries inflate a whole stream to its BFINAL; the block
;;;; loop here is the same primitives (%MAKE-BITR, INFLATE-HUFFMAN,
;;;; READ-DYNAMIC-TABLES) driven one flush at a time instead.  The tile
;;;; subencodings are zrle.lisp's ZRLE-TILE read backwards — same tile grid, same
;;;; palette-index bit packing, same CPIXEL.

(defpackage #:glass-client
  (:use #:cl)
  (:documentation
   "An RFB (VNC) CLIENT in pure Common Lisp: connect to a remote RFB server and
    keep a local glass framebuffer holding what it is showing, forwarding key and
    pointer events back to it.  The mirror image of glass's server, sharing its
    framebuffer and its ZRLE tables.  No McCLIM, no window manager — a REMOTE is a
    framebuffer, two input functions and a thread.")
  (:export #:remote #:remote-p #:connect-remote #:remote-stop
           #:remote-fb #:remote-host #:remote-port #:remote-name
           #:remote-state #:remote-connected-p #:remote-width #:remote-height
           #:remote-key #:remote-pointer #:remote-take-dirty #:remote-take-copy
           #:remote-on-resize #:remote-report #:remote-stats
           #:*pass-copyrect* #:*update-hook* #:*reconnect-delay* #:*reconnect-max-delay*
           #:*input-queue-limit* #:*rfb-client-name*))

(in-package #:glass-client)

;;; ---- wire I/O ---------------------------------------------------------------
;;; Straight from scroll-bench's client: RFB is big-endian on the wire and every
;;; read is a byte count somebody has to keep honest.

(defun r-u8 (s) (read-byte s))
(defun r-u16 (s) (logior (ash (r-u8 s) 8) (r-u8 s)))
(defun r-u32 (s) (logior (ash (r-u16 s) 16) (r-u16 s)))
(defun r-s32 (s) (let ((v (r-u32 s))) (if (logbitp 31 v) (- v (ash 1 32)) v)))

(defun r-bytes (s n &optional buf)
  "Read N bytes into BUF (allocated if NIL or too small); return the buffer."
  (let ((b (if (and buf (>= (length buf) n)) buf
               (make-array n :element-type '(unsigned-byte 8)))))
    (let ((got (read-sequence b s :end n)))
      (unless (= got n) (error "glass-client: short read (~d of ~d)" got n)))
    b))

(defun r-skip (s n)
  (let ((buf (make-array (min (max n 1) 65536) :element-type '(unsigned-byte 8))))
    (loop with left = n while (plusp left)
          for want = (min left (length buf))
          do (let ((got (read-sequence buf s :end want)))
               (unless (= got want) (error "glass-client: short skip"))
               (decf left want)))))

(defun w-u8 (s v) (write-byte (logand v #xff) s))
(defun w-u16 (s v) (w-u8 s (ash v -8)) (w-u8 s v))
(defun w-u32 (s v) (w-u16 s (ash v -16)) (w-u16 s v))

;;; ---- persistent inflate (one zlib stream for the whole connection) ----------

(defstruct (zin (:constructor %make-zin))
  ;; The decompressed history, kept because DEFLATE back-references in a later
  ;; rectangle reach into an earlier one — this is exactly the state cram's
  ;; ZSTREAM keeps on the encoding side, and the reason the server sync-flushes
  ;; rather than finishing the stream per rectangle.
  (out (make-array 65536 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  (started nil)
  (scratch (make-array 65536 :element-type '(unsigned-byte 8))))

(defparameter *zin-history* 32768
  "Bytes of decompressed history to keep when trimming — DEFLATE's window is 32 KB,
   so nothing older can ever be referenced.")
(defparameter *zin-trim-at* (* 4 1024 1024)
  "Trim the inflate history once it passes this; a long-lived connection would
   otherwise grow it without bound.")

(defun zin-inflate (z data end)
  "Inflate DATA[0..END) — one sync-flushed chunk of the connection's zlib stream —
   and return (values BUFFER LENGTH) naming just the bytes it produced.  BUFFER is
   reused between calls."
  (let* ((v (if (typep data '(simple-array (unsigned-byte 8) (*)))
                data (coerce data '(simple-array (unsigned-byte 8) (*)))))
         (start (if (zin-started z) 0 2))          ; the 2-byte zlib header, once
         (br (cram::%make-bitr :data v :pos start))
         (out (zin-out z))
         (from (fill-pointer out)))
    (setf (zin-started z) t)
    ;; cram's INFLATE-INTO runs to BFINAL, which never comes on a sync-flushed
    ;; stream; loop over blocks until this chunk's bytes are gone instead.  A
    ;; Z_SYNC_FLUSH ends with an empty stored block, so the chunk always ends on a
    ;; block boundary and byte-aligned — the loop stops exactly there.
    (loop while (< (cram::br-consumed br) end)
          do (let ((bfinal (cram::br-bit br)) (btype (cram::br-bits br 2)))
               (case btype
                 (0 (cram::br-align br)
                    (let ((len (cram::br-bits br 16)))
                      (cram::br-bits br 16)                       ; NLEN
                      (dotimes (i len) (vector-push-extend (cram::br-bits br 8) out))))
                 (1 (cram::inflate-huffman br out cram::*fixed-lit-huff* cram::*fixed-dist-huff*))
                 (2 (multiple-value-bind (lh dh) (cram::read-dynamic-tables br)
                      (cram::inflate-huffman br out lh dh)))
                 (t (error "glass-client: reserved DEFLATE block type")))
               (when (= bfinal 1) (return))))
    (let* ((n (- (fill-pointer out) from))
           (buf (if (>= (length (zin-scratch z)) n)
                    (zin-scratch z)
                    (setf (zin-scratch z)
                          (make-array (max n (* 2 (length (zin-scratch z))))
                                      :element-type '(unsigned-byte 8))))))
      (replace buf out :start2 from :end2 (fill-pointer out))
      ;; keep only what a back-reference could still reach
      (when (> (fill-pointer out) *zin-trim-at*)
        (let ((keep (min *zin-history* (fill-pointer out))))
          (replace out out :start2 (- (fill-pointer out) keep))
          (setf (fill-pointer out) keep)))
      (values buf n))))

;;; ---- the connection ---------------------------------------------------------

(defconstant +enc-raw+ 0)
(defconstant +enc-copyrect+ 1)
(defconstant +enc-zrle+ 16)
(defconstant +pseudo-desktop-size+ -223)

(defparameter *rfb-client-name* "glass"
  "What we call ourselves in log lines; RFB has no client-name field.")

(defparameter *pass-copyrect* t
  "Turn a CopyRect ARRIVING from the remote into a translation hint on our own
   framebuffer, so a compositor hosting this as a window can pass it on (see
   REMOTE-TAKE-COPY).  NIL makes every remote scroll a plain repaint — the control
   arm of the nested-CopyRect measurement, and the switch to flip if a compositor
   ever mis-applies a hint.")

(defvar *update-hook* nil
  "(REMOTE RECTS) called after each FramebufferUpdate is applied, with RECTS =
   ((encoding x y w h) ...) in arrival order.  The diagnostic that answers \"why was
   that translation hint refused?\", which is not a question the counters can
   answer — it is always some other rectangle landing on the copy's destination, and
   what matters is which one.  NIL (the default) costs one test per update.")

(defparameter *reconnect-delay* 0.5 "Seconds before the first reconnect attempt.")
(defparameter *reconnect-max-delay* 5.0 "Ceiling for the reconnect backoff.")
(defparameter *input-queue-limit* 256
  "Input events buffered for a remote that is not draining them.  Full means the
   remote is stalled; the OLDEST pointer motion is dropped first (a stale position
   is worthless), and a full queue of keys drops the new event rather than block.
   Nothing in the input path may ever wait on the remote: the caller is the local
   window manager's input thread.")

(defstruct (remote (:conc-name remote-) (:constructor %make-remote))
  host port
  fb                                        ; the local framebuffer holding the remote screen
  (name "" :type string)                    ; the remote's desktop name
  (state :connecting)                       ; :connecting | :up | :down
  (running t)
  (dirty t)                                 ; content changed since the last REMOTE-TAKE-DIRTY
  (resized nil)                             ; the remote changed size since last asked
  on-resize                                 ; (w h) -> (), called when the remote resizes
  ;; input, queued for the writer thread — the caller must never block on a socket
  (queue '())                               ; reversed list of pending events
  (qlen 0)
  (qlock (sb-thread:make-mutex :name "glass-client-input"))
  (qcv (sb-thread:make-waitqueue :name "glass-client-input"))
  socket stream
  (want-update nil)                         ; a FramebufferUpdateRequest is due
  (want-full nil)                           ; ...and it must be a NON-incremental one
  reader writer
  ;; standing counters (REMOTE-REPORT)
  (frames 0) (bytes 0) (rects 0) (raw 0) (zrle 0) (copyrects 0)
  (hints 0) (hints-trimmed 0) (hints-refused 0) (connects 0) (drops 0)
  ;; area accounting for the hints, in pixels: how much translation ARRIVED
  ;; (COPY-PX), how much a later rectangle in the same update forced off it
  ;; (TRIM-PX) or killed outright (REFUSE-PX), and how much the compositor
  ;; actually got to move (TAKEN-PX over TAKEN takes).  The counts alone say a
  ;; hint survived; only the areas say how much of it did — which is the whole
  ;; question for a gesture whose exposed region is an L rather than a strip.
  (copy-px 0) (trim-px 0) (refuse-px 0) (taken 0) (taken-px 0)
  (last-error nil)
  (last-frame 0)                            ; when the last update landed (stall clock)
  (t0 (get-internal-real-time)))

(defun remote-connected-p (r) (eq (remote-state r) :up))
(defun remote-width (r) (glass:fb-width (remote-fb r)))
(defun remote-height (r) (glass:fb-height (remote-fb r)))

;;; ---- input: queued, never blocking ------------------------------------------

(defun %enqueue (r event)
  "Queue EVENT for the writer thread.  Returns immediately, always: a stalled
   remote costs a dropped event, never a blocked caller."
  (sb-thread:with-mutex ((remote-qlock r))
    (when (>= (remote-qlen r) *input-queue-limit*)
      ;; drop the oldest pointer event (queue is reversed: oldest is last)
      (let* ((rev (reverse (remote-queue r)))
             (pos (position :pointer rev :key #'car)))
        (if pos
            (setf (remote-queue r) (reverse (append (subseq rev 0 pos) (subseq rev (1+ pos))))
                  (remote-qlen r) (1- (remote-qlen r)))
            (return-from %enqueue nil))))                ; all keys: drop the newcomer
    (push event (remote-queue r))
    (incf (remote-qlen r))
    (sb-thread:condition-notify (remote-qcv r)))
  t)

(defun %drain (r)
  "Take everything queued, oldest first."
  (sb-thread:with-mutex ((remote-qlock r))
    (prog1 (nreverse (remote-queue r))
      (setf (remote-queue r) '() (remote-qlen r) 0))))

(defun remote-key (r down keysym)
  "Forward a key to the remote.  RFB keysyms both ends — the local server handed us
   the client's keysym unchanged, and that is exactly what the remote's KeyEvent
   wants, so there is no translation at all."
  (%enqueue r (list :key (if down 1 0) keysym)))

(defun remote-pointer (r mask x y)
  "Forward a pointer event, in the remote's coordinates.  A wm-surface's on-pointer
   is already SURFACE-LOCAL (the WM subtracted the window origin), and the surface
   is the remote's screen, so the translation is the identity plus a clamp — the
   remote must never be told about a pixel it does not have."
  (%enqueue r (list :pointer (logand mask #xff)
                    (max 0 (min x (1- (remote-width r))))
                    (max 0 (min y (1- (remote-height r)))))))

;;; ---- what changed, and how it moved -----------------------------------------

(defun remote-take-dirty (r)
  "Did the remote screen change since the last call?  The wm-surface DIRTY-P."
  (prog1 (remote-dirty r) (setf (remote-dirty r) nil)))

(defun remote-take-copy (r)
  "The pending content translation, consumed — the wm-surface COPY-P.  See
   %NOTE-COPY for what makes a hint survive to be offered here."
  (let ((c (glass:fb-take-copy (remote-fb r))))
    (when c
      (incf (remote-taken r))
      (incf (remote-taken-px r) (* (fifth c) (sixth c))))
    c))

(defun %box-overlap-p (a b)
  (destructuring-bind (ax ay aw ah) a
    (destructuring-bind (bx by bw bh) b
      (and (< ax (+ bx bw)) (< bx (+ ax aw)) (< ay (+ by bh)) (< by (+ ay ah))))))

(defun %compose-copy (old new)
  "Fold a pending translation OLD and a new one NEW into the single translation
   that still describes both — the client-side twin of glass's %FB-COPY-COMPOSE,
   and for the same reason: the compositor may be several updates behind, so the
   hint has to mean \"since you last looked\", not \"since the last rectangle\".
   The pixels one CopyRect can still carry are those NEW moved AND OLD had already
   put there, so intersect OLD's destination with NEW's source and map that patch
   back to OLD's source and forward to NEW's destination.  NIL is always safe: a
   dropped hint costs a repaint, never a wrong pixel."
  (cond
    ((null old) new)
    ((null new) old)
    (t (destructuring-bind (osx osy odx ody ow oh) old
         (destructuring-bind (nsx nsy ndx ndy nw nh) new
           (let* ((x0 (max odx nsx)) (y0 (max ody nsy))
                  (x1 (min (+ odx ow) (+ nsx nw)))
                  (y1 (min (+ ody oh) (+ nsy nh)))
                  (w (- x1 x0)) (h (- y1 y0)))
             (when (and (plusp w) (plusp h))
               (list (+ osx (- x0 odx)) (+ osy (- y0 ody))
                     (+ ndx (- x0 nsx)) (+ ndy (- y0 nsy))
                     w h))))))))

;;; The hint the compositor eventually takes has to satisfy exactly one property:
;;; the framebuffer's pixels in the DESTINATION now must equal the pixels that were
;;; in the SOURCE when the compositor last looked.  That is what lets it move the
;;; screen block it already has instead of re-blitting ours.  So the hint is
;;; maintained rectangle by rectangle, IN ARRIVAL ORDER, across however many
;;; updates arrive between two composites:
;;;
;;;   * a CopyRect composes onto the pending hint (%COMPOSE-COPY);
;;;   * ANY other rectangle that lands on the pending hint's destination destroys
;;;     it — those pixels are no longer a translation of anything.
;;;
;;; A scroll survives that unscathed, because a scroll's other rectangle is the
;;; strip the copy exposed, which is by construction outside the copy's
;;; destination.  A window dragged over a scrolling one does not, and should not.
;;;
;;; The hint lives in the framebuffer's own COPY slot rather than beside it, so
;;; that the compositor's take (GLASS:FB-TAKE-COPY) is what resets the base it is
;;; measured from — there is no second place for the two to disagree.  All of this
;;; runs under the framebuffer lock, which the compositor also holds while it takes
;;; the hint and blits, so it can never see a half-applied update.

(defun %note-copy (r copy)
  (when *pass-copyrect*
    (incf (remote-copy-px r) (* (fifth copy) (sixth copy)))
    (let ((fb (remote-fb r)))
      (setf (glass:fb-copy fb) (%compose-copy (glass:fb-copy fb) copy)))))

(defun %box-minus (d p)
  "The largest single rectangle of D that P does not touch — the four slabs of D
   outside P, best by area — or NIL if P covers D.  One rectangle rather than the
   full difference because a CopyRect is one rectangle; taking the biggest is the
   whole of the answer for the case this exists for, where P is a full-width band
   along one edge of D and the difference IS a rectangle."
  (destructuring-bind (dx dy dw dh) d
    (destructuring-bind (px py pw ph) p
      (let* ((top    (list dx dy dw (max 0 (- (min (+ dy dh) py) dy))))
             (bot-y  (max dy (+ py ph)))
             (bottom (list dx bot-y dw (max 0 (- (+ dy dh) bot-y))))
             (left   (list dx dy (max 0 (- (min (+ dx dw) px) dx)) dh))
             (rt-x   (max dx (+ px pw)))
             (right  (list rt-x dy (max 0 (- (+ dx dw) rt-x)) dh))
             (best nil))
        (dolist (c (list top bottom left right) best)
          (when (and (plusp (third c)) (plusp (fourth c))
                     (or (null best) (> (* (third c) (fourth c)) (* (third best) (fourth best)))))
            (setf best c)))))))

(defun %note-paint (r box)
  "A rectangle of plain pixels landed at BOX (x y w h).  Whatever of the pending
   translation's destination it covers is no longer a translation of anything, so the
   hint SHRINKS to the largest part of itself the paint did not touch — and is
   dropped only if nothing is left.

   Shrinking rather than refusing is not a refinement, it is the difference between
   the hint surviving and never surviving.  A scroll's exposed strip is found by a
   TILE-aligned diff, so it arrives 32-pixel aligned and overlaps the bottom of the
   copy destination by up to a tile even though not one moved pixel is inside it.
   Refusing on that overlap threw away a 1100x589 copy to protect ten rows of it —
   measured, every single hint of a scroll."
  (let* ((fb (remote-fb r)) (c (glass:fb-copy fb)))
    (when c
      (destructuring-bind (sx sy dx dy w h) c
        (when (%box-overlap-p box (list dx dy w h))
          (let ((keep (%box-minus (list dx dy w h) box)))
            (if keep
                (destructuring-bind (kx ky kw kh) keep
                  (setf (glass:fb-copy fb)
                        (list (+ sx (- kx dx)) (+ sy (- ky dy)) kx ky kw kh))
                  (incf (remote-hints-trimmed r))
                  (incf (remote-trim-px r) (- (* w h) (* kw kh))))
                (progn (setf (glass:fb-copy fb) nil)
                       (incf (remote-hints-refused r))
                       (incf (remote-refuse-px r) (* w h))))))))))

;;; ---- pixel decoding ---------------------------------------------------------
;;; We SetPixelFormat to glass's native 32bpp little-endian 0x00RRGGBB straight
;;; after ClientInit, which RFC 6143 §7.5.1 obliges any server to honour (glass's
;;; own PARSE-PXFMT recognises it as native and converts nothing).  So a Raw pixel
;;; is 4 bytes B,G,R,x and a ZRLE CPIXEL is 3 bytes B,G,R — the same CPIX the
;;; encoder writes.  Negotiating one format rather than decoding whatever arrives
;;; keeps the hot loop a shift and two ors.

(declaim (inline %px3 %px4))
(defun %px3 (b i) (logior (ash (aref b (+ i 2)) 16) (ash (aref b (+ i 1)) 8) (aref b i)))
(defun %px4 (b i) (%px3 b i))

(defun %fb-row-start (fb y) (* y (glass:fb-width fb)))

(defun %rect-in-bounds-p (fb x y w h)
  (and (>= x 0) (>= y 0) (>= w 0) (>= h 0)
       (<= (+ x w) (glass:fb-width fb)) (<= (+ y h) (glass:fb-height fb))))

(defparameter *raw-band-rows* 64
  "Rows of a Raw rectangle read into memory before the framebuffer lock is taken.
   Bounds both the buffer and how long the lock is held — the reason for a band
   rather than the whole rectangle is that a full-screen Raw rect is megabytes, and
   the reason for a band rather than a row is that a lock per row of a 1280-pixel
   screen is 800 lock round-trips for one frame.")

(defun %decode-raw (r s x y w h)
  "Raw: W*H pixels, in bands — read a band with no lock held, apply it under the
   lock, repeat."
  (let* ((fb (remote-fb r))
         (band (make-array (* (max w 1) 4 *raw-band-rows*) :element-type '(unsigned-byte 8))))
    (loop for y0 from 0 below h by *raw-band-rows*
          for rows = (min *raw-band-rows* (- h y0))
          do (r-bytes s (* w 4 rows) band)
             (incf (remote-bytes r) (* w 4 rows))
             (glass:with-fb-locked (fb)
               (%note-paint r (list x (+ y y0) w rows))
               (let ((px (glass:fb-pixels fb)))
                 (dotimes (ly rows)
                   (let ((o (+ (%fb-row-start fb (+ y y0 ly)) x))
                         (b (* ly w 4)))
                     (dotimes (lx w)
                       (setf (aref px (+ o lx)) (%px4 band (+ b (* lx 4))))))))))))

(defun %zrle-tile (px fw x y tw th buf pos)
  "Unpack one ZRLE tile at (X,Y) from BUF at POS; return the next POS.  The exact
   inverse of zrle.lisp's ZRLE-TILE, plus the two run-length subencodings the
   encoder never emits but another server will."
  (declare (type (simple-array (unsigned-byte 32) (*)) px)
           (type (simple-array (unsigned-byte 8) (*)) buf)
           (type fixnum fw x y tw th pos)
           (optimize (speed 3) (safety 1)))
  (macrolet ((put (lx ly c) `(setf (aref px (+ (* (+ y ,ly) fw) x ,lx)) ,c)))
    (let ((sub (aref buf pos)))
      (incf pos)
      (cond
        ((= sub 0)                                        ; raw CPIXELs
         (dotimes (ly th)
           (dotimes (lx tw) (put lx ly (%px3 buf pos)) (incf pos 3))))
        ((= sub 1)                                        ; solid
         (let ((c (%px3 buf pos)))
           (incf pos 3)
           (dotimes (ly th) (dotimes (lx tw) (put lx ly c)))))
        ((<= 2 sub 16)                                    ; packed palette
         (let ((pal (make-array 16 :element-type '(unsigned-byte 32)))
               (bpp (cond ((<= sub 2) 1) ((<= sub 4) 2) (t 4))))
           (dotimes (k sub) (setf (aref pal k) (%px3 buf pos)) (incf pos 3))
           (dotimes (ly th)
             (let ((acc 0) (nb 0))
               (declare (fixnum acc nb))
               (dotimes (lx tw)
                 (when (< nb bpp)
                   (setf acc (logior (ash acc 8) (aref buf pos)) nb (+ nb 8))
                   (incf pos))
                 (decf nb bpp)
                 (put lx ly (aref pal (logand (ash acc (- nb)) (1- (ash 1 bpp)))))
                 (setf acc (logand acc (1- (ash 1 nb)))))))))
        ((= sub 128)                                      ; plain RLE
         (let ((n (* tw th)) (i 0))
           (declare (fixnum n i))
           (loop while (< i n) do
             (let ((c (%px3 buf pos)) (run 1))
               (declare (fixnum run))
               (incf pos 3)
               (loop while (= (aref buf pos) 255) do (incf run 255) (incf pos))
               (incf run (aref buf pos)) (incf pos)
               (dotimes (k run)
                 (when (< i n)
                   (put (mod i tw) (floor i tw) c)
                   (incf i)))))))
        ((>= sub 130)                                     ; palette RLE
         (let* ((psize (- sub 128))
                (pal (make-array psize :element-type '(unsigned-byte 32)))
                (n (* tw th)) (i 0))
           (declare (fixnum n i))
           (dotimes (k psize) (setf (aref pal k) (%px3 buf pos)) (incf pos 3))
           (loop while (< i n) do
             (let* ((b (aref buf pos)) (c (aref pal (logand b #x7f))) (run 1))
               (declare (fixnum run))
               (incf pos)
               (when (logbitp 7 b)
                 (loop while (= (aref buf pos) 255) do (incf run 255) (incf pos))
                 (incf run (aref buf pos)) (incf pos))
               (dotimes (k run)
                 (when (< i n)
                   (put (mod i tw) (floor i tw) c)
                   (incf i)))))))
        (t (error "glass-client: ZRLE subencoding ~d" sub)))))
  pos)

(defun %decode-zrle (r s z x y w h)
  "Read the rectangle's compressed bytes, inflate them, and only THEN take the
   framebuffer lock to unpack the tiles.  The split is the whole rule of this file
   (see %READ-UPDATE): reading a socket and holding the lock a compositor waits on
   are two things that must never happen at the same instant."
  (let* ((len (r-u32 s))
         (data (r-bytes s len)))
    (incf (remote-bytes r) (+ 4 len))
    (multiple-value-bind (buf n) (zin-inflate z data len)
      (declare (ignore n))
      (let ((fb (remote-fb r)))
        (glass:with-fb-locked (fb)
          (%note-paint r (list x y w h))
          (let ((px (glass:fb-pixels fb)) (fw (glass:fb-width fb)) (pos 0))
            (loop for ty from 0 below h by 64 for th = (min 64 (- h ty)) do
              (loop for tx from 0 below w by 64 for tw = (min 64 (- w tx)) do
                (setf pos (%zrle-tile px fw (+ x tx) (+ y ty) tw th buf pos))))))))))

;;; ---- one FramebufferUpdate --------------------------------------------------
;;;
;;; NOTHING here holds the framebuffer lock across a read from the socket, and that
;;; is the single rule the whole hosted-window story rests on.  The compositor takes
;;; that lock to blit this window; a remote that stops talking half way through a
;;; rectangle would, if the lock were held for the update, stop the local desktop
;;; dead — not slow it, stop it, on a thread that has no timeout and no business
;;; waiting for a foreign machine.  So each rectangle is READ whole (or, for Raw, in
;;; bounded bands) with no lock, and applied under the lock in memory time.  The
;;; visible cost is that a compositor can catch an update half applied — a torn
;;; frame on a window, corrected on the next tick, which is a cosmetic transient in
;;; exchange for the local desktop never being able to hang on a remote one.

(defun %read-update (r s z)
  "Read and apply one FramebufferUpdate."
  (r-skip s 1)                                            ; padding
  (let ((n (r-u16 s)) (fb (remote-fb r)) (trace '()))
    (incf (remote-bytes r) 4)
    (dotimes (i n)
      (let ((x (r-u16 s)) (y (r-u16 s)) (w (r-u16 s)) (h (r-u16 s)) (enc (r-s32 s)))
        (incf (remote-bytes r) 12)
        (when *update-hook* (push (list enc x y w h) trace))
        (cond
          ;; DesktopSize: the rect header IS the new size; there is no payload.
          ((= enc +pseudo-desktop-size+)
           (glass:with-fb-locked (fb) (%resize r w h)))
          ((= enc +enc-copyrect+)
           (let ((sx (r-u16 s)) (sy (r-u16 s)))
             (incf (remote-bytes r) 4)
             (incf (remote-rects r)) (incf (remote-copyrects r))
             (glass:with-fb-locked (fb)
               (when (and (%rect-in-bounds-p fb x y w h) (%rect-in-bounds-p fb sx sy w h))
                 (glass:fb-move-rect fb sx sy x y w h)
                 (%note-copy r (list sx sy x y w h))))))
          ((= enc +enc-raw+)
           (unless (%rect-in-bounds-p fb x y w h)
             (error "glass-client: Raw rect ~d,~d ~dx~d outside ~dx~d"
                    x y w h (glass:fb-width fb) (glass:fb-height fb)))
           (incf (remote-rects r)) (incf (remote-raw r))
           (%decode-raw r s x y w h))
          ((= enc +enc-zrle+)
           (unless (%rect-in-bounds-p fb x y w h)
             (error "glass-client: ZRLE rect ~d,~d ~dx~d outside ~dx~d"
                    x y w h (glass:fb-width fb) (glass:fb-height fb)))
           (incf (remote-rects r)) (incf (remote-zrle r))
           (%decode-zrle r s z x y w h))
          ;; Anything else is self-framing only to a client that asked for it, and
          ;; we advertised three encodings and one pseudo-encoding.  Its length is
          ;; unknowable, so the stream is now unreadable: drop and reconnect.
          (t (error "glass-client: unadvertised encoding ~d" enc)))))
    (glass:with-fb-locked (fb)
      (when (glass:fb-copy fb) (incf (remote-hints r)))
      (glass:fb-touch fb))
    (setf (remote-dirty r) t (remote-last-frame r) (get-internal-real-time))
    (incf (remote-frames r))
    (when *update-hook* (ignore-errors (funcall *update-hook* r (nreverse trace))))))

(defun %resize (r w h)
  "The remote's screen changed size.  Our framebuffer follows it — a remote desktop
   window is exactly as big as the desktop in it — and any pending translation goes
   with the pixels it described."
  (let ((fb (remote-fb r)))
    (unless (and (= w (glass:fb-width fb)) (= h (glass:fb-height fb)))
      (glass:fb-resize fb (max 1 w) (max 1 h) (glass:rgb 24 24 24))
      (setf (glass:fb-copy fb) nil
            (remote-resized r) t
            (remote-dirty r) t
            ;; ...and ask for the whole screen rather than a difference from a
            ;; snapshot that is now the wrong shape.  Without this the window sits on
            ;; the fill colour until something happens to move over there: an
            ;; incremental request is only answered when the remote has something to
            ;; say, and a resize it has already told us about is not something.
            (remote-want-full r) t
            (remote-want-update r) t)
      (when (remote-on-resize r)
        (ignore-errors (funcall (remote-on-resize r) w h))))))

;;; ---- handshake --------------------------------------------------------------

(defun %tcp-connect (host port)
  "Connect to an RFB server.  HOST is an endpoint in either form — a hostname beside PORT, or
   `unix:/path/to/rfb.sock' (or a bare absolute path) for a socket file.  RFB is a stream
   protocol; a viewer over a socket file gets the identical bytes in the identical order, which
   is why the whole of the difference is which socket gets made.  (Its name is now half a lie
   and kept anyway: every caller in this tree and outside it says %TCP-CONNECT.)"
  (glass:open-connection :host host :port port))

(defun %handshake (r s)
  "RFB 3.8 up to ServerInit, then our SetPixelFormat and SetEncodings.  Security
   type None only: this is a client for desktops on the same trust boundary as the
   one running it (VNC's DES challenge is not what would be protecting them), and a
   server that will not offer None is one to say so about rather than half-support."
  (let ((ver (r-bytes s 12)))
    (declare (ignore ver))
    (write-sequence (map '(simple-array (unsigned-byte 8) (*)) #'char-code
                         (format nil "RFB 003.008~c" #\Newline)) s)
    (force-output s))
  (let ((ntypes (r-u8 s)))
    (when (zerop ntypes)                                   ; failure: u32 length + reason
      (let* ((len (r-u32 s)) (reason (r-bytes s len)))
        (error "glass-client: server refused the connection: ~a"
               (map 'string #'code-char reason))))
    (let ((types (r-bytes s ntypes)))
      (unless (find 1 types :end ntypes)
        (error "glass-client: server offers no None security type (~a)"
               (coerce (subseq types 0 ntypes) 'list)))))
  (w-u8 s 1) (force-output s)
  (let ((res (r-u32 s)))
    (unless (zerop res)
      (let* ((len (r-u32 s)) (reason (r-bytes s len)))
        (error "glass-client: security failed: ~a" (map 'string #'code-char reason)))))
  (w-u8 s 1) (force-output s)                              ; ClientInit: shared
  (let* ((w (r-u16 s)) (h (r-u16 s)))
    (r-skip s 16)                                          ; the server's format; we override it
    (let* ((nl (r-u32 s)) (nm (r-bytes s nl)))
      (setf (remote-name r) (map 'string #'code-char (subseq nm 0 nl))))
    ;; SetPixelFormat: glass's native 32bpp 0x00RRGGBB, little-endian.
    (w-u8 s 0) (w-u8 s 0) (w-u8 s 0) (w-u8 s 0)
    (w-u8 s 32) (w-u8 s 24) (w-u8 s 0) (w-u8 s 1)
    (w-u16 s 255) (w-u16 s 255) (w-u16 s 255)
    (w-u8 s 16) (w-u8 s 8) (w-u8 s 0)
    (w-u8 s 0) (w-u8 s 0) (w-u8 s 0)
    ;; SetEncodings: ZRLE, CopyRect, Raw, and DesktopSize.
    ;;
    ;; CopyRect is here for the whole point of this client — a scroll on the remote
    ;; arrives as a translation instead of a screenful of pixels, and can then be
    ;; passed on as one.  The Cursor pseudo-encoding is deliberately ABSENT: with it
    ;; the remote would send a cursor sprite for us to draw, and the person driving
    ;; this window would see two pointers, theirs and a painted one a round trip
    ;; behind it.  Without it the remote paints no cursor at all and the only arrow
    ;; on the screen is the local one — which is over the right pixel by
    ;; construction, because the coordinates we forward are the ones it is at.
    (w-u8 s 2) (w-u8 s 0) (w-u16 s 4)
    (w-u32 s +enc-zrle+) (w-u32 s +enc-copyrect+) (w-u32 s +enc-raw+)
    (w-u32 s (logand +pseudo-desktop-size+ #xffffffff))
    (force-output s)
    (values w h)))

;;; ---- the two threads --------------------------------------------------------

(defun %writer-loop (r s)
  "Everything written to the socket after the handshake goes through here — input
   events and FramebufferUpdateRequests both — so that a remote which has stopped
   reading blocks THIS thread and nothing else.  The local window manager's input
   path only ever touches a mutex and a list."
  (handler-case
      (loop while (and (remote-running r) (eq (remote-state r) :up))
            do (let* ((events (%drain r))
                      (full (prog1 (remote-want-full r) (setf (remote-want-full r) nil)))
                      (want (or full (prog1 (remote-want-update r)
                                       (setf (remote-want-update r) nil)))))
                 (dolist (e events)
                   (ecase (car e)
                     (:key (destructuring-bind (down keysym) (cdr e)
                             (w-u8 s 4) (w-u8 s down) (w-u16 s 0) (w-u32 s keysym)))
                     (:pointer (destructuring-bind (mask x y) (cdr e)
                                 (w-u8 s 5) (w-u8 s mask) (w-u16 s x) (w-u16 s y)))))
                 (when want
                   (w-u8 s 3) (w-u8 s (if full 0 1)) (w-u16 s 0) (w-u16 s 0)
                   (w-u16 s (remote-width r)) (w-u16 s (remote-height r)))
                 (when (or events want) (force-output s))
                 (unless (or events want)
                   (sb-thread:with-mutex ((remote-qlock r))
                     (when (and (null (remote-queue r)) (not (remote-want-update r)))
                       (sb-thread:condition-wait (remote-qcv r) (remote-qlock r)
                                                 :timeout 0.05))))))
    (error (e) (setf (remote-last-error r) (princ-to-string e)))))

(defun %nudge (r)
  (setf (remote-want-update r) t)
  (sb-thread:with-mutex ((remote-qlock r)) (sb-thread:condition-notify (remote-qcv r))))

(defun %session (r)
  "One connection, from TCP connect to the socket dying.  Returns normally when the
   remote goes away; signals only on a programming error."
  (multiple-value-bind (sock s) (%tcp-connect (remote-host r) (remote-port r))
    (setf (remote-socket r) sock (remote-stream r) s)
    (unwind-protect
         (multiple-value-bind (w h) (%handshake r s)
           (glass:with-fb-locked ((remote-fb r))
             (glass:fb-resize (remote-fb r) (max 1 w) (max 1 h) (glass:rgb 24 24 24))
             (setf (glass:fb-copy (remote-fb r)) nil))
           (setf (remote-resized r) t (remote-dirty r) t)
           (when (remote-on-resize r) (ignore-errors (funcall (remote-on-resize r) w h)))
           (incf (remote-connects r))
           (setf (remote-state r) :up)
           (format *trace-output* "~&[glass-client] ~a:~d up — ~dx~d ~s~%"
                   (remote-host r) (remote-port r) w h (remote-name r))
           (force-output *trace-output*)
           ;; Prime with one full frame BEFORE the writer exists, so there is exactly
           ;; one thread on this socket's output at any moment; from here on every
           ;; write is the writer's, and this thread only reads.
           (w-u8 s 3) (w-u8 s 0) (w-u16 s 0) (w-u16 s 0) (w-u16 s w) (w-u16 s h)
           (force-output s)
           (setf (remote-writer r)
                 (sb-thread:make-thread (lambda () (%writer-loop r s)) :name "glass-client-writer"))
           (let ((z (%make-zin)))
             (loop while (remote-running r)
                   do (let ((msg (read-byte s nil :eof)))
                        (incf (remote-bytes r))
                        (case msg
                          (0 (%read-update r s z) (%nudge r))
                          (2 nil)                          ; Bell
                          ;; ServerCutText — the remote's selection.  DRAINED AND
                          ;; DROPPED, on purpose: see the note at the foot of this
                          ;; file.  The bytes have to be consumed to keep the stream
                          ;; in frame; nothing is done with them.
                          (3 (r-skip s 3)
                             (let ((len (r-u32 s))) (r-skip s len) (incf (remote-bytes r) (+ 7 len))))
                          (1 (r-skip s 1)                  ; SetColourMapEntries
                             (r-u16 s)
                             (let ((n (r-u16 s))) (r-skip s (* 6 n))))
                          (t (return)))))))               ; :eof or unknown -> this session is over
      (setf (remote-state r) :down)
      (ignore-errors (sb-thread:condition-notify (remote-qcv r)))
      (ignore-errors (close s))
      (ignore-errors (sb-bsd-sockets:socket-close sock))
      (when (remote-writer r)
        (ignore-errors (sb-thread:join-thread (remote-writer r) :timeout 2))
        (setf (remote-writer r) nil)))))

(defun %notice (r text)
  "Write a one-line status across the top of the framebuffer, so a window showing a
   dead remote says so instead of showing a stale desktop that looks alive."
  (ignore-errors
   (let ((fb (remote-fb r)))
     (glass:with-fb-locked (fb)
       (glass:fb-rect fb 0 0 (glass:fb-width fb) 22 (glass:rgb 40 40 48))
       (let ((f (find-symbol "FB-TEXT" '#:glass)))          ; glass/text is optional
         (when (and f (fboundp f))
           (funcall f fb 8 4 text :color (glass:rgb 230 230 230))))
       (setf (glass:fb-copy fb) nil))
     (setf (remote-dirty r) t))))

(defun %reader-loop (r)
  "Connect, run a session, and — when it ends — connect again.  A remote desktop
   that reboots comes back in this window; a remote that never was says so and goes
   on trying.  Nothing here can take the local desktop with it: every failure is a
   log line and a sleep."
  (let ((delay *reconnect-delay*))
    (loop while (remote-running r)
          do (handler-case
                 (progn (setf (remote-state r) :connecting)
                        (%session r)
                        (setf delay *reconnect-delay*))    ; a session that ran resets the backoff
               (error (e)
                 (setf (remote-last-error r) (princ-to-string e))
                 (format *trace-output* "~&[glass-client] ~a:~d — ~a~%"
                         (remote-host r) (remote-port r) e)
                 (force-output *trace-output*)))
             (setf (remote-state r) :down)
             (when (remote-running r)
               (incf (remote-drops r))
               (%notice r (format nil "~a:~d — reconnecting…~@[ (~a)~]"
                                  (remote-host r) (remote-port r) (remote-last-error r)))
               (sleep delay)
               (setf delay (min *reconnect-max-delay* (* 2 delay)))))))

;;; ---- the public handle ------------------------------------------------------

(defun connect-remote (host port &key fb (width 1280) (height 800) on-resize)
  "Open a live view of the RFB server at HOST:PORT.  Returns immediately with a
   REMOTE whose framebuffer starts out WIDTH x HEIGHT (or FB, if you brought your
   own — a window manager has usually already made one) and becomes the remote's
   size the moment the handshake says what that is.  ON-RESIZE (w h) fires whenever
   the remote's size changes, including that first time."
  (let ((r (%make-remote :host host :port port
                         :fb (or fb (glass:make-framebuffer width height (glass:rgb 24 24 24)))
                         :on-resize on-resize)))
    (%notice r (format nil "~a:~d — connecting…" host port))
    (setf (remote-reader r)
          (sb-thread:make-thread (lambda () (%reader-loop r))
                                 :name (format nil "glass-client ~a:~d" host port)))
    r))

(defun remote-stop (r)
  "Close the connection and stop reconnecting."
  (setf (remote-running r) nil (remote-state r) :down)
  (ignore-errors (sb-thread:condition-notify (remote-qcv r)))
  (ignore-errors (close (remote-stream r)))
  (ignore-errors (sb-bsd-sockets:socket-close (remote-socket r)))
  (ignore-errors (sb-thread:join-thread (remote-reader r) :timeout 2))
  r)

(defun remote-stats (r)
  (list :state (remote-state r) :name (remote-name r)
        :size (list (remote-width r) (remote-height r))
        :frames (remote-frames r) :bytes (remote-bytes r)
        :rects (remote-rects r) :zrle (remote-zrle r) :raw (remote-raw r)
        :copyrects (remote-copyrects r)
        :hints (remote-hints r) :hints-trimmed (remote-hints-trimmed r)
        :hints-refused (remote-hints-refused r)
        :copy-px (remote-copy-px r) :trim-px (remote-trim-px r)
        :refuse-px (remote-refuse-px r)
        :taken (remote-taken r) :taken-px (remote-taken-px r)
        :connects (remote-connects r) :drops (remote-drops r)
        :queued (remote-qlen r) :last-error (remote-last-error r)
        :since-frame (if (plusp (remote-last-frame r))
                         (/ (- (get-internal-real-time) (remote-last-frame r))
                            (float internal-time-units-per-second))
                         nil)
        :seconds (/ (- (get-internal-real-time) (remote-t0 r))
                    (float internal-time-units-per-second))))

(defun remote-report (r)
  (let ((p (remote-stats r)))
    (format nil "~a:~d ~a ~a ~dx~d | ~d frames, ~,1f KB, ~d rects (~d ZRLE, ~d Raw, ~d CopyRect) | ~
                 hints ~d passed / ~d trimmed / ~d refused | ~d connect(s), ~d drop(s)~@[ | last: ~a~]"
            (remote-host r) (remote-port r) (getf p :state) (getf p :name)
            (first (getf p :size)) (second (getf p :size))
            (getf p :frames) (/ (getf p :bytes) 1024.0) (getf p :rects)
            (getf p :zrle) (getf p :raw) (getf p :copyrects)
            (getf p :hints) (getf p :hints-trimmed) (getf p :hints-refused)
            (getf p :connects) (getf p :drops) (getf p :last-error))))

;;; ---- what this client deliberately does NOT carry ---------------------------
;;;
;;; THE CLIPBOARD.  ServerCutText arrives and is dropped on the floor (see
;;; %SESSION), and ClientCutText is never sent.  glass's own clipboard is
;;; SESSION-wide by design — one selection, every transport of that session reading
;;; and writing it — and that is right for one desktop.  A remote desktop is a
;;; different session on the other side of a trust boundary, and a clipboard
;;; bridged across it is a channel: everything copied on either side crosses
;;; automatically, in both directions, with nothing said and no way for the person
;;; at the keyboard to see it happen.  Qubes makes inter-domain copy an explicit,
;;; per-transfer act for exactly that reason, and the version of this worth having
;;; is that one, not an implicit bridge that is convenient until it is a leak.
;;;
;;; AUDIO.  The remote's mix is on its own port beside its screen
;;; (glass/audio-stream), which is a stream to subscribe to, not a thing to decode
;;; out of RFB; a viewer that wanted it would open that port, and this one does not.
