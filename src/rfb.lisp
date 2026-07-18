;;;; rfb.lisp — a VNC/RFB server (RFC 6143).
;;;;
;;;; Speaks RFB 3.8 over TCP: version + security handshake (None auth), ServerInit
;;;; advertising our 32-bit X8R8G8B8 pixel format, then the message loop.
;;;;
;;;; Updates are DIRTY-REGION tracked: each client keeps a snapshot of what it has
;;;; been shown, and an incremental FramebufferUpdateRequest sends only the tiles
;;;; that changed since — so a mostly-static desktop costs almost nothing (the
;;;; "fast" of fast/sharp/vibrant).  Pixels stay lossless; the client gets the best
;;;; encoding it advertises — ZRLE (zlib-compressed, see zrle.lisp), else Hextile,
;;;; else Raw — so it is still any stock VNC client.  KeyEvent / PointerEvent are
;;;; dispatched to caller callbacks.

(in-package #:glass)

(defvar *desktop-name* "glass")
(defparameter *tile* 32 "Dirty-tracking granularity (pixels).")

(defun string->bytes (s)
  (map '(simple-array (unsigned-byte 8) (*)) #'char-code s))

;;; ---- stream byte I/O (big-endian on the wire, per RFB) ----------------------

(declaim (inline w-u8 w-u16 w-u32 r-u8 r-u16 r-u32))
(defun w-u8  (s v) (write-byte (logand v #xff) s))
(defun w-u16 (s v) (w-u8 s (ash v -8)) (w-u8 s v))
(defun w-u32 (s v) (w-u16 s (ash v -16)) (w-u16 s v))
(defun w-bytes (s b) (write-sequence b s))
(defun r-u8  (s) (read-byte s))
(defun r-u16 (s) (logior (ash (read-byte s) 8) (read-byte s)))
(defun r-u32 (s) (logior (ash (r-u16 s) 16) (r-u16 s)))
(defun r-bytes (s n)
  (let ((b (make-array n :element-type '(unsigned-byte 8)))) (read-sequence b s) b))
(defun skip (s n) (dotimes (i n) (read-byte s)))

;;; ---- transport --------------------------------------------------------------

(defun tcp-listen (port &key (backlog 8) (address "0.0.0.0"))
  "A listening TCP socket bound to PORT."
  (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address sock) t)
    (sb-bsd-sockets:socket-bind sock (sb-bsd-sockets:make-inet-address address) port)
    (sb-bsd-sockets:socket-listen sock backlog)
    sock))

(defun accept-stream (listen)
  (let ((sock (sb-bsd-sockets:socket-accept listen)))
    (sb-bsd-sockets:socket-make-stream
     sock :input t :output t :element-type '(unsigned-byte 8) :buffering :full)))

;;; ---- RFB pixel format + handshake ------------------------------------------

;; 16-byte PIXEL_FORMAT: bpp=32 depth=24 big-endian=0 true-colour=1,
;; {r,g,b}-max=255, r-shift=16 g-shift=8 b-shift=0.  Matches fb pixel 0x00RRGGBB.
(defun write-pixel-format (s)
  (w-u8 s 32) (w-u8 s 24) (w-u8 s 0) (w-u8 s 1)
  (w-u16 s 255) (w-u16 s 255) (w-u16 s 255)
  (w-u8 s 16) (w-u8 s 8) (w-u8 s 0)
  (w-u8 s 0) (w-u8 s 0) (w-u8 s 0))

(defun handshake (fb s name)
  "RFB 3.8 handshake through ServerInit.  Returns T on success."
  (w-bytes s (string->bytes "RFB 003.008")) (w-u8 s 10) (force-output s)
  (r-bytes s 12)                              ; client ProtocolVersion
  (w-u8 s 1) (w-u8 s 1) (force-output s)       ; 1 security type: None (1)
  (r-u8 s)                                     ; client's choice
  (w-u32 s 0) (force-output s)                 ; SecurityResult = OK
  (r-u8 s)                                     ; ClientInit (shared-flag)
  (w-u16 s (fb-width fb)) (w-u16 s (fb-height fb))
  (write-pixel-format s)
  (let ((nb (string->bytes name))) (w-u32 s (length nb)) (w-bytes s nb))
  (force-output s)
  t)

;;; ---- snapshots + dirty-tile detection --------------------------------------

(defun copy-pixels (fb)
  (let* ((p (fb-pixels fb))
         (c (make-array (length p) :element-type '(unsigned-byte 32))))
    (replace c p) c))

(defun clip-rect (fb x y w h)
  "The requested rect clipped to the framebuffer, as (x y w h)."
  (let ((x0 (max 0 x)) (y0 (max 0 y))
        (x1 (min (fb-width fb) (+ x w))) (y1 (min (fb-height fb) (+ y h))))
    (list x0 y0 (max 0 (- x1 x0)) (max 0 (- y1 y0)))))

(defun tile-changed-p (fb snap x0 y0 x1 y1)
  (declare (optimize (speed 3) (safety 0)) (fixnum x0 y0 x1 y1))
  (let ((px (fb-pixels fb)) (fw (fb-width fb)))
    (declare (type (simple-array (unsigned-byte 32) (*)) px snap) (fixnum fw))
    (loop for y fixnum from y0 below y1 for row fixnum = (* y fw) do
      (loop for x fixnum from x0 below x1 for i fixnum = (+ row x) do
        (unless (= (aref px i) (aref snap i)) (return-from tile-changed-p t))))
    nil))

(defun dirty-rects (fb snap)
  "Coalesced (x y w h) rectangles where FB differs from SNAP: changed tiles on a
   *TILE* grid, merged into horizontal runs per tile-row."
  (let ((fw (fb-width fb)) (fh (fb-height fb)) (ts *tile*) (rects '()))
    (loop for ty from 0 below fh by ts for y1 = (min fh (+ ty ts)) do
      (let ((run -1))
        (loop for tx from 0 below fw by ts do
          (if (tile-changed-p fb snap tx ty (min fw (+ tx ts)) y1)
              (when (< run 0) (setf run tx))
              (when (>= run 0) (push (list run ty (- tx run) (- y1 ty)) rects) (setf run -1))))
        (when (>= run 0) (push (list run ty (- fw run) (- y1 ty)) rects))))
    (nreverse rects)))

(defun update-snapshot (fb snap rects)
  "Copy the pixels of RECTS from FB into SNAP (they're now what the client has)."
  (declare (optimize (speed 3) (safety 0)))
  (let ((px (fb-pixels fb)) (fw (fb-width fb)))
    (declare (type (simple-array (unsigned-byte 32) (*)) px snap) (fixnum fw))
    (dolist (r rects)
      (destructuring-bind (x y w h) r
        (declare (fixnum x y w h))
        (loop for yy fixnum from y below (+ y h) for row fixnum = (* yy fw) do
          (replace snap px :start1 (+ row x) :end1 (+ row x w) :start2 (+ row x)))))))

;;; ---- encodings --------------------------------------------------------------

(defconstant +enc-raw+ 0)
(defconstant +enc-hextile+ 5)
(defconstant +enc-zrle+ 16)     ; encoder in zrle.lisp (loaded after this file)

(defun write-rect-raw (s fb x y w h)
  (w-u16 s x) (w-u16 s y) (w-u16 s w) (w-u16 s h) (w-u32 s +enc-raw+)
  (let ((px (fb-pixels fb)) (fw (fb-width fb))
        (buf (make-array (* w h 4) :element-type '(unsigned-byte 8))) (o 0))
    (loop for yy from y below (+ y h) for row = (* yy fw) do
      (loop for xx from x below (+ x w) for p = (aref px (+ row xx)) do
        (setf (aref buf o)       (logand p #xff)             ; B (little-endian)
              (aref buf (+ o 1)) (logand (ash p -8) #xff)    ; G
              (aref buf (+ o 2)) (logand (ash p -16) #xff)   ; R
              (aref buf (+ o 3)) 0)                           ; X
        (incf o 4)))
    (w-bytes s buf)))

;;; ---- Hextile encoding (RFC 6143 §7.7.4) ------------------------------------
;;; Each 16x16 tile: a solid tile costs a byte (or a byte + colour); a tile of a
;;; few colours is a background plus coloured sub-rectangles over it; a busy tile
;;; falls back to raw.  Lossless (sharp text, full colour), no zlib — great for
;;; desktop UI where most tiles are solid or near-solid.  Background persists
;;; across tiles, so runs of the same colour cost one byte each.

(defun %push-pixel (buf p)
  (vector-push-extend (logand p #xff) buf)
  (vector-push-extend (logand (ash p -8) #xff) buf)
  (vector-push-extend (logand (ash p -16) #xff) buf)
  (vector-push-extend 0 buf))

(defun tile-info (px fw ax ay tw th)
  "(values distinct-colour-count most-common-colour) for the tile."
  (let ((counts (make-hash-table)) (best 0) (bestc 0))
    (dotimes (ly th)
      (let ((row (* (+ ay ly) fw)))
        (dotimes (lx tw)
          (let* ((c (aref px (+ row ax lx))) (n (1+ (gethash c counts 0))))
            (setf (gethash c counts) n)
            (when (> n best) (setf best n bestc c))))))
    (values (hash-table-count counts) bestc)))

(defun tile-subrects (px fw ax ay tw th bg)
  "Non-background horizontal runs in the tile, each a (colour lx ly len) subrect."
  (let ((subs '()))
    (dotimes (ly th)
      (let ((row (* (+ ay ly) fw)) (lx 0))
        (loop while (< lx tw) do
          (let ((c (aref px (+ row ax lx))))
            (if (= c bg)
                (incf lx)
                (let ((start lx))
                  (loop while (and (< lx tw) (= (aref px (+ row ax lx)) c)) do (incf lx))
                  (push (list c start ly (- lx start)) subs)))))))
    (nreverse subs)))

(defun write-rect-hextile (s fb x y w h)
  (w-u16 s x) (w-u16 s y) (w-u16 s w) (w-u16 s h) (w-u32 s +enc-hextile+)
  (let ((px (fb-pixels fb)) (fw (fb-width fb)) (cur-bg -1)
        (buf (make-array 512 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for ty from 0 below h by 16 for th = (min 16 (- h ty)) do
      (loop for tx from 0 below w by 16 for tw = (min 16 (- w tx)) do
        (let ((ax (+ x tx)) (ay (+ y ty)))
          (multiple-value-bind (ncol bg) (tile-info px fw ax ay tw th)
            (cond
              ((= ncol 1)                                  ; solid tile
               (if (= bg cur-bg)
                   (vector-push-extend 0 buf)              ; mask 0 — same background
                   (progn (vector-push-extend 2 buf) (%push-pixel buf bg) (setf cur-bg bg))))
              (t
               (let* ((subs (tile-subrects px fw ax ay tw th bg))
                      (nsub (length subs)))
                 (if (and (<= nsub 255) (< (* nsub 6) (* tw th 4)))   ; hextile beats raw?
                     (let ((mask (logior 8 16)))            ; AnySubrects | SubrectsColoured
                       (unless (= bg cur-bg) (setf mask (logior mask 2)))
                       (vector-push-extend mask buf)
                       (unless (= bg cur-bg) (%push-pixel buf bg) (setf cur-bg bg))
                       (vector-push-extend nsub buf)
                       (dolist (sr subs)
                         (destructuring-bind (c lx ly len) sr
                           (%push-pixel buf c)
                           (vector-push-extend (logior (ash lx 4) ly) buf)
                           (vector-push-extend (logior (ash (1- len) 4) 0) buf))))
                     (progn                                 ; raw tile
                       (vector-push-extend 1 buf)           ; mask 1 = Raw
                       (setf cur-bg -1)
                       (dotimes (ly th)
                         (let ((row (* (+ ay ly) fw)))
                           (dotimes (lx tw) (%push-pixel buf (aref px (+ row ax lx)))))))))))))))
    (w-bytes s buf)))

;;; ---- update assembly --------------------------------------------------------

(defun write-rect (s fb x y w h enc zs)
  (cond
    ((= enc +enc-zrle+)    (write-rect-zrle s fb x y w h zs))
    ((= enc +enc-hextile+) (write-rect-hextile s fb x y w h))
    (t                     (write-rect-raw s fb x y w h))))

(defun send-rects (s fb rects enc zs)
  "One FramebufferUpdate carrying RECTS in encoding ENC.  ZS is the client's
   persistent ZRLE zlib stream (used only when ENC is ZRLE)."
  (w-u8 s 0) (w-u8 s 0) (w-u16 s (length rects))             ; msg-type, pad, #rects
  (dolist (r rects) (destructuring-bind (x y w h) r (write-rect s fb x y w h enc zs)))
  (force-output s))

;;; ---- desktop resize (RFC 6143 §7.8) -----------------------------------------
;;; DesktopSize (-223): a pseudo-encoding the client lists in SetEncodings to say
;;; "tell me when the framebuffer changes size."  We send a single pseudo-rect
;;; whose width/height ARE the new size.  ExtendedDesktopSize (-308) additionally
;;; lets the client REQUEST a size (SetDesktopSize, msg 251) — e.g. by resizing
;;; its window; we forward that to the app via the ON-RESIZE callback.

(defconstant +pseudo-desktop-size+          #xFFFFFF21)   ; -223 as unsigned u32
(defconstant +pseudo-extended-desktop-size+ #xFFFFFECC)   ; -308

(defun send-desktop-size (s w h)
  "A FramebufferUpdate carrying just the DesktopSize pseudo-rect (new size W x H)."
  (w-u8 s 0) (w-u8 s 0) (w-u16 s 1)
  (w-u16 s 0) (w-u16 s 0) (w-u16 s w) (w-u16 s h) (w-u32 s +pseudo-desktop-size+)
  (force-output s))

(defun snap-matches-p (snap fb)
  (= (length snap) (* (fb-width fb) (fb-height fb))))

;;; ---- mouse cursor (Cursor pseudo-encoding, -239) ----------------------------
;;; The server sends the cursor SHAPE once; the client renders it at its own
;;; pointer position (so it tracks the mouse locally, no per-move server work).
;;; A classic arrow: 'o' = black outline, 'x' = white fill, '.' = transparent —
;;; visible on any background, hotspot at the tip (0,0).

(defconstant +pseudo-cursor+ #xFFFFFF11)   ; -239 as unsigned u32

(defparameter *cursor-arrow*
  '("o.........."
    "oo........."
    "oxo........"
    "oxxo......."
    "oxxxo......"
    "oxxxxo....."
    "oxxxxxo...."
    "oxxxxxxo..."
    "oxxxxxxxo.."
    "oxxxxxxxxo."
    "oxxxxxoooo."
    "oxxoxxo...."
    "oxo.oxxo..."
    "oo..oxxo..."
    "o....oxxo.."
    "......oo..."))

(defun send-cursor (s &optional (rows *cursor-arrow*))
  "Send the cursor shape as a Cursor pseudo-rect (hotspot 0,0)."
  (let ((w (reduce #'max rows :key #'length)) (h (length rows)))
    (w-u8 s 0) (w-u8 s 0) (w-u16 s 1)                        ; FramebufferUpdate, 1 rect
    (w-u16 s 0) (w-u16 s 0) (w-u16 s w) (w-u16 s h) (w-u32 s +pseudo-cursor+)
    (loop for row in rows do                                 ; pixels: w*h, format order (B,G,R,X)
      (dotimes (x w)
        (let ((c (if (< x (length row)) (char row x) #\.)))
          (multiple-value-bind (px) (case c (#\o 0) (#\x #xffffff) (t 0))
            (w-u8 s (logand px #xff)) (w-u8 s (logand (ash px -8) #xff))
            (w-u8 s (logand (ash px -16) #xff)) (w-u8 s 0)))))
    (loop for row in rows do                                 ; 1-bpp mask, MSB first, row-padded
      (let ((acc 0) (nb 0))
        (dotimes (x w)
          (let ((opaque (and (< x (length row)) (member (char row x) '(#\o #\x)))))
            (setf acc (logior (ash acc 1) (if opaque 1 0)) nb (1+ nb))
            (when (= nb 8) (w-u8 s acc) (setf acc 0 nb 0))))
        (when (plusp nb) (w-u8 s (ash acc (- 8 nb))))))
    (force-output s)))

;;; ---- client message loop ----------------------------------------------------

;;; A per-client sender runs in its OWN thread so that reading input (key/pointer)
;;; never blocks on producing a framebuffer update — the single-threaded design
;;; used to park in the update wait for up to ~5s, stalling input AND frames to
;;; that cadence.  The reader thread only stashes the latest FramebufferUpdate-
;;; Request in WANT; the sender fulfils it the moment the fb has something to show
;;; (polling ~60 Hz).  ONLY the sender writes pixels to the socket.
(defstruct (rfb-client (:conc-name rc-))
  (enc +enc-raw+) dss cursor cursor-sent
  (snap-box (list nil)) (zs (cram:make-zstream))
  last-size                     ; (cons w h) — fb size last announced to this client
  (want nil)                    ; latest pending request (inc x y w h), or NIL
  (running t)
  (lock (sb-thread:make-mutex :name "rfb-client")))

(defun send-update (client fb s req)
  "Fulfil one FramebufferUpdateRequest REQ = (inc x y w h).  Returns T if bytes
   were written (pixels or a resize), NIL if there was nothing to send (incremental
   with no dirty tiles) — the caller then keeps REQ pending and polls again."
  (destructuring-bind (inc x y w h) req
    (when (and (rc-cursor client) (not (rc-cursor-sent client)))     ; cursor shape, once
      (send-cursor s) (setf (rc-cursor-sent client) t))
    (let ((ls (rc-last-size client)))                                ; resize takes priority
      (when (rc-dss client)
        (with-fb-locked (fb)
          (when (or (/= (fb-width fb) (car ls)) (/= (fb-height fb) (cdr ls)))
            (send-desktop-size s (fb-width fb) (fb-height fb))
            (setf (car ls) (fb-width fb) (cdr ls) (fb-height fb) (car (rc-snap-box client)) nil)
            (return-from send-update t)))))
    (let ((snap-box (rc-snap-box client)) (enc (rc-enc client)) (zs (rc-zs client)))
      (if (or (zerop inc) (null (car snap-box)))                     ; full / first frame
          (with-fb-locked (fb)
            (send-rects s fb (list (clip-rect fb x y w h)) enc zs)
            (setf (car snap-box) (copy-pixels fb))
            t)
          (with-fb-locked (fb)                                       ; incremental: dirty tiles
            (let ((snap (car snap-box)))
              (cond
                ((not (snap-matches-p snap fb)) (setf (car snap-box) nil) nil)  ; resized; resync
                (t (let ((rects (dirty-rects fb snap)))
                     (when rects (send-rects s fb rects enc zs) (update-snapshot fb snap rects))
                     (and rects t))))))))))

(defun rfb-sender-loop (client fb s)
  "Fulfil the client's pending request as soon as the fb has something to show;
   poll at ~60 Hz.  Runs in its own thread; exits when the client stops running."
  (loop while (rc-running client) do
    (let ((req (sb-thread:with-mutex ((rc-lock client)) (rc-want client))))
      (if (null req)
          (sleep 1/60)
          (let ((sent (handler-case (send-update client fb s req)
                        (error () (setf (rc-running client) nil) nil))))
            (if sent
                (sb-thread:with-mutex ((rc-lock client))
                  (when (eq (rc-want client) req) (setf (rc-want client) nil)))
                (sleep 1/60)))))))          ; nothing dirty yet — keep REQ, poll again

(defun choose-encoding (encs)
  "Pick the best encoding we implement from the client's advertised list.  ZRLE
   (lossless, zlib-compressed) is preferred, then Hextile, then Raw."
  (cond ((member +enc-zrle+ encs) +enc-zrle+)
        ((member +enc-hextile+ encs) +enc-hextile+)
        (t +enc-raw+)))

(defun client-loop (fb s on-key on-pointer on-resize)
  "Read RFB client messages, handling input (key/pointer) IMMEDIATELY; framebuffer
   updates are produced by a companion sender thread, so input is never blocked on
   a frame.  Only the sender writes pixels; this thread never writes to S."
  (let* ((client (make-rfb-client :last-size (cons (fb-width fb) (fb-height fb))))
         (sender (sb-thread:make-thread (lambda () (rfb-sender-loop client fb s))
                                        :name "glass-sender")))
    (unwind-protect
         (loop
           (let ((msg (read-byte s nil :eof)))
             (case msg
               (0 (skip s 3) (r-bytes s 16))               ; SetPixelFormat (we keep ours)
               (2 (skip s 1)                               ; SetEncodings
                  (let ((n (r-u16 s)) (encs '()))
                    (dotimes (i n) (push (r-u32 s) encs))
                    (sb-thread:with-mutex ((rc-lock client))
                      (setf (rc-enc client) (choose-encoding encs)
                            (rc-dss client) (or (member +pseudo-desktop-size+ encs)
                                                (member +pseudo-extended-desktop-size+ encs))
                            (rc-cursor client) (and (member +pseudo-cursor+ encs) t)))))
               (3 (let ((inc (r-u8 s)) (x (r-u16 s)) (y (r-u16 s)) (w (r-u16 s)) (h (r-u16 s)))
                    (sb-thread:with-mutex ((rc-lock client))   ; park the latest request
                      (setf (rc-want client) (list inc x y w h)))))
               (4 (let ((down (r-u8 s)))                   ; KeyEvent
                    (skip s 2)
                    (let ((key (r-u32 s))) (when on-key (funcall on-key (plusp down) key)))))
               (5 (let ((buttons (r-u8 s)) (x (r-u16 s)) (y (r-u16 s)))   ; PointerEvent
                    (when on-pointer (funcall on-pointer buttons x y))))
               (6 (skip s 3) (let ((n (r-u32 s))) (r-bytes s n)))         ; ClientCutText
               (251 (skip s 1)                             ; SetDesktopSize (client wants a size)
                    (let ((rw (r-u16 s)) (rh (r-u16 s)) (nscreens (r-u8 s)))
                      (skip s 1)
                      (dotimes (i nscreens) (r-bytes s 16))   ; per-screen layout (ignored)
                      (when on-resize (funcall on-resize rw rh))))
               (t (return)))))                             ; :eof or unknown -> done
      (setf (rc-running client) nil)
      (ignore-errors (sb-thread:join-thread sender)))))

;;; ---- server -----------------------------------------------------------------

(defun serve (fb port &key on-key on-pointer on-resize (name *desktop-name*) once)
  "Serve framebuffer FB over RFB on PORT.  ON-KEY (down-p keysym), ON-POINTER
   (button-mask x y) and ON-RESIZE (requested-w requested-h, from the client
   resizing its window) are optional callbacks.  With :ONCE, handle a single
   client and return; otherwise loop, each client in its own thread."
  (let ((listen (tcp-listen port)))
    (format *error-output* "~&glass: RFB server listening on port ~d (~dx~d)~%"
            port (fb-width fb) (fb-height fb))
    (force-output *error-output*)
    (flet ((run (stream)
             (unwind-protect
                  (progn (handshake fb stream name)
                         (client-loop fb stream on-key on-pointer on-resize))
               (ignore-errors (close stream)))))
      (unwind-protect
           (loop
             (let ((stream (accept-stream listen)))
               (if once
                   (progn (run stream) (return))
                   (sb-thread:make-thread (lambda () (ignore-errors (run stream)))
                                          :name "glass-client"))))
        (ignore-errors (sb-bsd-sockets:socket-close listen))))))

(defun serve-one (fb port &rest args)
  "Serve exactly one client, then return (handy for tests)."
  (apply #'serve fb port :once t args))
