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

(defvar *desktop-name* "glass"
  "THE SESSION'S NAME — not a seat's, and not the box's.  Several seats can be watching
   one session; what they are watching is this, and it is what an RFB client puts in its
   title bar.

   \"glass\" is a placeholder and launchers are expected to replace it, typically with
   GLASS:WORD-NAME — a few BIP-39 words, which can be read off a screen, repeated down a
   phone and typed without spelling.  DELIBERATELY NOT (WORD-NAME) as the initform: a
   DEFVAR runs at load time, and in a saved core that is image-BUILD time, so every
   container started from it would come up with the same name baked in.  A name is
   something a session picks when it starts.")
(defparameter *tile* 32 "Dirty-tracking granularity (pixels).")

(defun string->bytes (s)
  (map '(simple-array (unsigned-byte 8) (*)) #'char-code s))

;;; ---- stream byte I/O (big-endian on the wire, per RFB) ----------------------

(declaim (inline w-u8 w-u16 w-u32 r-u8 r-u16 r-u32))
(defun w-u8  (s v) (tx+ 1) (write-byte (logand v #xff) s))
(defun w-u16 (s v) (w-u8 s (ash v -8)) (w-u8 s v))
(defun w-u32 (s v) (w-u16 s (ash v -16)) (w-u16 s v))
(defun w-bytes (s b) (tx+ (length b)) (write-sequence b s))
(defun r-u8  (s) (read-byte s))
(defun r-u16 (s) (logior (ash (read-byte s) 8) (read-byte s)))
(defun r-u32 (s) (logior (ash (r-u16 s) 16) (r-u16 s)))
(defun r-bytes (s n)
  (let ((b (make-array n :element-type '(unsigned-byte 8)))) (read-sequence b s) b))
(defun skip (s n) (dotimes (i n) (read-byte s)))

;;; ---- transport --------------------------------------------------------------
;;;
;;; TCP-LISTEN, CLOSE-LISTENER, ACCEPT-STREAM and SOCKET-UNSENT-BYTES moved to
;;; src/socket.lisp when a wire stopped being only a port: RFB is a stream protocol and
;;; has no opinion about what carries it, which is what makes a UNIX-domain listener a
;;; sibling of the TCP one rather than a case inside this file.  Nothing about the TCP
;;; path changed.

;;; ---- RFB pixel format + handshake ------------------------------------------

;; 16-byte PIXEL_FORMAT: bpp=32 depth=24 big-endian=0 true-colour=1,
;; {r,g,b}-max=255, r-shift=16 g-shift=8 b-shift=0.  Matches fb pixel 0x00RRGGBB.
(defun write-pixel-format (s)
  (w-u8 s 32) (w-u8 s 24) (w-u8 s 0) (w-u8 s 1)
  (w-u16 s 255) (w-u16 s 255) (w-u16 s 255)
  (w-u8 s 16) (w-u8 s 8) (w-u8 s 0)
  (w-u8 s 0) (w-u8 s 0) (w-u8 s 0))

;;; ---- client pixel-format conversion ----------------------------------------
;;; We advertise 32bpp 0xRRGGBB, but a client may SetPixelFormat to something
;;; smaller (mobile VNC — RealVNC/Remotix iOS — ask for 8bpp RGB222 to save
;;; bandwidth).  RFB then REQUIRES every pixel be sent in the client's format, so
;;; we convert.  NIL = the native fast path (client kept our format); a PXFMT here
;;; drives every pixel/CPIXEL write.  Per-channel LUTs (8-bit fb channel -> its
;;; shifted, range-scaled contribution) keep conversion a few array reads/pixel.
(defstruct (pxfmt (:constructor %make-pxfmt))
  (pbytes 4) (cbytes 3) (big-endian nil) (chi nil)     ; chi: 3-byte CPIXEL takes the HIGH 3 bytes
  rtab gtab btab)

(defun %scale-tab (max shift)
  (let ((tab (make-array 256 :element-type '(unsigned-byte 32))))
    (dotimes (c 256 tab) (setf (aref tab c) (ash (floor (+ (* c max) 127) 255) shift)))))

(defun parse-pxfmt (pf)
  "A 16-byte RFB PIXEL_FORMAT -> a PXFMT, or NIL for our native format (32bpp,
   little-endian, max 255, shift 16/8/0) or any non-true-colour request (we only
   serve true-colour)."
  (let ((bpp (aref pf 0)) (depth (aref pf 1)) (be (plusp (aref pf 2))) (tc (plusp (aref pf 3)))
        (rmax (logior (ash (aref pf 4) 8) (aref pf 5)))
        (gmax (logior (ash (aref pf 6) 8) (aref pf 7)))
        (bmax (logior (ash (aref pf 8) 8) (aref pf 9)))
        (rsh (aref pf 10)) (gsh (aref pf 11)) (bsh (aref pf 12)))
    (cond
      ((not (and tc (member bpp '(8 16 32)))) nil)                    ; can't serve -> keep native
      ((and (= bpp 32) (not be) (= rmax 255) (= gmax 255) (= bmax 255)
            (= rsh 16) (= gsh 8) (= bsh 0)) nil)                      ; exactly native
      (t (let* ((pbytes (ash bpp -3))
                (hi (max (+ rsh (integer-length rmax)) (+ gsh (integer-length gmax)) (+ bsh (integer-length bmax))))
                (lo (min rsh gsh bsh))
                (fits-low (<= hi 24)) (fits-high (>= lo 8))
                (cbytes (if (and (= bpp 32) (<= depth 24) (or fits-low fits-high)) 3 pbytes)))
           (%make-pxfmt :pbytes pbytes :cbytes cbytes :big-endian be
                        :chi (and (= cbytes 3) (not fits-low) fits-high)
                        :rtab (%scale-tab rmax rsh) :gtab (%scale-tab gmax gsh) :btab (%scale-tab bmax bsh)))))))

(declaim (inline pxval put-px put-cpix))
(defun pxval (p fmt)                          ; fb 0xRRGGBB -> client pixel value
  (logior (aref (the (simple-array (unsigned-byte 32) (256)) (pxfmt-rtab fmt)) (logand (ash p -16) #xff))
          (aref (the (simple-array (unsigned-byte 32) (256)) (pxfmt-gtab fmt)) (logand (ash p -8) #xff))
          (aref (the (simple-array (unsigned-byte 32) (256)) (pxfmt-btab fmt)) (logand p #xff))))
(defun %put-bytes (buf i v n be)              ; write V as N bytes, LE or BE; return i+n
  (if be (dotimes (k n) (setf (aref buf (+ i k)) (logand (ash v (* -8 (- n 1 k))) #xff)))
         (dotimes (k n) (setf (aref buf (+ i k)) (logand (ash v (* -8 k)) #xff))))
  (+ i n))
(defun put-px (buf i p fmt)                   ; full pixel (Raw/Hextile): PBYTES bytes
  (%put-bytes buf i (pxval p fmt) (pxfmt-pbytes fmt) (pxfmt-big-endian fmt)))
(defun put-cpix (buf i p fmt)                 ; CPIXEL (ZRLE/TRLE): CBYTES bytes
  (let ((v (pxval p fmt)) (cb (pxfmt-cbytes fmt)))
    (if (= cb (pxfmt-pbytes fmt))
        (%put-bytes buf i v cb (pxfmt-big-endian fmt))                 ; full-size CPIXEL
        (let ((base (if (pxfmt-chi fmt) 8 0)) (be (pxfmt-big-endian fmt)))   ; 32bpp -> drop unused byte
          (if be (dotimes (k 3) (setf (aref buf (+ i k)) (logand (ash v (- (+ base (* 8 (- 2 k))))) #xff)))
                 (dotimes (k 3) (setf (aref buf (+ i k)) (logand (ash v (- (+ base (* 8 k)))) #xff))))
          (+ i 3)))))
(defun w-pixel (s px fmt)                      ; write ONE full pixel straight to the stream
  (if fmt
      (let ((v (pxval px fmt)) (n (pxfmt-pbytes fmt)))
        (if (pxfmt-big-endian fmt)
            (loop for k from (1- n) downto 0 do (w-u8 s (logand (ash v (* -8 k)) #xff)))
            (dotimes (k n) (w-u8 s (logand (ash v (* -8 k)) #xff)))))
      (progn (w-u8 s (logand px #xff)) (w-u8 s (logand (ash px -8) #xff))
             (w-u8 s (logand (ash px -16) #xff)) (w-u8 s 0))))

(defun client-minor-version (ver)
  "Parse the RFB minor version from a 12-byte \"RFB 003.00X\" ProtocolVersion, or 8."
  (or (ignore-errors (parse-integer (map 'string #'code-char ver) :start 8 :end 11)) 8))

(defparameter *legacy-vnc-auth* t
  "For RFB 3.3 clients (macOS Screen Sharing), offer VNC Authentication (security
   type 2) instead of None: macOS refuses/​spins on a None-auth 3.3 server, and only
   its VNC-auth path (a password prompt) proceeds.  With *VNC-PASSWORD* NIL the
   challenge/response completes but the password is NOT verified (any password is
   accepted — the open posture of None, just the form macOS demands).  NIL here
   dictates None for 3.3 instead.")

(defvar *vnc-password* nil
  "The SESSION-WIDE credential, and the one every transport that did not name its own
   inherits.  If a string, VNC Authentication is REQUIRED for those clients and the DES
   challenge/response is VERIFIED against it — a wrong password is rejected, so this
   secures a desktop bound to 0.0.0.0 (and macOS saves the password to its Keychain,
   so it stops prompting).  NIL = the open posture (None for 3.7+, any-password VNC
   auth for 3.3/macOS).  Set it live: (setf glass:*vnc-password* \"...\") — a running
   listener picks that up, because an inherited credential is read per HANDSHAKE and
   not once at SERVE.

   IT IS NO LONGER THE ONLY PLACE A CREDENTIAL CAN LIVE, and that is what makes a
   password stop breaking video: SERVE takes :PASSWORD, so a seat can demand one on the
   wire strangers can reach and demand nothing on the socket file only its owner can
   open — which is where the VP8 capture lives.  Setting this variable applies to every
   inheriting transport at once, which is exactly the conflation the per-transport
   argument exists to undo.")

(defvar *vnc-verify-fn* nil
  "Installed by the optional :glass/vncauth system: (password challenge response) ->
   bool, the DES verify via seal.  Kept a HOOK so core :glass carries no crypto
   dependency — with a password set but this NIL, connections FAIL CLOSED.")

(defun vnc-auth-available-p ()
  "Can this image actually VERIFY a VNC password?  T iff :glass/vncauth is loaded.
   Worth asking BEFORE opening a listener with a credential: without the verifier a
   password does not secure the wire, it closes it — every client is rejected — so a
   caller generating a credential on somebody's behalf should ask first and say so."
  (and *vnc-verify-fn* t))

(defparameter +vnc-credential-alphabet+
  "abcdefghijkmnpqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ23456789"
  "The alphabet MAKE-VNC-CREDENTIAL draws from: the unambiguous ASCII — no l/I/1, no
   o/O/0 — because this password is READ OFF A SCREEN and typed into a viewer by hand.")

(defun make-vnc-credential (&optional (length 8))
  "A fresh VNC password, LENGTH characters from /dev/urandom.

   EIGHT, and not more, because the RFB VNC-auth key IS eight bytes: the protocol takes
   the first 8 characters of the password, zero-pads, and reverses each byte's bits to
   make a DES key (see src/vncauth.lisp).  A 20-character password would be a 20-
   character password on the screen and the same 8 bytes on the wire, so the extra
   twelve are theatre — the honest thing is to generate the length that is actually
   used.  Eight characters of this alphabet is ~46 bits, drawn from the kernel's CSPRNG
   and not from CL:RANDOM, whose state is not a secret.

   That said: VNC authentication is DES, the challenge/response is offline-crackable by
   anyone who watches one handshake, and EVERYTHING AFTER IT IS PLAINTEXT.  This is a
   credential that keeps the internet's scanners out of a desktop, not a channel that
   keeps a network attacker out of one.  For that there is the UNIX transport with an
   SSH tunnel, or the WebRTC gateway, both of which encrypt."
  (let ((n (length +vnc-credential-alphabet+)))
    (with-open-file (in "/dev/urandom" :element-type '(unsigned-byte 8))
      (let ((out (make-string length)))
        (dotimes (i length out)
          ;; Rejection sampling: 256 is not a multiple of 56, so a bare MOD would deal
          ;; the first 32 characters five chances each and the other 24 only four.
          (setf (char out i)
                (char +vnc-credential-alphabet+
                      (loop for b = (read-byte in)
                            until (< b (* n (floor 256 n)))
                            finally (return (mod b n))))))))))

(defun effective-vnc-password (password)
  "The credential a handshake should demand, given a transport's PASSWORD setting.

   :INHERIT — the default everywhere — means *VNC-PASSWORD*, read HERE, at handshake
   time, so that setting the session-wide password live still reaches a listener that
   is already running (which is what its docstring has always promised).  A string is
   this wire's own credential; NIL is this wire demanding nothing, whatever the session
   says, which is how the UNIX transport stays open to the capture client that speaks
   security type None."
  (if (eq password :inherit) *vnc-password* password))

(defun vnc-auth-exchange (s &optional (password :inherit))
  "VNC-auth challenge/response over S.  Returns T to admit the client: with no
   password, any response is accepted; with one, the DES verifier (*vnc-verify-fn*,
   from :glass/vncauth) must confirm it — and if that verifier is absent, the client is
   rejected (fail closed)."
  (let ((password (effective-vnc-password password))
        (challenge (make-array 16 :element-type '(unsigned-byte 8))))
    (dotimes (i 16) (setf (aref challenge i) (random 256)))
    (w-bytes s challenge) (force-output s)
    (let ((response (r-bytes s 16)))
      (cond ((null password) t)                                        ; open: accept any
            (*vnc-verify-fn* (funcall *vnc-verify-fn* password challenge response))
            (t nil)))))                                                ; secured, no verifier -> reject

(defun vnc-auth-and-result (s minor &optional (password :inherit))
  "Run VNC auth, send the RFB SecurityResult (0 OK / 1 failed, + a reason string on
   3.8), and return whether the client is admitted."
  (let ((ok (vnc-auth-exchange s password)))
    (w-u32 s (if ok 0 1)) (force-output s)
    (when (and (not ok) (>= minor 8))
      (let ((reason (string->bytes "Authentication failed")))
        (w-u32 s (length reason)) (w-bytes s reason) (force-output s)))
    ok))

(defun handshake (fb s name &optional (password :inherit))
  "RFB handshake through ServerInit, honoring the client's protocol version.  We
   advertise 3.8, but macOS Screen Sharing (and other legacy clients) answer 3.3,
   whose security phase is INCOMPATIBLE with 3.7+: 3.3 wants a single u32 security
   type from the server (client doesn't choose, no SecurityResult), while 3.7+ wants
   a type LIST + the client's choice + a SecurityResult.  Getting this wrong is a
   protocol deadlock (a spinning connection that never opens).  Returns T on success.

   PASSWORD is THIS WIRE's credential (see EFFECTIVE-VNC-PASSWORD): :INHERIT for the
   session's, a string for its own, NIL for none.  It decides which security type is
   offered at all, so a client of an unauthenticated transport is offered None and a
   client of an authenticated one is not — one desktop, two answers, which is the whole
   of why a password on the TCP wire no longer breaks the capture on the UNIX one."
  (let ((password (effective-vnc-password password)))
    (w-bytes s (string->bytes "RFB 003.008")) (w-u8 s 10) (force-output s)
    (let ((minor (client-minor-version (r-bytes s 12))))   ; client ProtocolVersion
      (if (>= minor 7)
          ;; 3.7 / 3.8: a security-type LIST, the client's choice, then the exchange
          (if password                         ; secured -> require VNC auth (type 2)
              (progn (w-u8 s 1) (w-u8 s 2) (force-output s)
                     (r-u8 s)                      ; client's chosen type (2)
                     (unless (vnc-auth-and-result s minor password)
                       (return-from handshake nil)))
              (progn (w-u8 s 1) (w-u8 s 1) (force-output s)    ; None (1)
                     (r-u8 s) (w-u32 s 0) (force-output s)))   ; SecurityResult = OK
          ;; 3.3: the server dictates ONE u32 security type
          (if (or password *legacy-vnc-auth*)
              (progn (w-u32 s 2) (force-output s)              ; VNC auth (verified if PASSWORD)
                     (unless (vnc-auth-and-result s minor password)
                       (return-from handshake nil)))
              (progn (w-u32 s 1) (force-output s))))))         ; None -> straight to ClientInit
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

(defun dirty-rects (fb snap &optional region)
  "Coalesced (x y w h) rectangles where FB differs from SNAP: changed tiles on a
   *TILE* grid, merged into horizontal runs per tile-row.  REGION ((x0 y0 x1 y1))
   confines the scan to tiles overlapping that box — so when the compositor tells
   us what it changed, we don't re-scan the whole screen."
  (let* ((fw (fb-width fb)) (fh (fb-height fb)) (ts *tile*) (rects '())
         (rx0 (if region (max 0 (min fw (first region)))  0))
         (ry0 (if region (max 0 (min fh (second region))) 0))
         (rx1 (if region (max 0 (min fw (third region)))  fw))
         (ry1 (if region (max 0 (min fh (fourth region))) fh)))
    (loop for ty from (* ts (floor ry0 ts)) below ry1 by ts for y1 = (min fh (+ ty ts)) do
      (let ((run -1) (lastx1 0))
        (loop for tx from (* ts (floor rx0 ts)) below rx1 by ts for x1 = (min fw (+ tx ts)) do
          (setf lastx1 x1)
          (if (tile-changed-p fb snap tx ty x1 y1)
              (when (< run 0) (setf run tx))
              (when (>= run 0) (push (list run ty (- tx run) (- y1 ty)) rects) (setf run -1))))
        (when (>= run 0) (push (list run ty (- lastx1 run) (- y1 ty)) rects))))
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
(defconstant +enc-copyrect+ 1)  ; "copy this rect from elsewhere in the fb" — no pixel data
(defconstant +enc-hextile+ 5)
(defconstant +enc-trle+ 15)     ; ZRLE's tiles WITHOUT zlib (no serial deflate) — encoder in zrle.lisp
(defconstant +enc-zrle+ 16)     ; encoder in zrle.lisp (loaded after this file)
(defparameter *trle-threshold* 16384
  "Rects with at least this many pixels go out as TRLE (no zlib — ~200x cheaper to
   encode than ZRLE, only a little larger; the deflate cost isn't worth it on a
   fast link).  Smaller rects stay ZRLE (compression is ~free at that size).")
(defparameter *zrle-stored-threshold* 0
  "ZRLE rects at least this many pixels use STORED deflate blocks (no LZ77) instead
   of a full deflate.  Default 0 = ALWAYS stored, because deflate's per-byte LZ77 is
   the encode wall (measured: a calculator repaint = ~10 tile-row strips of ~11.5k px
   each, all just under the old 16384 threshold, so all on the slow path -> 73 ms/
   frame; at 0 -> 5.5 ms, a 13x win) and ZRLE's tile packing already compresses UI
   content (solid/palette tiles) WITHOUT deflate, so on a fast link stored costs a
   little bandwidth for a lot of CPU.  Raise it (e.g. 16384, or higher) to favour
   bandwidth over encode CPU on a slow/metered link, where deflating big rects pays.")

(defun write-rect-copy (s dx dy w h sx sy)
  "A CopyRect rect: the client copies its own W x H pixels from (SX,SY) to (DX,DY)."
  (w-u16 s dx) (w-u16 s dy) (w-u16 s w) (w-u16 s h) (w-u32 s +enc-copyrect+)
  (w-u16 s sx) (w-u16 s sy))

(defun copy-in-bounds-p (copy fb)
  "True when both the source and destination of COPY lie fully within FB (so a
   CopyRect references only pixels the client actually has)."
  (destructuring-bind (sx sy dx dy w h) copy
    (let ((fw (fb-width fb)) (fh (fb-height fb)))
      (and (<= 0 sx) (<= 0 sy) (<= (+ sx w) fw) (<= (+ sy h) fh)
           (<= 0 dx) (<= 0 dy) (<= (+ dx w) fw) (<= (+ dy h) fh)))))

(defun snapshot-move (snap fbw sx sy dx dy w h)
  "Apply a window move to the client's snapshot: copy the W x H block at (SX,SY)
   to (DX,DY) (via a temp, so overlapping src/dst is safe) — so the subsequent
   diff only re-sends the EXPOSED area, not the moved (CopyRect'd) window."
  (let ((tmp (make-array (* w h) :element-type '(unsigned-byte 32))))
    (dotimes (yy h)
      (let ((src (+ (* (+ sy yy) fbw) sx)))
        (replace tmp snap :start1 (* yy w) :start2 src :end2 (+ src w))))
    (dotimes (yy h)
      (let ((dst (+ (* (+ dy yy) fbw) dx)))
        (replace snap tmp :start1 dst :end1 (+ dst w) :start2 (* yy w))))))

(defun snapshot-scroll (snap fbw sy dy h)
  "SNAPSHOT-MOVE's whole-row case: move H FULL rows from row SY to row DY.  Full rows
   are contiguous runs of the flat pixel array, so ONE REPLACE does it — and REPLACE on
   a single sequence is defined to behave as if the source were copied aside first, so
   the overlap a scroll always has is handled without the temp (no allocation, no GC).
   This is the difference between a scroll's CopyRect paying for itself and not: measured
   ~0.1 ms here against 3.8-18 ms for the general routine on a 1280x800 screen, where the
   whole-screen encode it saves is ~29 ms."
  (declare (type (simple-array (unsigned-byte 32) (*)) snap)
           (type fixnum fbw sy dy h)
           (optimize (speed 3) (safety 0)))
  (let ((src (* sy fbw)) (dst (* dy fbw)) (len (* h fbw)))
    (declare (fixnum src dst len))
    (replace snap snap :start1 dst :end1 (+ dst len) :start2 src :end2 (+ src len))))

(defun apply-snapshot-copy (snap fbw sx sy dx dy w h)
  "Apply COPY's move to the client's snapshot, taking the flat whole-row path when the
   block spans the full framebuffer width at x=0 (a SCROLL) and the general blocked one
   otherwise (a WM window MOVE)."
  (if (and (= sx 0) (= dx 0) (= w fbw))
      (snapshot-scroll snap fbw sy dy h)
      (snapshot-move snap fbw sx sy dx dy w h)))

(defun write-rect-raw (s fb x y w h &optional fmt)
  (w-u16 s x) (w-u16 s y) (w-u16 s w) (w-u16 s h) (w-u32 s +enc-raw+)
  (let* ((px (fb-pixels fb)) (fw (fb-width fb)) (pb (if fmt (pxfmt-pbytes fmt) 4))
         (buf (make-array (* w h pb) :element-type '(unsigned-byte 8))) (o 0))
    (loop for yy from y below (+ y h) for row = (* yy fw) do
      (loop for xx from x below (+ x w) for p = (aref px (+ row xx)) do
        (if fmt
            (setf o (put-px buf o p fmt))
            (progn (setf (aref buf o)       (logand p #xff)             ; B (little-endian)
                         (aref buf (+ o 1)) (logand (ash p -8) #xff)    ; G
                         (aref buf (+ o 2)) (logand (ash p -16) #xff)   ; R
                         (aref buf (+ o 3)) 0)                           ; X
                   (incf o 4)))))
    (w-bytes s buf)))

;;; ---- Hextile encoding (RFC 6143 §7.7.4) ------------------------------------
;;; Each 16x16 tile: a solid tile costs a byte (or a byte + colour); a tile of a
;;; few colours is a background plus coloured sub-rectangles over it; a busy tile
;;; falls back to raw.  Lossless (sharp text, full colour), no zlib — great for
;;; desktop UI where most tiles are solid or near-solid.  Background persists
;;; across tiles, so runs of the same colour cost one byte each.

(defun %push-pixel (buf p &optional fmt)
  (if fmt
      (let ((v (pxval p fmt)) (n (pxfmt-pbytes fmt)) (be (pxfmt-big-endian fmt)))
        (if be (loop for k from (1- n) downto 0 do (vector-push-extend (logand (ash v (* -8 k)) #xff) buf))
               (dotimes (k n) (vector-push-extend (logand (ash v (* -8 k)) #xff) buf))))
      (progn (vector-push-extend (logand p #xff) buf)
             (vector-push-extend (logand (ash p -8) #xff) buf)
             (vector-push-extend (logand (ash p -16) #xff) buf)
             (vector-push-extend 0 buf))))

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

(defun write-rect-hextile (s fb x y w h &optional fmt)
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
                   (progn (vector-push-extend 2 buf) (%push-pixel buf bg fmt) (setf cur-bg bg))))
              (t
               (let* ((subs (tile-subrects px fw ax ay tw th bg))
                      (nsub (length subs)))
                 (if (and (<= nsub 255) (< (* nsub 6) (* tw th 4)))   ; hextile beats raw?
                     (let ((mask (logior 8 16)))            ; AnySubrects | SubrectsColoured
                       (unless (= bg cur-bg) (setf mask (logior mask 2)))
                       (vector-push-extend mask buf)
                       (unless (= bg cur-bg) (%push-pixel buf bg fmt) (setf cur-bg bg))
                       (vector-push-extend nsub buf)
                       (dolist (sr subs)
                         (destructuring-bind (c lx ly len) sr
                           (%push-pixel buf c fmt)
                           (vector-push-extend (logior (ash lx 4) ly) buf)
                           (vector-push-extend (logior (ash (1- len) 4) 0) buf))))
                     (progn                                 ; raw tile
                       (vector-push-extend 1 buf)           ; mask 1 = Raw
                       (setf cur-bg -1)
                       (dotimes (ly th)
                         (let ((row (* (+ ay ly) fw)))
                           (dotimes (lx tw) (%push-pixel buf (aref px (+ row ax lx)) fmt)))))))))))))
    (w-bytes s buf)))

;;; ---- update assembly --------------------------------------------------------

(defun write-rect (s fb x y w h enc zs &optional fmt)
  (cond
    ((= enc +enc-trle+)    (write-rect-trle s fb x y w h fmt))
    ((= enc +enc-zrle+)    (write-rect-zrle s fb x y w h zs nil fmt))
    ((= enc +enc-hextile+) (write-rect-hextile s fb x y w h fmt))
    (t                     (write-rect-raw s fb x y w h fmt))))

(defparameter *use-trle* nil
  "Whether to upgrade big rects to TRLE (enc 15) for clients that advertise it.
   OFF by default: TRLE has no validation against a real client's decoder (only our
   own parser), and macOS Screen Sharing — which DOES negotiate TRLE — fails to
   render when we send it.  Stored-block ZRLE is chipz-verified, decoded by every
   ZRLE client, and nearly as fast, so it's the universal big-frame path.  Kept as
   a flag in case a specific client benefits and is known to decode our TRLE.")

(defun emit-rect (s fb x y w h enc zs trle &optional fmt)
  "Write one rect, choosing the cheapest encoding a large rect's client can take.
   Big ZRLE rect -> STORED-block ZRLE (~5x cheaper than full deflate, still ordinary
   ZRLE, so every client decodes it); TRLE only if explicitly enabled AND negotiated.
   Small rects take normal ENC (cheap because small, best ratio)."
  (cond
    ((and *use-trle* trle (= enc +enc-zrle+) (>= (* w h) *trle-threshold*))
     (write-rect-trle s fb x y w h fmt))                   ; opt-in TRLE (off by default)
    ((and (= enc +enc-zrle+) (>= (* w h) *zrle-stored-threshold*))
     (write-rect-zrle s fb x y w h zs t fmt))              ; big ZRLE: stored fast path (universal)
    (t (write-rect s fb x y w h enc zs fmt))))

(defparameter *max-band-rows* 64
  "Cap a single rectangle at this many rows: a tall rect is split into horizontal
   bands, each its own rect in the same FramebufferUpdate.  A whole-screen refresh
   as ONE 1280x800 ZRLE rect is a big incompressible unit some progressive clients
   (RealVNC iOS, which itself requests ~13-row strips) reject; banding keeps each
   rect's decode buffer bounded and pipelines the paint.  NIL disables banding.")

(defun band-rects (rects)
  "Split any rect taller than *MAX-BAND-ROWS* into horizontal bands (row order)."
  (if (null *max-band-rows*)
      rects
      (loop for (x y w h) in rects
            if (<= h *max-band-rows*) collect (list x y w h)
            else nconc (loop for yy from y below (+ y h) by *max-band-rows*
                             collect (list x yy w (min *max-band-rows* (- (+ y h) yy)))))))

(defun send-rects (s fb rects enc zs &optional trle fmt cursor)
  "One FramebufferUpdate carrying RECTS (tall ones banded).  ENC is the client's
   chosen encoding; ZS its persistent ZRLE zlib stream; TRLE whether it also
   accepts TRLE (used for big rects); FMT the client's pixel format (NIL = native).
   CURSOR non-NIL prepends the cursor pseudo-rect to THIS update (one update per
   request — never a separate cursor message)."
  (let ((rects (band-rects rects)))
    (w-u8 s 0) (w-u8 s 0) (w-u16 s (+ (if cursor 1 0) (length rects)))   ; msg-type, pad, #rects
    (when cursor (emit-cursor-rect s fmt))
    (dolist (r rects) (destructuring-bind (x y w h) r (emit-rect s fb x y w h enc zs trle fmt)))
    (force-output s)))

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

(defun emit-cursor-rect (s &optional fmt (rows *cursor-arrow*))
  "Write the cursor shape as ONE Cursor pseudo-rect (hotspot 0,0) — just the rect,
   NO FramebufferUpdate header — so the caller folds it into a frame update as an
   extra leading rect.  (A separate unsolicited FramebufferUpdate for the cursor
   throws off clients that map responses to their requests by order — RealVNC:
   the extra update shifts its request/format correlation by one, so a later
   SetPixelFormat lands on the wrong update and it desyncs.)  Pixels in FMT."
  (let ((w (reduce #'max rows :key #'length)) (h (length rows)))
    (w-u16 s 0) (w-u16 s 0) (w-u16 s w) (w-u16 s h) (w-u32 s +pseudo-cursor+)
    (loop for row in rows do                                 ; pixels: w*h in the client pixel format
      (dotimes (x w)
        (let ((c (if (< x (length row)) (char row x) #\.)))
          (w-pixel s (case c (#\o 0) (#\x #xffffff) (t 0)) fmt))))
    (loop for row in rows do                                 ; 1-bpp mask, MSB first, row-padded
      (let ((acc 0) (nb 0))
        (dotimes (x w)
          (let ((opaque (and (< x (length row)) (member (char row x) '(#\o #\x)))))
            (setf acc (logior (ash acc 1) (if opaque 1 0)) nb (1+ nb))
            (when (= nb 8) (w-u8 s acc) (setf acc 0 nb 0))))
        (when (plusp nb) (w-u8 s (ash acc (- 8 nb))))))))

;;; ---- the selection (ClientCutText / ServerCutText) --------------------------
;;;
;;; RFB carries the clipboard in two messages that are each a length and some Latin-1 bytes.
;;; Neither of them is where the clipboard LIVES — that is SESSION-CLIPBOARD, beside the
;;; framebuffer, for the same reason the audio mixer is (see clipboard.lisp).  This is only the
;;; conversion, and it is deliberately small.

(defun %read-discard (s n)
  "Consume N bytes and throw them away, in bounded memory.  A length that arrives from the
network decides how much we READ; it must not decide how much we ALLOCATE."
  (let ((buf (make-array (min (max n 1) 4096) :element-type '(unsigned-byte 8))))
    (loop while (plusp n)
          do (let ((k (min n (length buf))))
               (read-sequence buf s :end k)
               (decf n k)))))

(defun read-client-cut-text (client s &optional (clipboard (session-clipboard)))
  "ClientCutText (RFC 6143 §7.5.6): a 32-bit length, then that many Latin-1 bytes, which become
CLIPBOARD's selection with this client as its owner.  CLIPBOARD defaults to the session's, which
is what a one-seat desktop has; a multi-seat one hands each seat's transports that SEAT's
clipboard, so two people copying do not clobber each other.

The length is SIGNED, and a NEGATIVE one is not a length: it is the extended-clipboard
pseudo-encoding (-1063) re-using message type 6, with |len| bytes of a capability-tagged,
zlib-compressed payload — the route by which UTF-8 clipboard text travels.  We do not advertise
-1063, so a well-behaved client never sends it, but reading that length as unsigned would try to
allocate close to 4 GB on the strength of four bytes from the network.  Consume it and carry on;
implementing it is the follow-on that makes this clipboard UTF-8 instead of Latin-1."
  (let* ((raw (r-u32 s))
         (len (if (logbitp 31 raw) (- raw (ash 1 32)) raw)))
    (cond
      ((minusp len) (%read-discard s (- len)) nil)             ; extended clipboard: not ours yet
      ((> len *max-cut-text*)                                  ; keep the head, drop the rest
       (let ((keep (r-bytes s *max-cut-text*)))
         (%read-discard s (- len *max-cut-text*))
         (format *trace-output* "~&glass: ClientCutText ~d bytes truncated to ~d~%"
                 len *max-cut-text*)
         (force-output *trace-output*)
         (clipboard-own clipboard client :text (latin1-string keep) :name "vnc client")))
      (t (clipboard-own clipboard client
                        :text (if (plusp len) (latin1-string (r-bytes s len)) "")
                        :name "vnc client")))))

(defun send-cut-text (s text)
  "ServerCutText (RFC 6143 §7.6.4): type 3, three bytes of padding, a 32-bit length, then that
many Latin-1 bytes.

Sent as its OWN message, unlike the cursor shape — and the difference is worth stating, because
the RealVNC failure that forced EMIT-CURSOR-RECT's discipline looks at first like the same
hazard.  The cursor is a pseudo-ENCODING: it is a RECT, so it can only travel inside a
FramebufferUpdate, and an extra unsolicited FramebufferUpdate is exactly what shifted RealVNC's
request<->response correlation by one.  ServerCutText is a message TYPE of its own; a client
demultiplexes it on the type byte and it answers no FramebufferUpdateRequest, so it cannot
perturb that correlation.

What it does share is the single-writer rule: only the sender thread writes to this socket, so
this is called from the sender loop BETWEEN updates and never from the reader thread — otherwise
its bytes would land in the middle of a rect."
  (let ((bytes (latin1-bytes text)))
    (w-u8 s 3) (w-u8 s 0) (w-u8 s 0) (w-u8 s 0)
    (w-u32 s (length bytes))
    (w-bytes s bytes)
    (force-output s)))

;;; ---- client message loop ----------------------------------------------------

;;; A per-client sender runs in its OWN thread so that reading input (key/pointer)
;;; never blocks on producing a framebuffer update — the single-threaded design
;;; used to park in the update wait for up to ~5s, stalling input AND frames to
;;; that cadence.  The reader thread only stashes the latest FramebufferUpdate-
;;; Request in WANT; the sender fulfils it the moment the fb has something to show
;;; (polling ~60 Hz).  ONLY the sender writes pixels to the socket.
(defstruct (rfb-client (:conc-name rc-))
  (enc +enc-raw+) dss cursor cursor-sent copyrect trle
  (fmt nil)                     ; client pixel format (a PXFMT), or NIL = our native 32bpp
  (snap-box (list nil)) (zs (cram:make-zstream))
  last-size                     ; (cons w h) — fb size last announced to this client
  (want nil)                    ; latest pending request (inc x y w h), or NIL
  (last-gen -1)                 ; fb generation this client has already caught up to
  (last-frame -1)               ; fb composite-frame this client has already caught up to
  (cut-serial 0)                ; clipboard serial this client has already been told about
  (shift nil)                   ; Shift latched, for the paste chord (a modifier is its own event)
  (running t)
  (lock (sb-thread:make-mutex :name "rfb-client")))

(defun send-update (client fb s req region copy)
  "Fulfil one FramebufferUpdateRequest REQ = (inc x y w h).  REGION ((x0 y0 x1 y1)
   or :FULL) confines an incremental diff to what the compositor just changed.
   COPY = (sx sy dx dy w h) means a window moved: emit a CopyRect + only the
   exposed area, instead of re-encoding the moved pixels.  Returns T if bytes
   were written, NIL if there was nothing to send."
  (destructuring-bind (inc x y w h) req
    (let ((ls (rc-last-size client)))                                ; resize takes priority
      (when (rc-dss client)
        (with-fb-locked (fb)
          (when (or (/= (fb-width fb) (car ls)) (/= (fb-height fb) (cdr ls)))
            (send-desktop-size s (fb-width fb) (fb-height fb))
            (setf (car ls) (fb-width fb) (cdr ls) (fb-height fb) (car (rc-snap-box client)) nil)
            (return-from send-update t)))))
    (let ((snap-box (rc-snap-box client)) (enc (rc-enc client)) (zs (rc-zs client))
          (trle (rc-trle client)) (fmt (rc-fmt client))
          ;; cursor shape rides ALONG with a frame update (as its leading rect), never
          ;; as its own message — see emit-cursor-rect.
          (cur (and (rc-cursor client) (not (rc-cursor-sent client)))))
      (flet ((cursor-done () (when cur (setf (rc-cursor-sent client) t))))
        (if (or (zerop inc) (null (car snap-box)))                   ; full / first frame
            (with-fb-locked (fb)
              (send-rects s fb (list (clip-rect fb x y w h)) enc zs trle fmt cur)
              (cursor-done)
              (setf (car snap-box) (copy-pixels fb))
              t)
            (with-fb-locked (fb)                                     ; incremental: dirty tiles
              (let ((snap (car snap-box)))
                (cond
                  ((not (snap-matches-p snap fb)) (setf (car snap-box) nil) nil)  ; resized; resync
                  ;; a window MOVE and the client can CopyRect: copy the moved block
                  ;; in the snapshot, then diff only leaves the EXPOSED area to send.
                  ((and copy (rc-copyrect client) (copy-in-bounds-p copy fb))
                   (destructuring-bind (sx sy dx dy w h) copy
                     (apply-snapshot-copy snap (fb-width fb) sx sy dx dy w h)
                     (let ((rects (band-rects (dirty-rects fb snap (and (consp region) region)))))
                       (w-u8 s 0) (w-u8 s 0) (w-u16 s (+ (if cur 1 0) 1 (length rects)))  ; cursor? + CopyRect + exposed
                       (when cur (emit-cursor-rect s fmt))
                       (write-rect-copy s dx dy w h sx sy)
                       (dolist (r rects) (destructuring-bind (rx ry rw rh) r (emit-rect s fb rx ry rw rh enc zs trle fmt)))
                       (force-output s)
                       (cursor-done)
                       (update-snapshot fb snap rects)
                       t)))
                  (t (let ((rects (dirty-rects fb snap (and (consp region) region))))
                       (when (or rects cur)                          ; send if there's dirt OR a pending cursor
                         (send-rects s fb (or rects '()) enc zs trle fmt cur)
                         (cursor-done)
                         (when rects (update-snapshot fb snap rects)))
                       (and (or rects cur) t)))))))))))

;;; A WAKE lets the compositor and the reader thread nudge a parked sender the
;;; instant there's something to send, instead of it polling every ~16ms — the
;;; single biggest interactive-latency win.  It's a plain condvar; the 1/60
;;; timeout is only a safety net against a missed signal (so worst case = the old
;;; poll, best case = immediate).
(defstruct (wake (:constructor make-wake ()))
  (cv (sb-thread:make-waitqueue :name "glass-wake"))
  (lock (sb-thread:make-mutex :name "glass-wake")))

(defun wake-signal (w)
  (when w (sb-thread:with-mutex ((wake-lock w)) (sb-thread:condition-broadcast (wake-cv w)))))

(defun wake-wait (w timeout)
  (if w (sb-thread:with-mutex ((wake-lock w)) (sb-thread:condition-wait (wake-cv w) (wake-lock w) :timeout timeout))
      (sleep timeout)))

(defun send-pending-cut-text (client s &optional (clipboard (session-clipboard)))
  "Tell this client about ITS SEAT's selection if it has changed since it last heard.

   Runs at the top of the sender loop — on the ONE thread that writes to this socket, between
   framebuffer updates, never inside one.  A client is never sent its own cut text back: it
   already has it, and echoing it is how two viewers of the same session end up handing one
   string back and forth forever."
  (let ((cb clipboard))
    ;; The gate is an unlocked read of one fixnum, because this runs ~60 times a second per
    ;; client and almost always has nothing to do: taking the lock and materializing the text
    ;; every tick would make the common case (nobody copied anything) the expensive one, and
    ;; would call a provider thunk sixty times a second for no reader.  Losing the race costs
    ;; one 1/60 tick of latency, and the listener wakes us anyway.
    (when (> (clipboard-serial cb) (rc-cut-serial client))
      (multiple-value-bind (text serial owner) (clipboard-text cb)
        (setf (rc-cut-serial client) serial)             ; seen is seen, even when we don't send
        (when (and text (not (eq owner client)))
          (handler-case (progn (send-cut-text s text) t)
            (error () (setf (rc-running client) nil) nil)))))))

(defun rfb-sender-loop (client fb s wake &optional (clipboard (session-clipboard)))
  "Fulfil the client's pending request the moment the fb changes (parked on WAKE,
   ~60 Hz safety timeout).  Runs in its own thread; exits when the client stops."
  (let ((fd (ignore-errors (sb-sys:fd-stream-fd s))))    ; for the socket-queue backlog
  (loop while (rc-running client) do
    (send-pending-cut-text client s clipboard)
    (let ((req (sb-thread:with-mutex ((rc-lock client)) (rc-want client))))
      (cond
        ((null req) (wake-wait wake 1/60))
        ;; incremental request, but the fb hasn't changed since we last caught up:
        ;; nothing to do — skip the (whole-frame) diff entirely and park.
        ((and (plusp (first req)) (= (fb-generation fb) (rc-last-gen client)))
         (wake-wait wake 1/60))
        (t
         ;; TAKE AND DIFF ARE ONE STEP.  The frame triple and the pixels are two
         ;; different locks, and the COPY only describes the pixels as they stood at
         ;; the composite that marked it — so taking the copy and then reading the
         ;; pixels under a separate lock leaves a window in which another composite
         ;; can land.  When it does, the copy is one translation behind the pixels,
         ;; APPLY-SNAPSHOT-COPY moves the client's snapshot to the wrong place, and
         ;; the diff that follows finds the whole screen changed — the CopyRect saving
         ;; inverted into a full re-encode.  Holding the pixel lock across both closes
         ;; it: a composite either lands entirely before the take (and is in the copy)
         ;; or entirely after (and is in the NEXT take).  The lock is recursive, so
         ;; SEND-UPDATE's own WITH-FB-LOCKED is a no-op inside this one, and it already
         ;; held the lock for the whole diff/encode — this only adds the take.
         (let ((sent
                 (with-fb-locked (fb)
                   (let ((gen (fb-generation fb)) (incremental (plusp (first req))))
                     ;; Take everything the compositor accumulated since our last update: a unioned
                     ;; DAMAGE box and a COMPOSED COPY (one window move spanning however many frames we
                     ;; fell behind — "CopyRect farther").  An incremental request with a real damage
                     ;; box diffs only that and trusts the copy; a :FULL mark (or full request) is a
                     ;; whole-screen diff with no copy.
                     (multiple-value-bind (frame damage copy mark-time) (fb-take-frame fb)
                       (when (and damage (plusp mark-time)) (note-send-lag mark-time))  ; backlog clock
                       (let* ((region (if (and incremental (consp damage)) damage :full))
                              (copy   (and incremental (consp damage) copy))
                              (tx (list 0)) (t0 (get-internal-real-time))
                              (sent (let ((*tx* tx))
                                      (handler-case (send-update client fb s req region copy)
                                        (error () (setf (rc-running client) nil) nil)))))
                         (when (and sent *perf-on*)
                           (perf-record-send (- (get-internal-real-time) t0) region copy (car tx))
                           (when fd (note-send-queue (socket-unsent-bytes fd))))  ; real downstream backlog
                         (setf (rc-last-gen client) gen (rc-last-frame client) frame)
                         (when sent
                           (sb-thread:with-mutex ((rc-lock client))
                             (when (eq (rc-want client) req) (setf (rc-want client) nil))))
                         sent))))))
           ;; parking happens OFF the pixel lock — holding it here would stall every
           ;; compositor for the whole timeout
           (unless sent (wake-wait wake 1/60))))))))) ; nothing dirty (gen bumped, pixels same) — park

(defconstant +enc-zrle2+ 24
  "RealVNC's proprietary ZRLE2.  We don't implement it, but a client that lists it
   is RealVNC-family — and RealVNC's RLE decoder rejects our standard ZRLE tile
   stream with \"bad xrle data\" (it decodes encoding-16 rects through its ZRLE2
   path).  Its presence is our cue to serve Hextile instead.")

(defparameter *realvnc-encoding* +enc-hextile+
  "Encoding to serve a RealVNC-family client (one that advertises ZRLE2).  The real
   RealVNC bug was the cursor going out as its OWN FramebufferUpdate, which shifted
   RealVNC's response<->request (and thus pixel-format) mapping by one — that, not
   the encoding, produced both \"bad xrle data\" (ZRLE) and \"invalid message type\"
   (Hextile/Raw).  With the cursor folded into the frame update, Hextile (proven
   byte-exact, lossless, zlib-free) is the safe choice.  Live-tunable.")

(defun choose-encoding (encs)
  "Pick the best encoding we implement from the client's advertised list.  ZRLE
   (lossless, zlib-compressed) is preferred, then Hextile, then Raw — EXCEPT a
   RealVNC-family client (advertises ZRLE2) gets *REALVNC-ENCODING*."
  (cond ((member +enc-zrle2+ encs) *realvnc-encoding*)
        ((member +enc-zrle+ encs) +enc-zrle+)
        ((member +enc-hextile+ encs) +enc-hextile+)
        (t +enc-raw+)))

(defparameter *paste-chord* '(:shift #xff63)
  "The keystroke that pastes the session selection into the focused window, as
   (MODIFIER KEYSYM) — Shift+Insert, the X11 convention for \"paste the selection\" and the one
   a VNC user already has in their fingers.  NIL passes every key through untouched.

   It is recognised HERE, in the transport, and not in the window manager, for two reasons: the
   selection is a session-level thing, so every transport should be able to ask for a paste the
   same way; and paste has to work without the WM's cooperation, since glass core serves plain
   framebuffers with no window manager at all.  The key is CONSUMED when it fires — which is
   right, because Shift+Insert in the app underneath means exactly this.")

(defun rfb-paste-chord (client down keysym &optional (clipboard (session-clipboard)))
  "Track Shift and notice the paste chord.  Returns T if the key was consumed by a paste.
   Pastes from CLIPBOARD — this client's seat's selection, not necessarily the session's.

   A modifier arrives as its own key event and never as a flag on the keystroke it modifies, so
   Shift has to be latched; it is passed through as well as latched, or the app underneath would
   lose track of it."
  (case keysym
    ((#xffe1 #xffe2) (setf (rc-shift client) (plusp down)) nil)      ; Shift L/R
    (t (let ((chord *paste-chord*))
         (when (and chord (plusp down) (eql keysym (second chord))
                    (or (not (eq (first chord) :shift)) (rc-shift client)))
           (clipboard-paste :clipboard clipboard)                     ; types on its own thread
           t)))))

(defun client-loop (fb s on-key on-pointer on-resize wake &optional (clipboard (session-clipboard)))
  "Read RFB client messages, handling input (key/pointer) IMMEDIATELY; framebuffer
   updates are produced by a companion sender thread, so input is never blocked on
   a frame.  Only the sender writes pixels; this thread never writes to S."
  (let* ((client (make-rfb-client :last-size (cons (fb-width fb) (fb-height fb))))
         (sender (sb-thread:make-thread (lambda () (rfb-sender-loop client fb s wake clipboard))
                                        :name "glass-sender")))
    ;; A selection change should reach this client now, not at the next 1/60 safety tick — the
    ;; same nudge the compositor gives the sender when it has drawn.  Keyed by the client, so a
    ;; reconnect replaces its listener instead of stacking another one.
    (clipboard-listen clipboard client
                      (lambda (cb serial owner)
                        (declare (ignore cb serial owner))
                        (wake-signal wake)))
    (unwind-protect
         (loop
           (let ((msg (read-byte s nil :eof)))
             (case msg
               (0 (skip s 3)                               ; SetPixelFormat
                  (let* ((pf (r-bytes s 16)) (fmt (parse-pxfmt pf)))
                    (sb-thread:with-mutex ((rc-lock client))
                      ;; format change: full re-send (drop snapshot) and re-send the cursor in the
                      ;; new format.  Do NOT touch the ZRLE zlib stream: it is ONE persistent stream
                      ;; for the whole connection (RFC 6143), and clients keep their inflater across
                      ;; a SetPixelFormat — a fresh zlib header mid-stream desyncs them (RealVNC).
                      ;; Only the CPIXEL size inside the stream changes, which is fine.
                      (setf (rc-fmt client) fmt (car (rc-snap-box client)) nil
                            (rc-cursor-sent client) nil))
                    (format *trace-output* "~&glass: client SetPixelFormat bpp=~d depth=~d big-endian=~d true-colour=~d rgb-max=~d,~d,~d shift=~d,~d,~d -> ~a~%"
                            (aref pf 0) (aref pf 1) (aref pf 2) (aref pf 3)
                            (logior (ash (aref pf 4) 8) (aref pf 5)) (logior (ash (aref pf 6) 8) (aref pf 7))
                            (logior (ash (aref pf 8) 8) (aref pf 9)) (aref pf 10) (aref pf 11) (aref pf 12)
                            (if fmt (format nil "converted (~d-byte pixel, ~d-byte cpixel)" (pxfmt-pbytes fmt) (pxfmt-cbytes fmt)) "native"))
                    (force-output *trace-output*)))
               (2 (skip s 1)                               ; SetEncodings
                  (let ((n (r-u16 s)) (encs '()))
                    (dotimes (i n) (push (r-u32 s) encs))
                    (sb-thread:with-mutex ((rc-lock client))
                      (setf (rc-enc client) (choose-encoding encs)
                            (rc-dss client) (or (member +pseudo-desktop-size+ encs)
                                                (member +pseudo-extended-desktop-size+ encs))
                            (rc-cursor client) (and (member +pseudo-cursor+ encs) t)
                            (rc-copyrect client) (and (member +enc-copyrect+ encs) t)
                            (rc-trle client) (and (member +enc-trle+ encs) t)))
                    (format *trace-output* "~&glass: client encodings — chosen=~a TRLE=~:[no~;YES~] CopyRect=~:[no~;YES~] (~d offered)~%"
                            (rc-enc client) (rc-trle client) (rc-copyrect client) n)
                    (force-output *trace-output*)))
               (3 (let ((inc (r-u8 s)) (x (r-u16 s)) (y (r-u16 s)) (w (r-u16 s)) (h (r-u16 s)))
                    (sb-thread:with-mutex ((rc-lock client))   ; park the latest request
                      (setf (rc-want client) (list inc x y w h)))
                    (wake-signal wake)))                       ; nudge the sender to fulfil it now
               (4 (let ((down (r-u8 s)))                   ; KeyEvent
                    (skip s 2)
                    (let ((key (r-u32 s)))
                      (unless (rfb-paste-chord client down key clipboard)
                        (when on-key (funcall on-key (plusp down) key))))))
               (5 (let ((buttons (r-u8 s)) (x (r-u16 s)) (y (r-u16 s)))   ; PointerEvent
                    (when on-pointer (funcall on-pointer buttons x y))))
               (6 (skip s 3) (read-client-cut-text client s clipboard))   ; ClientCutText
               (251 (skip s 1)                             ; SetDesktopSize (client wants a size)
                    (let ((rw (r-u16 s)) (rh (r-u16 s)) (nscreens (r-u8 s)))
                      (skip s 1)
                      (dotimes (i nscreens) (r-bytes s 16))   ; per-screen layout (ignored)
                      (when on-resize (funcall on-resize rw rh))))
               (t (unless (eq msg :eof)                    ; EOF = normal disconnect; anything else is notable
                    (format *trace-output* "~&glass: dropping client on unhandled message-type ~a~%" msg)
                    (force-output *trace-output*))
                  (return)))))                             ; :eof or unknown -> done
      (setf (rc-running client) nil)
      ;; The listener goes; the SELECTION does not.  A client that disconnects after copying
      ;; still leaves its text on the session clipboard — CLIPBOARD-DISOWN here would wipe the
      ;; user's clipboard every time they closed a viewer tab, and the content is a plain string
      ;; that needs no owner to serve it.  Disowning is for an owner whose CONTENT dies with it.
      (clipboard-unlisten clipboard client)
      (ignore-errors (sb-thread:join-thread sender)))))

;;; ---- server -----------------------------------------------------------------

(defun serve (fb port &key on-key on-pointer on-resize (name *desktop-name*) once wake
                           (clipboard (session-clipboard)) (install-injector t) on-clients
                           (address "0.0.0.0") listen (password :inherit))
  "Serve framebuffer FB over RFB on PORT.  ON-KEY (down-p keysym), ON-POINTER
   (button-mask x y) and ON-RESIZE (requested-w requested-h, from the client
   resizing its window) are optional callbacks.  With :ONCE, handle a single
   client and return; otherwise loop, each client in its own thread.  WAKE (a
   glass:make-wake) lets the caller nudge parked senders the instant it has
   drawn — call glass:wake-signal after compositing; NIL falls back to polling.

   CLIPBOARD is the selection every client of THIS listener shares, defaulting to the
   session's.  Several transports of one seat pass the same one (a VNC viewer and a
   WebRTC channel showing the same screen must paste each other's text); separate SEATS
   pass different ones, because two people copying must not clobber each other.

   INSTALL-INJECTOR t (the default) makes ON-KEY the SESSION's keyboard — what a paste
   or a dictation types on when nobody named a seat.  A further seat's listener passes
   NIL: there is one *KEY-INJECTOR* and the last listener to start would otherwise own
   it, so the session's typing would land in the newest person's focused window.  The
   seat keeps its own ON-KEY and hands it to whatever types for that seat.

   ON-CLIENTS (n) is called whenever a client of THIS listener arrives or goes away, with
   the number still connected.  It exists so a caller can tell that NOBODY is watching
   this screen any more: a seat with no viewers has no hands, and anything it was holding
   on everybody else's behalf (see the McCLIM token in the backend) should be let go
   rather than waiting out a timeout.  Called inside IGNORE-ERRORS — a callback must not
   be able to take down the accept loop.

   ADDRESS is the interface to bind, defaulting to 0.0.0.0 exactly as before.

   LISTEN is an ALREADY-LISTENING socket or LISTENER (from TCP-LISTEN or OPEN-LISTENER) to
   accept on instead of making one.  It matters for the same reason CLOSE-LISTENER does: a
   caller that wants to be able to STOP serving needs to hold the socket, and a socket
   created inside this function is only reachable from the thread parked on it.  With
   LISTEN, the port is bound and listening by the time the caller starts this function's
   thread — so \"is it serving yet?\" is answered by the call that returned the socket, not
   by a sleep.  It is closed on the way out either way.

   PASSWORD IS THIS LISTENER'S CREDENTIAL, and it is per LISTENER and not per session.
   :INHERIT (the default) means *VNC-PASSWORD*, read at each handshake, so nothing that
   ever set that variable — before or during a run — changes behaviour.  A string is a
   credential only clients of THIS wire are asked for; NIL demands nothing here whatever
   the session-wide variable says.

   THAT DISTINCTION IS THE POINT.  *VNC-PASSWORD* is one global read inside the handshake,
   so it applied to every listener at once — which is why turning it on used to break
   video: the VP8 capture is an RFB client of the same desktop, speaks security type None,
   and a session-wide password locked it out while the browser's bridged RFB (which does
   VNC auth) kept working.  The SECURE configuration was the broken one.  With the
   credential here, a seat can demand a password on the TCP wire a stranger can reach and
   demand nothing on the socket file only its owner can open, in one process, at once.

   NIL ON A LISTENER BOUND OFF-BOX IS A DECISION SOMEBODY HAS TO MAKE, and this function
   does not make it: SERVE binds what it is told, as it always has.  The place that
   chooses an address on a person's behalf is the one that must refuse to expose an
   unauthenticated wire — see CLIM-GLASS:SERVE-SEAT-VNC.

   IT IS ALSO HOW RFB REACHES A UNIX SOCKET, and the whole of how: pass a UNIX-LISTENER and
   this function is unchanged — PORT is then only a label for the log line.  RFB is bytes
   on a stream (RFC 6143 specifies TCP but nothing in the protocol depends on it), so a
   viewer connecting over a socket file gets the identical `RFB 003.008' and the identical
   everything after it."
  (let ((listen (or listen (tcp-listen port :address address)))
        (live 0)
        (live-lock (sb-thread:make-mutex :name "glass-rfb-clients")))
    ;; Paste's fallback consumer types the selection into whatever has focus, and the only path
    ;; that knows where focus IS, is the one a real keystroke takes.  So the callback the caller
    ;; gave us for client keys becomes the session's key injector: an injected key is
    ;; indistinguishable from a typed one, and nothing downstream needs to learn about pasting.
    (when (and on-key install-injector) (setf *key-injector* on-key))
    ;; Say which posture this wire is in, because "am I asking anybody for anything?" is
    ;; the question a log line about a listener should answer and this one could not.
    (format *error-output* "~&glass: RFB server listening on ~a (~dx~d) — ~a~%"
            (listener-endpoint listen) (fb-width fb) (fb-height fb)
            (let ((pw (effective-vnc-password password)))
              (cond ((null pw) "no authentication")
                    ((vnc-auth-available-p) "VNC authentication REQUIRED")
                    (t "VNC auth required but NO VERIFIER (:glass/vncauth absent) — every client will be rejected"))))
    (force-output *error-output*)
    (flet ((note-clients (delta)
             (let ((n (sb-thread:with-mutex (live-lock) (incf live delta))))
               (when on-clients (ignore-errors (funcall on-clients n))))))
      (flet ((run (stream)
               (note-clients 1)
               (unwind-protect
                    (when (handshake fb stream name password)   ; NIL = auth failed -> drop the client
                      (client-loop fb stream on-key on-pointer on-resize wake clipboard))
                 (ignore-errors (close stream))
                 (note-clients -1))))
        (unwind-protect
             (loop
               (let ((stream (accept-stream listen)))
                 (if once
                     (progn (run stream) (return))
                     (sb-thread:make-thread (lambda () (ignore-errors (run stream)))
                                            :name "glass-client"))))
          (close-listener listen))))))

(defun serve-one (fb port &rest args)
  "Serve exactly one client, then return (handy for tests)."
  (apply #'serve fb port :once t args))
