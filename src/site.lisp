;;;; src/site.lisp — the box publishes its own client, in the image that mints links to it.
;;;;
;;;; nostr.lisp says it about admission: "WHO MAY OPEN THIS DESKTOP is a property of the desktop,
;;;; not of whichever wire somebody arrived on."  This file is the same sentence about the OTHER
;;;; half of a login link.  A link is two things joined — a token, minted here, and a URL, which
;;;; until now was minted somewhere else and carried across a process boundary in an environment
;;;; variable.
;;;;
;;;; ==============================================================================================
;;;; THE BUG THIS EXISTS TO MAKE IMPOSSIBLE
;;;; ==============================================================================================
;;;;
;;;; `publish.lisp' put a new build at a new PATH (/k42.html) and wrote that path into
;;;; `site-url.env'.  `gw-keepalive.sh' re-sourced that file on every loop, so the GATEWAY was
;;;; always current.  Then the DM bot — the thing that actually mints links — moved out of the
;;;; gateway and into this image, which has no such loop, and got its LOGIN_URL_BASE the way any
;;;; process gets an environment variable: once, at exec, from whatever its launcher had.
;;;;
;;;; Observed live on 2026-08-18: the site was `nsite.run/k42.html' and the desktop was handing out
;;;; `nsite.lol/k27.html' — a tag from weeks earlier, on a gateway host that had served a stale
;;;; manifest all week, from a build predating the box-key rotation.  Every one of those three
;;;; faults would have been enough on its own; a person tapped the link and could not connect.
;;;;
;;;; None of that is a bad value.  It is the SHAPE:
;;;;
;;;;     PUBLISHING AND MINTING WERE DIFFERENT PROCESSES COORDINATING THROUGH A FILE.
;;;;
;;;; A file handoff has a writer, a reader, and a moment when the reader last looked — and the last
;;;; of those is invisible.  A process that read it at exec and a process that re-reads it every
;;;; three seconds are indistinguishable from the writer's side, which is exactly why the bug
;;;; survived the bot moving house: nothing about the write changed, and nothing could report that
;;;; the reader had stopped.
;;;;
;;;; Put them in ONE IMAGE and the tag is a variable.  PUBLISH-SITE sets *LOGIN-URL-BASE* in
;;;; memory, in the same function call that published the manifest, and NOSTR-COMMAND-REPLY reads
;;;; that variable.  There is no file, no socket and no environment between them, so there is no
;;;; reader left to be stale — and no way to write a version of this that is stale, which is the
;;;; property a frozen variable cannot have and the one the evidence has to show.
;;;;
;;;; ==============================================================================================
;;;; WHERE THE BYTES COME FROM, AND WHY THAT IS A SEPARATE QUESTION
;;;; ==============================================================================================
;;;;
;;;; The BUILD is still external: `mksplit.py' shells out to esbuild, and nothing here is going to
;;;; bundle JavaScript.  So this image is handed an artefact, and there are two ways to hand it one:
;;;; the ~300 KB itself, or a path to it.
;;;;
;;;; The decision is to make PUBLISH-SITE take BYTES — a list of (path . content) conses, the same
;;;; shape CL-NOSTR.NSITE:NSITE-PUBLISH takes — and to let the FILE be a convenience on top of it
;;;; (PUBLISH-SITE-FILE).  The transport that carries a publish request from outside carries a
;;;; PATH today, because both processes are on one box that already shares ~/.glass and pushing
;;;; 300 KB through a control socket's READ as an escaped literal buys nothing while that is true.
;;;;
;;;; That is the shape that collapses cleanly.  The stated direction is a Lisp OS with no second
;;;; SBCL and eventually no external filesystem; in it the artefact is already an object in this
;;;; image and the call is PUBLISH-SITE with bytes.  What disappears is PUBLISH-SITE-FILE and the
;;;; path in the request — a wrapper and an argument.  Had the only entry point been a path, the
;;;; filesystem going away would be an interface rewrite instead of a deletion.
;;;;
;;;; And it is worth being clear that this is NOT the fix.  Removing the cross-process URL handoff
;;;; is; the artefact path is a local convenience between two processes that share a disk, and it
;;;; can stay wrong for years without anybody's login breaking.
;;;;
;;;; ==============================================================================================
;;;; THE SITE KEY, AND WHAT IT COSTS TO KEEP IT HERE
;;;; ==============================================================================================
;;;;
;;;; This image gains a FOURTH key.  docs/seats-and-transports.md counts three — transport, seat,
;;;; session/box — and wonders aloud whether the box key is redundant.  The site key is not
;;;; redundant and is not a destination: it is the authority to REPLACE EVERY PAGE served at that
;;;; npub, and publishing is destructive by construction, because kind 15128 is replaceable and a
;;;; new manifest retires the old one's paths the moment it lands.
;;;;
;;;; Three consequences, said out loud rather than glided past:
;;;;
;;;;   1. THE AUTHORITY DID NOT MOVE, BUT ITS RESIDENCE DID.  ~/.glass/site-key is 0600 and this
;;;;      process already runs as its owner, so any process that could reach this image could
;;;;      already read that file.  What changes is that the secret is now handled by a process that
;;;;      stays up for days, holds relay connections, and answers an unauthenticated EVAL on a
;;;;      socket.  So it is READ PER PUBLISH AND NOT CACHED, AND NEVER ON THE HEAP: WITH-SITE-SECRET
;;;;      reads the file straight into gossamer memory (non-moving, outside the collector's
;;;;      reach), signs from there, and wipes it on the way out — see "the site key" below.
;;;;      Between publishes this image holds no site secret at all; SITE-NPUB caches only the
;;;;      public name.
;;;;   2. A TYPO ON THE CONTROL SOCKET CAN NOW TAKE THE SITE DOWN.  It could not before, because
;;;;      the key only ever entered a short-lived `sbcl --script' somebody ran on purpose.
;;;;      NSITE-PUBLISH's :REQUIRE-BLOBS covers the worst version (a manifest naming a blob nobody
;;;;      holds), and PUBLISH-SITE additionally refuses to point the login link at a path the
;;;;      manifest it just published does not contain.  Neither covers "published the wrong bytes".
;;;;   3. THE GATEWAY MUST NOT GET THIS.  It loads :glass/nostr for the CLIENT half of admission,
;;;;      and it is the disposable, internet-facing, respawn-on-crash half.  That is why publishing
;;;;      is its own system (:glass/site) rather than another section of nostr.lisp: a system
;;;;      boundary is the only thing that reliably keeps code out of an image.
;;;;
;;;;      THE HOSTED CARD BREAKS THIS, and says so.  With CARD_ONE_PROCESS=1 the gateway IS the
;;;;      desktop: one SBCL holds the relay connections, the WebRTC endpoint and the mint.  So on
;;;;      the card this system is OFF unless CARD_SITE=1 (glass-webrtc/hosted-card/card.sh), and
;;;;      when it is on, what bounds the exposure is not a process boundary but these three: the
;;;;      key is in memory only for the length of a publish, in gossamer memory, wiped after; a
;;;;      publish can only be ASKED for over the card's control socket (CARD_CONTROL=1: 0600,
;;;;      same uid, refused at accept(2) otherwise); and the internet-facing code shares no
;;;;      variable with it.  What it does NOT bound: a compromise of that process DURING a
;;;;      publish could read the key — and, being this uid, could read ~/.glass/site-key at any
;;;;      time anyway.  The residence of the authority is unchanged from the desktop; the
;;;;      process it is used in is the exposed one.
;;;;
;;;; ==============================================================================================
;;;; ~/.glass/site-url — A MEMO, NOT A HANDOFF
;;;; ==============================================================================================
;;;;
;;;; There is still one file, and it is fair to ask how it differs from the one that caused all
;;;; this.  It differs in who reads it and when: `site-url.env' was read by a DIFFERENT, LIVE
;;;; process which might have last looked at any time; this is read by THIS image, at load, before
;;;; it has minted anything, and never again.  A publish writes it AFTER setting the variable and
;;;; only best-effort — if the write fails the mint is still right, and the only thing lost is what
;;;; the next cold start knows.  Nothing coordinates through it, so nothing can be stale across it.
;;;;
;;;; IT OUTRANKS THE ENVIRONMENT, and that inversion is deliberate.  $LOGIN_URL_BASE is a
;;;; launcher's guess, and a launcher's guess is the thing that was wrong; this file is written by
;;;; the code that did the publishing.  A publish is the authority on what is published.

(in-package #:glass)

;;; ---- the site's identity -----------------------------------------------------

(defvar *site-key-file*
  (or (sb-ext:posix-getenv "GLASS_SITE_KEY")
      (namestring (merge-pathnames ".glass/site-key" (user-homedir-pathname))))
  "Where the SITE's secret key lives: 64 hex, mode 0600, one line.

Not *BOX-SECRET* and deliberately not near it.  The box key is an identity people DM; this one is
the authority to replace every page served at the site's npub, and the two want different blast
radii.  DEFVAR, so a hot-load cannot repoint it at a different site under a running desktop.")

(defvar *site-url-file*
  (or (sb-ext:posix-getenv "GLASS_SITE_URL_FILE")
      (namestring (merge-pathnames ".glass/site-url" (user-homedir-pathname))))
  "Where this image remembers the URL of its last publish, so a COLD start mints the right link.

See the header: this is a memo to our own next boot, not a channel to another process.")

(defvar *site-npub* nil
  "The site's public name, cached once computed.  Public by definition, so caching it costs
nothing — unlike the secret it came from, which is deliberately never held between publishes.")

;; %BLANK->NIL is nostr.lisp's — an exported empty variable and an unset one mean the same thing.

(defun %read-first-line (path)
  "PATH's first line that is neither blank nor a `#' comment, trimmed, or NIL.  For PUBLIC things
only (the site-url memo): it reads through a Lisp string.  The site key never comes through here."
  (handler-case
      (with-open-file (s path :if-does-not-exist nil)
        (and s (loop for line = (read-line s nil)
                     while line
                     for trimmed = (%blank->nil line)
                     unless (or (null trimmed) (char= #\# (char trimmed 0)))
                       return trimmed)))
    (error () nil)))

;;; ---- the site key: read into gossamer memory, used, wiped --------------------
;;;
;;; "Secrets aren't supposed to be on the heap, they're supposed to be in separate memory that
;;; doesn't get copied around."  The moving heap is the wrong place for a key for a reason that has
;;; nothing to do with who can read this process: a copying collector COPIES.  Every collection
;;; that moves a string leaves the old bytes behind on a page nobody zeroes until it is reused, so
;;; a key that touched the heap once is in an unknown number of places for an unknown time, and no
;;; wipe can find them all.  So the site key never touches it:
;;;
;;;   FILE  read(2) straight into a gossamer buffer (natrium's `gossamer': on SBCL a static vector,
;;;         outside dynamic space, never moved).  No Lisp stream, so no stream buffer either.
;;;   HEX   decoded byte by byte out of that buffer into a second 32-byte gossamer buffer.  The
;;;         digits pass through registers as fixnums; nothing is consed.
;;;   KEY   CL-NOSTR.KEYS:KEYPAIR-FROM-SECRET with that fixed buffer copies it into ANOTHER pooled
;;;         gossamer buffer (fixed to fixed) — and from there every signature is secp256k1-fast's
;;;         BIP-340 code, whose scalars are fixed limb buffers released (wiped) per call.
;;;   WIPE  the raw read buffer and the decoded buffer go back to the pool zeroed when the read
;;;         returns; the keypair's buffer is wiped by WITH-SITE-SECRET's UNWIND-PROTECT, on every
;;;         exit, error or not.  Between publishes this image holds no site secret at all.
;;;
;;; $GLASS_SITE_SEC / $SITE_SEC ARE THE WEAKER PATH, and it is worth being exact about why.  The
;;; value is read with getenv(3) and decoded from the C string where the kernel put it, never via
;;; SB-EXT:POSIX-GETENV (which would make a Lisp string of it) — so it is not on the heap either.
;;; But the environment block is not ours to wipe: it stays in the process for its whole life,
;;; /proc/PID/environ shows it to anything of this uid, and every child (a terminal's shell, an
;;; app) inherits it.  The FILE is the path for a long-running image; the card does not pass the
;;; variables on at all (card.sh).

(defconstant +site-key-read-max+ 1024
  "How much of the key file is read: room for a few comment lines and the 64 hex digits.")

(defun %hex-value (b)
  "The value of the ASCII hex digit B (an octet), or NIL."
  (declare (type (unsigned-byte 8) b))
  (cond ((<= 48 b 57) (- b 48))
        ((<= 97 b 102) (- b 87))
        ((<= 65 b 70) (- b 55))
        (t nil)))

(defun %parse-site-key (get n out)
  "Decode the site key from bytes [0,N) — each read by calling GET with an index — into OUT, a
fixed 32-byte buffer.  T on success; NIL, with OUT zeroed, otherwise.

The layout is the one the key file always had: blank lines and `#' comment lines are skipped, and
the first other line, trimmed of blanks, must be exactly 64 hex digits.  GET rather than a vector
because the bytes are in a gossamer buffer for the file and in C memory for the environment, and
neither may be copied into a Lisp object to be parsed."
  (declare (type fixnum n) (type function get))
  (flet ((blank-p (b) (or (= b 32) (= b 9) (= b 13))))
    (let ((i 0))
      (declare (type fixnum i))
      (loop
        (loop while (and (< i n) (blank-p (funcall get i))) do (incf i))
        (when (>= i n) (return nil))
        (let ((b (funcall get i)))
          (cond ((= b 10) (incf i))
                ((= b 35) (loop while (and (< i n) (/= (funcall get i) 10)) do (incf i)))
                (t                                   ; the key line
                 (return
                   (if (and (<= (+ i 64) n)
                            (dotimes (k 32 t)
                              (let ((hi (%hex-value (funcall get (+ i (* 2 k)))))
                                    (lo (%hex-value (funcall get (+ i (* 2 k) 1)))))
                                (unless (and hi lo) (return nil))
                                (setf (aref out k) (logior (ash hi 4) lo))))
                            ;; and nothing after the 64 digits but blanks up to the end of the line
                            (loop for j from (+ i 64) below n
                                  for c = (funcall get j)
                                  until (= c 10)
                                  always (blank-p c)))
                       t
                       (progn (gossamer:wipe out) nil))))))))))

#+(and sbcl (not modus))
(defun %read-into-fixed (path buf)
  "Read up to (LENGTH BUF) bytes of PATH straight into BUF with read(2).  Returns the count, or NIL
if PATH cannot be opened.  No Lisp stream: an fd-stream would put the bytes through a buffer of its
own first, and that buffer is not ours to wipe."
  (let ((fd (ignore-errors (sb-unix:unix-open (sb-ext:native-namestring (merge-pathnames path))
                                              sb-unix:o_rdonly 0))))
    (when fd
      (unwind-protect
           (sb-sys:with-pinned-objects (buf)
             (let ((sap (sb-sys:vector-sap buf)) (got 0) (n (length buf)))
               (declare (type fixnum got n))
               (loop
                 (let ((r (sb-unix:unix-read fd (sb-sys:sap+ sap got) (- n got))))
                   (if (and r (plusp r))
                       (when (>= (incf got r) n) (return got))
                       (return got))))))
        (sb-unix:unix-close fd)))))

#-(and sbcl (not modus))
(defun %read-into-fixed (path buf)
  "Read up to (LENGTH BUF) bytes of PATH into BUF.  Through a Lisp stream here, so its buffer holds
a copy for as long as the stream's buffer lives: the direct read(2) is SBCL's."
  (handler-case
      (with-open-file (s path :element-type '(unsigned-byte 8) :if-does-not-exist nil)
        (and s (read-sequence buf s)))
    (error () nil)))

#+(and sbcl (not modus))
(defun %env-key-source (name)
  "NAME's value as (values SAP LENGTH) in the C environment, or NIL if it is unset or blank.
getenv(3), not SB-EXT:POSIX-GETENV: the latter returns a Lisp string, which is a heap copy."
  (let ((sap (sb-alien:alien-funcall
              (sb-alien:extern-alien "getenv" (function sb-sys:system-area-pointer sb-alien:c-string))
              name)))
    (unless (zerop (sb-sys:sap-int sap))
      (let ((n (loop for i fixnum from 0 below 4096
                     until (zerop (sb-sys:sap-ref-8 sap i))
                     finally (return i))))
        (when (loop for i below n
                    thereis (not (member (sb-sys:sap-ref-8 sap i) '(32 9 10 13))))
          (values sap n))))))

#+(and sbcl (not modus))
(defun %env-getter (sap) (lambda (i) (sb-sys:sap-ref-8 sap i)))

#-(and sbcl (not modus))
(defun %env-getter (sap) (declare (ignore sap)) (error "no C environment reader here"))

#-(and sbcl (not modus))
(defun %env-key-source (name)
  "Elsewhere there is no way to read the environment without making a Lisp string of it, so the
environment is not a source of the site key at all: put it in the file."
  (declare (ignore name))
  nil)

(defun %site-key-into (out file)
  "Decode the site key into OUT (a fixed 32-byte buffer) from $GLASS_SITE_SEC, else $SITE_SEC,
else FILE.  T if a well-formed key was found; NIL with OUT zeroed otherwise.  A variable that is
set but malformed is NIL, NOT a fall-through to the file: a typo must not quietly pick a key."
  (dolist (name '("GLASS_SITE_SEC" "SITE_SEC"))
    (multiple-value-bind (sap n) (%env-key-source name)
      (when sap
        (return-from %site-key-into
          (%parse-site-key (%env-getter sap) n out)))))
  (gossamer:with-fixed-scratch ((raw :octets +site-key-read-max+))
    (let ((n (and file (%read-into-fixed file raw))))
      (and n (%parse-site-key (lambda (i) (aref raw i)) n out)))))

(defun %load-site-keypair (file)
  "A CL-NOSTR KEYPAIR for the site key, its secret in gossamer memory, or NIL.  The caller wipes it
(CL-NOSTR.KEYS:KEYPAIR-WIPE) — WITH-SITE-SECRET is the form that does."
  (gossamer:with-fixed-scratch ((sk :octets 32))
    (when (%site-key-into sk file)
      ;; fixed -> fixed: KEYPAIR-FROM-SECRET POOLED-COPYs a fixed buffer; SK is wiped on the way out
      (handler-case (cl-nostr.keys:keypair-from-secret sk)
        (error () nil)))))                ; 0 or >= n: not a key

(defun call-with-site-secret (function &key (file *site-key-file*))
  "Call FUNCTION with the site's KEYPAIR (or NIL if no site key is reachable), and wipe the key on
every exit.  See WITH-SITE-SECRET."
  (let ((keypair nil))
    (unwind-protect
         (progn (setf keypair (%load-site-keypair file))
                (funcall function keypair))
      (when keypair (cl-nostr.keys:keypair-wipe keypair)))))

(defmacro with-site-secret ((var &key (file '*site-key-file*)) &body body)
  "Run BODY with VAR bound to the site's CL-NOSTR KEYPAIR — NIL if no site key is reachable — read
from $GLASS_SITE_SEC / $SITE_SEC / FILE straight into gossamer memory, and WIPED when BODY exits,
however it exits.

READ PER USE AND NEVER CACHED, which is the whole of consequence 1 in the header: between publishes
this image holds no site secret, so a backtrace, an inspector or a heap dump has nothing to find.
VAR is only good inside BODY: the keypair's secret is NIL afterwards, so nothing may keep it."
  `(call-with-site-secret (lambda (,var) ,@body) :file ,file))

(defun site-secret (&rest args)
  "GONE: the site key is never returned as a value.  A returned value is a heap copy the caller
cannot wipe — use WITH-SITE-SECRET, which hands the key to a body and wipes it after."
  (declare (ignore args))
  (error "GLASS:SITE-SECRET is gone: the site key is never handed out as a value. ~
          Use (glass:with-site-secret (keypair) ...), which wipes it on exit."))

(defun site-identity-p (&key (file *site-key-file*))
  "T iff this image can publish — i.e. a site key is reachable.  Asks WITHOUT keeping the answer."
  (with-site-secret (keypair :file file) (and keypair t)))

(defun site-npub (&key secret (file *site-key-file*) (cache t))
  "The site's npub, or NIL if no site key is reachable.  Cached in *SITE-NPUB* once known.
SECRET, if given, is anything CL-NOSTR.KEYS:PUBLIC-KEY-OF-SECRET takes (a KEYPAIR is the form that
puts nothing on the heap); without it the key is read, used and wiped."
  (or (and cache (null secret) *site-npub*)
      (let ((npub (if secret
                      (ignore-errors
                       (cl-nostr.bech32:npub-encode (cl-nostr.keys:public-key-of-secret secret)))
                      (with-site-secret (keypair :file file)
                        (and keypair (cl-nostr.bech32:npub-encode
                                      (cl-nostr.keys:keypair-public-key keypair)))))))
        (when (and npub cache (null secret)) (setf *site-npub* npub))
        npub)))

(export '(with-site-secret call-with-site-secret) '#:glass)

;;; ---- the site's policy -------------------------------------------------------
;;;
;;; Which servers, which relays, which gateway host.  This was `publish.lisp''s policy and it is
;;; the desktop's now, for the same reason the enrolment store is: the process that mints links to
;;; a site is the process that should know where the site is.

(defparameter *site-blossom*
  '("https://cdn.hzrd149.com" "https://blossom.primal.net" "https://nostr.download")
  "Blossom servers a blob is uploaded to.  All of them, in parallel, each individually bounded —
one of these reliably HANGS rather than refusing, and an unbounded upload once sat on a single POST
for twenty minutes while the blob went live with nothing pointing at it.  One success is enough.")

(defparameter *site-relays*
  '("wss://relay.damus.io" "wss://nos.lol" "wss://relay.primal.net")
  "Relays the manifest is published to AND advertised in kind 10002 — where a reader should look.")

(defparameter *site-lookup-relays*
  '("wss://purplepag.es" "wss://user.kindpag.es")
  "Also published to, deliberately NOT advertised: indexers that carry replaceable events, which
helps a gateway find the site without claiming they are where the site lives.")

(defparameter *site-gateway* "nsite.run"
  "The nsite gateway host a login link names.

NOT the library default.  CL-NOSTR.NSITE:*GATEWAY* is `nsite.lol', and publish.lisp inherited it,
so every publish wrote a `.lol' URL and every deploy ended with somebody hand-editing it back —
which is a step that gets skipped.  nsite.lol has served a stale manifest for several builds
running while nsite.run resolved the current one (check-deploy.lisp checks BOTH for exactly this
reason and has twice reported the wrong verdict from one).  A default that has to be corrected by
hand on every deploy is not a default; this is the host the links are actually meant to name.")

(defparameter *site-title* "glass over WebRTC"
  "The manifest's `title' tag.")

;;; ---- what a publish produced -------------------------------------------------

(defclass site-publication ()
  ((npub    :initarg :npub    :initform nil :reader site-publication-npub)
   (version :initarg :version :initform nil :reader site-publication-version
            :documentation "The tag whose path the login link names, e.g. \"k43\".")
   (path    :initarg :path    :initform nil :reader site-publication-path
            :documentation "That tag as a site path, e.g. \"/k43.html\".")
   (url     :initarg :url     :initform nil :reader site-publication-url
            :documentation "The gateway URL for it — what *LOGIN-URL-BASE* was set to, or NIL if
             the publish did not land and the link base was therefore left alone.")
   (hash    :initarg :hash    :initform nil :reader site-publication-hash)
   (at      :initarg :at      :initform (unix-now) :reader site-publication-at)
   (report  :initarg :report  :initform nil :reader site-publication-report
            :documentation "The CL-NOSTR.NSITE:NSITE-REPORT, or NIL for a publication that was
             recorded rather than performed (see NOTE-SITE-PUBLICATION)."))
  (:documentation
   "What this image last put on the network, and what it pointed the login link at.

    A class and not a structure for the reason SEAT-IDENTITY is one: this is long-lived state in an
    image that runs for weeks and gets patched while it runs, and redefining a structure strands
    every instance already made."))

(defmethod print-object ((p site-publication) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~@[~a ~]~@[~a~]~:[~; (link not moved)~]"
            (site-publication-version p) (site-publication-url p)
            (null (site-publication-url p)))))

(defun site-publication-problems (publication)
  "What did not work, as strings — the report's, if this publication has one."
  (let ((r (site-publication-report publication)))
    (if r (cl-nostr.nsite:report-problems r) '())))

(defvar *last-site-publication* nil
  "The last publish this image made, as a SITE-PUBLICATION, or NIL if it has made none.")

;;; ---- the link base: in memory first, on disk afterwards ----------------------

(defun save-site-url (url &key (file *site-url-file*))
  "Remember URL as what this box published, for the next cold start.  T on success.

BEST EFFORT ON PURPOSE.  The mint reads a variable; this file is read once, at load.  A failure
here costs the next boot its memory and costs the currently running bot nothing at all, so it must
never be allowed to turn a successful publish into an error."
  (handler-case
      (progn
        (ensure-directories-exist file)
        (with-open-file (s file :direction :output :if-exists :supersede :if-does-not-exist :create)
          (format s "# written by GLASS:PUBLISH-SITE — the URL this box last published.~%")
          (format s "# A memo to our own next start, not a channel to another process.~%")
          (format s "~a~%" url))
        (%chmod-600 file)
        t)
    (error () nil)))

(defun set-login-url-base (url &key (persist t))
  "Point the login link at URL.  Returns URL.

THE ORDER IS THE POINT.  *LOGIN-URL-BASE* is what NOSTR-COMMAND-REPLY reads, and it is set FIRST,
in this process, with nothing between the two but a variable reference.  The file write is a
consequence of the decision, not the medium for it."
  (setf *login-url-base* url)
  (when persist (save-site-url url))
  url)

(defun load-site-url (&key (file *site-url-file*) (install t))
  "The URL of the last publish this box recorded, or NIL.  With INSTALL, make it the link base.

CALLED AT LOAD, and it OUTRANKS $LOGIN_URL_BASE — see the header.  A launcher's environment is a
guess made before anything was published; this file was written by the publish itself."
  (let ((url (%read-first-line file)))
    (when (and url install) (setf *login-url-base* url))
    url))

;;; ---- recording a publish -----------------------------------------------------

(defun site-link-path (version)
  "The site path a login link should name for VERSION.  \"k43\" -> \"/k43.html\"; NIL -> the index.

A VERSIONED PATH, NOT A ?v= QUERY.  An nsite gateway resolves a request by PATH against the
kind-15128 manifest, so a query string selects the same blob and the browser is free to go on
serving its cached copy — which is why ?v= never busted anything for anybody."
  (let ((v (%blank->nil (and version (princ-to-string version)))))
    (cond ((null v) "/index.html")
          ((char= #\/ (char v 0)) v)
          (t (format nil "/~a.html" v)))))

(defun note-site-publication (npub version &key (gateway *site-gateway*) hash report
                                                (link t) (persist t) (install t))
  "Record that NPUB is now serving VERSION, and point the login link at it.

Returns (values URL PUBLICATION).  Separate from PUBLISH-SITE because the two answer different
questions — `did the bytes land' and `what should a link say now' — and because it is the whole of
the in-memory effect, which makes it the thing a test can exercise without a relay."
  (let* ((path (site-link-path version))
         (url (cl-nostr.nsite:nsite-gateway-url npub path :gateway gateway))
         (publication (make-instance 'site-publication
                                     :npub npub :version version :path path
                                     :url (and link url) :hash hash :report report)))
    (when install (setf *last-site-publication* publication))
    (when link (set-login-url-base url :persist persist))
    (values url publication)))

;;; ---- publishing --------------------------------------------------------------

(defun %say-upload (upload)
  (format *error-output* "~&[blossom] ~a ~:[SKIPPED: ~a~;-> ~a~]~%"
          (cl-nostr.blossom:upload-server upload)
          (cl-nostr.blossom:upload-ok-p upload)
          (if (cl-nostr.blossom:upload-ok-p upload)
              (let ((h (cl-nostr.blossom:upload-hash upload)))
                (subseq h 0 (min 16 (length h))))
              (cl-nostr.blossom:upload-error upload)))
  (finish-output *error-output*))

(defun %say-publication (publication)
  ;; Only the manifest: it is the event that decides whether the site changed, and the two
  ;; discovery lists succeeding or failing changes nothing anybody can see today.
  (when (= (cl-nostr.nsite:publication-kind publication) cl-nostr.nsite:+kind-nsite-root+)
    (dolist (ack (cl-nostr.nsite:publication-acks publication))
      (format *error-output* "~&[manifest] ~a accepted=~:[NIL~;T~] ~a~%"
              (cl-nostr.pool:ack-url ack) (cl-nostr.pool:ack-accepted-p ack)
              (or (cl-nostr.pool:ack-message ack) "")))
    (finish-output *error-output*)))

(defun %refuse-no-site-key ()
  (format *error-output*
          "~&@@ site: no site key — put the 64-hex secret in ~a (chmod 600).~@
             @@   Refusing to publish without one; a site is its key.~%"
          *site-key-file*)
  (finish-output *error-output*)
  (values nil nil))

(defun publish-site (files &rest args &key version secret &allow-other-keys)
  "Publish FILES as this box's site, and point the login link at VERSION.  Returns (values URL
PUBLICATION); URL is NIL when the publish did not land, and the link base is then untouched.

FILES is a list of (PATH . CONTENT) conses, CONTENT octets or a string — CL-NOSTR.NSITE's own
shape, and the reason this takes bytes rather than a filename is in the header.  VERSION names
which of those paths a login link should point at (\"k43\" -> \"/k43.html\").

THE KEY.  Without SECRET the site key is read for this publish into gossamer memory and wiped when
it returns, however it returns (WITH-SITE-SECRET).  SECRET, if given, must be a CL-NOSTR KEYPAIR
the caller owns and wipes — a hex string or a heap byte vector is refused, because by the time it
got here the secret was already on the heap and this function cannot take that back.

THE LINK BASE MOVES ONLY IF THE MANIFEST LANDED.  Publishing REPLACES the manifest, so until a
relay has accepted the new one the OLD paths are still what resolves — and a link base moved ahead
of the manifest names a page that does not exist yet, which is the same 404 this file exists to
prevent, arriving from the other direction.

AND ONLY AT A PATH THE MANIFEST HAS.  A version whose path is not in FILES would produce a link
into a site that never had it, so the publish still happens and the link is left alone, loudly.

Every other keyword is %PUBLISH-SITE-AS's: GATEWAY RELAYS LOOKUP SERVERS TITLE CONTENT-TYPE LINK
PERSIST ON-UPLOAD ON-PUBLISH."
  (declare (ignore version))
  (let ((rest (copy-list args)))
    (remf rest :secret)
    (cond (secret
           (unless (cl-nostr.keys:keypair-p secret)
             (error "PUBLISH-SITE :SECRET must be a CL-NOSTR KEYPAIR, not ~a: a hex string or ~
                     heap vector is a copy of the site key on the moving heap.  Omit :SECRET to ~
                     have the key read into gossamer memory and wiped."
                    (type-of secret)))
           (apply #'%publish-site-as secret files rest))
          (t (with-site-secret (keypair)
               (if keypair
                   (apply #'%publish-site-as keypair files rest)
                   (%refuse-no-site-key)))))))

(defun %publish-site-as (keypair files &key version
                                            (gateway *site-gateway*)
                                            (relays *site-relays*) (lookup *site-lookup-relays*)
                                            (servers *site-blossom*) (title *site-title*)
                                            (content-type "text/html")
                                            (link t) (persist t)
                                            (on-upload #'%say-upload)
                                            (on-publish #'%say-publication))
  "PUBLISH-SITE's work, signed by KEYPAIR, which the caller owns (and wipes)."
  (let ((path (site-link-path version)))
    (unless (assoc path files :test #'string=)
      (format *error-output*
              "~&@@ site: nothing in this publish is at ~a — the link base will NOT be moved.~%" path)
      (finish-output *error-output*)
      (setf link nil))
    (let ((where (append relays lookup)))
      (multiple-value-bind (npub report)
          (cl-nostr.nsite:nsite-publish where servers keypair files
                                        :relays relays :title title
                                        :content-type content-type
                                        :on-upload on-upload :on-publish on-publish)
        (let ((hash (cdr (assoc path (cl-nostr.nsite:report-paths report) :test #'string=))))
          (cond
            ((cl-nostr.nsite:report-published-p report)
             (note-site-publication npub version :gateway gateway :hash hash :report report
                                                 :link link :persist persist))
            (t
             ;; Nothing landed: NSITE-PUBLISH has already declined to move the manifest if no
             ;; Blossom server took the blob, and a manifest no relay accepted is a manifest that
             ;; was not published.  Either way the site still serves what it served, so the link
             ;; must go on naming what it named.
             (let ((publication (make-instance 'site-publication
                                               :npub npub :version version :path path
                                               :url nil :hash hash :report report)))
               (setf *last-site-publication* publication)
               (values nil publication)))))))))

(defun read-file-octets (path)
  "PATH's bytes."
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s)
      v)))

(defun publish-site-file (path &rest args
                          &key (version (or (%blank->nil (sb-ext:posix-getenv "SITE_VERSION"))
                                            "latest"))
                               (index t)
                          &allow-other-keys)
  "Publish the single built page at PATH as this site, at /VERSION.html (and, with INDEX, at
/index.html and / as well).  Returns (values URL PUBLICATION), exactly as PUBLISH-SITE does; every
other keyword is PUBLISH-SITE's.

THE THIN ONE.  Everything real is in PUBLISH-SITE; this reads a file and names three paths for the
same bytes — which Blossom stores ONCE, being content-addressed.  It is also the part that goes
away when there is no filesystem left to read from, which is the point of the split."
  (let* ((bytes (read-file-octets path))
         (link-path (site-link-path version))
         (files (append (list (cons link-path bytes))
                        (when index (list (cons "/index.html" bytes) (cons "/" bytes)))))
         (rest (copy-list args)))
    (remf rest :index)
    (remf rest :version)
    (format *error-output* "~&@@ site: publishing ~d bytes of ~a at ~a~%"
            (length bytes) path link-path)
    (finish-output *error-output*)
    (apply #'publish-site files :version version rest)))

;;; ---- saying what happened ----------------------------------------------------

(defun site-report (&optional (publication *last-site-publication*) &key (detail t))
  "What the last publish did, in words — the string a control-socket caller gets back.

COMPLETE BY DEFAULT, and that is the point: the live [blossom]/[manifest] lines go to the
*ERROR-OUTPUT* of whichever image did the publishing, which for a handoff is a desktop log the
person running the deploy is not watching.  Every server and every relay is named here instead, so
the answer that comes back down the socket is the whole of what happened.  :DETAIL NIL drops them,
for the one caller who already watched them go by — the process that did the publishing itself.

Human-first, because the caller of a publish is a person watching a deploy, and the one thing they
must be able to read off it without interpretation is WHICH URL A LINK NOW NAMES."
  (if (null publication)
      "site: nothing published by this image"
      (let ((report (and detail (site-publication-report publication))))
        (with-output-to-string (s)
          (format s "site ~a~@[ ~a~]" (or (site-publication-npub publication) "?")
                  (site-publication-version publication))
          (let ((h (site-publication-hash publication)))
            (when h (format s " blob ~a" (subseq h 0 (min 16 (length h))))))
          (when report
            (dolist (upload (cl-nostr.nsite:report-uploads report))
              (format s "~%[blossom]  ~a ~:[SKIPPED: ~a~;stored~*~]"
                      (cl-nostr.blossom:upload-server upload)
                      (cl-nostr.blossom:upload-ok-p upload)
                      (cl-nostr.blossom:upload-error upload)))
            (let ((manifest (cl-nostr.nsite:report-manifest report)))
              (dolist (ack (and manifest (cl-nostr.nsite:publication-acks manifest)))
                (format s "~%[manifest] ~a accepted=~:[NIL~;T~]~@[ ~a~]"
                        (cl-nostr.pool:ack-url ack) (cl-nostr.pool:ack-accepted-p ack)
                        (%blank->nil (cl-nostr.pool:ack-message ack))))))
          (if (site-publication-url publication)
              (format s "~%link base -> ~a" (site-publication-url publication))
              (format s "~%link base UNCHANGED (~a) — this publish did not land"
                      (or *login-url-base* "unset")))
          (dolist (problem (site-publication-problems publication))
            (format s "~%WARNING: ~a" problem))))))

;;; ---- what a launcher gets for free -------------------------------------------
;;;
;;; No START- function and nothing to call.  LOADING THIS SYSTEM IS THE STATEMENT: an image that
;;; has :glass/site in it is the image that publishes this site, so the last thing published is
;;; what its links should name, and reading that is the whole of the setup.  A desktop that never
;;; loads it goes on using $LOGIN_URL_BASE exactly as before.

(eval-when (:load-toplevel :execute)
  (let ((url (load-site-url)))
    (when url
      (format *error-output* "~&@@ site: link base ~a (last published by this box)~%" url)
      (finish-output *error-output*))))
