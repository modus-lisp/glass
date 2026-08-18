;;;; src/nostr.lisp — the desktop's own identity, and the terminals it trusts.
;;;;
;;;; audio-stream.lisp says it in one line about a different thing: "a mixer in the gateway could
;;;; only carry what the gateway decided to play."  This file is that sentence applied to the other
;;;; half of the session.  WHO MAY OPEN THIS DESKTOP is a property of the desktop, not of whichever
;;;; wire somebody arrived on — so the enrolment store, the token mint, the allowlist and the DM
;;;; command surface belong in the image the desktop's applications run in, and a transport is a
;;;; thin thing that ASKS.
;;;;
;;;; It used to live in the WebRTC gateway, and the three costs of that were all real:
;;;;
;;;;   * THE DIAGNOSTIC CHANNEL DIED WITH THE THING BEING DIAGNOSED.  The gateway is supervised,
;;;;     restarts on every config change, respawns on every crash, and is deliberately
;;;;     crashloop-on-missing-identity.  In exactly the situation where you would DM the box to ask
;;;;     what is wrong, nothing was listening.  This process stays up for days.
;;;;   * IT DID NOT SURVIVE A SECOND TRANSPORT.  A LAN gateway, a native client, or a second
;;;;     WebRTC gateway would each have needed the secret and its own copy of the store — two
;;;;     writers to one file synchronised by mtime.
;;;;   * THE DEVICE MANAGER WANTED IT.  warp's panel ran inside the gateway only because the data
;;;;     was there.  With the store here it is an ordinary query against an ordinary service.
;;;;
;;;; OPTIONAL, LIKE :glass/speech AND :glass/hearing.  cl-nostr is a dependency of THIS system and
;;;; of nothing else, so core glass carries no crypto and no relay client, and a desktop that never
;;;; loads this is a working desktop that simply admits nobody of its own accord.
;;;;
;;;; ==============================================================================================
;;;; THE FIVE THINGS THIS OWNS
;;;; ==============================================================================================
;;;;
;;;;   0. THE SEATS' IDENTITIES.  An npub per SEAT — per place at the session, not per person and
;;;;      not per wire — minted on demand and persisted in a store of their own.  Numbered zero
;;;;      because it is the newest and the one the other four do not depend on: nothing here
;;;;      admits anybody by a seat key, and nothing signs with one.  See its section below for why
;;;;      a place having a name is worth a keypair, and why it is kept apart from (2).
;;;;   1. THE IDENTITY.  One secret key; the box's npub is its public name.  Required — there is
;;;;      deliberately no fallback, for the reason the gateway learned the hard way: the same
;;;;      secret is the HMAC key for login tokens, so a committed placeholder is not an identity
;;;;      leak, it is a credential minter anybody with the source can run.
;;;;   2. THE ENROLMENT STORE.  Pubkey -> expiry, persisted, mtime-synced so the FILE is the source
;;;;      of truth and any process (a shell one-liner, warp's panel) can revoke without a restart.
;;;;   3. THE TOKENS.  Mint and verify, byte-compatible with the format already in the field:
;;;;        token = <nonce-hex> "." <exp-unix> "." <mac-hex>
;;;;        mac   = HMAC-SHA256(box-secret, "glass-login|" nonce "|" exp)
;;;;      Compatible on purpose and not by accident — a token minted by the old gateway code must
;;;;      verify here, or every link ever issued breaks on the day this is deployed.
;;;;   4. THE COMMAND SURFACE.  `link', `devices', `revoke', `help', over gift-wrapped DMs, with
;;;;      `devices' and `revoke' ALLOWLIST-ONLY.  That restriction is the point rather than an
;;;;      oversight: a device key is a bearer credential that can be lifted out of a browser, and
;;;;      it must not be able to keep itself alive, revoke the others, or enumerate the fleet.
;;;;
;;;; ==============================================================================================
;;;; THE SEAM THAT IS DELIBERATELY NOT CROSSED YET
;;;; ==============================================================================================
;;;;
;;;; The box secret is SHARED with the gateway for now: the gateway still unwraps gift-wrapped SDP
;;;; offers itself, and it cannot decrypt an offer without the key.  Making this image the only
;;;; holder — an unwrap/sign oracle the gateway asks, so a compromised gateway could neither mint a
;;;; login token nor speak as the box — is a real and worthwhile follow-up, and it is a bigger
;;;; change than moving the data: it puts NIP-44 conversation keys on the request path of every
;;;; offer.  Where it would go is marked at ADMISSION-SERVE: two more verbs (`unwrap', `wrap') and
;;;; NOSTR_SEC deleted from the gateway's environment.  Until then the arrangement is exactly what
;;;; it was — one secret, two processes — and moving the data has not made it worse.

(in-package #:glass)

;;; ---- small text helpers ------------------------------------------------------

(defun %split-on (string char)
  "STRING split at CHAR, each piece trimmed of surrounding blanks."
  (loop with start = 0
        for i = (position char string :start start)
        collect (string-trim '(#\Space #\Tab) (subseq string start i))
        while i do (setf start (1+ i))))

(defun %blank->nil (s)
  "S trimmed, or NIL if it is not a string or has nothing in it.

An UNSET environment variable and one exported EMPTY are the same statement — `this launcher has
no value for you' — and only one of them is NIL to POSIX-GETENV.  Reading them differently is how
`\"\"' ends up in a login link."
  (let ((s (and (stringp s) (string-trim '(#\Space #\Tab #\Newline #\Return) s))))
    (and s (plusp (length s)) s)))

(defun %one-line (thing)
  "THING printed with every blank turned into a dash, so it cannot break a line-framed protocol."
  (map 'string (lambda (c) (if (member c '(#\Space #\Tab #\Newline #\Return)) #\- c))
       (princ-to-string thing)))

;;; ---- positional fields -------------------------------------------------------
;;;
;;; Every store this file keeps is one space-separated line per record with `-' for a field that
;;; does not apply, because `grep revoked .glass-devices' has to be the diagnostic and a line has to
;;; be readable next to a log entry without a parser.  These two are that convention, written once:
;;; a field goes out through %DASH (which also flattens blanks, so a cause can never eat the field
;;; after it) and comes back through %UNDASH.

(defun %dash (thing)
  "THING as one positional field: its printed text with blanks flattened, or \"-\" for nothing."
  (let ((s (and thing (%one-line thing))))
    (if (and s (plusp (length s)) (not (string= s "-"))) s "-")))

(defun %undash (s)
  "One positional field back: NIL for \"-\", for \"\" and for a field that was not there at all."
  (and (stringp s) (plusp (length s)) (not (string= s "-")) s))

(defun %int (s &optional (default 0))
  "S as an integer, or DEFAULT if it is not one.  A store edited by hand must not be a store that
throws: an unparseable timestamp is a missing timestamp, and the record is still worth having."
  (or (and (stringp s) (ignore-errors (parse-integer s))) default))

;;; ---- the box's identity ------------------------------------------------------
;;;
;;; DEFVAR and not DEFPARAMETER, for the reason given at *HEARING-MODELS*: a launcher fills this in
;;; after load, and a hot-load of this file must not reset it.  Losing the secret to a recompile
;;; would silently invalidate every issued link and un-enrol every terminal.

(defvar *box-secret*
  (or (sb-ext:posix-getenv "GLASS_NOSTR_SEC") (sb-ext:posix-getenv "NOSTR_SEC"))
  "The box's Nostr secret key, 64 hex chars — its identity AND the login-token HMAC key.

NIL means this desktop has no identity of its own.  That is not an error at load time and does not
stop a desktop starting; it stops it SERVING, which is the correct failure: START-SESSION-NOSTR
refuses loudly, no admission service listens, and a gateway asking gets no answer at all.  A box
with no identity must not admit anybody, and a loud refusal is easier to diagnose than a desktop
quietly admitting people as somebody else.")

(defun box-identity-p ()
  "T iff this image holds a usable box secret."
  (let ((s *box-secret*))
    (and (stringp s) (= (length s) 64) (every (lambda (c) (digit-char-p c 16)) s) t)))

(defun box-pubkey ()
  "The box's x-only public key, 64 hex, or NIL without an identity."
  (when (box-identity-p)
    (ignore-errors
     (string-downcase (cl-nostr.keys:public-hex
                       (cl-nostr.keys:keypair-from-secret *box-secret*))))))

(defun box-npub ()
  "The box's npub — its public name — or NIL without an identity."
  (when (box-identity-p)
    (ignore-errors
     (cl-nostr.bech32:npub-encode (cl-nostr.keys:public-key-of-secret *box-secret*)))))

;;; ---- a seat's identity -------------------------------------------------------
;;;
;;; A SEAT IS WHAT YOU CONNECT TO (backend/seat.lisp; docs/seats-and-transports.md), and a
;;; seat gets an npub OF ITS OWN.  "DM this npub and get a link" is then a way in to a
;;; SEAT rather than to a wire: a seat reachable over VNC today and over something else
;;; tomorrow is the same seat, and rotating a transport's key changes nothing about the
;;; destination.  (We rotated the box key on 2026-08-12 and it invalidated every link and
;;; every enrolment, because the key WAS the destination.)
;;;
;;; THIS IS NOT PERSON IDENTITY.  The seat's key says WHICH PLACE THIS IS and belongs to
;;; the session's configuration; *NOSTR-ALLOW* and the enrolments below say WHO MAY SIT IN
;;; IT.  Both are npubs, they answer different questions, and they are kept in different
;;; files so that they cannot be quietly merged: collapse them and "the owner sat down at
;;; the guest seat" stops being sayable.
;;;
;;; PERSISTED, KEYED BY THE SEAT'S NAME, because a seat that got a new npub on every
;;; restart would be a destination nobody could write down — which is the whole objection
;;; to addressing a transport.  The store is this file's because the KEYS are: core glass
;;; carries no crypto (cl-nostr and ironclad are dependencies of :glass/nostr alone), so
;;; the backend holds an opaque slot and asks for one of these by name.
;;;
;;; NOTHING HERE SIGNS ANYTHING, and that is a decision rather than an omission.  Identity
;;; is given now because retrofitting it is a migration; verification is left out because
;;; adding it later is a feature, and there is no attacker today on a call between two
;;; objects in one image.  SEAT-IDENTITY-SECRET is the key a signature would be taken
;;; with, and it is deliberately the only thing here that touches it.

(defvar *seat-key-file*
  (or (sb-ext:posix-getenv "GLASS_SEAT_KEYS")
      (namestring (merge-pathnames ".glass-seats" (user-homedir-pathname))))
  "Where seat keys are persisted: one `<seat-name> <secret-hex>' line per seat.

SEPARATE FROM *ENROLMENT-FILE* on purpose — that one holds other people's public keys and
is meant to be read and edited by anything (warp's panel, a shell one-liner); this one
holds SECRETS and is written 0600.  DEFVAR, so a hot-load cannot move the store out from
under a running desktop and hand every seat a new name.")

(defvar *seat-keys* (make-hash-table :test 'equal))
(defvar *seat-keys-lock* (sb-thread:make-mutex :name "glass-seat-keys"))

(defclass seat-identity ()
  ((name   :initarg :name   :reader seat-identity-name)
   (secret :initarg :secret :reader seat-identity-secret)
   (pubkey :initarg :pubkey :reader seat-identity-pubkey)
   (npub   :initarg :npub   :reader seat-identity-npub))
  (:documentation
   "One seat's key: its NAME (which place this is), its 32-byte secret as 64 hex, its
    x-only PUBKEY and its NPUB — the seat's public name, the thing you would address.

    A class and not a structure for the reason the seat itself is one: a desktop runs for
    weeks and grows slots while it runs, and redefining a structure strands the instances
    already made."))

(defmethod print-object ((id seat-identity) stream)
  ;; The secret is NOT printed.  A seat identity ends up in a backtrace, an inspector and
  ;; the control socket's output, and a key that prints itself is a key that leaks.
  (print-unreadable-object (id stream :type t)
    (format stream "~s ~a" (slot-value id 'name)
            (let ((n (slot-value id 'npub))) (if n (subseq n 0 (min 16 (length n))) "?")))))

(defun %seat-identity (name secret)
  "Build a SEAT-IDENTITY for NAME from a 64-hex SECRET, or NIL if the secret is not one."
  (ignore-errors
   (let* ((kp (cl-nostr.keys:keypair-from-secret secret))
          (pub (cl-nostr.keys:public-key-of-secret secret)))
     (make-instance 'seat-identity
                    :name name
                    :secret (string-downcase (cl-nostr.keys:secret-hex kp))
                    :pubkey (string-downcase (cl-nostr.keys:public-hex kp))
                    :npub (cl-nostr.bech32:npub-encode pub)))))

(defun %seat-key-name (name)
  "NAME as a store key: a seat name with nothing in it that could break a line-framed
   file, so \"Front desk\" is stored as \"Front-desk\".  The reader splits on the LAST space
   anyway — belt and braces, because the file is edited by hand and a name that ate its
   own secret would hand a seat a new identity in silence."
  (%one-line (or name "seat")))

(defun %chmod-600 (path)
  "Make PATH readable by its owner alone, if this image can say so.  Best effort: a
   umask-tightened file is already 0600 and a platform without sb-posix is not a reason to
   refuse to remember a key."
  (ignore-errors
   (let* ((pkg (or (find-package "SB-POSIX")
                   (progn (require :sb-posix) (find-package "SB-POSIX"))))
          (chmod (and pkg (find-symbol "CHMOD" pkg))))
     (when (and chmod (fboundp chmod)) (funcall chmod (namestring path) #o600)))))

(defun %load-seat-keys ()
  "Merge the store into *SEAT-KEYS*.  Never overwrites what this process already holds —
   an identity handed out is an identity in use, and a file that changed underneath must
   not make a live seat answer to a second name.  Call with the lock held."
  (handler-case
      (with-open-file (s *seat-key-file* :if-does-not-exist nil)
        (when s
          (loop for line = (read-line s nil) while line do
            (let* ((sp (position #\Space line :from-end t))   ; the secret is the last field
                   (name (and sp (subseq line 0 sp)))
                   (sec (and sp (string-downcase (subseq line (1+ sp))))))
              (when (and name (plusp (length name)) (= 64 (length sec))
                         (not (gethash name *seat-keys*)))
                (let ((id (%seat-identity name sec)))
                  (when id (setf (gethash name *seat-keys*) id))))))))
    (error () nil)))

(defun %save-seat-keys ()
  "Write the store.  Call with the lock held."
  (handler-case
      (progn
        (with-open-file (s *seat-key-file* :direction :output :if-exists :supersede
                                           :if-does-not-exist :create)
          (maphash (lambda (name id)
                     (format s "~a ~a~%" name (seat-identity-secret id)))
                   *seat-keys*))
        (%chmod-600 *seat-key-file*)
        t)
    (error () nil)))

(defun seat-identity-for (name)
  "THE SEAT NAMED NAME'S IDENTITY, minted and persisted the first time it is asked for and
   the same one every time after — including across a restart, which is what makes a seat
   an address somebody can write down.  NIL only if a key can be neither read nor made.

   This is the function CLIM-GLASS::ENSURE-SEAT-IDENTITY looks up by name, and it is the
   whole of the seam: the backend never learns what an npub is, and core glass goes on
   having no crypto dependency at all."
  (let ((name (%seat-key-name name)))
    (sb-thread:with-mutex (*seat-keys-lock*)
      (or (gethash name *seat-keys*)
          ;; Not ours: another process (or an earlier run) may have minted it.  Read
          ;; before minting, or two desktops sharing a store would each make their own
          ;; key for one seat and the second write would lose the first.
          (progn (%load-seat-keys) (gethash name *seat-keys*))
          (let ((id (ignore-errors
                     (let ((kp (cl-nostr.keys:generate-keypair)))
                       (%seat-identity name (cl-nostr.keys:secret-hex kp))))))
            (when id
              (setf (gethash name *seat-keys*) id)
              (%save-seat-keys)
              id))))))

(defun seat-identity-known (name)
  "The identity stored for NAME, WITHOUT minting one.  NIL if this seat has never had a
   key — which is how to ask the question without answering it."
  (let ((name (%seat-key-name name)))
    (sb-thread:with-mutex (*seat-keys-lock*)
      (or (gethash name *seat-keys*) (progn (%load-seat-keys) (gethash name *seat-keys*))))))

(defun list-seat-identities ()
  "The seats this store knows, as ((name . npub) …), by name.  Secrets are not returned:
   the listing is for saying which places exist and how to address them."
  (sb-thread:with-mutex (*seat-keys-lock*)
    (%load-seat-keys)
    (let ((rows '()))
      (maphash (lambda (name id) (push (cons name (seat-identity-npub id)) rows)) *seat-keys*)
      (sort rows #'string< :key #'car))))

(defun forget-seat-identity (name)
  "Drop NAME's key from the store.  T if there was one.

   THIS IS NOT REVOCATION and there is nothing to revoke: a seat key is a destination, not
   a credential, and nobody was ever admitted by it.  What it does is make the next seat
   of that name a DIFFERENT place — so every link that named the old one stops naming
   anything, which is exactly the failure rotating the box key caused and the reason seats
   have their own keys at all.  Kept because a store you cannot clean up is a store that
   accumulates every seat anybody ever typo'd."
  (let ((name (%seat-key-name name)))
    (sb-thread:with-mutex (*seat-keys-lock*)
      (%load-seat-keys)
      (when (remhash name *seat-keys*) (%save-seat-keys) t))))

;;; WHERE A SEAT'S SIGNATURE WOULD GO.  A remote seat's request — open a transport, move a
;;; window, take the CLIM token — would be signed with SEAT-IDENTITY-SECRET (BIP340, via
;;; CL-NOSTR.KEYS:SIGN) over the request's canonical bytes, and the session would check it
;;; against SEAT-IDENTITY-PUBKEY for the seat the request names.  That is the step which
;;; would let a seat live in another process without the trust boundary being "whoever can
;;; reach the port".  It is not built: see the header of the section above.

;;; ---- time --------------------------------------------------------------------

(defconstant +unix-epoch-universal+ 2208988800
  "Universal time at the unix epoch.  Written out rather than computed, so a token's MAC cannot
depend on ENCODE-UNIVERSAL-TIME's timezone handling being what we assumed it was.")

(defun unix-now () (- (get-universal-time) +unix-epoch-universal+))

;;; ---- login tokens ------------------------------------------------------------
;;;
;;; A code is self-authenticating and stateless to mint and to verify: whoever holds the box secret
;;; can check a token's MAC and expiry with no shared state at all.  It is delivered to a person
;;; inside a gift-wrapped DM — only that npub can read it — so holding a valid code IS the proof of
;;; identity, and no browser signer is needed.
;;;
;;; A CODE IS REDEEMED ONCE, BY ONE KEY, and the store that decides it is THE REDEMPTION STORE
;;; below.  The MAC and the expiry are still stateless — that has to stay true, because they are what
;;; a token minted years ago is checked against — so single use is a separate fact kept beside the
;;; enrolments rather than baked into the token.  What a successful login now means is `nobody else
;;; used this code', which is what turns an intercepted link from undetectable into detected.
;;;
;;; IT IS A BINDING AND NOT A BURN, for the reason the burn was rejected the first time: signalling
;;; is one-shot and non-trickle over three relays with no renegotiation, so a lost answer means the
;;; phone RE-OFFERS with the same code, and relay fan-out delivers one offer several times anyway.
;;; A code that could be shown exactly once refused the honest retry and stranded the user — and the
;;; failure looked exactly like the bug it was supposed to fix.  Binding the nonce to the pubkey
;;; that redeemed it keeps the retry working and still refuses the interceptor: see REDEEM-NONCE.
;;;
;;; THE WIRE FORMAT IS FROZEN.  Every link in somebody's message history is a string in this shape
;;; keyed by this MAC, so changing any of it — the separator, the label, the epoch — invalidates
;;; credentials that are already out there.  inspect/nostr-gate.lisp holds a token minted by the
;;; PREVIOUS implementation as a literal and verifies it here, so that claim is checked and not
;;; merely asserted.

;;; TWO TTLS, BECAUSE THERE ARE TWO JOBS.  These were ONE parameter (*LOGIN-TTL*, 1800 s) doing both,
;;; which is why neither could be tuned: the numbers want to move in opposite directions.
;;;
;;;   A LINK is a credential in transit.  It is DM'd to a person, sits in a notification, and is
;;;   tapped — the window it is exposed for IS its TTL, and nothing else about it wants to be long.
;;;   A LINK'S TTL IS THE ONLY THING BOUNDING A LEAKED LINK.
;;;
;;;   A RENEWAL is a credential at rest.  It rides back in the answer envelope into localStorage and
;;;   has to still be there on that browser's NEXT LOAD, which may be tomorrow.  Shortening it does
;;;   not make anything safer — it is already sitting in the same browser's storage as the device
;;;   key it would fall back to — it just quietly demotes a `code' admission to a `device' one.
;;;
;;; So they are split, and a deployment that shortens one no longer silently shortens the other.

(defparameter *login-ttl*
  (or (ignore-errors (parse-integer (or (sb-ext:posix-getenv "GLASS_LOGIN_TTL")
                                        (sb-ext:posix-getenv "LINK_TTL"))))
      600)
  "Lifetime of a minted LINK, seconds.  Ten minutes, and the reasoning is worth keeping because the
number is the only bound on a leaked link:

  WHAT IT HAS TO COVER.  Gift-wrap publish and relay fan-out are seconds, not minutes — that is not
  what sets the floor.  What sets it is a person: the DM lands, the phone is face down in another
  room, they walk over, unlock it, open the DM, and tap.  Then the client's tap-to-open gate asks
  for one more deliberate tap (a link-preview bot must not be able to redeem a code by rendering
  the page).  Five minutes covers that with nothing to spare, and a link that fails because
  somebody answered the door is a link they ask for again — which is not a security event but IS a
  worse experience than the risk it buys back.

  WHAT IT HAS TO NOT COVER.  Thirty minutes was `long enough to walk to the other room' with
  twenty-five minutes of slack attached, and that slack is exactly the window in which a
  screenshot, a forwarded message or a mirrored notification is a working key.  Ten minutes keeps
  the human path and deletes the slack.

  WHY TEN AND NOT FIVE.  Five is defensible and this dial is one environment variable, but five
  starts failing the ordinary case, and the failure of a too-short link is indistinguishable at the
  far end from the failures that matter.  Ten is the largest number that still makes a DM found
  later useless.

  AND IT IS NO LONGER THE ONLY DEFENCE, which is what makes shortening it safe rather than merely
  strict: the code is bound to the first key that redeems it, superseded the moment a newer link is
  minted for the same person, and burned on use even when the caller did not need it.

LINK_TTL is honoured as a fallback because that is the name the gateway's launcher already exports,
and a value that silently stopped being read on the day this moved would be the worst kind of
regression — invisible, and only in the direction of longer-lived credentials.")

(defparameter *renewal-ttl*
  (or (ignore-errors (parse-integer (sb-ext:posix-getenv "GLASS_RENEWAL_TTL")))
      1800)
  "Lifetime of the renewal code handed back in an answer envelope, seconds.

UNCHANGED AT HALF AN HOUR, deliberately: this is the value the field is already running, and the
point of splitting it out of *LOGIN-TTL* was to shorten the LINK without touching this.  A renewal
that expires is not a lockout — the browser falls back to its device enrolment (*ENROLMENT-TTL*, a
day, renewed by use) — so the failure of a short renewal is invisible, gradual, and exactly the kind
of behaviour change that should be somebody's decision rather than a side effect of tightening
links.  Raise it toward *ENROLMENT-TTL* if you want `via=code' on a next-day cold load.")

(defun %secret-bytes (secret)
  "SECRET as 64-hex string, byte vector, or integer -> (unsigned-byte 8) vector."
  (etypecase secret
    (string (ironclad:hex-string-to-byte-array secret))
    (integer (ironclad:integer-to-octets secret :n-bits 256))
    (sequence (coerce secret '(vector (unsigned-byte 8))))))

(defun %token-mac (secret nonce exp)
  (let ((m (ironclad:make-mac :hmac (%secret-bytes secret) :sha256)))
    (ironclad:update-mac m (ironclad:ascii-string-to-byte-array
                            (format nil "glass-login|~a|~a" nonce exp)))
    (ironclad:byte-array-to-hex-string (ironclad:produce-mac m))))

(defun %constant-time-equal (a b)
  "Constant-time string compare — a MAC checked with STRING= is a timing oracle for that MAC."
  (and (stringp a) (stringp b) (= (length a) (length b))
       (loop with diff = 0
             for ca across a for cb across b
             do (setf diff (logior diff (logxor (char-code ca) (char-code cb))))
             finally (return (zerop diff)))))

(defun mint-login-token (&key (ttl *login-ttl*) (secret *box-secret*) for)
  "Mint a login code valid for TTL seconds, keyed by the box SECRET.  NIL without an identity.

FOR NAMES THE RECIPIENT A LINK IS BEING SENT TO, and passing it is what makes this mint SUPERSEDE
that recipient's earlier outstanding codes — see RECORD-MINT.  It is not part of the token: the MAC
is over `glass-login|nonce|exp' and nothing else, frozen, so the binding is the STORE's and the
token stays byte-compatible with every link ever issued.  That separation is what lets a link be
redeemed by a browser's own device key while still being cancellable by the person it was sent to.

WITHOUT :FOR NOTHING IS RECORDED and nothing is superseded, which is the correct behaviour for the
two mints that have no recipient: the renewal that rides back in an answer envelope (it belongs to
whoever is already in the session) and the gateway's allowlist top-up.  A renewal that joined a
supersede group would cancel its own predecessor — or, on a reconnect race, itself — and lock a
person out of the terminal they are sitting at.

Returns (values TOKEN SUPERSEDED), where SUPERSEDED is how many of the recipient's earlier
outstanding codes this mint cancelled — NIL when there was no :FOR.  The `link' command says so in
its reply, because a person who taps an older link afterwards is going to be refused and the only
place that can be explained is the message carrying the replacement."
  (when secret
    (ignore-errors
     (let* ((nonce (ironclad:byte-array-to-hex-string (ironclad:random-data 16)))
            (exp (+ (unix-now) ttl))
            (token (format nil "~a.~a.~a" nonce exp (%token-mac secret nonce exp))))
       ;; recorded AFTER the token exists and before it is handed out.  If the store cannot be
       ;; written the code is still a valid single-use code — it simply supersedes nothing — which
       ;; is the right way for this to degrade: a failure here must not stop somebody logging in.
       (values token (when for (record-mint nonce exp (normalize-pubkey for))))))))

(defun verify-login-token (token &key (secret *box-secret*))
  "Verify TOKEN against the box SECRET.  Returns (values OK NONCE EXP): OK is T only if the MAC
checks out AND the code has not expired.  A good MAC on an expired code answers (NIL nonce exp),
which is what lets a denial say `expired' rather than `bad' and be diagnosable from the far end."
  (when (and secret (stringp token))
    (let ((dots (loop for i from 0 for c across token when (char= c #\.) collect i)))
      (when (= (length dots) 2)
        (let* ((nonce (subseq token 0 (first dots)))
               (exp-s (subseq token (1+ (first dots)) (second dots)))
               (mac   (subseq token (1+ (second dots))))
               (exp   (ignore-errors (parse-integer exp-s)))
               (want  (and exp (ignore-errors (%token-mac secret nonce exp)))))
          (when (and exp want (%constant-time-equal mac want))
            (values (> exp (unix-now)) nonce exp)))))))

(defun login-token-status (token)
  "Classify TOKEN: :OK, :EXPIRED, :BAD (wrong MAC or malformed), or :ABSENT.  Distinct reasons, so
a denied login can be told apart from a login that was never attempted.

CRYPTOGRAPHY ONLY — this asks nothing of the redemption store and takes no lock, so it stays a pure
function of the token and the box secret.  :OK here means `this box minted it and it has not
expired', which is a strictly weaker claim than `this key may use it': whether it has already been
traded is ADMIT-PEER's question, because the answer depends on WHO is asking."
  (if (or (not (stringp token)) (zerop (length token)))
      :absent
      (multiple-value-bind (ok nonce) (verify-login-token token)
        (cond ((null nonce) :bad)
              ((not ok) :expired)
              (t :ok)))))

;;; ---- the allowlist -----------------------------------------------------------
;;;
;;; The identities this desktop belongs to.  A sender is the VERIFIED seal signer from a gift wrap
;;; (a forged rumour pubkey is rejected before anything here sees it), so an allowlist hit is a real
;;; cryptographic identity and not a claim.
;;;
;;; UNSET MEANS DENY EVERYONE.  With no allowlist there is nobody to authorise, and answering "yes"
;;; to that would be the least defensible default in the file.
;;;
;;; REFRESHABLE AT RUN TIME, which the gateway's version was not.  There it was resolved once at
;;; process start, inside IGNORE-ERRORS, so a NIP-05 lookup that failed at that instant left an
;;; empty allowlist on a gateway that started anyway — fail-open-to-empty, discoverable only by
;;; being locked out.  Here the same call can simply be made again, from the control socket.

(defvar *nostr-allow* '()
  "Authorised client pubkeys, lowercase 64-hex.  Empty means deny everyone.")

(defun normalize-pubkey (s)
  "npub1… / 64-hex / name@domain (NIP-05) -> lowercase 64-hex; blank or unresolvable -> NIL."
  (when (stringp s)
    (let ((s (string-trim '(#\Space #\Tab #\Newline #\Return) s)))
      (cond
        ((zerop (length s)) nil)
        ((ignore-errors (cl-nostr.nip05:nip05-address-p s))
         (ignore-errors (string-downcase (cl-nostr.nip05:resolve-pubkey s))))
        ((and (>= (length s) 4) (string-equal (subseq s 0 4) "npub"))
         (ignore-errors (string-downcase
                         (cl-nostr.util:bytes->hex (cl-nostr.bech32:npub-decode s)))))
        ((and (= (length s) 64) (every (lambda (c) (digit-char-p c 16)) s)) (string-downcase s))
        (t nil)))))

(defun refresh-nostr-allow (&optional (spec (or (sb-ext:posix-getenv "GLASS_NOSTR_ALLOW")
                                                (sb-ext:posix-getenv "NOSTR_ALLOW"))))
  "Re-read the allowlist from SPEC, a comma-separated list of npub / 64-hex / NIP-05 addresses.

Callable at any time — from the desktop's control socket, say — so adding an owner needs no restart,
and a NIP-05 address that could not be resolved at boot can be resolved later instead of silently
costing somebody their access until the next deploy."
  (setf *nostr-allow*
        (when spec
          (remove nil (mapcar #'normalize-pubkey
                              (remove "" (%split-on spec #\,) :test #'string=)))))
  *nostr-allow*)

(defun allowed-pubkey-p (pubkey)
  "T iff PUBKEY is on the allowlist.  No allowlist => NIL (deny all)."
  (and (stringp pubkey) *nostr-allow*
       (member (string-downcase pubkey) *nostr-allow* :test #'string=)
       t))

;;; ---- the enrolment store -----------------------------------------------------
;;;
;;; A browser cannot hold a person's Nostr identity without a signer, so each page keeps its OWN key
;;; and signs with it.  A page admitted on a valid code is ENROLLED for *ENROLMENT-TTL*, after which
;;; it can ask for a fresh link itself over the same authenticated DM channel, with nobody involved.
;;;
;;; ==============================================================================================
;;; A RECORD, NOT A ROW — and `revoke' MARKS rather than DELETES
;;; ==============================================================================================
;;;
;;; This store was `<pubkey> <expiry>' and revocation deleted the line.  Both halves of that are
;;; fine right up to the moment somebody needs them, and then neither is: when the ALREADY REDEEMED
;;; alarm fires (see ADMIT-PEER), the questions are HOW DID THIS KEY GET IN, WHEN, ON WHOSE
;;; AUTHORITY, and WAS IT REMOVED OR DID IT SIMPLY LAPSE — and a two-field row cannot answer one of
;;; them.  Worse, the answer was destroyed by the very act performed in response to the alarm:
;;; revoking deleted the evidence it was performed for.
;;;
;;; So an enrolment is an ENROLMENT object with a state, and `revoke' moves it to :REVOKED with the
;;; time and the cause rather than removing it.  A revoked terminal is findable afterwards, and it
;;; is DISTINGUISHABLE from one that merely lapsed, which is the distinction the whole thing is for.
;;;
;;; ONE RECORD PER KEY, HOLDING ITS CURRENT STATE.  Not an append-only log: warp's DESIGN.md rule 4
;;; is explicit that a presentation stream carries state and that event history is a different
;;; product, read from a command log.  This store is what the panel projects, so it is state; a key
;;; that is revoked and later re-admitted has its record moved back to :active with the cause saying
;;; so, and the box does not pretend to keep a timeline it has nowhere to put.
;;;
;;; PROVENANCE IS HOW IT GOT IN, NOT HOW IT LAST CONNECTED.  VIA/NONCE/FOR are stamped when the key
;;; is first enrolled and are NOT overwritten by the renewals that follow — every subsequent
;;; admission of an enrolled terminal is `device', and a record that reported the last one would
;;; answer "how did this key get in" with "it was already in", which is no answer at all.
;;;
;;; ==============================================================================================
;;; RETENTION, AND WHY ZERO HAS TO MEAN ZERO
;;; ==============================================================================================
;;;
;;; A record that outlives its enrolment is a log of who connected to this desktop and when.  On the
;;; owner's own box that is exactly what you want after an alarm; on a SHARED box it is surveillance
;;; nobody asked for.  So *AUDIT-RETENTION* is a dial with a real zero: at zero, a settled record is
;;; dropped the instant it settles AND the forensic fields are never recorded in the first place, so
;;; the file on disk is `<pubkey> <expiry>' — byte for byte the store this box kept before any of
;;; this.  An opt-out that still wrote the timestamps and merely declined to keep them would be a
;;; setting that lies.
;;;
;;; ==============================================================================================
;;; THE FILE, AND WHAT ELSE READS IT
;;; ==============================================================================================
;;;
;;;     <pubkey> <expiry> <state> <created> <seen> <via|-> <nonce|-> <for|-> <since> <cause|->
;;;
;;; THE FIRST TWO FIELDS ARE WHAT THEY ALWAYS WERE, and that is a compatibility decision rather
;;; than an accident of layout.  warp-monitor's READ-DEVICES, `cut -d" " -f1,2', and every shell
;;; one-liner anybody has written against this file take the pubkey and the expiry positionally;
;;; appending is the only change that leaves all of them working.  A two-field line still LOADS —
;;; that is the migration, and it needs no conversion step, no version marker and no flag day.
;;;
;;; AND A SETTLED RECORD READS AS LAPSED TO A READER THAT DOES NOT KNOW ABOUT STATES.  Revoking
;;; sets the expiry to the instant of revocation as well as the state, so an old reader sees a past
;;; expiry — not enrolled — which is the safe direction and the true one.  Getting this wrong is the
;;; one way keeping the line could have been worse than deleting it: a revoked terminal that an
;;; unaware reader reported as enrolled.
;;;
;;; That is a deliberate trust delegation — the device key becomes a bearer credential — so it is
;;; bounded two ways: it only ever comes from a session that already authenticated, and it lapses
;;; unless refreshed by connecting.
;;;
;;; THE FILE IS THE SOURCE OF TRUTH AND THIS TABLE IS A CACHE OF IT.  SYNC-ENROLMENTS re-reads on an
;;; mtime change, which is what lets a shell one-liner, an admin app, or warp's device panel revoke
;;; a terminal and have it honoured on the next check with no restart anywhere.  Persistence is not
;;; a convenience: an in-memory set would silently un-enrol every terminal on any restart.
;;;
;;; ONE LOCK, HELD ACROSS THE WHOLE OF EVERY OPERATION, and it has to be.  This used to sync outside
;;; the mutex and then take it, and to open the file for :SUPERSEDE outside it as well, which is a
;;; real race and not a theoretical one:
;;;
;;;   thread A enrols and enters SAVE-ENROLMENTS.  Opening the file truncates it.  Before A writes a
;;;   byte, thread B calls DEVICE-ENROLLED-P, sees a changed mtime, CLRHASHes and reloads — from the
;;;   empty file.  A then finishes writing and records the final mtime, so the next sync sees no
;;;   change and never reloads.  The file is correct and THE TABLE IS EMPTY, and every terminal is
;;;   un-enrolled until something else touches the file.
;;;
;;; It went unnoticed because nothing ever admitted two peers at the same instant — until the
;;; concurrency proof for the login-code store started doing exactly that, and caught it.  So the
;;; file operations moved inside the lock: %-prefixed helpers assume it is HELD, the exported names
;;; take it, and no public call in this section leaves the table visible mid-rewrite.

(defparameter *enrolment-ttl*
  (or (ignore-errors (parse-integer (or (sb-ext:posix-getenv "GLASS_DEVICE_TTL")
                                        (sb-ext:posix-getenv "DEVICE_TTL"))))
      86400)
  "How long a terminal stays enrolled without connecting, seconds.  Renewed on every admission, so
an active terminal keeps working and an idle one lapses.  DEVICE_TTL is honoured as a fallback for
the reason *LOGIN-TTL* gives about LINK_TTL: it is the name the launcher already exports.")

(defparameter *audit-retention*
  (let ((s (%blank->nil (or (sb-ext:posix-getenv "GLASS_AUDIT_RETENTION")
                            (sb-ext:posix-getenv "GLASS_DEVICE_RETENTION")))))
    (or (and s (ignore-errors (max 0 (parse-integer s)))) 1209600))
  "How long a record is kept after the thing it describes stopped being usable, seconds.  Two weeks.

ONE POLICY, TWO STORES, AND DELIBERATELY NOT THE SAME CLOCK AS EITHER.  This is the FORENSIC
lifetime — how long the box remembers what happened — and it is a different question from how long
anything is VALID FOR.  The enrolments keep it after they lapse or are revoked; the login codes keep
a redemption after the code itself has died.  That second one is the whole reason this is its own
number: a code is redeemable for MINUTES, because the MAC refuses it after that with no help from
any store, and a redemption record is worth having for WEEKS, because it is the only thing that can
answer `who traded that link'.  One number for both gives you an unbounded store (if you take the
long one) or an empty audit trail exactly for the codes that mattered (if you take the short one).

ZERO IS A REAL OPT-OUT AND NOT A SMALL NUMBER.  At zero a settled record is dropped the moment it
settles, and the fields that constitute a log — created, last-seen, via, nonce, recipient, cause —
are never written at all, so `.glass-devices' is the two-field file it always was.  A shared box may
simply not want a record of when its owner connected, and a privacy setting that still recorded
everything and merely pruned it sooner would be worth nothing.

GLASS_DEVICE_RETENTION is honoured as a fallback name because that is what the dial was called while
this was only about the enrolment store.")

(defun audit-retained-p ()
  "T iff this box keeps records after they stop being usable.  The one place the opt-out is read, so
it cannot be half-implemented in one store and not the other."
  (plusp *audit-retention*))

(defvar *enrolment-file*
  (or (sb-ext:posix-getenv "GLASS_DEVICE_FILE")
      (namestring (merge-pathnames ".glass-devices" (user-homedir-pathname))))
  "Where enrolments are persisted, one record per line:

    <pubkey> <expiry> <state> <created> <seen> <via|-> <nonce|-> <for|-> <since> <cause|->

THE FIRST TWO FIELDS ARE THE FORMAT THE WEBRTC GATEWAY WROTE, unchanged and still first, so
warp-monitor's reader and every shell one-liner that takes `$1 $2' go on working and an existing
store is loaded with no conversion.  A line with only those two fields IS the migration: it reads as
an active enrolment whose history this box was not keeping yet.  DEFVAR, so a hot-load cannot move
the store out from under a running desktop.

THE LOGIN-CODE STORE IS DERIVED FROM THIS PATH (`<this>.codes', see LOGIN-CODE-FILE), so redirecting
this one redirects both.")

;;; CLOS AND NOT DEFSTRUCT, for the reason given at LOGIN-CODE and in the modus stack generally:
;;; this is long-lived state in an image that is patched while it is serving people, and a DEFSTRUCT
;;; whose slots change strands every instance already made.

(defclass enrolment ()
  ((pubkey  :initarg :pubkey  :reader   enrolment-pubkey)
   (expiry  :initarg :expiry  :accessor enrolment-expiry)
   (state   :initarg :state   :accessor enrolment-state   :initform :active)
   (created :initarg :created :accessor enrolment-created :initform 0)
   (seen    :initarg :seen    :accessor enrolment-seen    :initform 0)
   (via     :initarg :via     :accessor enrolment-via     :initform nil)
   (nonce   :initarg :nonce   :accessor enrolment-nonce   :initform nil)
   (issued  :initarg :issued  :accessor enrolment-issued  :initform nil)
   (since   :initarg :since   :accessor enrolment-since   :initform 0)
   (cause   :initarg :cause   :accessor enrolment-cause   :initform nil))
  (:documentation "One enrolled terminal, and how it came to be one.

  PUBKEY   the browser's own device key — the bearer credential this record admits
  EXPIRY   when the enrolment stops being valid.  For a REVOKED record this is the instant of
           revocation, which is what makes a reader that knows nothing about states still correct.
  STATE    :ACTIVE / :EXPIRED / :REVOKED
  CREATED  when this key was FIRST enrolled; survives renewal and re-admission
  SEEN     the last admission that renewed it
  VIA      what admitted it: :CODE / :ALLOWLIST / :DEVICE — how it GOT in, not how it last connected
  NONCE    for a code admission, the nonce of the code that was traded for this enrolment
  ISSUED   who that code was minted for — the RECIPIENT of the link, which is the answer to `on
           whose authority', and is usually a different key from PUBKEY (a link is DM'd to a
           person's npub and redeemed by a browser's device key)
  SINCE    when the record entered its current state
  CAUSE    why — `first-admission', `lapsed', `revoked-by-<8hex>' …

ISSUED and not FOR, because FOR is a symbol in COMMON-LISP and a slot reader named for it would be a
package-lock violation on the first accessor call.  The wire and the file both spell it `for'."))

(defmethod print-object ((e enrolment) stream)
  (print-unreadable-object (e stream :type t)
    (format stream "~a… ~(~a~)" (subseq (slot-value e 'pubkey) 0 (min 8 (length (slot-value e 'pubkey))))
            (slot-value e 'state))))

(defvar *enrolments* (make-hash-table :test 'equal)
  "Pubkey -> ENROLMENT.  Holds settled records too, which is why nothing may read a value out of
here and treat it as an enrolment without asking ENROLMENT-LIVE-P.")
(defvar *enrolments-lock* (sb-thread:make-mutex :name "glass-enrolments"))
(defvar *enrolments-mtime* nil)

;;; ---- reading a record -------------------------------------------------------

(defun enrolment-live-p (e &optional (now (unix-now)))
  "T iff E is an enrolment that would admit its key right now.  ACTIVE AND UNEXPIRED, and both
halves are load-bearing: a revoked record keeps its line, and a lapsed one keeps its expiry."
  (and (typep e 'enrolment) (eq (enrolment-state e) :active) (> (enrolment-expiry e) now)))

(defun %settle-enrolment (e now)
  "Move an ACTIVE record whose expiry has passed to :EXPIRED, at the instant it expired.

LAZILY, when somebody looks, because there is no clock in this file and there does not need to be:
the transition is a function of the expiry that is already written down, so computing it on read
gives the same answer a timer would and cannot drift."
  (when (and (eq (enrolment-state e) :active) (<= (enrolment-expiry e) now))
    (setf (enrolment-state e) :expired
          (enrolment-since e) (enrolment-expiry e)
          (enrolment-cause e) (and (audit-retained-p) "lapsed")))
  e)

(defun %enrolment-keep-p (e now)
  "T iff E is still worth a line: live, or settled inside the retention window."
  (or (enrolment-live-p e now)
      (and (audit-retained-p) (> (+ (enrolment-since e) *audit-retention*) now))))

(defun %enrolment-line (e)
  "E as one file/wire line.  Two fields with retention off — which is the old format exactly, and
the opt-out: with nothing retained there is no state but :ACTIVE to record and no history to keep."
  (if (audit-retained-p)
      (format nil "~a ~a ~(~a~) ~a ~a ~(~a~) ~a ~a ~a ~a"
              (enrolment-pubkey e) (enrolment-expiry e) (enrolment-state e)
              (enrolment-created e) (enrolment-seen e)
              (%dash (enrolment-via e)) (%dash (enrolment-nonce e)) (%dash (enrolment-issued e))
              (enrolment-since e) (%dash (enrolment-cause e)))
      (format nil "~a ~a" (enrolment-pubkey e) (enrolment-expiry e))))

(defun %parse-enrolment (line)
  "One line -> an ENROLMENT, or NIL if it is not one.

A TWO-FIELD LINE IS AN ACTIVE ENROLMENT WITH NO HISTORY, which is what the old store's lines are and
what a retention-zero box writes.  An UNKNOWN STATE is dropped rather than guessed at: a row this
build cannot interpret must not become a terminal it silently admits."
  (let ((w (remove "" (%split-on line #\Space) :test #'string=)))
    (when (>= (length w) 2)
      (let ((pk (string-downcase (first w)))
            (exp (and (every #'digit-char-p (second w)) (%int (second w) nil)))
            (state (if (third w) (intern (string-upcase (third w)) :keyword) :active)))
        (when (and (plusp (length pk)) (integerp exp)
                   (member state '(:active :expired :revoked)))
          (make-instance 'enrolment
                         :pubkey pk :expiry exp :state state
                         :created (%int (fourth w)) :seen (%int (fifth w))
                         :via (let ((v (%undash (sixth w))))
                                (and v (intern (string-upcase v) :keyword)))
                         :nonce (let ((n (%undash (seventh w)))) (and n (string-downcase n)))
                         :issued (let ((f (%undash (eighth w)))) (and f (string-downcase f)))
                         :since (%int (ninth w) (if (eq state :active) 0 exp))
                         :cause (%undash (tenth w))))))))

;;; ---- internals.  ALL OF THESE REQUIRE *ENROLMENTS-LOCK* ALREADY HELD. ---------

(defun %load-enrolments ()
  "Merge the file into the table.  Does NOT clear first — %SYNC-ENROLMENTS decides that."
  (handler-case
      (with-open-file (s *enrolment-file* :if-does-not-exist nil)
        (when s
          (let ((now (unix-now)))
            (loop for line = (read-line s nil) while line do
              (let ((e (%parse-enrolment line)))
                (when e
                  (%settle-enrolment e now)
                  (when (and (%enrolment-keep-p e now)
                             (not (gethash (enrolment-pubkey e) *enrolments*)))
                    (setf (gethash (enrolment-pubkey e) *enrolments*) e))))))))
    (error () nil)))

(defun %prune-enrolments (now)
  "Settle what has expired and drop what retention no longer covers.  Returns how many went."
  (let ((dead '()))
    (maphash (lambda (pk e)
               (%settle-enrolment e now)
               (unless (%enrolment-keep-p e now) (push pk dead)))
             *enrolments*)
    (dolist (pk dead) (remhash pk *enrolments*))
    (length dead)))

(defun %save-enrolments ()
  "Rewrite the file from the table, records past retention dropped."
  (handler-case
      (let ((now (unix-now)))
        (%prune-enrolments now)
        (with-open-file (s *enrolment-file* :direction :output :if-exists :supersede
                                            :if-does-not-exist :create)
          (maphash (lambda (pk e) (declare (ignore pk)) (write-line (%enrolment-line e) s))
                   *enrolments*))
        ;; remember our own write, so the next SYNC does not re-read what we have just said
        (setf *enrolments-mtime* (ignore-errors (file-write-date *enrolment-file*))))
    (error () nil)))

(defun %enrolments-stale-p ()
  "T if the table holds values from before this file grew records.

THE HOT-PATCH GUARD, and it is the difference between this change being live-loadable and needing a
restart.  *ENROLMENTS* is a DEFVAR — deliberately, so a recompile cannot un-enrol every terminal —
so an image that loads this file over a running desktop keeps a table of `pubkey -> expiry INTEGER'.
Every one of those reads as `not enrolled' through ENROLMENT-LIVE-P, and %SYNC-ENROLMENTS would not
re-read, because the FILE has not changed.  The desktop would go on answering admissions and quietly
deny everybody until somebody touched the store.

O(1): the two shapes never mix — every writer after the patch writes records — so the first entry
settles it."
  (block nil
    (maphash (lambda (k v) (declare (ignore k)) (return (not (typep v 'enrolment)))) *enrolments*)
    nil))

(defun %sync-enrolments ()
  "Re-read if the file changed underneath us — or if this image was patched under a live table."
  (handler-case
      (let ((mt (file-write-date *enrolment-file*)))
        (unless (and (eql mt *enrolments-mtime*) (not (%enrolments-stale-p)))
          (setf *enrolments-mtime* mt)
          (clrhash *enrolments*)
          (%load-enrolments)))
    (error () nil)))

;;; ---- and the same three, taking the lock, for everybody else -----------------

(defun load-enrolments ()
  (sb-thread:with-mutex (*enrolments-lock*) (%load-enrolments)))

(defun save-enrolments ()
  (sb-thread:with-mutex (*enrolments-lock*) (%save-enrolments)))

(defun sync-enrolments ()
  "Re-read the store if the file changed underneath us."
  (sb-thread:with-mutex (*enrolments-lock*) (%sync-enrolments)))

(defun enrol-device (pubkey &optional (ttl *enrolment-ttl*) &key via nonce for)
  "Trust PUBKEY to ask for its own login links for TTL seconds.  Renews an existing enrolment.
Returns the enrolment's new EXPIRY (a unix time, so a caller can tell the terminal when it runs out),
or NIL if there was nothing to enrol.

VIA / NONCE / FOR are the PROVENANCE — what admitted this key, and for a code the nonce it traded
and the recipient that code was minted for.  They are stamped when the record is CREATED and left
alone on every renewal after it: see the section header.  They are also the reason ADMIT-PEER is the
only caller that passes them; anything else enrolling a key by hand genuinely does not know.

TTL IS STILL POSITIONAL, and the keywords come after it, because `(enrol-device k 3600)' is the call
that exists in scripts and in the gates.  The footgun that arrangement leaves — `(enrol-device k
:via :code)' would bind TTL to the keyword — is named here rather than designed away, because the
alternative renames the argument every existing caller passes.

SYNC, SET AND SAVE IN ONE CRITICAL SECTION.  Two peers admitted at the same instant otherwise race
each other's rewrite of the file, and a reader between them sees the truncation."
  (when (stringp pubkey)
    (let ((pk (string-downcase pubkey))
          (now (unix-now))
          (audit (audit-retained-p)))
      (sb-thread:with-mutex (*enrolments-lock*)
        (%sync-enrolments)
        (let* ((prior (gethash pk *enrolments*))
               (expiry (+ now ttl)))
          (if (enrolment-live-p prior now)
              ;; RENEWAL.  The expiry moves and the last-seen moves; nothing else does.
              (setf (enrolment-expiry prior) expiry
                    (enrolment-seen prior) (if audit now 0))
              ;; A NEW RECORD — first admission, or one coming back after lapsing or being revoked.
              ;; CREATED survives that, because "when did this key first get in" is still the
              ;; question, and the cause says which of the three this was.
              (setf (gethash pk *enrolments*)
                    (make-instance 'enrolment
                                   :pubkey pk :expiry expiry :state :active :since now
                                   :created (if audit (if prior (enrolment-created prior) now) 0)
                                   :seen (if audit now 0)
                                   :via (and audit via)
                                   :nonce (and audit nonce)
                                   :issued (and audit for)
                                   :cause (and audit
                                               (cond ((null prior) "first-admission")
                                                     ((eq (enrolment-state prior) :revoked)
                                                      "re-admitted-after-revoke")
                                                     (t "re-admitted"))))))
          (%save-enrolments)
          expiry)))))

(defun device-enrolled-p (pubkey)
  (and (stringp pubkey)
       (sb-thread:with-mutex (*enrolments-lock*)
         (%sync-enrolments)
         (enrolment-live-p (gethash (string-downcase pubkey) *enrolments*)))))

(defun device-enrolment (pubkey)
  "PUBKEY's record — whatever state it is in — or NIL if this box has never heard of it.
THE ANSWER TO A QUESTION ASKED AFTER THE FACT, which is why it does not filter: `we revoked that
terminal on Tuesday' is exactly what the caller is trying to find out."
  (when (stringp pubkey)
    (sb-thread:with-mutex (*enrolments-lock*)
      (%sync-enrolments)
      (gethash (string-downcase pubkey) *enrolments*))))

(defun list-enrolments ()
  "The live enrolments as ((pubkey . expiry) …), longest remaining first.  A lapsed or revoked
record is not returned even while the file still holds it: neither is an enrolled terminal.

THE SHAPE IS UNCHANGED, and deliberately so — warp's device panel and the admission service's
`devices' verb are both built on these pairs, and the records are offered beside this call rather
than through it (LIST-ENROLMENT-RECORDS) so that a richer store did not become a broken panel."
  (let ((now (unix-now)) (rows '()))
    (sb-thread:with-mutex (*enrolments-lock*)
      (%sync-enrolments)
      (maphash (lambda (pk e) (when (enrolment-live-p e now) (push (cons pk (enrolment-expiry e)) rows)))
               *enrolments*))
    (sort rows #'> :key #'cdr)))

(defun list-enrolment-records (&key (state :active))
  "The records this store holds, as ENROLMENT objects.

STATE selects: :ACTIVE (the live ones, the default and what a panel wants), :ALL (everything still
retained), or one of :EXPIRED / :REVOKED to ask the question that made this a record store — `what
happened to that terminal'.  Live records first, longest remaining first; settled ones after,
most recently settled first, because that is the order somebody reading after an alarm wants."
  (let ((now (unix-now)) (rows '()))
    (sb-thread:with-mutex (*enrolments-lock*)
      (%sync-enrolments)
      (%prune-enrolments now)
      (maphash (lambda (pk e)
                 (declare (ignore pk))
                 (when (case state
                         (:active (enrolment-live-p e now))
                         (:all t)
                         (t (eq (enrolment-state e) state)))
                   (push e rows)))
               *enrolments*))
    (sort rows (lambda (a b)
                 (let ((la (enrolment-live-p a now)) (lb (enrolment-live-p b now)))
                   (cond ((not (eq la lb)) la)
                         (la (> (enrolment-expiry a) (enrolment-expiry b)))
                         (t (> (enrolment-since a) (enrolment-since b)))))))))

(defun find-enrolments (prefix &key (state :all))
  "The records whose pubkey starts with PREFIX (four characters and up), in any state by default.
This is the `the alarm named a key — what do we know about it' call."
  (when (and (stringp prefix) (>= (length prefix) 4))
    (let ((prefix (string-downcase prefix)))
      (remove-if-not (lambda (e)
                       (let ((pk (enrolment-pubkey e)))
                         (and (>= (length pk) (length prefix))
                              (string= prefix (subseq pk 0 (length prefix))))))
                     (list-enrolment-records :state state)))))

(defun enrolment-count () (length (list-enrolments)))

(defun revoke-enrolments (arg &key by)
  "Revoke the terminals matching ARG — a pubkey prefix, or \"all\".  Returns the list revoked.

IT MARKS; IT DOES NOT DELETE.  The record moves to :REVOKED with the time and the cause, and its
EXPIRY is set to that same instant — so it stops admitting anybody immediately (which is the whole
job), it reads as lapsed to any reader that does not know about states, and it is still there
afterwards to answer what was done and by whom.  Deleting the line destroyed the evidence the
revocation was performed for, which is the one thing you want most at that exact moment.

BY names the authority — the invoking pubkey, from the DM surface or from warp's panel — and lands
in the cause as `revoked-by-<first-8>'.  It is a keyword and optional because REVOKE-ENROLMENTS is
also called from a REPL where the authority is `whoever is at the keyboard'.

ONLY LIVE RECORDS MATCH.  Revoking something that already lapsed would rewrite its cause and lose
the true one, and nothing is gained: it admits nobody either way.

The prefix is matched at four characters and up while the surface advertises eight.  Kept as it was:
a prefix refused for being one character short is worse than a prefix that is generous, and the
operator typed it themselves — the ADVERTISED length is the one to copy."
  (let ((killed '())
        (now (unix-now)))
    (sb-thread:with-mutex (*enrolments-lock*)
      (%sync-enrolments)
      (let ((cause (if by (format nil "revoked-by-~a" (subseq (%one-line by) 0 (min 8 (length by))))
                       "revoked")))
        (flet ((revoke (e)
                 (setf (enrolment-state e) :revoked
                       (enrolment-expiry e) now
                       (enrolment-since e) now
                       (enrolment-cause e) (and (audit-retained-p) cause))
                 (push (enrolment-pubkey e) killed)))
          (cond
            ((and (stringp arg) (string-equal arg "all"))
             (maphash (lambda (pk e) (declare (ignore pk))
                        (when (enrolment-live-p e now) (revoke e)))
                      *enrolments*))
            ((and (stringp arg) (>= (length arg) 4))
             (let ((arg (string-downcase arg)))
               (maphash (lambda (pk e)
                          (when (and (enrolment-live-p e now)
                                     (>= (length pk) (length arg))
                                     (string= arg (subseq pk 0 (length arg))))
                            (revoke e)))
                        *enrolments*))))))
      (%save-enrolments))
    killed))

;;; ---- the login-code store: what became of every code this box minted ---------
;;;
;;; A code used to be a pure function of the box secret: mint it, and from then on the only facts
;;; about it were its MAC and its expiry.  That made two things impossible to say, and this store
;;; exists to say both.
;;;
;;; ONE — A CODE IS REDEEMED ONCE, BY ONE KEY.  The first key to present a code binds it and is
;;; enrolled; the code has been TRADED for the enrolment and there is nothing left to spend.  That
;;; same key may present it again as often as it likes and is admitted every time.  A DIFFERENT key
;;; is refused, and that refusal is worth waking somebody for: the code went to exactly one npub
;;; inside a gift wrap, so two keys holding it means the link leaked.  A successful login therefore
;;; MEANS `nobody else used this code', which turns interception from undetectable into detected.
;;;
;;; TWO — A CODE NOBODY TAPPED STOPS BEING LIVE WHEN IT IS CLEARLY UNNECESSARY.  Single use does
;;; nothing about the commonest shape there is: ask for a link, tap nothing, ask again.  The second
;;; link works, the person is content, and the first one is still armed in a DM until its TTL runs
;;; out.  So MINTING SUPERSEDES: a link minted FOR a recipient invalidates that recipient's earlier
;;; outstanding codes, in the same critical section that mints it.
;;;
;;; THE ENDS A CODE CAN COME TO, kept as distinct reasons and not collapsed to a boolean, because a
;;; person locked out deserves to be told which one happened and the three have different answers:
;;;
;;;   :outstanding  minted, not yet presented.  Live.
;;;   :redeemed     traded, and REDEEMED-BY names the key that traded it.  Live for that key alone.
;;;   :superseded   a newer link was minted for the same recipient.  Ask for a fresh one.
;;;                 (Expiry is the fourth end and is not a state: an expired row is simply pruned,
;;;                  because VERIFY-LOGIN-TOKEN refuses the code at the same instant with no help.)
;;;
;;; WHAT DOES *NOT* INVALIDATE A CODE, and this is a decision rather than an omission:
;;;
;;;   AN ADMISSION DOES NOT BURN THE ADMITTED IDENTITY'S OTHER CODES.  "They are already logged in,
;;;   so kill their outstanding links" sounds right and has a trap in it.  A phone connected on its
;;;   enrolment and a laptop that was just sent a link are the same person; burning on the phone's
;;;   next reconnect kills the laptop's link before anybody taps it, and the laptop then fails
;;;   EXACTLY the way an intercepted code fails — which is the one distinction this whole store was
;;;   built to make legible.  The rule kept instead is the narrower one that cannot misfire:
;;;   A CODE IS INVALIDATED ONLY BY A LATER MINT TO THE SAME RECIPIENT.  It satisfies "a code minted
;;;   after the last admission survives" by construction — nothing about admission is consulted —
;;;   and it still kills the shape that motivated any of this, because asking again IS a later mint.
;;;
;;;   A RENEWAL SUPERSEDES NOTHING, because it has no recipient.  Every admitted `code'/`device'
;;;   session is handed a fresh code in the answer envelope, minted with a fresh random nonce and no
;;;   :FOR — so it is never recorded at mint time, never joins a supersede group, and cannot cancel
;;;   the link somebody is walking to another room to tap.  A renewal that superseded its own
;;;   predecessor, or worse itself, would lock a user out of their own terminal on reconnect; that
;;;   is the failure this feature is most at risk of introducing, so nostr-gate.lisp walks a
;;;   twenty-deep renewal chain rather than reasoning about it.
;;;
;;; THE THREE THINGS THAT MAKE IT TRUE, in the order they are easy to get wrong:
;;;
;;;   ATOMICITY.  Check-and-bind is ONE critical section, and so is supersede-and-insert — each
;;;   takes the store's lock, reloads, prunes, decides and persists without releasing it.  A gap
;;;   anywhere in there lets two racing redemptions both see a nonce free, or two racing mints both
;;;   survive, and either is the entire property gone to a missing lock.  Note the shape difference
;;;   from ENROL-DEVICE, which syncs OUTSIDE its mutex and then takes it: that is fine for a store
;;;   where the LAST writer wins, and fatal for one where the FIRST writer must.
;;;
;;;   PERSISTENCE.  A restart that forgot would silently restore reusability — the feature would
;;;   test green on a live desktop for the fifteen minutes before its next deploy and be gone
;;;   afterwards.  So it is a file, in the same shape and beside the same store as the enrolments,
;;;   and pruned at each code's own expiry so it stays a handful of lines: a code lives 15–30
;;;   minutes, and a row for an expired code is dead weight the MAC check already refuses.
;;;
;;;   THE RENEWAL PATH, above.  Asserted rather than assumed.
;;;
;;; A SEPARATE FILE, DERIVED FROM THE ENROLMENT ONE, and both halves of that are deliberate.
;;; Separate, because `<pubkey> <expiry>' lines are a format other readers already parse — warp's
;;; device panel, a shell one-liner — and SAVE-ENROLMENTS rewrites the whole file, so a second
;;; record type sharing it would be either misparsed as a terminal or truncated away by the other
;;; writer.  Derived (`<enrolment-file>.codes') rather than independently configured, because the
;;; one thing that must never happen is a test or a second desktop writing the LIVE store: anything
;;; that redirects GLASS_DEVICE_FILE now redirects this too, in the same breath, with nothing to
;;; remember.  Resolved per call and not at load, so rebinding *ENROLMENT-FILE* moves both.

(defvar *login-code-file*
  (%blank->nil (sb-ext:posix-getenv "GLASS_CODE_FILE"))
  "Where the fate of minted codes is persisted, or NIL to derive one beside the enrolments.

One space-separated line per code, positional, `-' for a field that does not apply:

    <nonce-hex> <state> <expiry> <recipient|-> <redeemed-by|-> <minted-at> <settled-at>

Positional and greppable rather than structured, like every other store this desktop keeps: `grep
superseded' is the diagnostic, and a line is readable next to a log entry without a parser.  The
two timestamps are what make it an audit record instead of a set — MINTED-AT is what orders a
supersede group, and SETTLED-AT is what lets a leak report say the code was traded BEFORE its owner
ever tapped it, which is a different sentence from `somebody has your link'.")

(defun login-code-file ()
  "The path minted codes live at, resolved now rather than at load time."
  (or *login-code-file* (concatenate 'string (princ-to-string *enrolment-file*) ".codes")))

;;; CLOS AND NOT DEFSTRUCT, for the reason the modus stack defaults to it: this is long-lived state
;;; in an image that gets patched while it is serving people, and a DEFSTRUCT whose slots change
;;; strands every live instance, where UPDATE-INSTANCE-FOR-REDEFINED-CLASS migrates them.  A store
;;; that admits people is the last thing that should need a restart to grow a field — and this one
;;; grew two the same week it was written.

(defclass login-code ()
  ((nonce     :initarg :nonce     :reader code-nonce)
   (expiry    :initarg :expiry    :reader code-expiry)
   (state     :initarg :state     :accessor code-state    :initform :outstanding)
   (recipient :initarg :recipient :reader   code-recipient :initform nil)
   (redeemer  :initarg :redeemer  :accessor code-redeemer :initform nil)
   (minted    :initarg :minted    :reader   code-minted   :initform 0)
   (settled   :initarg :settled   :accessor code-settled  :initform 0))
  (:documentation "One login code, and what became of it.

RECIPIENT is who the link was sent to and is the key a supersede group is formed on; REDEEMER is the
key that actually traded it in.  They are DIFFERENT ROLES and usually different keys — a link is
DM'd to a person's npub and redeemed by the browser's own device key, which is the whole reason the
gateway tops an allowlist admission up with a bearer code at all.  Collapsing them into one `pubkey'
slot would make superseding and leak detection the same question, and they are not."))

(defclass login-code-store ()
  ((table  :initform (make-hash-table :test 'equal) :reader code-table)
   (lock   :initform (sb-thread:make-mutex :name "glass-login-codes") :reader code-lock)
   (source :initform nil :accessor code-source)
   (mtime  :initform nil :accessor code-mtime))
  (:documentation "Minted codes, and the file they came from.

SOURCE is the path last read or written, not merely the mtime: rebinding *ENROLMENT-FILE* points
LOGIN-CODE-FILE somewhere else entirely, and a store that only watched the mtime would happily
answer a question about the new file out of the old file's table."))

(defvar *login-codes* (make-instance 'login-code-store)
  "What became of the codes this desktop minted.  DEFVAR for the reason *BOX-SECRET* is one — a
hot-load of this file must not drop the set of codes already spent.")

;;; ---- internals.  ALL OF THESE REQUIRE THE STORE'S LOCK ALREADY HELD.  They are separate from
;;; the public calls for exactly that reason: a helper that took the lock itself would deadlock the
;;; critical sections this whole feature rests on.

(defun %code-redeemable-p (c now)
  "T iff C could still be traded in.  ITS OWN EXPIRY AND NOTHING ELSE — this is the short of the two
lifetimes, and the point of keeping it separate is that it does not lengthen because the record does."
  (> (code-expiry c) now))

(defun %code-keep-p (c now)
  "T iff C's row is still worth keeping: redeemable, or a SETTLED record inside retention.

THE TWO LIFETIMES, in one function.  A code is redeemable for minutes; what became of it is worth
remembering for weeks — but only if something BECAME of it.  An outstanding code that nobody ever
tapped is dropped at its expiry, because there is no redemption to record, the MAC refuses it at
that same instant, and keeping every link ever minted is how a store with a retention window turns
into a store with no bound."
  (or (%code-redeemable-p c now)
      (and (audit-retained-p)
           (member (code-state c) '(:redeemed :superseded))
           (> (+ (code-settled c) *audit-retention*) now))))

(defun %codes-load (store)
  "Re-read STORE if it is looking at a different file, or the file changed underneath it."
  (let* ((file (login-code-file))
         (mt (ignore-errors (file-write-date file))))
    (when (or (not (equal file (code-source store)))
              (not (eql mt (code-mtime store))))
      (setf (code-source store) file
            (code-mtime store) mt)
      (clrhash (code-table store))
      (handler-case
          (with-open-file (s file :if-does-not-exist nil)
            (when s
              (let ((now (unix-now)))
                (loop for line = (read-line s nil) while line do
                  (let ((w (remove "" (%split-on line #\Space) :test #'string=)))
                    (when (>= (length w) 7)
                      (let ((nonce (string-downcase (first w)))
                            (state (intern (string-upcase (second w)) :keyword))
                            (exp (ignore-errors (parse-integer (third w))))
                            (rcpt (%undash (fourth w)))
                            (by (%undash (fifth w)))
                            (minted (ignore-errors (parse-integer (sixth w))))
                            (settled (ignore-errors (parse-integer (seventh w)))))
                        ;; an unknown state is dropped rather than trusted: a row this build cannot
                        ;; interpret must not become a code it silently treats as live
                        (when (and exp (member state '(:outstanding :redeemed :superseded)))
                          (let ((c (make-instance 'login-code
                                                  :nonce nonce :expiry exp :state state
                                                  :recipient (and rcpt (string-downcase rcpt))
                                                  :redeemer (and by (string-downcase by))
                                                  :minted (or minted 0)
                                                  :settled (or settled 0))))
                            (when (%code-keep-p c now)
                              (setf (gethash nonce (code-table store)) c)))))))))))
        (error () nil)))))

(defun %codes-prune (store now)
  "Drop rows that are neither redeemable nor retained.  Returns how many went.

NEVER BEFORE THE CODE'S OWN EXPIRY — that is the same instant VERIFY-LOGIN-TOKEN starts refusing it,
so there is never a window where the row is gone and the code still works — and, for a code that was
actually traded or superseded, not until *AUDIT-RETENTION* has run out either.  This used to prune
at expiry full stop, which is exactly the collapse the two lifetimes exist to prevent: the record
of the redemption vanished minutes after the redemption, so the store was empty precisely for the
codes an investigation would have asked about."
  (let ((dead '()))
    (maphash (lambda (k c) (unless (%code-keep-p c now) (push k dead))) (code-table store))
    (dolist (k dead) (remhash k (code-table store)))
    (length dead)))

(defun %codes-save (store)
  "Write STORE out, rows past both lifetimes dropped.  Whole-file rewrite, like the enrolments."
  (handler-case
      (let ((file (login-code-file))
            (now (unix-now)))
        (with-open-file (s file :direction :output :if-exists :supersede
                                :if-does-not-exist :create)
          (maphash (lambda (k c)
                     (declare (ignore k))
                     (when (%code-keep-p c now)
                       (format s "~a ~(~a~) ~a ~a ~a ~a ~a~%"
                               (code-nonce c) (code-state c) (code-expiry c)
                               (%dash (code-recipient c)) (%dash (code-redeemer c))
                               (code-minted c) (code-settled c))))
                   (code-table store)))
        ;; remember our own write, so the next load does not re-read what we have just said
        (setf (code-source store) file
              (code-mtime store) (ignore-errors (file-write-date file))))
    (error () nil)))

;;; ---- the two public transitions ----------------------------------------------

(defun record-mint (nonce expiry recipient &key (store *login-codes*))
  "Record NONCE as outstanding for RECIPIENT, and supersede RECIPIENT's earlier outstanding codes.
Returns the number superseded, or NIL if there was nothing to record.

ONE CRITICAL SECTION, and that is the point rather than an implementation detail: two `link' DMs
answered at once must not both leave a live code behind, so supersede-and-insert cannot be two
calls.  ONLY :OUTSTANDING ROWS ARE SUPERSEDED — a code the recipient already redeemed is a terminal
they are sitting in front of, and a later link must not reach back and log them out of it."
  (when (and (stringp nonce) (plusp (length nonce)) (integerp expiry)
             (stringp recipient) (plusp (length recipient)))
    (let ((nonce (string-downcase nonce))
          (recipient (string-downcase recipient))
          (now (unix-now)))
      (sb-thread:with-mutex ((code-lock store))
        (%codes-load store)
        (%codes-prune store now)
        (let ((killed 0))
          (maphash (lambda (k c)
                     (declare (ignore k))
                     (when (and (eq (code-state c) :outstanding)
                                (equal (code-recipient c) recipient)
                                (not (equal (code-nonce c) nonce)))
                       (setf (code-state c) :superseded
                             (code-settled c) now)
                       (incf killed)))
                   (code-table store))
          (setf (gethash nonce (code-table store))
                (make-instance 'login-code :nonce nonce :expiry expiry :state :outstanding
                                           :recipient recipient :minted now))
          (%codes-save store)
          killed)))))

(defun redeem-nonce (nonce pubkey expiry &key (bind t) (store *login-codes*))
  "Trade NONCE, a code valid until EXPIRY, for PUBKEY's admission.

Returns (values OK STATE HOLDER RECIPIENT):
  OK      T if PUBKEY may use this code
  STATE   :BOUND       it was live and is now PUBKEY's        (first redemption)
          :AGAIN       PUBKEY already holds it                (the retry, and it passes)
          :FREE        it was live and BIND was NIL           (a probe; nothing was written)
          :TAKEN       another key holds it   (OK NIL — the link leaked)
          :SUPERSEDED  a newer link replaced it (OK NIL — ask for a fresh one)
          :EXPIRED     its own expiry has passed (OK NIL — and the ROW may still be here)
  HOLDER  on :TAKEN, the key that got there first
  RECIPIENT  who the code was minted FOR, when the store knows — the provenance an enrolment
             bought with this code records, and the answer to `on whose authority'

:EXPIRED IS THE SHORT LIFETIME, STATED.  A row now outlives its code by *AUDIT-RETENTION* so that
the redemption stays on the record, which means the row's presence can no longer be read as `this
code is live' — so redeemability is asked of the EXPIRY, first, before anything else is consulted.
ADMIT-PEER never reaches it (VERIFY-LOGIN-TOKEN refuses an expired code before this is called) and
that is exactly why it is written down here: the guard has no caller to keep it honest.

A NONCE WITH NO ROW AT ALL IS LIVE, and that is deliberate: a renewal token and a code from the
command-line link minter are never recorded at mint time, because neither has a recipient to form a
supersede group with.  They are recorded HERE, at the moment they are redeemed, which is all
single-use needs.

WITH BIND NIL THIS ONLY ASKS.  ADMIT-PEER passes its own :ENROL through, because a call that
deliberately does not enrol is a `would this peer be admitted' probe, and a probe that silently
spent somebody's code would be a worse bug than the one this fixes.

THE WHOLE BODY IS ONE CRITICAL SECTION.  Load, prune, look up, bind and persist happen without
releasing the lock, so N threads presenting one live code produce exactly one :BOUND and N-1
:TAKEN — never two winners.  nostr-gate.lisp runs that race rather than reading this paragraph."
  (when (and (stringp nonce) (stringp pubkey) (integerp expiry)
             (plusp (length nonce)) (plusp (length pubkey)))
    (let ((nonce (string-downcase nonce))
          (pubkey (string-downcase pubkey))
          (now (unix-now)))
      (sb-thread:with-mutex ((code-lock store))
        (%codes-load store)
        (let* ((dirty (plusp (%codes-prune store now)))
               (prior (gethash nonce (code-table store)))
               (rcpt (and prior (code-recipient prior)))
               (result
                 (cond
                   ((<= (if prior (code-expiry prior) expiry) now)
                    (list nil :expired (and prior (code-redeemer prior)) rcpt))
                   ((and prior (eq (code-state prior) :redeemed))
                    (if (equal (code-redeemer prior) pubkey)
                        (list t :again pubkey rcpt)
                        (list nil :taken (code-redeemer prior) rcpt)))
                   ((and prior (eq (code-state prior) :superseded))
                    (list nil :superseded nil rcpt))
                   ((not bind) (list t :free nil rcpt))
                   (prior                                    ; :outstanding — trade it in
                    (setf (code-state prior) :redeemed
                          (code-redeemer prior) pubkey
                          (code-settled prior) now
                          dirty t)
                    (list t :bound pubkey rcpt))
                   (t                                        ; a bare token: renewal, or the CLI
                    (setf (gethash nonce (code-table store))
                          (make-instance 'login-code :nonce nonce :expiry expiry :state :redeemed
                                                     :redeemer pubkey :minted now :settled now)
                          dirty t)
                    (list t :bound pubkey nil)))))
          (when dirty (%codes-save store))
          (values-list result))))))

;;; ---- reading it ---------------------------------------------------------------

(defun list-login-codes (&optional (store *login-codes*))
  "Every row the store still holds, as LOGIN-CODE objects, most recently settled first — the
redeemable ones AND the retained records of ones that are not.  Diagnostic only: nothing decides
anything from this list, because the decisions are made inside the lock, above.

A CALLER THAT WANTS `IS THIS CODE STILL USABLE' MUST ASK CODE-LIVE-P and not merely find a row.
That used to be the same question; retention is what separated them."
  (let ((now (unix-now)) (rows '()))
    (sb-thread:with-mutex ((code-lock store))
      (%codes-load store)
      (%codes-prune store now)
      (maphash (lambda (k c) (declare (ignore k)) (push c rows)) (code-table store)))
    (sort rows #'> :key (lambda (c) (max (code-settled c) (code-minted c))))))

(defun code-live-p (c &optional (now (unix-now)))
  "T iff C is still redeemable — its own expiry, which is the short of its two lifetimes."
  (and (typep c 'login-code) (%code-redeemable-p c now)))

(defun find-login-code (nonce &optional (store *login-codes*))
  "The row for NONCE, or NIL."
  (when (stringp nonce)
    (find (string-downcase nonce) (list-login-codes store) :key #'code-nonce :test #'equal)))

(defun login-code-count (&optional (store *login-codes*))
  "How many codes are still REDEEMABLE.  Not how many rows there are: a retained record is history,
and counting it here would make the number grow for two weeks after the last person logged in."
  (count-if #'code-live-p (list-login-codes store)))

(defun describe-login-codes (&optional (store *login-codes*))
  "The outstanding, spent and remembered codes in words, for an operator staring at a lockout."
  (let* ((rows (list-login-codes store)) (now (unix-now))
         (live (count-if (lambda (c) (code-live-p c now)) rows)))
    (if (null rows)
        "No codes are outstanding."
        (format nil "~a live code~:p~@[, ~a kept on the record~]:~%~{~a~%~}"
                live (and (> (length rows) live) (- (length rows) live))
                (mapcar (lambda (c)
                          (format nil "  ~a  ~10@a  ~a~@[  to ~a…~]~@[  by ~a…~]"
                                  (subseq (code-nonce c) 0 8)
                                  (string-downcase (symbol-name (code-state c)))
                                  (if (code-live-p c now)
                                      (format nil "expires in ~d min"
                                              (max 0 (round (- (code-expiry c) now) 60)))
                                      (format nil "dead ~d min ago"
                                              (max 0 (round (- now (code-expiry c)) 60))))
                                  (and (code-recipient c) (subseq (code-recipient c) 0 8))
                                  (and (code-redeemer c) (subseq (code-redeemer c) 0 8))))
                        rows)))))

;;; ---- admission ---------------------------------------------------------------
;;;
;;; THREE WAYS IN, CHECKED IN THIS ORDER, FIRST MATCH WINS, with deliberately different lifetimes:
;;;
;;;   code       a login token from a magic link.  Valid for its own TTL, redeemable by ONE key,
;;;              and it authorises INDEPENDENTLY of the allowlist — that is what a magic link is for.
;;;   allowlist  the identities this desktop belongs to.  Permanent.
;;;   device     a browser enrolled after arriving on a valid code.  *ENROLMENT-TTL*, renewed by use.
;;;
;;; ANY ADMITTED PEER IS ENROLLED, which is what makes a terminal keep working across reconnects and
;;; is also why one leaked link is durable access until somebody runs `revoke'.  Both halves of that
;;; are true, and the second is why `revoke' exists.
;;;
;;; A CODE THAT IS NO LONGER USABLE DOES NOT DEMOTE A PEER WHO HAS STANDING OF THEIR OWN.  :spent
;;; and :superseded kill the `code' row and nothing more, so an owner or an enrolled terminal that
;;; presents a code somebody else beat them to — or one a newer link replaced — is still let in the
;;; way they always were, and the STATUS carries the fact so the event is visible in the log of an
;;; admission that SUCCEEDED.
;;;
;;; AND REDEMPTION IS NOT CONTINGENT ON NEED.  A code that could have been redeemed IS redeemed,
;;; before anything asks whether the caller had another way in.  It looks like waste — the terminal
;;; was already enrolled, the code bought it nothing — and it is the point: an offer that carries a
;;; code and is admitted on an enrolment would otherwise leave that code LIVE, in a DM, for the rest
;;; of its TTL, having already been used by the person it was sent to.  The client sends the code
;;; whenever it has one for exactly this reason; the box must spend it whenever it is sent one, and
;;; the obvious optimisation ("we already have an enrolment, skip the code") is the bug.  It is
;;; stated here, and asserted in nostr-gate.lisp, because nothing about the code path's POSITION in
;;; the COND makes it true on purpose rather than by accident.

(defun admit-peer (pubkey code &key (enrol t) (ttl *renewal-ttl*))
  "Decide whether PUBKEY, holding CODE, may open this desktop.

Returns (values VIA TOKEN STATUS EXPIRY):
  VIA     :code / :allowlist / :device, or NIL when refused
  TOKEN   a fresh login code to hand back with the answer, or NIL
  EXPIRY  when this peer's enrolment runs out, unix — NIL when nothing was enrolled
  STATUS  the code's own status — ALWAYS, so a peer admitted by the allowlist while presenting a
          rotten code is visible rather than silently fine, and so a failure screen can eventually
          tell an unremarkable expiry from something worth investigating:
            :absent      none was offered
            :bad         malformed, or not minted by this box
            :expired     good MAC, past its expiry                      (unremarkable)
            :superseded  good, but a NEWER LINK for the same recipient replaced it
            :spent       good and live, but ANOTHER KEY REDEEMED IT     (the link leaked)
            :ok          good, live, and this key's to redeem

REDEMPTION HAPPENS HERE AND NOWHERE ELSE, in the same call that enrols and mints: the code is
traded for the enrolment, atomically, and a peer whose enrolment is the thing it bought can never
find the trade half-done.  It is attempted for EVERY offer that carries a live code, whether or not
the caller needed it, and bound only when ENROL is true — a non-enrolling call is a probe, and a
probe must not spend somebody's code (see REDEEM-NONCE's :BIND).

The renewal token is minted for a peer that came in on a code or as an enrolled device, and not for
one admitted purely by the allowlist — an owner has a signer and does not need a bearer credential
pushed at them.  That is the rule the field is already running; it is preserved exactly.  It is
minted with NO :FOR, so it joins no supersede group and cancels nothing, and with a fresh random
nonce, so it never arrives already spent.  TTL defaults to *RENEWAL-TTL* and not to *LOGIN-TTL*:
this is a credential at rest in localStorage, not one in transit through a DM.

AND IT SAYS WHEN THE ENROLMENT RUNS OUT, which is the fact the client could not otherwise have.  A
denial is answered with SILENCE by design — the box will not confirm to an unknown sender that it is
listening — so a browser holding a lapsed device key had no way to learn that except by offering
into the dark and concluding by timeout, half a minute of a page looking broken before it could even
offer the way back in.  The expiry rides the answer envelope the renewal code already rides, is
stored per box, and lets the NEXT cold load show the options at once.  It is a PREDICTION and the
client is told so: it still offers, because a working credential must never be pre-empted by a
guess about it."
  (multiple-value-bind (code-live nonce code-exp) (verify-login-token code)
    (let* ((crypto-status (cond ((or (not (stringp code)) (zerop (length code))) :absent)
                                ((null nonce) :bad)
                                ((not code-live) :expired)
                                (t :ok)))
           ;; REDEEMABLE and not merely :OK: a code presented without a pubkey cannot be bound to
           ;; one, and admitting on a code nobody can be held to would be a way to use a code
           ;; without redeeming it — the one hole this whole section exists to close.
           (redeemable (and (eq crypto-status :ok) (stringp pubkey) (plusp (length pubkey))))
           (holder nil)
           (recipient nil)
           (status (if (not redeemable)
                       crypto-status
                       ;; UNCONDITIONAL.  Not inside the COND below, not guarded by "unless the
                       ;; allowlist would have taken them anyway" — see the note above.
                       (multiple-value-bind (ok state who for)
                           (redeem-nonce nonce pubkey code-exp :bind enrol)
                         (setf holder who recipient for)
                         (cond (ok :ok)
                               ((eq state :superseded) :superseded)
                               (t :spent)))))
           (via (cond ((and (eq status :ok) redeemable) :code)
                      ((allowed-pubkey-p pubkey) :allowlist)
                      ((device-enrolled-p pubkey) :device)
                      (t nil))))
      ;; LOUD, AND ON PURPOSE.  Every other denial here is quiet because it is ordinary — a lapsed
      ;; terminal, a typo'd code, a link the owner replaced.  This one is not ordinary: two
      ;; different keys held one code that was delivered to exactly one npub inside a gift wrap, so
      ;; either that DM was read by somebody it was not for or the link was forwarded.  It is the
      ;; only signal this system has that an interception HAPPENED, and a signal nobody prints is a
      ;; signal nobody gets.  :superseded is deliberately NOT loud: that one is routine hygiene.
      (when (eq status :spent)
        (flet ((tag (s) (subseq s 0 (min 8 (length s)))))
          (format *error-output*
                  "~&@@ LOGIN CODE ALREADY REDEEMED — nonce ~a… was traded by ~a… and has just been~@
                   @@   presented by ~a…  One code, two keys: that link leaked.  The FIRST key is~@
                   @@   the one that is enrolled;  revoke ~a…  if it was not you.~%"
                  (tag nonce) (tag (or holder "?")) (tag pubkey) (tag (or holder "?")))
          (finish-output *error-output*)))
      ;; THE PROVENANCE IS RECORDED HERE OR NOWHERE.  This is the only place that knows all three
      ;; facts at once — which of the three doors opened, the nonce that was traded if it was the
      ;; code door, and who that code had been minted for.  A store asked afterwards can reconstruct
      ;; none of them, which is precisely the position the two-field file left us in.
      (let ((expiry (when (and via enrol)
                      (enrol-device pubkey *enrolment-ttl*
                                    :via via
                                    :nonce (and (eq via :code) nonce)
                                    :for recipient))))
        (values via
                (when (member via '(:code :device)) (mint-login-token :ttl ttl))
                status
                expiry)))))

;;; ---- the command surface -----------------------------------------------------
;;;
;;; The box has no HTTP and no console, but it does have an authenticated DM channel, so that is
;;; where administration lives.  A DM is treated as a command only if it is a SHORT string; anything
;;; longer is somebody's signalling and none of this file's business.
;;;
;;;   link                 a fresh magic link           allowlist or an enrolled device
;;;   devices              list enrolled terminals      ALLOWLIST ONLY
;;;   revoke <prefix|all>  un-enrol one or all          ALLOWLIST ONLY
;;;   help / ?             this list                    allowlist or device
;;;
;;; A DENIED COMMAND PRODUCES NO REPLY AT ALL, only a local log line.  Silence rather than "denied"
;;; is on purpose: a reply is an oracle telling an unknown sender that the box is here, is listening,
;;; and understood them.

(defun parse-nostr-command (payload)
  "A short text DM -> (values VERB ARG), or NIL if it is not a command.

`link' is matched as a SUBSTRING of the verb, which is why `blink' also mints one.  Kept that way
because it is what the deployed clients and the people using them have been typing at it, and a
surface that silently stops recognising a word somebody has in their message history is a worse bug
than a generous match."
  (when (and (stringp payload) (<= (length payload) 80))
    (let* ((txt (string-trim '(#\Space #\Tab #\Newline #\Return) (string-downcase payload)))
           (sp (position #\Space txt))
           (verb (if sp (subseq txt 0 sp) txt))
           (arg (and sp (string-trim '(#\Space) (subseq txt (1+ sp))))))
      (cond ((zerop (length txt)) nil)
            ((search "link" verb) (values :link nil))
            ;; `devices' carries its argument now — `devices all' asks for the settled records too.
            ;; A bare `devices' is what it always was, which is what an operator's muscle memory and
            ;; every message history are going to keep typing.
            ((string= verb "devices") (values :devices arg))
            ((string= verb "revoke") (values :revoke arg))
            ((or (string= verb "help") (string= verb "?")) (values :help nil))
            (t nil)))))

(defun %ago (seconds)
  "A duration in the coarsest unit that still says something: min / h / d."
  (let ((s (max 0 seconds)))
    (cond ((< s 3600) (format nil "~d min" (max 1 (round s 60))))
          ((< s 172800) (format nil "~,1f h" (/ s 3600.0)))
          (t (format nil "~,1f d" (/ s 86400.0))))))

(defun describe-enrolment (e &optional (now (unix-now)))
  "One record in one line: what it is, and — for a settled one — what happened to it and when."
  (format nil "  ~a  ~a~@[  via ~(~a~)~]~@[  first seen ~a ago~]~@[  ~a~]"
          (subseq (enrolment-pubkey e) 0 8)
          (if (enrolment-live-p e now)
              (format nil "expires in ~a" (%ago (- (enrolment-expiry e) now)))
              (format nil "~:@(~a~) ~a ago" (enrolment-state e)
                      (%ago (- now (enrolment-since e)))))
          (enrolment-via e)
          (and (plusp (enrolment-created e)) (%ago (- now (enrolment-created e))))
          (and (not (enrolment-live-p e now)) (enrolment-cause e))))

(defun describe-enrolments (&key (state :active))
  "Human-readable listing of enrolled terminals.  STATE :ALL includes the records of terminals that
lapsed or were revoked — which is the listing somebody wants AFTER something has gone wrong, and the
reason revoking marks instead of deleting."
  (let ((rows (list-enrolment-records :state state)) (now (unix-now)))
    (if (null rows)
        (if (eq state :active) "No terminals are enrolled." "Nothing is on the record.")
        (format nil "~a ~:[record~;enrolled terminal~]~p:~%~{~a~%~}~@
                     Use \"revoke <first-8>\" or \"revoke all\"~:[~;, \"devices all\" for the record~]."
                (length rows) (eq state :active) (length rows)
                (mapcar (lambda (e) (describe-enrolment e now)) rows)
                (eq state :active)))))

(defun describe-revoke (arg &key by)
  "Run a `revoke' and report it in words.  BY is the authority it is performed on, and it goes onto
each record's cause — `on whose authority' being one of the questions the record exists to answer."
  (if (null arg)
      "Usage: revoke <first-8-of-pubkey> | revoke all"
      (let ((killed (revoke-enrolments arg :by by)))
        (if (null killed)
            (format nil "Nothing matched \"~a\"." arg)
            (format nil "Revoked ~a terminal~:p:~%~{  ~a~%~}~:[~;~
                         They stay on the record, revoked, until the retention window runs out.~]"
                    (length killed)
                    (mapcar (lambda (pk) (subseq pk 0 8)) killed)
                    (audit-retained-p))))))

(defvar *login-url-base*
  (or (%blank->nil (sb-ext:posix-getenv "GLASS_LOGIN_URL_BASE"))
      (%blank->nil (sb-ext:posix-getenv "LOGIN_URL_BASE")))
  "The published client a magic link points at.  It must name a PATH (…/k23.html), not a ?v= query:
an nsite gateway resolves a request by path against its manifest, so a query string selects the same
blob and the browser is free to keep serving its cached copy.

NIL means `link' has nowhere to send anybody, and says so rather than handing out half a URL.

A VARIABLE, AND THAT IS THE FIX.  This used to be filled in once from an environment variable a
DIFFERENT process wrote into a file — and the day the `link' command moved out of the gateway and
into this image, the reader that was re-reading that file every three seconds stopped existing and
nothing noticed.  The site was serving /k42.html; this box handed out /k27.html from weeks before.
:glass/site publishes IN THIS IMAGE and SETFs this in the same call, so there is no handoff left to
be stale.  Without that system loaded the environment is still where the value comes from, which is
exactly as good as it ever was and no worse.")

(defun nostr-command-reply (pubkey payload)
  "The reply this box owes PUBKEY for a command DM, or NIL if PAYLOAD is not a command.

Returns (values REPLY VERB ROLE).  REPLY is a string to send, or :DENIED — which the caller answers
with SILENCE and a log line, never with a message.  ROLE is :allowlist / :device / NIL, for the log.

THIS IS THE ONE PLACE THE AUTHORIZATION RULES LIVE.  warp's panel is a second surface onto the same
commands and asks this file's ALLOWED-PUBKEY-P for its invoker rather than computing its own answer;
that is the whole reason the two surfaces cannot drift apart."
  (multiple-value-bind (verb arg) (parse-nostr-command payload)
    (when verb
      (let* ((admin (allowed-pubkey-p pubkey))
             (dev (and (not admin) (device-enrolled-p pubkey)))
             (role (cond (admin :allowlist) (dev :device) (t nil)))
             (reply
               (case verb
                 (:link
                  ;; the one command an enrolled device may also use: a terminal whose code is about
                  ;; to lapse can refresh itself with nobody doing anything.
                  ;;
                  ;; MINTED :FOR THE SENDER, which is what makes asking twice safe.  "Ask for a
                  ;; link, tap nothing, ask again" is the commonest shape there is, and without a
                  ;; recipient on the mint every unused link in that history stays armed until its
                  ;; TTL runs out.  With one, the new link cancels the old ones in the same critical
                  ;; section that mints it — so at most ONE outstanding code per person exists, and
                  ;; it is always the most recent.
                  (cond ((not (or admin dev)) :denied)
                        ((null *login-url-base*)
                         "This box has no published client to link to (LOGIN_URL_BASE is unset).")
                        (t (multiple-value-bind (token killed)
                               (mint-login-token :ttl *login-ttl* :for pubkey)
                             (if token
                                 (format nil "Fresh glass login link (expires in ~a min):~%~%~a#box=~a&code=~a~@[~%~%~a~]"
                                         (max 1 (round *login-ttl* 60)) *login-url-base*
                                         (or (box-npub) "") token
                                         ;; said out loud, because a person who taps an older link
                                         ;; after this will be refused and deserves to know why
                                         (and (integerp killed) (plusp killed)
                                              "This replaces any earlier link I sent you — those no longer work."))
                                 "This box has no identity and cannot mint a link.")))))
                 (:devices
                  (if admin
                      (describe-enrolments
                       :state (if (and arg (string-equal arg "all")) :all :active))
                      :denied))
                 (:revoke (if admin (describe-revoke arg :by pubkey) :denied))
                 (:help
                  (if (or admin dev)
                      (format nil "Commands:~%  link~%~@[~a~]"
                              (and admin "  devices [all]~%  revoke <first-8> | revoke all~%"))
                      :denied))
                 (t nil))))
        (values reply verb role)))))

;;; ==============================================================================================
;;; THE ADMISSION SERVICE — the transport's end of all of the above
;;; ==============================================================================================
;;;
;;; A gateway in another process cannot call ADMIT-PEER across a process boundary, exactly as it
;;; cannot call MIXER-SUBSCRIBE across one, so this serves the answer on a socket.  The shape is
;;; audio-stream.lisp's and mic-stream.lisp's, one port further along the same convention:
;;;
;;;     5903  RFB          the screen
;;;     5913  glass-audio  the session's mix, out           (+10)
;;;     5914  glass-mic    a peer's microphone, in          (+11)
;;;     5915  glass-admit  who may open any of it           (+12)
;;;
;;; TEXT-FRAMED, so `nc localhost 5915` is a diagnostic and not a hex dump:
;;;
;;;     client -> server:  glass-admit/1 <verb> [k=v …]
;;;     server -> client:  glass-admit/1 ok   [k=v …] [rows=<n>]     then N body lines
;;;                        glass-admit/1 deny reason=<why>
;;;                        glass-admit/1 err  reason=<why>
;;;
;;;     $ printf 'glass-admit/1 ping\n' | nc 127.0.0.1 5915
;;;     glass-admit/1 ok box=<64hex> allow=1 devices=2 ttl=86400
;;;
;;; THREE STATUSES AND NOT TWO.  `deny' is an answer — this box says no — and `err'/no connection is
;;; the ABSENCE of one.  A caller that cannot tell them apart cannot implement a failure policy, and
;;; the entire reason a gateway can fail CLOSED safely is that "denied" and "unreachable" arrive
;;; differently.
;;;
;;; LOOPBACK BY DEFAULT, and unauthenticated on it.  That is the same trust boundary the desktop's
;;; control socket already draws: anything that can open 127.0.0.1 on this box can already eval in
;;; this image.  From off the box it must come through something that authenticates — which is what
;;; the gateway IS.
;;;
;;; ONE REQUEST PER LINE, ANY NUMBER PER CONNECTION.  A caller may hold one open or dial per
;;; question; a gateway dials per question, because a stateless call has no stale socket to notice
;;; and admission happens once per offer rather than once per frame.
;;;
;;; THE SEAM FOR THE FOLLOW-UP IS HERE.  Making this image the only holder of the box secret adds
;;; two verbs — `unwrap' (a gift wrap in, plaintext and a verified sender out) and `wrap' (plaintext
;;; and a recipient in, a signed kind-1059 out) — and deletes NOSTR_SEC from the gateway's
;;; environment.  Everything else here is already the right shape for it.

(defparameter *admission-port* 5915
  "Where this desktop answers admission questions.  Beside the screen by the established
convention (5903 -> 5913 audio out, 5914 microphone in, 5915 who may connect at all).")

(defparameter *admission-port-offset* 12
  "How far a seat's admission port sits from its RFB screen port.  Read as arithmetic rather than as
a number typed into a startup script, the same way *AUDIO-PORT-OFFSET* is.")

(defun seat-admission-port (rfb-port) (+ rfb-port *admission-port-offset*))

(defparameter *admission-host* "127.0.0.1"
  "Where a client looks for the service.  A gateway's GLASS_HOST names the same box.

Both endpoint forms are understood, here and everywhere else a host is configured: a hostname
beside a port, or `unix:/path/to/glass-admit.sock' (or a bare absolute path) for a socket file.
The second is worth wanting HERE more than anywhere: this service answers WHO MAY OPEN THIS
DESKTOP, and on a loopback port the qualification to ask it — including to ask it as somebody
else — is a process on this machine.")

(defparameter *admission-timeout* 2.0d0
  "Seconds a client waits for an answer before calling the service unreachable.  Short, because this
is on the path of somebody's login, the service is on loopback, and a slow answer is a broken one.")

;;; ---- the wire, both ends -----------------------------------------------------

(defun %net-write-line (stream string)
  (loop for ch across string do (write-byte (char-code ch) stream))
  (write-byte 10 stream))

(defun %net-read-line (stream &key (limit 8192))
  "One newline-terminated ASCII line, or NIL at end of stream.  A blank line reads as \"\" and not
as end of stream — they are different, and the caller decides what to do about each.  LIMIT caps a
peer that opens a connection and sends an unbounded line without ever ending it."
  (let ((out (make-string-output-stream)) (ended nil))
    (loop repeat limit
          for b = (read-byte stream nil nil)
          do (cond ((null b) (return))
                   ((= b 10) (setf ended t) (return))
                   ((/= b 13) (write-char (code-char b) out))))
    (let ((s (get-output-stream-string out)))
      (if (or ended (plusp (length s))) s nil))))

(defun %split-on-space (string)
  (loop with start = 0
        for i = (position #\Space string :start start)
        collect (subseq string start i)
        while i do (setf start (1+ i))))

(defun %net-params (line)
  "\"glass-admit/1 ok via=code token=abc\" -> (values \"ok\" (:via \"code\" :token \"abc\")).

The first word is the protocol tag and the second is the verb or the status — positional in both
directions, so one parser reads a request and a reply.  Unknown keys are kept rather than refused: a
newer caller asking about something this server has not heard of should get an answer, not a
disconnect."
  (let ((words (remove "" (%split-on-space line) :test #'string=)))
    (values (second words)
            (loop for w in (cddr words)
                  for eq = (position #\= w)
                  when eq collect (intern (string-upcase (subseq w 0 eq)) :keyword)
                    and collect (subseq w (1+ eq))))))

(defun %net-line (verb &rest params)
  "One line of the protocol: the tag, the verb or status, then k=v pairs.  Same shape both
directions, so one writer serialises a request and a reply."
  (format nil "glass-admit/1 ~(~a~)~{ ~a~}" verb
          (loop for (k v) on params by #'cddr
                when (and v (plusp (length (princ-to-string v))))
                  collect (format nil "~(~a~)=~(~a~)" k (%one-line v)))))

;;; ---- the server --------------------------------------------------------------

(defstruct (admission-service (:constructor %make-admission-service))
  (port 0 :type fixnum)
  socket
  thread
  (running nil)
  (lock (sb-thread:make-mutex :name "glass-admission"))
  (served 0 :type fixnum)
  (admitted 0 :type fixnum)
  (denied 0 :type fixnum))

(defvar *session-admission-service* nil
  "The admission service this image is running, if any.")

(defun admission-serve (verb params)
  "Answer one request.  Returns (values REPLY-LINE BODY-LINES).

Every verb that can act on somebody's behalf takes PUB and applies THIS FILE's rules to it, so the
DM surface and warp's panel cannot end up enforcing different policies: the second surface asks the
same question of the same function instead of reimplementing the answer."
  (let ((pub (normalize-pubkey (getf params :pub))))
    (cond
      ;; ---- liveness, and the box's posture in one line.  This is what a caller uses to tell "the
      ;; desktop is up and says no" from "there is no desktop", which is the whole basis of a
      ;; fail-closed policy at the other end.
      ((string= verb "ping")
       (%net-line :ok :box (or (box-pubkey) "none") :allow (length *nostr-allow*)
                  :devices (enrolment-count) :ttl *enrolment-ttl*
                  :retention *audit-retention*))
      ;; ---- the authentication path: decide, enrol, and mint the renewal in one round trip
      ;;
      ;; EXPIRES IS THE NEW FIELD AND IT IS THE POINT OF THE ROUND TRIP HAVING AN ANSWER AT ALL.  A
      ;; transport copies it into the answer envelope beside the renewal code; the browser stores it
      ;; against this box and can then tell, at its NEXT cold load and before it has spoken to
      ;; anybody, whether the credential it is about to offer is one this box still honours.  A
      ;; denial is silence by design, so this is the only channel that fact has.
      ((string= verb "admit")
       (if (null pub)
           (%net-line :err :reason "no-pub")
           (multiple-value-bind (via token status expires) (admit-peer pub (getf params :code))
             (if via
                 (%net-line :ok :via via :code status :token token :expires expires
                            :devices (enrolment-count))
                 (%net-line :deny :reason status)))))
      ;; ---- the allowlist predicate, asked as a question.  A second surface's invoker is THIS and
      ;; nothing else — not the session's `via', which takes the first of code/allowlist/device and
      ;; would therefore demote an owner arriving on a magic link and promote a guest sent one.
      ((string= verb "allowed")
       (%net-line :ok :allowed (if (allowed-pubkey-p pub) 1 0)))
      ;; ---- the store as a result-set.  No PUB: this is the shared query behind warp's panel, one
      ;; for however many phones are looking, and the per-peer rule is on the COMMANDS below.
      ;;
      ;; THE BODY LINE IS THE FILE LINE, field for field, so one parser reads both and a reader that
      ;; wants only `<pubkey> <expiry>' — which is what ADMISSION-DEVICES and every deployed caller
      ;; want — goes on taking the first two words and never notices the rest arrived.  That is the
      ;; whole compatibility story for this verb: the row got longer at the end.
      ;;
      ;; `state=all' (or expired / revoked) asks for the settled records as well.  DEFAULT ACTIVE,
      ;; because the panel projects CURRENT STATE (DESIGN.md rule 4) and a list that quietly grew
      ;; two weeks of revoked terminals would be an event log wearing a result-set's clothes.
      ((string= verb "devices")
       (let* ((want (let ((s (%undash (getf params :state))))
                      (if s (intern (string-upcase s) :keyword) :active)))
              (rows (list-enrolment-records
                     :state (if (member want '(:active :all :expired :revoked)) want :active))))
         (values (%net-line :ok :rows (length rows) :state want)
                 (mapcar #'%enrolment-line rows))))
      ;; ---- and the destructive one, which is allowlist-only wherever it is invoked from.  PUB is
      ;; the authority, and now it is also what the record says it was revoked BY: the panel and the
      ;; DM surface reach one function, so they leave one kind of evidence.
      ((string= verb "revoke")
       (cond ((not (allowed-pubkey-p pub)) (%net-line :deny :reason "not-allowed"))
             ((null (getf params :arg)) (%net-line :err :reason "no-arg"))
             (t (let ((killed (revoke-enrolments (getf params :arg) :by pub)))
                  (values (%net-line :ok :rows (length killed)) killed)))))
      ;; ---- a link, for the surfaces that are not the DM bot.  Same rule the `link' command
      ;; applies: an allowlisted owner or an enrolled device, and nobody else.
      ;;
      ;; PUB IS THE AUTHORITY; `for' IS THE RECIPIENT, and the presence of `for' is what makes this
      ;; a LINK rather than a TOP-UP.  The two callers want opposite things from the same verb:
      ;;
      ;;   mint pub=X          the gateway topping up an allowlist admission — a credential going
      ;;                       straight into THAT session's localStorage for its next cold load.  It
      ;;                       has no recipient, must not cancel anybody's outstanding link, and
      ;;                       wants *RENEWAL-TTL* because it has to still be there tomorrow.
      ;;   mint pub=X for=Y    a surface sending Y a link (warp's device panel, an admin app).  It
      ;;                       is a credential in transit, wants the short *LOGIN-TTL*, and
      ;;                       supersedes Y's earlier outstanding codes exactly as `link' does.
      ;;
      ;; An explicit ttl= still wins over both, so a caller that knows what it is doing can say so.
      ((string= verb "mint")
       (let* ((for (normalize-pubkey (getf params :for)))
              (ttl (or (ignore-errors (parse-integer (getf params :ttl)))
                       (if for *login-ttl* *renewal-ttl*))))
         (if (not (or (allowed-pubkey-p pub) (device-enrolled-p pub)))
             (%net-line :deny :reason "not-allowed")
             (multiple-value-bind (token killed) (mint-login-token :ttl ttl :for for)
               (if token
                   (%net-line :ok :token token :ttl ttl :for for :superseded killed)
                   (%net-line :err :reason "no-identity"))))))
      (t (%net-line :err :reason "unknown-op")))))

(defun %serve-admission-client (srv sock)
  "One connection: a request line, an answer, repeat until the peer goes away."
  (let ((stream nil))
    (unwind-protect
         (handler-case
             (progn
               (ignore-errors (setf (sb-bsd-sockets:sockopt-tcp-nodelay sock) t))
               (setf stream (sb-bsd-sockets:socket-make-stream
                             sock :input t :output t :element-type '(unsigned-byte 8)
                             :buffering :full :timeout 30))
               (loop while (admission-service-running srv) do
                 (let ((line (%net-read-line stream)))
                   (when (null line) (return))                     ; peer hung up
                   (unless (zerop (length line))
                     (multiple-value-bind (verb params) (%net-params line)
                       (multiple-value-bind (reply body)
                           (handler-case (admission-serve (or verb "") params)
                             (serious-condition (e) (%net-line :err :reason (%one-line e))))
                         (sb-thread:with-mutex ((admission-service-lock srv))
                           (incf (admission-service-served srv))
                           (when (equal verb "admit")
                             (if (equal "ok" (nth-value 0 (%net-params reply)))
                                 (incf (admission-service-admitted srv))
                                 (incf (admission-service-denied srv)))))
                         (%net-write-line stream reply)
                         (dolist (b body) (%net-write-line stream b))
                         (force-output stream)))))))
           (serious-condition () nil))       ; a peer hanging up is the normal way this ends
      (ignore-errors (when stream (close stream)))
      (ignore-errors (sb-bsd-sockets:socket-close sock)))))

(defun start-admission-service (&key (port *admission-port*) (address "127.0.0.1") path
                                    (peer-policy *peer-policy*) (install t))
  "Answer admission questions on PORT, forever.  Returns an ADMISSION-SERVICE, or NIL.

REFUSES WITHOUT AN IDENTITY, and that refusal is load-bearing: a service answering with no box
secret could neither verify a code nor mint one, so every code login would be denied while allowlist
logins carried on — a half-working door, which is worse than a shut one.  A shut one is diagnosable
in one line: the caller cannot connect."
  (unless (box-identity-p)
    (format *error-output*
            "~&@@ admission: no box identity — set GLASS_NOSTR_SEC (64 hex) — service NOT started~%")
    (finish-output *error-output*)
    (return-from start-admission-service nil))
  (load-enrolments)
  (let* ((listener (if path
                       (open-listener :unix :path path :backlog 8 :peer-policy peer-policy)
                       (open-listener :tcp :port port :address address :backlog 8)))
         (srv (%make-admission-service
               :port (if path 0 port) :running t
               :socket listener)))
    (setf (admission-service-thread srv)
          (sb-thread:make-thread
           (lambda ()
             (loop while (admission-service-running srv) do
               (handler-case
                   (let ((sock (listener-accept (admission-service-socket srv))))
                     (sb-thread:make-thread (lambda () (%serve-admission-client srv sock))
                                            :name "glass-admission-client"))
                 (serious-condition () (sleep 0.2)))))
           :name "glass-admission"))
    (when install (setf *session-admission-service* srv))
    srv))

(defun stop-admission-service (&optional (srv *session-admission-service*))
  (when srv
    (setf (admission-service-running srv) nil)
    ;; CLOSE-LISTENER, not SOCKET-CLOSE: the accept loop is parked on this socket and a bare close
    ;; leaves the kernel listening.  See GLASS:CLOSE-LISTENER — an admission service that believes
    ;; it has stopped answering while it is still answering is the worst version of that bug.
    (ignore-errors (close-listener (admission-service-socket srv)))
    (when (eq srv *session-admission-service*) (setf *session-admission-service* nil)))
  t)

(defun admission-service-report (&optional (srv *session-admission-service*))
  (if (null srv)
      "admission: not running"
      (format nil "admission ~a served=~d admitted=~d denied=~d devices=~d allow=~d"
              (listener-endpoint (admission-service-socket srv)) (admission-service-served srv)
              (admission-service-admitted srv) (admission-service-denied srv)
              (enrolment-count) (length *nostr-allow*))))

;;; ---- the client's end --------------------------------------------------------
;;;
;;; The other half of the same thin thing, carried here for the reason MAKE-AUDIO-TAP is carried in
;;; audio-stream.lisp: the process that asks is not this one, and a protocol whose two ends live in
;;; different repositories drifts.  A gateway loads :glass/nostr for these functions and starts
;;; nothing at all.
;;;
;;; EVERY ONE OF THEM DISTINGUISHES `NO' FROM `NO ANSWER'.  A second value of :UNREACHABLE means the
;;; service could not be reached — that is what a caller's failure policy turns on, and conflating
;;; it with a denial is exactly how a fail-closed door becomes a fail-open one.

(defun admission-request (verb &rest params &key host port path &allow-other-keys)
  "Ask the admission service one question.  Returns (values STATUS PLIST BODY), where STATUS is
:OK / :DENY / :ERR / :UNREACHABLE.  Never signals.

PATH, or a HOST in either UNIX form, asks over a socket file instead of a port.  The protocol is
identical — it is a request line and an answer line, and it never knew what carried it."
  (let ((host (or host *admission-host*))
        (port (or port *admission-port*))
        (sock nil) (stream nil))
    (setf params (copy-list params))
    (remf params :host) (remf params :port) (remf params :path)
    (handler-case
        (unwind-protect
             (progn
               (multiple-value-setq (sock stream)
                 (open-connection :host host :port port :path path
                                  :timeout *admission-timeout*))
               (%net-write-line stream (apply #'%net-line verb params))
               (force-output stream)
               (let ((line (or (%net-read-line stream) (error "no answer"))))
                 (multiple-value-bind (status plist) (%net-params line)
                   (let* ((rows (or (ignore-errors (parse-integer (getf plist :rows))) 0))
                          (body (loop repeat rows
                                      for b = (%net-read-line stream)
                                      while b collect b)))
                     (values (intern (string-upcase (or status "err")) :keyword) plist body)))))
          (ignore-errors (when stream (close stream)))
          (ignore-errors (when sock (sb-bsd-sockets:socket-close sock))))
      (serious-condition (e) (values :unreachable (list :reason (%one-line e)) nil)))))

(defun admission-ping (&rest where)
  "The service's posture, as a plist, or NIL if it is not answering."
  (multiple-value-bind (status plist) (apply #'admission-request :ping where)
    (and (eq status :ok) plist)))

(defun admission-admit (pubkey code &rest where)
  "Ask the desktop whether PUBKEY, holding CODE, may connect — and enrol it if so.

Returns (values VIA TOKEN PLIST), VIA one of :code :allowlist :device, or NIL when refused.  When
refused the second value says WHY, and :UNREACHABLE there means there was nobody to ask — which is
the distinction a caller's failure policy is built on."
  (multiple-value-bind (status plist)
      (apply #'admission-request :admit :pub pubkey :code code where)
    (case status
      (:ok (values (intern (string-upcase (or (getf plist :via) "device")) :keyword)
                   (getf plist :token) plist))
      (:unreachable (values nil :unreachable plist))
      (t (values nil (intern (string-upcase (or (getf plist :reason) "denied")) :keyword) plist)))))

(defun admission-allowed-p (pubkey &rest where)
  "T iff the desktop says PUBKEY is on its allowlist.  (values NIL :unreachable) when nobody
answered — a caller that cannot reach the desktop must not promote anybody, so NIL is also the safe
answer and the second value is how a caller can say so out loud."
  (multiple-value-bind (status plist) (apply #'admission-request :allowed :pub pubkey where)
    (cond ((eq status :unreachable) (values nil :unreachable))
          ((and (eq status :ok) (equal (getf plist :allowed) "1")) t)
          (t nil))))

(defun admission-devices (&rest where)
  "The desktop's LIVE enrolments, as ((pubkey . expiry) …).  (values NIL :unreachable) when nobody
answered — which a caller must not render as an empty list, because `no terminals are enrolled' and
`the desktop is not there' are different things to show somebody.

THE FIRST TWO FIELDS AND NOTHING ELSE, which is the contract this function has always had and the
reason the record could grow without a caller changing.  It takes the second WORD rather than the
rest of the line — the rest of the line is a record now, and PARSE-INTEGER over `1234 active 0 …'
does not answer 1234, it answers nothing, which would have rendered every terminal as lapsed."
  (multiple-value-bind (status plist body) (apply #'admission-request :devices where)
    (declare (ignore plist))
    (if (eq status :ok)
        (loop for line in body
              for w = (remove "" (%split-on line #\Space) :test #'string=)
              when (>= (length w) 2)
                collect (cons (first w) (%int (second w))))
        (values nil :unreachable))))

(defun admission-records (&rest where &key state &allow-other-keys)
  "The desktop's enrolment RECORDS, as plists:

    (:pubkey … :expiry … :state :active/:expired/:revoked :created … :seen …
     :via :code/:allowlist/:device :nonce … :for … :since … :cause …)

STATE selects, exactly as the service's verb does: :ACTIVE (default), :ALL, :EXPIRED, :REVOKED.
(values NIL :unreachable) when nobody answered, for the reason ADMISSION-DEVICES gives.

A PLIST AND NOT AN OBJECT, because the caller is in another image and another system: the transport
that asks this has no reason to load a class definition to read ten fields, and a plist is what its
own projection is going to map over anyway."
  (setf where (copy-list where))
  (remf where :state)
  (multiple-value-bind (status plist body)
      (apply #'admission-request :devices (append (when state (list :state state)) where))
    (declare (ignore plist))
    (if (eq status :ok)
        (loop for line in body
              for w = (remove "" (%split-on line #\Space) :test #'string=)
              when (>= (length w) 2)
                collect (list :pubkey (string-downcase (first w))
                              :expiry (%int (second w))
                              :state (if (third w)
                                         (intern (string-upcase (third w)) :keyword)
                                         :active)
                              :created (%int (fourth w)) :seen (%int (fifth w))
                              :via (let ((v (%undash (sixth w))))
                                     (and v (intern (string-upcase v) :keyword)))
                              :nonce (%undash (seventh w))
                              :for (%undash (eighth w))
                              :since (%int (ninth w))
                              :cause (%undash (tenth w))))
        (values nil :unreachable))))

(defun admission-revoke (pubkey arg &rest where)
  "Revoke on PUBKEY's authority — which the desktop checks, and which is allowlist-only.  Returns
the list revoked, or (values NIL :denied / :unreachable)."
  (multiple-value-bind (status plist body)
      (apply #'admission-request :revoke :pub pubkey :arg arg where)
    (declare (ignore plist))
    (case status
      (:ok body)
      (:unreachable (values nil :unreachable))
      (t (values nil :denied)))))

(defun admission-mint (pubkey &rest where)
  "A fresh login token on PUBKEY's authority, or NIL if refused or unreachable.

WHERE carries the endpoint (:HOST/:PORT/:PATH) and may also carry :FOR and :TTL, which go to the
server as ordinary parameters.  :FOR NAMES A RECIPIENT and is the difference between the two things
this verb does — with it the token is a LINK (short *LOGIN-TTL*, and it supersedes that recipient's
earlier outstanding codes); without it, a TOP-UP for the session already asking (*RENEWAL-TTL*, and
it supersedes nothing).  A gateway topping up an allowlist admission wants the second and passes no
:FOR, which is why its call is unchanged."
  (multiple-value-bind (status plist) (apply #'admission-request :mint :pub pubkey where)
    (and (eq status :ok) (getf plist :token))))

;;; ==============================================================================================
;;; THE DM BOT — the box, listening as itself
;;; ==============================================================================================
;;;
;;; Everything rides NIP-59 gift wrap (kind 1059).  The subscription is `kinds:(1059) #p:(box)' and
;;; UNWRAP-GIFTWRAP yields the VERIFIED SEAL SIGNER, so a forged rumour pubkey is rejected before
;;; anything here sees it.
;;;
;;; THIS BOT ANSWERS COMMANDS AND NOTHING ELSE.  The WebRTC gateway subscribes to the same box
;;; pubkey for the same kind and answers OFFERS; the two halves are disjoint by construction —
;;; PARSE-NOSTR-COMMAND refuses anything over 80 characters and an SDP offer is thousands.  This is
;;; a SWAP and not an addition: the gateway's command branch was removed in the same change, or
;;; every DM would get two replies.
;;;
;;; THREE GUARDS ON THE INBOUND SIDE, and the third is new here.  Relay fan-out delivers one DM
;;; several times (dedup by wrap id); a relay reconnect re-delivers its backlog (same set); and an
;;; OLD command in that backlog must not be answered again on every restart — which the gateway
;;; never guarded, so restarting it could re-answer a `link' from hours ago with a live credential.
;;; A rumour older than *NOSTR-COMMAND-MAX-AGE* is ignored.

(defparameter *nostr-relays*
  (let ((e (sb-ext:posix-getenv "NOSTR_RELAYS")))
    (if e (remove "" (%split-on e #\,) :test #'string=)
        '("wss://relay.damus.io" "wss://nos.lol" "wss://relay.primal.net")))
  "Relays the box listens on.  Fan-out means one DM arrives several times; that is the point, and
the wrap-id set is what makes it free.")

(defparameter *nostr-command-max-age* 600
  "A command DM older than this many seconds is ignored.  Relays replay their backlog on every
reconnect, so without this a restart re-answers old commands — and `link' answered twice is a second
live credential minted for a request nobody made.")

(defstruct (nostr-bot (:constructor %make-nostr-bot))
  pool
  keypair
  (pubkey "" :type string)
  npub
  (relays '())
  (running nil)
  (lock (sb-thread:make-mutex :name "glass-nostr-bot"))
  (seen (make-hash-table :test 'equal))
  (received 0 :type fixnum)
  (answered 0 :type fixnum)
  (denied 0 :type fixnum)
  (ignored 0 :type fixnum)
  log)

(defvar *session-nostr-bot* nil "The DM bot this image is running, if any.")

(defun %bot-say (bot fmt &rest args)
  (let ((log (nostr-bot-log bot))
        (msg (apply #'format nil fmt args)))
    (ignore-errors
     (if log
         (funcall log msg)
         (progn (format *error-output* "~&[nostr] ~a~%" msg) (finish-output *error-output*))))))

(defun %bot-seen-p (bot id)
  "T if this wrap has already been handled.  Also the fan-out deduplicator."
  (when id
    (sb-thread:with-mutex ((nostr-bot-lock bot))
      (let ((seen (nostr-bot-seen bot)))
        (prog1 (gethash id seen)
          ;; Flushed wholesale at a bound rather than aged out one at a time: what that briefly
          ;; reopens is a duplicate reply to a command sent in the last few minutes, and the
          ;; alternative costs a timestamp per entry forever.  The age guard covers the rest.
          (when (> (hash-table-count seen) 4096) (clrhash seen))
          (setf (gethash id seen) t))))))

(defun %bot-count (bot slot)
  (sb-thread:with-mutex ((nostr-bot-lock bot))
    (ecase slot
      (:received (incf (nostr-bot-received bot)))
      (:answered (incf (nostr-bot-answered bot)))
      (:denied (incf (nostr-bot-denied bot)))
      (:ignored (incf (nostr-bot-ignored bot))))))

(defun %bot-handle (bot wrap)
  "One gift wrap.  Never signals: a bad DM is a log line, not a bot that stops listening."
  (handler-case
      (unless (%bot-seen-p bot (ignore-errors (cl-nostr.event:event-id wrap)))
        (multiple-value-bind (payload sender rumor-at)
            (cl-nostr.nip59:unwrap-giftwrap (nostr-bot-keypair bot) wrap)
          (%bot-count bot :received)
          (multiple-value-bind (reply verb role) (nostr-command-reply sender payload)
            (cond
              ;; Not a command: an SDP offer, or somebody's chatter.  The gateway answers offers;
              ;; this file has no opinion about them and says nothing at all.
              ((null verb) (%bot-count bot :ignored))
              ((and (integerp rumor-at) (plusp rumor-at)
                    (> (- (unix-now) rumor-at) *nostr-command-max-age*))
               (%bot-count bot :ignored)
               (%bot-say bot "~(~a~) from ~a… is ~ds old — ignored (backlog replay)"
                         verb (subseq sender 0 8) (- (unix-now) rumor-at)))
              ((or (null reply) (eq reply :denied))
               (%bot-count bot :denied)
               ;; NO REPLY.  Silence is the denial: an answer would tell an unknown sender that this
               ;; box exists, is listening, and understood them.
               (%bot-say bot "~(~a~) DENIED ~a… (~:[not authorised~;device: management is allowlist-only~])"
                         verb (subseq sender 0 8) (eq role :device)))
              (t
               (cl-nostr.pool:pool-publish
                (nostr-bot-pool bot)
                ;; :AFTER — stamp the reply strictly later than the DM it answers.  We often reply
                ;; inside the same second and the box's clock need not agree with the phone's;
                ;; either way the answer would otherwise sort ABOVE the question in the recipient's
                ;; client, which reads as the box talking to itself.
                (cl-nostr.nip59:build-giftwrap (nostr-bot-keypair bot) sender reply :after rumor-at))
               (%bot-count bot :answered)
               (%bot-say bot "~(~a~) from ~a… (~(~a~)) -> replied"
                         verb (subseq sender 0 8) (or role :unknown)))))))
    (serious-condition (e) (%bot-say bot "dm: ~a" e))))

(defun start-nostr-bot (&key (relays *nostr-relays*) log (install t))
  "Listen as this box and answer command DMs.  Returns a NOSTR-BOT, or NIL if it cannot.

Deliberately total, for the reason START-SESSION-AUDIO is: a desktop that cannot reach a relay is
still a desktop, and a desktop that did not start is not."
  (unless (box-identity-p)
    (format *error-output* "~&@@ nostr: no box identity — set GLASS_NOSTR_SEC — bot NOT started~%")
    (finish-output *error-output*)
    (return-from start-nostr-bot nil))
  (handler-case
      (let* ((kp (cl-nostr.keys:keypair-from-secret *box-secret*))
             (pub (string-downcase (cl-nostr.keys:public-hex kp)))
             (pool (cl-nostr.pool:make-pool relays))
             (bot (%make-nostr-bot :pool pool :keypair kp :pubkey pub :npub (box-npub)
                                   :relays relays :running t :log log)))
        (cl-nostr.pool:pool-subscribe
         pool
         ;; :LIMIT caps the initial backlog — this pubkey has days of old wraps on the relays.
         ;; Live DMs still stream after EOSE.
         (list (cl-nostr.filter:make-filter :kinds '(1059)
                                            :tags (list (cons "p" (list pub)))
                                            :limit 20))
         :on-event (lambda (wrap relay)
                     (declare (ignore relay))
                     (when (nostr-bot-running bot) (%bot-handle bot wrap))))
        (when install (setf *session-nostr-bot* bot))
        bot)
    (serious-condition (e)
      (ignore-errors
       (format *error-output* "~&@@ nostr: bot unavailable (~a) — no DM command surface~%" e)
       (finish-output *error-output*))
      nil)))

(defun stop-nostr-bot (&optional (bot *session-nostr-bot*))
  (when bot
    (setf (nostr-bot-running bot) nil)
    (ignore-errors (cl-nostr.pool:close-pool (nostr-bot-pool bot)))
    (when (eq bot *session-nostr-bot*) (setf *session-nostr-bot* nil)))
  t)

(defun nostr-bot-report (&optional (bot *session-nostr-bot*))
  (if (null bot)
      "nostr bot: not running"
      (format nil "nostr bot ~a ~a relays rx=~d replied=~d denied=~d ignored=~d"
              (or (nostr-bot-npub bot) (subseq (nostr-bot-pubkey bot) 0 12))
              (length (nostr-bot-relays bot)) (nostr-bot-received bot)
              (nostr-bot-answered bot) (nostr-bot-denied bot) (nostr-bot-ignored bot))))

;;; ---- the one line a desktop startup script wants ------------------------------

(defun start-session-nostr (&key (port *admission-port*) (address "127.0.0.1") path
                                 (relays *nostr-relays*) (bot t))
  "Everything this desktop needs to own its own identity: the allowlist read, the enrolment store
loaded, the admission service on PORT, and the DM command bot on RELAYS.

Deliberately total — a box that cannot reach a relay must still be a desktop — and it refuses loudly
with no identity rather than serving as nobody.  Returns (values SERVICE BOT)."
  (handler-case
      (progn
        (refresh-nostr-allow)
        (unless (box-identity-p)
          (format *error-output*
                  "~&@@ nostr: GLASS_NOSTR_SEC unset or not 64 hex — this desktop has no identity.~@
                     @@   openssl rand -hex 32, and use the SAME value the gateway runs on, or~@
                     @@   every issued login link and every enrolled device stops verifying.~%")
          (finish-output *error-output*)
          (return-from start-session-nostr (values nil nil)))
        (let ((srv (start-admission-service :port port :address address :path path))
              (b (and bot (start-nostr-bot :relays relays))))
          (format *error-output* "~&@@ admission on ~a — box ~a, ~a enrolled, ~a allowed~%"
                  (endpoint-string :host address :port port :path path)
                  (or (box-npub) (box-pubkey)) (enrolment-count)
                  (length *nostr-allow*))
          (when b (format *error-output* "@@ nostr dm bot on ~{~a~^, ~}~%" (nostr-bot-relays b)))
          (finish-output *error-output*)
          (values srv b)))
    (serious-condition (e)
      (ignore-errors
       (format *error-output* "~&@@ nostr unavailable: ~a~%" e)
       (finish-output *error-output*))
      (values nil nil))))
