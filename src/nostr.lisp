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

(defvar *enrolment-file*
  (or (sb-ext:posix-getenv "GLASS_DEVICE_FILE")
      (namestring (merge-pathnames ".glass-devices" (user-homedir-pathname))))
  "Where enrolments are persisted: one `<pubkey-hex> <expiry-unix>' line per terminal.

The same format the WebRTC gateway used, on purpose — an existing store is copied across and works,
and warp-monitor's reader parses it unchanged.  DEFVAR, so a hot-load cannot move the store out
from under a running desktop.

THE LOGIN-CODE STORE IS DERIVED FROM THIS PATH (`<this>.codes', see LOGIN-CODE-FILE), so redirecting
this one redirects both.")

(defvar *enrolments* (make-hash-table :test 'equal))
(defvar *enrolments-lock* (sb-thread:make-mutex :name "glass-enrolments"))
(defvar *enrolments-mtime* nil)

;;; ---- internals.  ALL THREE REQUIRE *ENROLMENTS-LOCK* ALREADY HELD. ------------

(defun %load-enrolments ()
  "Merge the file into the table.  Does NOT clear first — %SYNC-ENROLMENTS decides that."
  (handler-case
      (with-open-file (s *enrolment-file* :if-does-not-exist nil)
        (when s
          (loop for line = (read-line s nil) while line do
            (let* ((sp (position #\Space line))
                   (pk (and sp (subseq line 0 sp)))
                   (exp (and sp (ignore-errors (parse-integer (subseq line (1+ sp)))))))
              (when (and pk exp (> exp (unix-now)))
                (setf (gethash (string-downcase pk) *enrolments*) exp))))))
    (error () nil)))

(defun %save-enrolments ()
  "Rewrite the file from the table, lapsed rows dropped."
  (handler-case
      (progn
        (with-open-file (s *enrolment-file* :direction :output :if-exists :supersede
                                            :if-does-not-exist :create)
          (maphash (lambda (pk exp) (when (> exp (unix-now)) (format s "~a ~a~%" pk exp)))
                   *enrolments*))
        ;; remember our own write, so the next SYNC does not re-read what we have just said
        (setf *enrolments-mtime* (ignore-errors (file-write-date *enrolment-file*))))
    (error () nil)))

(defun %sync-enrolments ()
  "Re-read if the file changed underneath us."
  (handler-case
      (let ((mt (file-write-date *enrolment-file*)))
        (unless (eql mt *enrolments-mtime*)
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

(defun enrol-device (pubkey &optional (ttl *enrolment-ttl*))
  "Trust PUBKEY to ask for its own login links for TTL seconds.  Renews an existing enrolment.

SYNC, SET AND SAVE IN ONE CRITICAL SECTION.  Two peers admitted at the same instant otherwise race
each other's rewrite of the file, and a reader between them sees the truncation."
  (when (stringp pubkey)
    (sb-thread:with-mutex (*enrolments-lock*)
      (%sync-enrolments)
      (setf (gethash (string-downcase pubkey) *enrolments*) (+ (unix-now) ttl))
      (%save-enrolments))
    t))

(defun device-enrolled-p (pubkey)
  (and (stringp pubkey)
       (sb-thread:with-mutex (*enrolments-lock*)
         (%sync-enrolments)
         (let ((exp (gethash (string-downcase pubkey) *enrolments*)))
           (and exp (> exp (unix-now)) t)))))

(defun list-enrolments ()
  "The live enrolments as ((pubkey . expiry) …), longest remaining first.  A lapsed row is not
returned even while the file still holds it: a lapsed terminal is not an enrolled one."
  (let ((now (unix-now)) (rows '()))
    (sb-thread:with-mutex (*enrolments-lock*)
      (%sync-enrolments)
      (maphash (lambda (pk exp) (when (> exp now) (push (cons pk exp) rows))) *enrolments*))
    (sort rows #'> :key #'cdr)))

(defun enrolment-count () (length (list-enrolments)))

(defun revoke-enrolments (arg)
  "Un-enrol terminals matching ARG — a pubkey prefix, or \"all\".  Returns the list revoked.

The prefix is matched at four characters and up while the surface advertises eight.  Kept as it was:
a prefix refused for being one character short is worse than a prefix that is generous, and the
operator typed it themselves — the ADVERTISED length is the one to copy."
  (let ((killed '()))
    (sb-thread:with-mutex (*enrolments-lock*)
      (%sync-enrolments)
      (cond
        ((and (stringp arg) (string-equal arg "all"))
         (maphash (lambda (pk exp) (declare (ignore exp)) (push pk killed)) *enrolments*)
         (clrhash *enrolments*))
        ((and (stringp arg) (>= (length arg) 4))
         (let ((arg (string-downcase arg)))
           (maphash (lambda (pk exp) (declare (ignore exp))
                      (when (and (>= (length pk) (length arg))
                                 (string= arg (subseq pk 0 (length arg))))
                        (push pk killed)))
                    *enrolments*))
         (dolist (pk killed) (remhash pk *enrolments*))))
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

(defun %code-field (s) (if (equal s "-") nil s))
(defun %code-out (s) (or s "-"))

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
                            (rcpt (%code-field (fourth w)))
                            (by (%code-field (fifth w)))
                            (minted (ignore-errors (parse-integer (sixth w))))
                            (settled (ignore-errors (parse-integer (seventh w)))))
                        ;; an unknown state is dropped rather than trusted: a row this build cannot
                        ;; interpret must not become a code it silently treats as live
                        (when (and exp (> exp now)
                                   (member state '(:outstanding :redeemed :superseded)))
                          (setf (gethash nonce (code-table store))
                                (make-instance 'login-code
                                               :nonce nonce :expiry exp :state state
                                               :recipient (and rcpt (string-downcase rcpt))
                                               :redeemer (and by (string-downcase by))
                                               :minted (or minted 0)
                                               :settled (or settled 0)))))))))))
        (error () nil)))))

(defun %codes-prune (store now)
  "Drop rows whose code has expired.  Returns how many went.

Pruned AT THE CODE'S OWN EXPIRY and not a moment before — that is the same instant
VERIFY-LOGIN-TOKEN starts refusing it, so there is never a window where the row is gone and the
code still works."
  (let ((dead '()))
    (maphash (lambda (k c) (unless (> (code-expiry c) now) (push k dead))) (code-table store))
    (dolist (k dead) (remhash k (code-table store)))
    (length dead)))

(defun %codes-save (store)
  "Write STORE out, expired rows dropped.  Whole-file rewrite, like the enrolments."
  (handler-case
      (let ((file (login-code-file))
            (now (unix-now)))
        (with-open-file (s file :direction :output :if-exists :supersede
                                :if-does-not-exist :create)
          (maphash (lambda (k c)
                     (declare (ignore k))
                     (when (> (code-expiry c) now)
                       (format s "~a ~(~a~) ~a ~a ~a ~a ~a~%"
                               (code-nonce c) (code-state c) (code-expiry c)
                               (%code-out (code-recipient c)) (%code-out (code-redeemer c))
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

Returns (values OK STATE HOLDER):
  OK      T if PUBKEY may use this code
  STATE   :BOUND       it was live and is now PUBKEY's        (first redemption)
          :AGAIN       PUBKEY already holds it                (the retry, and it passes)
          :FREE        it was live and BIND was NIL           (a probe; nothing was written)
          :TAKEN       another key holds it   (OK NIL — the link leaked)
          :SUPERSEDED  a newer link replaced it (OK NIL — ask for a fresh one)
  HOLDER  on :TAKEN, the key that got there first

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
               (result
                 (cond
                   ((and prior (eq (code-state prior) :redeemed))
                    (if (equal (code-redeemer prior) pubkey)
                        (list t :again pubkey)
                        (list nil :taken (code-redeemer prior))))
                   ((and prior (eq (code-state prior) :superseded))
                    (list nil :superseded nil))
                   ((not bind) (list t :free nil))
                   (prior                                    ; :outstanding — trade it in
                    (setf (code-state prior) :redeemed
                          (code-redeemer prior) pubkey
                          (code-settled prior) now
                          dirty t)
                    (list t :bound pubkey))
                   (t                                        ; a bare token: renewal, or the CLI
                    (setf (gethash nonce (code-table store))
                          (make-instance 'login-code :nonce nonce :expiry expiry :state :redeemed
                                                     :redeemer pubkey :minted now :settled now)
                          dirty t)
                    (list t :bound pubkey)))))
          (when dirty (%codes-save store))
          (values-list result))))))

;;; ---- reading it ---------------------------------------------------------------

(defun list-login-codes (&optional (store *login-codes*))
  "The live rows as LOGIN-CODE objects, most recently settled first.  Diagnostic only — nothing
decides anything from this list; the decisions are made inside the lock, above."
  (let ((now (unix-now)) (rows '()))
    (sb-thread:with-mutex ((code-lock store))
      (%codes-load store)
      (maphash (lambda (k c) (declare (ignore k))
                 (when (> (code-expiry c) now) (push c rows)))
               (code-table store)))
    (sort rows #'> :key (lambda (c) (max (code-settled c) (code-minted c))))))

(defun find-login-code (nonce &optional (store *login-codes*))
  "The row for NONCE, or NIL."
  (when (stringp nonce)
    (find (string-downcase nonce) (list-login-codes store) :key #'code-nonce :test #'equal)))

(defun login-code-count (&optional (store *login-codes*)) (length (list-login-codes store)))

(defun describe-login-codes (&optional (store *login-codes*))
  "The outstanding and spent codes in words, for an operator staring at a lockout."
  (let ((rows (list-login-codes store)) (now (unix-now)))
    (if (null rows)
        "No codes are outstanding."
        (format nil "~a live code~:p:~%~{~a~%~}" (length rows)
                (mapcar (lambda (c)
                          (format nil "  ~a  ~10@a  expires in ~d min~@[  to ~a…~]~@[  by ~a…~]"
                                  (subseq (code-nonce c) 0 8)
                                  (string-downcase (symbol-name (code-state c)))
                                  (max 0 (round (- (code-expiry c) now) 60))
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

Returns (values VIA TOKEN STATUS):
  VIA     :code / :allowlist / :device, or NIL when refused
  TOKEN   a fresh login code to hand back with the answer, or NIL
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
this is a credential at rest in localStorage, not one in transit through a DM."
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
           (status (if (not redeemable)
                       crypto-status
                       ;; UNCONDITIONAL.  Not inside the COND below, not guarded by "unless the
                       ;; allowlist would have taken them anyway" — see the note above.
                       (multiple-value-bind (ok state who)
                           (redeem-nonce nonce pubkey code-exp :bind enrol)
                         (setf holder who)
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
      (when (and via enrol) (enrol-device pubkey))
      (values via
              (when (member via '(:code :device)) (mint-login-token :ttl ttl))
              status))))

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
            ((string= verb "devices") (values :devices nil))
            ((string= verb "revoke") (values :revoke arg))
            ((or (string= verb "help") (string= verb "?")) (values :help nil))
            (t nil)))))

(defun describe-enrolments ()
  "Human-readable listing of enrolled terminals."
  (let ((rows (list-enrolments)) (now (unix-now)))
    (if (null rows)
        "No terminals are enrolled."
        (format nil "~a enrolled terminal~:p:~%~{~a~%~}~@
                     Use \"revoke <first-8>\" or \"revoke all\"."
                (length rows)
                (mapcar (lambda (r)
                          (let ((hrs (/ (- (cdr r) now) 3600.0)))
                            (if (< hrs 1)
                                (format nil "  ~a  expires in ~d min" (subseq (car r) 0 8)
                                        (max 1 (round (* hrs 60))))
                                (format nil "  ~a  expires in ~,1f h" (subseq (car r) 0 8) hrs))))
                        rows)))))

(defun describe-revoke (arg)
  "Run a `revoke' and report it in words."
  (if (null arg)
      "Usage: revoke <first-8-of-pubkey> | revoke all"
      (let ((killed (revoke-enrolments arg)))
        (if (null killed)
            (format nil "Nothing matched \"~a\"." arg)
            (format nil "Revoked ~a terminal~:p:~%~{  ~a~%~}" (length killed)
                    (mapcar (lambda (pk) (subseq pk 0 8)) killed))))))

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
                 (:devices (if admin (describe-enrolments) :denied))
                 (:revoke (if admin (describe-revoke arg) :denied))
                 (:help
                  (if (or admin dev)
                      (format nil "Commands:~%  link~%~@[~a~]"
                              (and admin "  devices~%  revoke <first-8> | revoke all~%"))
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
                  :devices (enrolment-count) :ttl *enrolment-ttl*))
      ;; ---- the authentication path: decide, enrol, and mint the renewal in one round trip
      ((string= verb "admit")
       (if (null pub)
           (%net-line :err :reason "no-pub")
           (multiple-value-bind (via token status) (admit-peer pub (getf params :code))
             (if via
                 (%net-line :ok :via via :code status :token token :devices (enrolment-count))
                 (%net-line :deny :reason status)))))
      ;; ---- the allowlist predicate, asked as a question.  A second surface's invoker is THIS and
      ;; nothing else — not the session's `via', which takes the first of code/allowlist/device and
      ;; would therefore demote an owner arriving on a magic link and promote a guest sent one.
      ((string= verb "allowed")
       (%net-line :ok :allowed (if (allowed-pubkey-p pub) 1 0)))
      ;; ---- the store as a result-set.  No PUB: this is the shared query behind warp's panel, one
      ;; for however many phones are looking, and the per-peer rule is on the COMMANDS below.
      ((string= verb "devices")
       (let ((rows (list-enrolments)))
         (values (%net-line :ok :rows (length rows))
                 (mapcar (lambda (r) (format nil "~a ~a" (car r) (cdr r))) rows))))
      ;; ---- and the destructive one, which is allowlist-only wherever it is invoked from
      ((string= verb "revoke")
       (cond ((not (allowed-pubkey-p pub)) (%net-line :deny :reason "not-allowed"))
             ((null (getf params :arg)) (%net-line :err :reason "no-arg"))
             (t (let ((killed (revoke-enrolments (getf params :arg))))
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
  "The desktop's enrolments, as ((pubkey . expiry) …).  (values NIL :unreachable) when nobody
answered — which a caller must not render as an empty list, because `no terminals are enrolled' and
`the desktop is not there' are different things to show somebody."
  (multiple-value-bind (status plist body) (apply #'admission-request :devices where)
    (declare (ignore plist))
    (if (eq status :ok)
        (loop for line in body
              for sp = (position #\Space line)
              when sp collect (cons (subseq line 0 sp)
                                    (or (ignore-errors (parse-integer (subseq line (1+ sp)))) 0)))
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
