;;;; site-gate.lisp — publishing and minting in one image, and the handoff that cannot go stale.
;;;;
;;;;   sbcl --non-interactive --load inspect/site-gate.lisp
;;;;   GLASS_SITE_GATE_LIVE=1 sbcl --non-interactive --load inspect/site-gate.lisp   # + the network
;;;;
;;;; src/site.lisp exists because of one live failure: the site was serving /k42.html and this box
;;;; was handing out /k27.html on a different gateway host, from a build older than the box key.
;;;; Nothing was misconfigured.  `publish.lisp' wrote the new tag into `site-url.env', the KEEPALIVE
;;;; re-sourced that file every loop, and the `link' command then moved out of the gateway and into
;;;; the desktop — a process with no such loop, holding whatever LOGIN_URL_BASE its launcher had at
;;;; exec.  The writer could not tell that its reader had stopped reading.
;;;;
;;;; So the claim under test is not "the URL is right".  It is:
;;;;
;;;;     A PUBLISH AND THE NEXT MINT CANNOT DISAGREE, BECAUSE THERE IS NOTHING BETWEEN THEM.
;;;;
;;;; which is checked the only way a claim about staleness can be — by publishing TWICE with
;;;; different tags and showing the second mint follows.  A frozen variable passes the first check
;;;; and cannot pass the second; that is the whole design of this file.
;;;;
;;;; THE REAL SITE KEY IS NEVER READ AND NOTHING IS PUBLISHED UNDER IT.  ~/.glass/site-key is the
;;;; authority to replace every page served at that npub, and publishing is destructive — kind
;;;; 15128 is replaceable, so a manifest from a test would retire every path in flight and 404
;;;; every link somebody is holding.  *SITE-KEY-FILE* is bound into /tmp before anything is called,
;;;; there is an assertion that it is, and the live section generates a THROWAWAY keypair of its
;;;; own.  The live site's npub is a literal below and nothing here is allowed to produce it.

(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (asdf:load-system :glass/site)))

(defpackage #:glass-site-gate (:use #:cl)) (in-package #:glass-site-gate)

(defvar *pass* 0) (defvar *fail* 0)
(defun ok (name p &optional detail)
  (if p (progn (incf *pass*) (format t "  ok   ~a~@[ — ~a~]~%" name detail))
      (progn (incf *fail*) (format t "  FAIL ~a~@[ — ~a~]~%" name detail)))
  (finish-output))
(defun banner (s) (format t "~&~%== ~a ==~%" s))

;;; ---- the fixture, in /tmp, and never anywhere else ---------------------------

(defparameter *key-fixture* "/tmp/glass-site-gate-key")
(defparameter *url-fixture* "/tmp/glass-site-gate-url")
(defparameter *devices-fixture* "/tmp/glass-site-gate-devices")

(defparameter *real-key-file* (namestring (merge-pathnames ".glass/site-key" (user-homedir-pathname))))
(defparameter *real-url-file* (namestring (merge-pathnames ".glass/site-url" (user-homedir-pathname))))
;; The LIVE site, as a literal (check-deploy.lisp's default).  Nothing in this suite may produce it.
(defparameter *real-site-npub* "npub1ajvjnhgcmdxkng22lzsh22qvl63es78gk6p9mwksepju974teguq4l4evc")

;; A throwaway site key, fixed rather than random so a failure is reproducible.  It is 64 hex and
;; it is not the site's; that is all it has to be.
(defparameter *toy-key* "1f2e3d4c5b6a79880011223344556677889900aabbccddeeff0123456789abcd")
(defparameter *box-secret* "00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff")
(defparameter *owner* "1111111111111111111111111111111111111111111111111111111111111111")
(defparameter *device* "2222222222222222222222222222222222222222222222222222222222222222")

(dolist (f (list *key-fixture* *url-fixture* *devices-fixture*)) (ignore-errors (delete-file f)))
;; …and the login-code store that rides on the enrolment file's path.  A `link' mint records the
;; recipient there, so a leftover from the LAST run makes this run's first link say "this replaces
;; an earlier one" — which is correct behaviour and a stale test.  Cleared with the rest.
(ignore-errors (delete-file (concatenate 'string *devices-fixture* ".codes")))
(with-open-file (s *key-fixture* :direction :output :if-exists :supersede) (write-line *toy-key* s))

(setf glass:*login-codes* (make-instance 'glass:login-code-store))
(setf glass:*site-key-file* *key-fixture*
      glass:*site-url-file* *url-fixture*
      glass:*site-npub* nil
      glass:*enrolment-file* *devices-fixture*
      glass::*enrolments-mtime* nil
      glass:*box-secret* *box-secret*
      glass:*login-url-base* nil
      glass:*last-site-publication* nil)
(clrhash glass:*enrolments*)
(glass:refresh-nostr-allow *owner*)
(glass:enrol-device *device*)

(defparameter *real-key-mtime* (and (probe-file *real-key-file*) (file-write-date *real-key-file*)))
(defparameter *real-url-exists* (and (probe-file *real-url-file*) t))

(banner "the fixture is in /tmp, and the real site key is not in the picture")
(ok "the site key store is under /tmp" (eql 0 (search "/tmp/" glass:*site-key-file*)))
(ok "…and it is not ~/.glass/site-key" (not (equal glass:*site-key-file* *real-key-file*)))
(ok "the site-url memo is under /tmp" (eql 0 (search "/tmp/" glass:*site-url-file*)))
(ok "…and it is not ~/.glass/site-url" (not (equal glass:*site-url-file* *real-url-file*)))
(ok "the toy key is not the live site's key"
    (not (equal *real-site-npub* (glass:site-npub :secret *toy-key*))))

;;; ==============================================================================
(banner "the site's identity — asked for, and not kept")
;;; ==============================================================================

(ok "a 64-hex key file is an identity" (glass:site-identity-p))
(ok "and it has an npub" (let ((n (glass:site-npub))) (and (stringp n) (eql 0 (search "npub1" n)))))
(ok "SITE-SECRET reads the file every time rather than caching it"
    ;; the observable half of "the image holds no site secret between publishes": rewrite the file
    ;; and the very next call answers with the new value, with nothing to invalidate.
    (let ((other (make-string 64 :initial-element #\a)))
      (unwind-protect
           (progn (with-open-file (s *key-fixture* :direction :output :if-exists :supersede)
                    (write-line other s))
                  (equal other (glass:site-secret)))
        (with-open-file (s *key-fixture* :direction :output :if-exists :supersede)
          (write-line *toy-key* s)))))
(ok "a missing key file is NIL, not an error"
    (null (glass:site-secret :file "/tmp/glass-site-gate-nope")))
(ok "and so is a file with something that is not 64 hex in it"
    (progn (with-open-file (s "/tmp/glass-site-gate-junk" :direction :output :if-exists :supersede)
             (write-line "not-a-key" s))
           (null (glass:site-secret :file "/tmp/glass-site-gate-junk"))))
(ok "comment lines are skipped, so a key file may say what it is"
    (progn (with-open-file (s "/tmp/glass-site-gate-cmt" :direction :output :if-exists :supersede)
             (format s "# the site key~%~a~%" *toy-key*))
           (equal *toy-key* (glass:site-secret :file "/tmp/glass-site-gate-cmt"))))

;;; ==============================================================================
(banner "a versioned PATH, and the gateway host that is actually current")
;;; ==============================================================================

(ok "a tag becomes a path" (equal "/k43.html" (glass:site-link-path "k43")))
(ok "an explicit path is left alone" (equal "/standalone.html" (glass:site-link-path "/standalone.html")))
(ok "no tag is the index" (equal "/index.html" (glass:site-link-path nil)))
(ok "…and so is a blank one — an empty SITE_VERSION is an unset one"
    (equal "/index.html" (glass:site-link-path "  ")))
;; THE WART.  CL-NOSTR.NSITE:*GATEWAY* is nsite.lol and publish.lisp inherited it, so every publish
;; wrote a .lol URL that somebody then had to hand-edit — while .lol served a stale manifest for
;; builds at a time.  A default that must be corrected by hand on every deploy is not a default.
(ok "the link base names nsite.run, not the library's nsite.lol default"
    (and (equal "nsite.run" glass:*site-gateway*)
         (not (equal glass:*site-gateway* cl-nostr.nsite:*gateway*))))

;;; ==============================================================================
(banner "THE BUG, MADE IMPOSSIBLE: publish twice, and the mint follows")
;;; ==============================================================================
;;; NOSTR-COMMAND-REPLY is the DM bot's reply function — %BOT-HANDLE calls exactly this — and it is
;;; the one place a token and a URL meet.  So minting through it is minting, and if it follows the
;;; second publish then no reader in this image can be holding the first.

(defun mint-link (&optional (who *owner*))
  "What the box would DM back for `link'."
  (glass:nostr-command-reply who "link"))

(ok "with nothing published, `link' says so rather than handing out half a URL"
    (let ((r (mint-link))) (and (stringp r) (search "no published client" r))))

(multiple-value-bind (url1 pub1) (glass:note-site-publication (glass:site-npub) "k101")
  (ok "publish #1 sets the link base in memory, in the call that published"
      (equal url1 glass:*login-url-base*))
  (ok "  …and it names the tag just published"
      (search "/k101.html" url1) url1)
  (ok "  …on the gateway host we chose" (search "nsite.run" url1))
  (ok "  …and the publication remembers what it was" (equal "k101" (glass:site-publication-version pub1))))

(defparameter *link1* (mint-link))
(ok "the MINT names k101" (search "/k101.html" *link1*))
;; taken up to whitespace, not to end of string: the reply may carry a second paragraph (the
;; `link' command says so when it supersedes an earlier link), and the browser reads the fragment
;; the same way.
(defun link-code (link)
  (let* ((at (search "&code=" link))
         (start (and at (+ at 6)))
         (end (and start (or (position-if (lambda (c)
                                            (member c '(#\Space #\Newline #\Return #\Tab)))
                                          link :start start)
                             (length link)))))
    (and start (subseq link start end))))
(ok "  …and carries a token that verifies against the box secret"
    (glass:verify-login-token (link-code *link1*)))

;;; --- and now the half a frozen variable cannot do -----------------------------
;;; Second publish, different tag, NOTHING RESTARTED and nothing re-read.  A LOGIN_URL_BASE frozen
;;; at exec passes every check above this line and fails every check below it.

(defparameter *url2* (glass:note-site-publication (glass:site-npub) "k102"))
(defparameter *link2* (mint-link))
(ok "publish #2 moves the link base with no restart and no re-read"
    (and (search "/k102.html" *url2*) (equal *url2* glass:*login-url-base*)))
(ok "THE SECOND MINT FOLLOWS — this is the property a frozen variable cannot have"
    (search "/k102.html" *link2*) *link2*)
(ok "  …and the first tag is gone from it, rather than both being offered"
    (not (search "/k101.html" *link2*)))
(ok "the two links differ in more than the token" (not (equal *link1* *link2*)))
(ok "an enrolled DEVICE asking for a link gets the new tag too — one variable, not one per caller"
    (search "/k102.html" (mint-link *device*)))

;;; --- there is no file in the path, and this is how you tell --------------------
;;; ~/.glass/site-url is a memo to our own next boot.  Break it — make the write fail outright —
;;; and the mint must still follow, because the mint never reads it.

;; Under /dev/null, so the write cannot succeed however hard SAVE-SITE-URL tries: it does
;; ENSURE-DIRECTORIES-EXIST (a first publish on a box with no ~/.glass yet is a real case), and a
;; merely-absent directory would therefore be created rather than refused.
(defparameter *unwritable* "/dev/null/glass-site-gate/site-url")
(let ((glass:*site-url-file* *unwritable*))
  (glass:note-site-publication (glass:site-npub) "k103")
  (ok "a publish whose memo could NOT be written still moves the link base"
      (search "/k103.html" glass:*login-url-base*))
  (ok "  …and the mint follows it anyway — nothing between them is a file"
      (search "/k103.html" (mint-link)))
  (ok "  …and the memo really did fail to write" (not (probe-file *unwritable*))))

;;; ==============================================================================
(banner "the memo: what a COLD start knows, and what outranks what")
;;; ==============================================================================

(glass:note-site-publication (glass:site-npub) "k104")
(ok "a publish writes the memo" (probe-file *url-fixture*))
(ok "…and it reads back as the URL that was published"
    (equal glass:*login-url-base* (glass:load-site-url :install nil)))
(ok "…owner-only, because it names this box's client"
    (let ((m (ignore-errors (logand #o777 (sb-posix:stat-mode (sb-posix:stat *url-fixture*))))))
      (eql m #o600) (format nil "~o" m)))

;; THE INVERSION, said out loud: a launcher's environment is a GUESS made before anything was
;; published, and it is the thing that was wrong.  The memo was written by the publish itself.
(setf glass:*login-url-base* "https://stale.example/k27.html")   ; as if inherited from a launcher
(glass:load-site-url)
(ok "loading the memo OVERRIDES a stale inherited link base"
    (and (search "/k104.html" glass:*login-url-base*)
         (not (search "stale.example" glass:*login-url-base*)))
    glass:*login-url-base*)
(ok "a missing memo leaves the link base alone — an image that never published keeps its env"
    (let ((glass:*login-url-base* "https://from-the-launcher.example/k9.html"))
      (glass:load-site-url :file "/tmp/glass-site-gate-no-memo")
      (equal glass:*login-url-base* "https://from-the-launcher.example/k9.html")))

;;; ==============================================================================
(banner "a publish that did not land must not move the link")
;;; ==============================================================================
;;; Publishing REPLACES the manifest, so until a relay has accepted the new one the OLD paths are
;;; what resolves.  A link base moved ahead of the manifest names a page that does not exist yet —
;;; the same 404, arriving from the other direction.

(defparameter *before* glass:*login-url-base*)

;; No Blossom servers at all, so no blob reaches one, so NSITE-PUBLISH declines to publish the
;; manifest (:REQUIRE-BLOBS) — and nothing here touches the network: the relay is a closed port on
;; loopback and there are no uploads to attempt.
(multiple-value-bind (url pub)
    (glass:publish-site (list (cons "/k999.html" "<html>nope</html>"))
                        :version "k999" :secret *toy-key*
                        :servers '() :relays '("ws://127.0.0.1:1") :lookup '()
                        :on-upload nil :on-publish nil)
  (ok "a publish that stored nothing returns NIL for the URL" (null url))
  (ok "  …and the link base is untouched" (equal *before* glass:*login-url-base*))
  (ok "  …and the publication says so rather than looking successful"
      (and pub (null (glass:site-publication-url pub))))
  (ok "  …and SITE-REPORT says UNCHANGED, in words a person reads off a deploy"
      (search "UNCHANGED" (glass:site-report pub))))

(ok "no site key is a refusal, not a fallback to some other identity"
    (let ((glass:*site-key-file* "/tmp/glass-site-gate-nope"))
      (multiple-value-bind (url pub)
          (glass:publish-site (list (cons "/k998.html" "x")) :version "k998"
                              :servers '() :relays '("ws://127.0.0.1:1") :lookup '()
                              :on-upload nil :on-publish nil)
        (and (null url) (null pub) (equal *before* glass:*login-url-base*)))))

(ok "a version naming a path this publish does NOT contain leaves the link alone"
    ;; the publish itself is fine; pointing a link into a site that never had that path is not
    (let ((glass:*login-url-base* "https://kept.example/k1.html"))
      (glass:publish-site (list (cons "/k997.html" "x")) :version "k996" :secret *toy-key*
                          :servers '() :relays '("ws://127.0.0.1:1") :lookup '()
                          :on-upload nil :on-publish nil)
      (equal "https://kept.example/k1.html" glass:*login-url-base*)))

;;; ==============================================================================
(banner "live: two real publishes under a THROWAWAY key (GLASS_SITE_GATE_LIVE=1)")
;;; ==============================================================================
;;; The offline sections prove the mechanism; this proves it end to end, against real Blossom
;;; servers and real relays, with a key generated here and used nowhere else.  Off by default
;;; because it puts two small blobs on public infrastructure every time it runs.

(if (not (equal "1" (sb-ext:posix-getenv "GLASS_SITE_GATE_LIVE")))
    (format t "  --   skipped (set GLASS_SITE_GATE_LIVE=1 to publish to real relays)~%")
    (let* ((kp (cl-nostr.keys:generate-keypair))
           (secret (string-downcase (cl-nostr.keys:secret-hex kp)))
           (npub (glass:site-npub :secret secret))
           (tag1 (format nil "t~a" (glass:unix-now)))
           (tag2 (format nil "t~a" (+ 1 (glass:unix-now)))))
      (ok "the throwaway key is not the live site's" (not (equal npub *real-site-npub*)))
      (format t "  --   throwaway site ~a~%" npub)
      (let ((glass:*site-url-file* *url-fixture*))
        (multiple-value-bind (url1)
            (glass:publish-site (list (cons (glass:site-link-path tag1)
                                            (format nil "<!doctype html><title>~a</title>" tag1)))
                                :version tag1 :secret secret)
          (ok "live publish #1 landed" (not (null url1)) url1)
          (when url1
            (ok "  …and the mint names it" (search (format nil "/~a.html" tag1) (mint-link)))))
        (multiple-value-bind (url2)
            (glass:publish-site (list (cons (glass:site-link-path tag2)
                                            (format nil "<!doctype html><title>~a</title>" tag2)))
                                :version tag2 :secret secret)
          (ok "live publish #2 landed" (not (null url2)) url2)
          (when url2
            (ok "  …AND THE MINT FOLLOWS IT, with nothing restarted"
                (search (format nil "/~a.html" tag2) (mint-link)))
            (ok "  …and no longer names the first" (not (search (format nil "/~a.html" tag1)
                                                                (mint-link)))))))))

;;; ==============================================================================
(banner "nothing outside /tmp was written, and the real site key was never opened")
;;; ==============================================================================

(ok "~/.glass/site-key has the mtime it had before this suite ran"
    (equal *real-key-mtime* (and (probe-file *real-key-file*) (file-write-date *real-key-file*))))
(ok "~/.glass/site-url was not created by this suite"
    (eq *real-url-exists* (and (probe-file *real-url-file*) t)))
(ok "no npub this suite produced is the live site's"
    (not (search *real-site-npub* (or glass:*login-url-base* ""))))

(dolist (f (list *key-fixture* *url-fixture* *devices-fixture*
                 (concatenate 'string *devices-fixture* ".codes")
                 "/tmp/glass-site-gate-junk" "/tmp/glass-site-gate-cmt"))
  (ignore-errors (delete-file f)))

(format t "~&~%~a passed, ~a failed~%" *pass* *fail*)
(finish-output)
(sb-ext:exit :code (if (zerop *fail*) 0 1))
