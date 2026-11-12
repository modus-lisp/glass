;;;; test/login-link-test.lisp — what a login link carries: the client, this box's relays, the code.
;;;;   sbcl --script test/login-link-test.lisp
;;;; (with glass/nostr findable by ASDF; prints a verdict, exits 0 iff all pass)
;;;;
;;;; The relays matter because the client cannot know them: it has a built-in list, and a box
;;;; listening elsewhere is one the page publishes its offer to and never hears from.  They ride in
;;;; the QUERY (the client reads `relays' there; the fragment is for credentials).

(require :asdf)
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream))) (asdf:load-system :glass/nostr)))

(defpackage #:glass-login-link-test (:use #:cl))
(in-package #:glass-login-link-test)

(defvar *fails* 0)
(defvar *checks* 0)
(defun check (name ok &optional detail)
  (incf *checks*)
  (unless ok (incf *fails*))
  (format t "  ~:[FAIL~;ok  ~] ~a~@[   ~a~]~%" ok name detail))

(defun link (base relays)
  (let ((glass::*login-url-base* base)
        (glass::*nostr-relays* relays)
        (glass::*turn-ice* nil))
    (glass:login-link-url "TOKEN")))

(format t "~&[glass login links]~%")
(let ((u (link "https://site.example/" '("wss://a.example" "wss://b.example"))))
  (check "the box's relays are in the query, in order, before the fragment"
         (eql 0 (search "https://site.example/?relays=wss://a.example,wss://b.example#" u)) u)
  (check "the fragment carries the code" (search "code=TOKEN" u)))
(let ((u (link "https://site.example/k39.html?x=1" '("wss://a.example"))))
  (check "a base with a query gets &relays=" (search "k39.html?x=1&relays=wss://a.example#" u) u))
(let ((u (link "https://site.example/?relays=wss://mine.example" '("wss://a.example"))))
  (check "a base that names relays is left alone"
         (and (search "?relays=wss://mine.example#" u) (not (search "a.example" u))) u))
(let ((u (link "https://site.example/" '())))
  (check "no relays, no query" (eql 0 (search "https://site.example/#" u)) u))
(check "the default list is the measured one (snort, oxtr, primal), not nos.lol or damus"
       (let ((d (sb-ext:posix-getenv "NOSTR_RELAYS")))
         (or d (and (member "wss://relay.snort.social" glass::*nostr-relays* :test #'string=)
                    (not (member "wss://nos.lol" glass::*nostr-relays* :test #'string=))
                    (not (member "wss://relay.damus.io" glass::*nostr-relays* :test #'string=)))))
       (format nil "~{~a~^ ~}" glass::*nostr-relays*))

(format t "checks: ~d   failures: ~d   => ~:[FAIL~;PASS~]~%" *checks* *fails* (zerop *fails*))
(uiop:quit (if (zerop *fails*) 0 1))
