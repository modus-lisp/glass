;;;; transports.lisp — glass borrows its sockets rather than owning them.
;;;;
;;;; The listener layer moved to cl-transport (src/listeners.lisp there).  This file is
;;;; what keeps every existing caller working: GLASS:OPEN-LISTENER and its relatives are
;;;; now literally CL-TRANSPORT's symbols, imported into GLASS and exported again, so
;;;; six files here and three other repos go on saying `glass:open-listener' and get the
;;;; same function.
;;;;
;;;; Why not name them in packages.lisp: that file belongs to :glass/fb, the portable
;;;; core with no dependencies at all, and an :export there would intern
;;;; GLASS::OPEN-LISTENER as a symbol of its own -- which is precisely what IMPORT would
;;;; then refuse.  The names have to arrive here, in :glass, after cl-transport is
;;;; loaded.

(in-package #:glass)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((names '(#:listener #:tcp-listener #:unix-listener #:open-listener #:unix-listen
                 #:tcp-listen #:close-listener
                 #:listener-kind #:listener-endpoint #:listener-open-p #:listener-socket
                 #:listener-path #:listener-port #:listener-address #:listener-mode
                 #:listener-peer-policy #:listener-refused #:listener-accept #:accept-stream
                 #:runtime-dir #:socket-path #:socket-sibling
                 #:*runtime-dir* #:*socket-file-mode* #:*socket-dir-mode*
                 #:clear-stale-socket #:unix-socket-live-p
                 #:peer-credentials #:peer-allowed-p #:peer-name #:socket-fd #:*peer-policy*
                 #:open-connection #:parse-endpoint #:endpoint-string #:socket-unsent-bytes)))
    (dolist (n names)
      (let ((sym (find-symbol (string n) '#:cl-transport.listeners)))
        (unless sym
          (error "glass: cl-transport.listeners does not export ~a — the listener layer moved ~
                  there; see cl-transport/src/listeners.lisp." n))
        (import sym '#:glass)
        (export sym '#:glass)))))
