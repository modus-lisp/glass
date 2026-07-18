;;;; packages.lisp — glass

(defpackage #:glass
  (:use #:cl)
  (:documentation
   "glass — a framebuffer and a from-scratch VNC/RFB server in pure Common Lisp.
    To glass is to see at a distance; a VNC server does exactly that — it exports a
    framebuffer so a remote client can view and drive it.  Draw into an in-memory
    FRAMEBUFFER with simple primitives, then SERVE it over RFB (RFC 6143) to any
    VNC client.  Clean-room, no FFI; sb-bsd-sockets is the only platform seam.
    Meant to give modus a display; grown and tested on SBCL first.")
  (:export
   ;; framebuffer
   #:make-framebuffer #:framebuffer #:framebuffer-p
   #:fb-width #:fb-height #:fb-pixels #:fb-resize #:with-fb-locked #:fb-generation #:fb-touch
   #:fb-put #:fb-get #:fb-fill #:fb-rect #:fb-hline #:fb-vline #:fb-frame #:fb-blit
   #:rgb #:+black+ #:+white+ #:+red+ #:+green+ #:+blue+
   ;; text (the :glass/text system; scribe-backed)
   #:fb-text #:text-width #:load-font #:default-font
   ;; server: (serve fb port &key on-key on-pointer on-resize name once)
   #:serve #:serve-one #:*desktop-name* #:tcp-listen))
