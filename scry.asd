;;;; scry.asd — a framebuffer + VNC (RFB) server in pure Common Lisp.

(asdf:defsystem :scry
  :description "A from-scratch VNC/RFB server in pure Common Lisp: an in-memory
framebuffer you draw into, exported over the RFB protocol so any VNC client can
view and interact with it.  Clean-room (RFC 6143) — no libvncserver, no FFI; the
only platform dependency is sb-bsd-sockets for the default transport.  Built to
give modus a remote display, developed and tested on SBCL first."
  :version "0.0.1"
  :author "ynniv"
  :license "MIT"
  :depends-on ("sb-bsd-sockets")
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "packages")
     (:file "framebuffer")
     (:file "rfb")))))

(asdf:defsystem :scry/test
  :description "Self-test for scry: an RFB client that drives the server."
  :depends-on ("scry")
  :serial t
  :components ((:module "test" :serial t :components ((:file "oracle")))))
