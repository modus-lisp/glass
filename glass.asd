;;;; glass.asd — a framebuffer + VNC (RFB) server in pure Common Lisp.

(asdf:defsystem :glass/fb
  :description "The pure display core: an in-memory framebuffer with clipped
drawing primitives.  Portable Common Lisp — no FFI, no sockets; the only platform
touch is an sb-thread lock that guards resize, feature-gated to a no-op where
sb-thread is absent.  This is the piece that drops onto modus on bare metal;
:glass adds the VNC/RFB transport on top, :glass/text adds scribe text."
  :version "0.0.1"
  :author "ynniv"
  :license "MIT"
  :depends-on ()
  :serial t
  :components ((:module "src" :serial t
                :components ((:file "packages")
                             (:file "framebuffer")))))

(asdf:defsystem :glass
  :description "A from-scratch VNC/RFB server in pure Common Lisp: an in-memory
framebuffer you draw into, exported over the RFB protocol so any VNC client can
view and interact with it.  Clean-room (RFC 6143) — no libvncserver, no FFI; the
only platform dependency is sb-bsd-sockets for the default transport.  Built to
give modus a remote display, developed and tested on SBCL first."
  :version "0.0.1"
  :author "ynniv"
  :license "MIT"
  :depends-on ("glass/fb" "sb-bsd-sockets" "cram")
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "rfb")
     (:file "zrle")))))

(asdf:defsystem :glass/text
  :description "First-class text on a glass framebuffer, via scribe (fb-text) —
a real, anti-aliased, gamma-correct text primitive with no McCLIM dependency.
Kept separate so the core framebuffer + RFB server stay dependency-light."
  :depends-on ("glass/fb" "scribe")
  :serial t
  :components ((:module "src" :serial t :components ((:file "text")))))

(asdf:defsystem :glass/test
  :description "Self-test for glass: an RFB client that drives the server.  Uses
chipz as an independent inflate oracle for the ZRLE stream (cram compresses;
a different library decompresses — a real cross-check)."
  :depends-on ("glass" "chipz")
  :serial t
  :components ((:module "test" :serial t :components ((:file "oracle")))))
