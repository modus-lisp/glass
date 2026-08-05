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

(asdf:defsystem :glass/clipboard
  :description "The session's selection: one clipboard, written and read by
however many transports a session has.  Discrete where the mixer is streamed —
no clock, no ring of frames, one owned value and a change hook — and modelled on
X11's ownership rather than a bare string, so a late reader can be answered by
the owner (a provider thunk), a closing app can retract only ITS OWN selection,
and a transport can recognise its own writes and not echo them.  Deliberately
outside the transports, on the mixer's argument: a clipboard inside one transport
is one only that transport's clients can paste from, and the next transport grows
a second one that disagrees.  Pure CL, no dependencies; carries the Latin-1
conversion RFB's cut text needs, and the paste-as-keystrokes fallback that makes
paste work into apps that do not read a clipboard yet."
  :version "0.0.1"
  :author "ynniv"
  :license "MIT"
  :depends-on ("glass/fb")
  :serial t
  :components ((:module "src" :serial t :components ((:file "clipboard")))))

(asdf:defsystem :glass
  :description "A from-scratch VNC/RFB server in pure Common Lisp: an in-memory
framebuffer you draw into, exported over the RFB protocol so any VNC client can
view and interact with it.  Clean-room (RFC 6143) — no libvncserver, no FFI; the
only platform dependency is sb-bsd-sockets for the default transport.  Built to
give modus a remote display, developed and tested on SBCL first."
  :version "0.0.1"
  :author "ynniv"
  :license "MIT"
  :depends-on ("glass/fb" "glass/clipboard" "sb-bsd-sockets" "cram")
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "perf")
     (:file "rfb")
     (:file "zrle")))))

(asdf:defsystem :glass/vncauth
  :description "VNC Authentication (the RFB DES challenge/response) for glass's
server, via seal's DES.  OPTIONAL — core :glass carries no crypto dependency and
leaves *vnc-password* verification to a hook; loading this system installs the
verifier (so a set *vnc-password* is enforced) and pulls in seal."
  :version "0.0.1"
  :author "ynniv"
  :license "MIT"
  :depends-on ("glass" "seal")
  :serial t
  :components ((:module "src" :serial t :components ((:file "vncauth")))))

(asdf:defsystem :glass/text
  :description "First-class text on a glass framebuffer, via scribe (fb-text) —
a real, anti-aliased, gamma-correct text primitive with no McCLIM dependency.
Kept separate so the core framebuffer + RFB server stay dependency-light."
  :depends-on ("glass/fb" "scribe")
  :serial t
  :components ((:module "src" :serial t :components ((:file "text")))))

(asdf:defsystem :glass/audio
  :description "The session's sound: one mix, read by however many listeners a
session has.  Sources are reed source-thunks; the mix runs on its OWN 20 ms clock
(so no consumer's pull advances it under another consumer's feet) at native
48 kHz, and each subscriber gets a private cursor + resampler, so a WebRTC peer
at 8 kHz and a VNC client at 48 kHz hear the same mix.  Deliberately outside the
transports: a mixer inside one transport is a mixer only that transport's clients
can hear, and the next transport builds a second one."
  :version "0.0.1"
  :author "ynniv"
  :license "MIT"
  :depends-on ("glass/fb" "reed")
  :serial t
  :components ((:module "src" :serial t :components ((:file "audio")))))

(asdf:defsystem :glass/audio-stream
  :description "The session mix over a socket, for a listener in another process.  The desktop
runs the mixer; a WebRTC gateway, a recorder or another box cannot call MIXER-SUBSCRIBE across a
process boundary, so this serves one subscription per connection: a one-line self-describing
header, then 20 ms frames of signed 16-bit mono, one per period including silence.  Carries both
ends — START-AUDIO-STREAM to serve, MAKE-AUDIO-TAP to listen without blocking the consumer's
clock.  (An RFB QEMU-audio pseudo-encoding would let plain VNC clients hear the same mix; that
is a bigger change inside the framebuffer session, and this needs none of it.)"
  :version "0.0.1"
  :author "ynniv"
  :license "MIT"
  :depends-on ("glass" "glass/audio")
  :serial t
  :components ((:module "src" :serial t :components ((:file "audio-stream")))))

(asdf:defsystem :glass/term
  :description "A terminal emulator on glass: a real PTY + shell, an ANSI/VT
parser, and a character grid rendered with scribe, served over VNC.  No xterm,
no X.  (sb-ext:run-program + one winsize ioctl are the platform seam.)"
  :depends-on ("glass" "glass/text" "scribe")
  :serial t
  :components ((:module "src" :serial t :components ((:file "term")))))

(asdf:defsystem :glass/test
  :description "Self-test for glass: an RFB client that drives the server.  Uses
chipz as an independent inflate oracle for the ZRLE stream (cram compresses;
a different library decompresses — a real cross-check)."
  :depends-on ("glass" "chipz")
  :serial t
  :components ((:module "test" :serial t :components ((:file "oracle")))))
