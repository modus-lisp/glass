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
                             (:file "record")
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

(asdf:defsystem :glass/client
  :description "The other end of the wire: an RFB (VNC) CLIENT, so a glass desktop
can show another one.  Connects to a remote RFB server and keeps a local glass
framebuffer holding what it displays, forwarding keys and pointer events back — a
remote desktop reduced to the same (framebuffer + on-key + on-pointer) a terminal or
a browser already is, which is what lets a window manager host it as an ordinary
window.  Shares the server's ZRLE tables (read backwards) and cram's inflate,
continued across rectangles the way RFB's one persistent zlib stream needs.  No
McCLIM and no window-manager dependency: usable headless as a screen scraper or as
the measuring instrument in a bandwidth benchmark.  Carries no clipboard and no
audio across the boundary, deliberately."
  :version "0.0.1"
  :author "ynniv"
  :license "MIT"
  :depends-on ("glass")
  :serial t
  :components ((:module "src" :serial t :components ((:file "rfb-client")))))

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

(asdf:defsystem :glass/mic-stream
  :description "A peer's microphone over a socket, the other direction of :glass/audio-stream's
relationship and on its own port beside it: the desktop listens, whatever holds the peer (the
WebRTC gateway does) connects and pushes 20 ms frames, and the desktop converts them once — by
reed, per connection — to the rate its consumer wants.  Deliberately NOT on the session mixer: a
microphone in the mix would be played back out of the desktop's own audio and down the outbound
stream to the peer that said it.  Carries both ends — START-MIC-STREAM to receive, MAKE-MIC-SENDER
to push without ever blocking the caller's receive path — and depends on no recognizer, so a
desktop that cannot transcribe can still carry a microphone."
  :version "0.0.1"
  :author "ynniv"
  :license "MIT"
  :depends-on ("glass" "glass/audio-stream")
  :serial t
  :components ((:module "src" :serial t :components ((:file "mic-stream")))))

(asdf:defsystem :glass/headset
  :description "One person's audio on a shared session: a mix of their own out (their selection
and their gains over the session's sources, composited on the one clock — never a second pull of
a source that can only be pulled once), their own microphone in, their own ear behind it, and the
keyboard their dictation types on.  The audio half of a SEAT, with no idea that McCLIM or RFB
exist: the ports are derived from the seat's screen port (5903 -> 5913 out, 5914 in), so the
primary seat's numbers are the ones a one-seat desktop has always served.  The ear and dictation
are looked up by name, so a desktop with no recognizer installed still gives every seat sound."
  :version "0.0.1"
  :author "ynniv"
  :license "MIT"
  :depends-on ("glass" "glass/audio-stream" "glass/mic-stream")
  :serial t
  :components ((:module "src" :serial t :components ((:file "headset")))))

(asdf:defsystem :glass/speech
  :description "The desktop's voice: chord (a neural TTS engine, also pure Common Lisp) as one
long-lived source in the session mix.  SPEAK queues text and returns; a thread behind it
synthesizes at whatever pace it can and hands 48 kHz samples to a thunk that only copies, because
a frame is due every 20 ms and a sentence takes about a second to build.  OPTIONAL — like
:glass/vncauth, a desktop without it is a working desktop, and chord is a dependency only of this
system.  Every listener on the session hears it, since the source is on the session's mixer and
not inside one transport."
  :version "0.0.1"
  :author "ynniv"
  :license "MIT"
  :depends-on ("glass/audio" "chord")
  :serial t
  :components ((:module "src" :serial t :components ((:file "speech")))))

(asdf:defsystem :glass/hearing
  :description "The desktop's ear: stave (a streaming Zipformer recognizer, also pure Common
Lisp) as one sink on the session mix.  A thread pulls 16 kHz frames off the mixer, gates them on
level so silence costs nothing, and transcribes what is left; HEARING-TEXT is the running
transcript.  The mirror image of :glass/speech, wired to the same mixer from the other end —
which is why the desktop can hear its own voice, and why that is the demonstration rather than a
mistake, on a box whose only audio hardware is four HDMI playbacks.  OPTIONAL: stave is a
dependency only of this system."
  :version "0.0.1"
  :author "ynniv"
  :license "MIT"
  :depends-on ("glass/audio" "stave")
  :serial t
  :components ((:module "src" :serial t :components ((:file "hearing")))))

(asdf:defsystem :glass/dictation
  :description "The ear as a keyboard: what the desktop hears, TYPED into whatever window has
focus.  Adds no input path — glass already has one in *KEY-INJECTOR*, which the RFB server fills
with its own key callback, so an injected key takes the identical route a client's keypress takes
(the WM's focused-surface rule, the terminal's pty write, the CLIM event queue) and nothing
downstream can tell the difference.  This is the wiring between that and the ear, and it is three
decisions: type only FINISHED utterances (a partial revises itself as audio arrives, and a
keystroke cannot be taken back), sentence-case on the way out (the recognizer emits bare
uppercase — HEARING-TEXT goes on saying what it really said, and only the typed copy is prettied
up), and go deaf while the desktop's own voice is talking.  That last one is why this is a system
and not three lines: an ear on the session mix hears the session's speaker, which is a
demonstration right up until it is typing, and then it is a loop."
  :version "0.0.1"
  :author "ynniv"
  :license "MIT"
  :depends-on ("glass/hearing" "glass/clipboard")
  :serial t
  :components ((:module "src" :serial t :components ((:file "dictation")))))

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
