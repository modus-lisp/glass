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
                             (:file "wordlist")   ; BIP-39 words, for naming a session
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
  ;; cl-transport carries the listeners now, and :glass takes ONLY that part of it.
  ;; cl-transport/listeners depends on nothing but sb-bsd-sockets, so glass stays free
  ;; of quicklisp -- cram and two SBCL contribs -- exactly as it was when it owned the
  ;; sockets itself.  Depending on the whole of cl-transport would have quietly put
  ;; usocket and bordeaux-threads under a display server.
  :depends-on ("glass/fb" "glass/clipboard" "cl-transport/listeners"
               "sb-bsd-sockets" "sb-posix" "cram")
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "perf")
     ;; socket before rfb: a wire is a TCP port OR a socket file only its owner can open,
     ;; and RFB is a stream protocol that cannot tell which it got.  sb-posix is here for
     ;; chmod/stat/unlink on the socket file — the access control IS the file mode.
     ;; socket.lisp moved to cl-transport/src/listeners.lisp; this borrows it back
     (:file "transports")
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

(asdf:defsystem :glass/mic
  :description "A microphone, as an object, with no wire under it.  The in-image half of audio IN,
and the mirror of what MIXER-SUBSCRIBE already was for audio OUT: ATTACH-MIC gives the session one,
MIC-PUSH feeds it, MIC-NEXT-FRAME reads it, and nothing in that requires a socket.

Split out of glass/mic-stream, which defined the microphone inside its transport -- so the only way
to have one was for something to dial in.  That was true while the only peer was a browser behind a
gateway; a viewer in the desktop's own process has no connection to accept, and would have had to
open a wire to itself for the object to exist."
  :depends-on ("glass/fb" "reed")
  :serial t
  :components ((:module "src" :serial t :components ((:file "mic")))))

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
  :depends-on ("glass/mic" "glass" "glass/audio-stream")
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
  :depends-on ("glass/mic" "glass/audio" "stave")
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

(asdf:defsystem :glass/nostr
  :description "The desktop's own identity, and the terminals it trusts: the box's Nostr key, the
enrolment store, login-token mint/verify, the `link'/`devices'/`revoke'/`help' command surface over
NIP-59 gift-wrapped DMs, and an admission service on a socket beside the screen (5903 -> 5915) for
the transports that carry people to it.  Same argument as :glass/audio-stream one port down — WHO
MAY OPEN THIS DESKTOP is a property of the desktop, not of whichever wire somebody arrived on, and
an authorization store inside one transport is one only that transport can answer from, which the
next transport then copies.  Carries both ends: START-SESSION-NOSTR to serve, and ADMISSION-ADMIT /
ADMISSION-DEVICES / ADMISSION-ALLOWED-P / ADMISSION-REVOKE for the process that asks.  OPTIONAL, so
core glass carries no crypto and no relay client: cl-nostr is a dependency of this system alone, and
a desktop without it starts exactly as before and simply admits nobody of its own accord."
  :version "0.0.1"
  :author "ynniv"
  :license "MIT"
  :depends-on ("glass" "cl-nostr" "ironclad")
  :serial t
  :components ((:module "src" :serial t :components ((:file "nostr")))))

(asdf:defsystem :glass/site
  :description "The box publishes its own client, in the image that mints links to it.  A login
link is a token AND a URL; the token was minted here and the URL was minted in another process and
carried over in an environment variable, so the day the DM bot moved into the desktop it froze —
the site was serving /k42.html while the desktop handed out a /k27.html from weeks before, on a
gateway host that had been stale all week.  That is not a bad value, it is publishing and minting
being different processes coordinating through a file.  PUBLISH-SITE (CL-NOSTR.NSITE underneath)
sets *LOGIN-URL-BASE* in the same call that published the manifest, with nothing between them but
a variable reference, so there is no reader left to go stale.  SEPARATE FROM :glass/nostr on
purpose: this system is the only thing that touches the SITE key — the authority to replace every
page served at that npub — and the WebRTC gateway loads :glass/nostr for the client half of
admission.  A system boundary is the only thing that reliably keeps code out of an image."
  :version "0.0.1"
  :author "ynniv"
  :license "MIT"
  :depends-on ("glass/nostr")
  :serial t
  :components ((:module "src" :serial t :components ((:file "site")))))

(asdf:defsystem :glass/term
  :description "A terminal emulator on glass: a real PTY + shell, an ANSI/VT
parser, and a character grid rendered with scribe, served over VNC.  No xterm,
no X.  (sb-ext:run-program + one winsize ioctl are the platform seam.)"
  ;; sb-posix for the pty's termios: SBCL knows the ioctl numbers and the struct
  ;; layout per platform, which the hand-rolled version in term.lisp did not — it
  ;; carried Linux's and so gave macOS a terminal with no echo.
  :depends-on ("glass" "glass/text" "scribe" "sb-posix")
  :serial t
  :components ((:module "src" :serial t :components ((:file "term")))))

(asdf:defsystem :glass/repl
  :description "The terminal with a Lisp listener on the other end instead of a
shell.  glass/term's grid, VT parser and scribe rendering are portable Common
Lisp; the pty, the shell and two ioctls were the only Unix in it, and on a Lisp
machine none of those exist.  TERMINAL-PTY is just a bidirectional character
stream, so this is a stream pair and a read-eval-print loop on the far end -- no
pty, no fork, no shell, no FFI."
  :version "0.0.1"
  :author "ynniv"
  :license "MIT"
  :depends-on ("glass/term")   ; sb-gray is in SBCL itself, not a module
  :serial t
  :components ((:module "src" :serial t :components ((:file "repl")))))

(asdf:defsystem :glass/test
  :description "Self-test for glass: an RFB client that drives the server.  Uses
chipz as an independent inflate oracle for the ZRLE stream (cram compresses;
a different library decompresses — a real cross-check)."
  :depends-on ("glass" "chipz")
  :serial t
  :components ((:module "test" :serial t :components ((:file "oracle")))))
