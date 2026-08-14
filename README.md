# glass

**A framebuffer and a from-scratch VNC/RFB server in pure Common Lisp.** Draw into
an in-memory framebuffer with simple primitives, then serve it over the RFB
protocol (RFC 6143) so any VNC client can view — and drive — it. Clean-room: no
libvncserver, no FFI. The only platform dependency is SBCL's `sb-bsd-sockets` for
the default transport.

A looking glass shows you a scene that isn't in front of you — and you reach
through this one. That's VNC: it exports a framebuffer so a remote client can
watch it and send back keyboard and pointer events. `glass` is meant to give
[modus](https://github.com/modus-lisp) — a bare-metal Lisp OS — a remote display,
developed and tested on SBCL first (so it drops onto modus once its display path
lands, without fighting bare-metal quirks in the meantime).

## Status & disclaimer

A version-adaptive handshake (**RFB 3.3 and 3.8**), `None` and **VNC-authentication**
security, keyboard/pointer input, and lossless encodings — Raw, Hextile, ZRLE
(zlib-compressed, via [cram](https://github.com/modus-lisp/cram)) and TRLE, plus
**CopyRect** for window moves and a **stored-block ZRLE** fast path — with
dirty-region tracking so a mostly-static screen costs almost nothing.
Interoperates with **TigerVNC** and **macOS Screen Sharing**. On top of the server
sits an optional **McCLIM OPEN LOOK desktop** (see below). **Research /
educational; not audited.**
No warranty (see [LICENSE](LICENSE)).

Validated in-process by a self-test (an RFB client that reads the framebuffer back
and checks the pixels) and interoperated with an independent third-party RFB
client. See [Build & test](#build--test).

```lisp
(asdf:load-system "glass")

(let ((fb (glass:make-framebuffer 640 480 glass:+blue+)))
  (glass:fb-rect  fb 40 20 200 120 glass:+red+)
  (glass:fb-frame fb 0 0 640 480 glass:+white+ 2)
  (glass:serve fb 5900
              :on-key     (lambda (down keysym) (format t "key ~a ~a~%" down keysym))
              :on-pointer (lambda (buttons x y)  (format t "ptr ~a @ ~a,~a~%" buttons x y))))
;; then point any VNC client at  localhost:5900  (display :0)
```

## What it does

- **Framebuffer** — a row-major `(unsigned-byte 32)` buffer of `0x00RRGGBB`
  pixels, with clipped drawing: `fb-put` / `fb-get`, `fb-fill`, `fb-rect`,
  `fb-hline` / `fb-vline`, `fb-frame` (outline), `fb-blit` (compose), `rgb`.
- **RFB server** — a **version-adaptive** handshake (**3.3** clients like macOS
  Screen Sharing get a single security type + `VNC` authentication; **3.7/3.8**
  clients get the type list + `None`), `ServerInit` (32-bit X8R8G8B8), and the
  message loop:
  - **Dirty-region tracking** — each client keeps a snapshot; an incremental
    `FramebufferUpdateRequest` sends only the tiles that changed since, so a
    static screen costs almost nothing.
  - **Encodings** (all lossless, negotiated via `SetEncodings`; best-first
    **ZRLE > Hextile > Raw**): **ZRLE** — 64×64 tiles (solid / packed-palette /
    raw) run through one *persistent* zlib stream per connection (via
    [cram](https://github.com/modus-lisp/cram)'s `Z_SYNC_FLUSH`), the strongest
    ratio; **Hextile** — zlib-free, excellent for desktop UI (solid runs cost a
    byte or two), ~**88× smaller than Raw** on a typical 800×600 frame; **TRLE**
    (16×16 tiles, no zlib); **Raw** as the fallback. All pixel-identical.
  - **`CopyRect`** — a window move sends "copy those pixels to the new spot"
    instead of re-encoding them (near-free drags on clients that support it).
  - **Stored-block ZRLE fast path** — for big/incompressible frames, skip
    deflate's LZ77 (it's the encode wall and barely helps) and emit stored zlib
    blocks: still ordinary ZRLE on the wire, ~an order of magnitude cheaper to
    encode on a fast link (`*zrle-stored-threshold*`).
  - **`TCP_NODELAY`** so small interactive frames aren't held ~40 ms by Nagle.
  - **`KeyEvent` / `PointerEvent`** dispatched to caller callbacks.
  - **Desktop resize**, both ways — `DesktopSize` tells the client when the
    framebuffer changes size, and `SetDesktopSize` (client-driven, via
    `ExtendedDesktopSize`) forwards a window-resize request to an `on-resize`
    callback.

  Each client runs in its own thread; `:once` serves a single client (for tests).

## An OPEN LOOK desktop over VNC (no X)

[`backend/`](backend/) is an optional **McCLIM backend** (`mcclim-glass`) that
renders CLIM applications into a glass framebuffer and serves them over VNC —
`(clim-glass:run-frame 'my-frame :port 5900)`, then point any VNC client at
`localhost:5900`. Pure Lisp end to end: McCLIM's software renderer draws into an
image, glass ships it, RFB input comes back as CLIM events. Stock apps
(`clim-demo::gadget-test`, the calculator, the listener, …) render and interact.

`(clim-glass:run-wm …)` goes further — a tiny **OPEN LOOK** window manager (Sun
teal workspace, title bars, drag/raise/resize/close, a workspace root menu) that
composites McCLIM apps, PTY terminals, and a browser side by side. The compositor
does the interesting work:

- **Damage-tracked, coalesced compositing** — a repaint recomposites (and
  re-encodes) only the region that actually changed, and a burst of McCLIM
  repaints coalesces to one composite per tick, not twenty.
- **Adaptive window drag** — moving a window is opaque (with `CopyRect`) while the
  connection keeps up; if the socket send-queue backs up (a big window on a client
  that can't `CopyRect`, e.g. macOS), the drag switches to a **wireframe** outline
  and snaps into place on release — so it never lags behind the cursor.
- **Live perf + control socket** — standing per-frame counters (composite/encode
  time, bytes, fps, send-queue backlog) readable, and every knob tunable, on the
  *running* server with no restart.

See [backend/README](backend/README.md).

## The other end of the wire (`:glass/client`)

`:glass/client` is an RFB **client**: connect to a remote VNC server and keep a
local glass framebuffer holding what it displays, forwarding keys and pointer
events back. ZRLE (the server's own tile tables, read backwards, over one
persistent zlib stream), CopyRect, Raw, and `DesktopSize`; it reconnects on its
own and never blocks its caller on the socket. No McCLIM, no window manager —
`glass-client:connect-remote` gives you a framebuffer and two input functions, so
it is equally a screen scraper or a bandwidth-measuring instrument.

Which makes a remote desktop a `wm-surface`: `mcclim-glass/remote` wraps it as one
and registers **"Remote desktop"** in the root menu, so another glass session runs
as a window on this one, decorated, dragged, raised and typed into like a
terminal. The part worth having is that the scroll optimisation **composes across
the boundary**: a CopyRect arriving from the inner desktop becomes this window's
translation hint, which the compositor turns into a screen strip-blit and a
CopyRect on the outer connection. Measured on a nested pair
([`inspect/nested-copyrect.lisp`](inspect/nested-copyrect.lisp)), a scroll two
desktops deep costs **153 KB/frame on the outer hop instead of 1168 KB** — 7.5x —
while the inner hop is unchanged. Deliberately carries **no clipboard and no
audio** across the boundary: ours is a session-wide selection, and bridging one
across a trust boundary automatically is a channel, not a feature.

## Sound (`:glass/audio`)

A session has a screen and it has a sound, and it can have more than one listener
for either. The optional `:glass/audio` system is the session **mixer**: sources
sum into one mix at native 48 kHz on the mixer's **own 20 ms clock**, and every
listener subscribes for a private cursor and resampler.

```lisp
(asdf:load-system "glass/audio")
(defparameter *m* (glass:mixer-start (glass:make-mixer)))
(glass:mixer-add-source *m* (reed:make-mp3-source "track.mp3" :rate 48000 :frame-samples 960)
                        :name "music" :finite t)
(glass:mixer-play *m* (glass:audio-tone 880 0.15))            ; a bell, once
(defparameter *peer* (glass:mixer-subscribe *m* :rate 8000 :frame-samples 160))
(glass:sink-source *peer*)   ; -> a thunk giving the next 20 ms frame, or NIL
```

Why it is here and not in a transport: a mixer inside one transport is a mixer
only that transport's clients can hear, and the next transport builds a second
one with its own idea of what the session sounds like. So the clock belongs to
the mix — if a consumer's pull advanced it, then with two consumers whoever asked
first would take the audio and the other would get a hole. Consumers pull from a
ring of recent frames, so a stalled listener drops frames instead of stalling the
mix or the other listeners, and each converts to its own rate (a WebRTC peer on
G.711 wants 8 kHz; a local listener should not have to sound like a phone call
because of it).

Sources are [reed](https://github.com/modus-lisp/reed) source thunks — the next
frame of mono samples, or `NIL` — so an MP3 player plugs in with no adapter, and
so does a sink (`sink-source`), which is what a transport is handed. The
arithmetic (resample / gain / sum) is reed's; this is the session policy on top.
Gate: `sbcl --non-interactive --load inspect/audio-gate.lisp` (29 checks; the
load-bearing ones are the ones a single-listener test cannot make).

### One bus, a mix per person (`:glass/headset`)

A session with two **seats** (two people watching, see
[backend/README](backend/README.md)) has two of everything a person owns, and
sound is no exception: *my mix is not yours*. It splits the way the screen does.

```lisp
(defparameter *b* (glass:make-headset :name "seat-b" :rfb-port 5923))  ; -> 5933 out, 5934 in
(glass:mix-mute (glass:headset-mix *b*) "podcast")        ; only for this person
(setf (glass:mix-source-gain (glass:headset-mix *b*) "music") 0.25d0)
(glass:speak "your build finished" :audience (list (glass:headset-mix *b*)))
```

A window's pixels are painted **once** and composited per seat; a source's frame
is pulled **once** and summed per seat. That is forced rather than pretty: a
source is a *destructive* pull, so a mixer per seat would give each of them
alternate frames of the podcast — double speed, half missing, on both screens.
So the mixer is a **bus** with one clock, a `glass:mix` is one listener's
composite (its own selection, gains, ring, sinks), and the mixer's default mix
*is* the session's — a one-seat desktop has exactly one and is exactly the old
code path, ports and all (`5903 -> 5913 / 5914`).

The **microphone** is the one thing that never joins a mix — it would echo back
to the phone that spoke and be heard by somebody who did not dial in for it — so
each seat has its own port, its own ear behind it, and its own dictation, typing
on **that seat's** keyboard into the window that seat has focused. A source may
also name an **audience**, which is how one voice reads one person's selection to
that person without a second speech engine. Gate:
`inspect/headset-gate.lisp` (39 checks, all on samples over real sockets).

## The selection (`:glass/clipboard`)

The third thing a session has exactly one of, after a screen and a sound: what
was last copied. `:glass/clipboard` is that one clipboard, and the RFB server
converts it to and from `ClientCutText` / `ServerCutText` — paste into the
desktop from your viewer, and a copy inside the session reaches every connected
viewer.

```lisp
(glass:clipboard-set (glass:session-clipboard) "https://example.com/")
(glass:clipboard-text (glass:session-clipboard))     ; -> text, serial, owner
(glass:clipboard-own  (glass:session-clipboard) my-app
                      :provider (lambda () (serialize-the-big-buffer))
                      :name "editor")                ; serialized only if pasted
(glass:clipboard-disown (glass:session-clipboard) my-app)   ; only if it still holds it
(glass:clipboard-listen (glass:session-clipboard) :me
                        (lambda (cb serial owner) ...))     ; change notification
```

It is here rather than in the RFB server for the mixer's reason: a clipboard
inside one transport is one only that transport's clients can paste from, and the
next transport grows a second one that disagrees. It is **not** shaped like the
mixer, because the selection is discrete — no clock, no ring of frames, no pull
cursor. One owned value and a change hook.

Ownership rather than a bare string is what lets a late reader be answered by the
owner (a provider thunk, so a big buffer is serialized only if somebody actually
pastes), lets a closing app retract **its own** selection and not somebody else's,
and lets a transport recognise its own writes and not echo them back — which is
also what stops two viewers of one session handing the same string back and forth
forever.

Cut text is **Latin-1** with LF line endings, per RFC 6143 §7.5.6/§7.6.4;
`latin1-bytes` / `latin1-string` do that conversion completely (CRLF and lone CR
fold to LF, characters above U+00FF substitute). UTF-8 needs the extended-clipboard
pseudo-encoding (-1063), which is not implemented — a `ClientCutText` carrying it
(a negative length) is consumed and ignored rather than half-decoded.

**Pasting works today via a documented fallback.** No app on the desktop reads a
clipboard yet, and VNC has no notion of "the focused text field", so
`glass:clipboard-paste` **types** the selection: it synthesizes key events through
the same `:on-key` callback a real client keystroke takes, so it inherits the
window manager's focus rules for free. `Shift+Insert` (the X11 paste convention,
`glass:*paste-chord*`) triggers it from any viewer. Being keystrokes, it carries
only what a keysym carries (Latin-1 printables, LF, TAB) and an app that
interprets keystrokes will interpret the paste — pasting into a shell prompt or a
text field works; pasting into a full-screen TUI does what those characters mean
there. An app that later reads the clipboard properly simply stops going through
this path.

Gate: `sbcl --non-interactive --load inspect/clipboard-gate.lisp` (66 checks; the
load-bearing ones are the ones a bare stored string cannot make — a foreign
disown, a re-asserted value, a client's own cut text coming back).

## Not yet

ZRLE's run-length subencodings (plain/palette RLE) and the Tight encoding; a
client's format request (`SetPixelFormat`) is read but not honored (we always
serve X8R8G8B8). Contributions welcome.

### Authentication

By default the server is **open** on its LAN: 3.7+ clients (TigerVNC) get `None`
(no prompt), and RFB 3.3 clients that insist on a security type — macOS Screen
Sharing — get VNC authentication with **any password accepted** (macOS refuses
`None` on 3.3, so this is the minimum that lets it connect). To actually **require
a password**, set `glass:*vnc-password*` to a string (or, for the desktop, put it
in `~/.glass-vnc-pass`): every client must then present it, DES-verified (the DES
lives in [seal](https://github.com/modus-lisp/seal); load the optional
`:glass/vncauth` system to enable enforcement — a set password with the verifier
absent fails closed). macOS saves a working password to its Keychain and stops
prompting.

**A credential belongs to one listener, not to the session.** `glass:*vnc-password*`
is what a listener that named none inherits (read at each handshake, so setting it
live still reaches a running one); `(serve fb port :password "…")` gives *that* wire
its own, and `:password nil` gives it none whatever the variable says. That
distinction is why a password no longer breaks video: the VP8 capture is an RFB
client that speaks `None`, and a session-wide password locked it out of the desktop
it was filming. A seat can now demand a password on a TCP port and demand nothing on
its UNIX socket, at once — and `clim-glass:open-seat-transport` forces the credential
off on `:rfb-unix`, whose access control is the filesystem and `SO_PEERCRED` instead.

**On demand, from the root menu.** A desktop reachable only over socket files can put
one seat on a plain VNC port while it runs: right-click the workspace → *Serve this
seat over VNC…*. It mints an 8-character credential (`glass:make-vnc-credential` —
eight because the VNC-auth DES key *is* eight bytes, so a longer one would be
theatre), opens the port, and shows the address, port and password in a window, with
the password on that seat's clipboard. The item then reads *Stop serving VNC
(0.0.0.0:5901)* — the label is the exposure indicator — and picking it closes the
port for real. With no credential (`~/.glass-vnc-pass` overrides the generated one;
`:password nil` declines it) the listener is bound to loopback instead of being
quietly exposed. See `docs/seats-and-transports.md`.

## Build & test

Pure Common Lisp; the runtime dependencies are `sb-bsd-sockets` and
[cram](https://github.com/modus-lisp/cram) (the ZRLE deflate); the test suite also
uses `chipz` as an independent inflate oracle.

```lisp
(push #p"/path/to/glass/" asdf:*central-registry*)
(asdf:load-system "glass")
```

Run the self-test (exits non-zero on any failure):

```sh
./run-tests.sh
```

or from a REPL: `(asdf:load-system "glass/test")` then `(glass/test:run-tests)` —
returns `T` iff every check passes. It serves a known pattern to an in-process
RFB client and asserts the received pixels are exactly what was drawn.

[docs/RUNNING.md](docs/RUNNING.md) covers the three ways to bring a session up — the
self-test, a bare framebuffer, the full desktop — along with what a fresh checkout
needs on its ASDF path and which rough edges still want hands.

## License

MIT — see [LICENSE](LICENSE).
