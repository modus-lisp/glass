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

## Not yet

ZRLE's run-length subencodings (plain/palette RLE) and the Tight encoding; a
client's format request (`SetPixelFormat`) is read but not honored (we always
serve X8R8G8B8); VNC authentication completes the challenge/response but does not
*verify* the password (any password is accepted — the same open posture as
`None`; real enforcement needs a DES verify). Contributions welcome.

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

## License

MIT — see [LICENSE](LICENSE).
