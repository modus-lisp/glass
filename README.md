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

Early and small: RFB 3.8, `None` security, keyboard/pointer input, and three
lossless encodings — Raw, Hextile, and ZRLE (zlib-compressed, via
[cram](https://github.com/modus-lisp/cram)) — with dirty-region tracking so a
mostly-static screen costs almost nothing. **Research / educational; not audited.**
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
- **RFB server** — the version + security handshake (`None`), `ServerInit`
  (32-bit X8R8G8B8), and the message loop:
  - **Dirty-region tracking** — each client keeps a snapshot; an incremental
    `FramebufferUpdateRequest` sends only the tiles that changed since, so a
    static screen costs almost nothing.
  - **Encodings** (all lossless, negotiated via `SetEncodings`; best-first
    **ZRLE > Hextile > Raw**): **ZRLE** — 64×64 tiles (solid / packed-palette /
    raw) run through one *persistent* zlib stream per connection (via
    [cram](https://github.com/modus-lisp/cram)'s `Z_SYNC_FLUSH`), the strongest
    ratio; **Hextile** — zlib-free, excellent for desktop UI (solid runs cost a
    byte or two), ~**88× smaller than Raw** on a typical 800×600 frame; **Raw** as
    the fallback. All pixel-identical.
  - **`KeyEvent` / `PointerEvent`** dispatched to caller callbacks.
  - **Desktop resize**, both ways — `DesktopSize` tells the client when the
    framebuffer changes size, and `SetDesktopSize` (client-driven, via
    `ExtendedDesktopSize`) forwards a window-resize request to an `on-resize`
    callback.

  Each client runs in its own thread; `:once` serves a single client (for tests).

## Running McCLIM apps over VNC (no X)

[`backend/`](backend/) is an optional **McCLIM backend** (`mcclim-glass`) that
renders CLIM applications into a glass framebuffer and serves them over VNC —
`(clim-glass:run-frame 'my-frame :port 5900)`, then point any VNC client at
`localhost:5900`. Pure Lisp end to end: McCLIM's software renderer draws into an
image, glass ships it, RFB input comes back as CLIM events. Stock apps
(`clim-demo::gadget-test`, …) render and interact. See [backend/README](backend/README.md).

## Not yet

ZRLE's run-length subencodings (plain/palette RLE) and the Tight encoding; a
client's format request (`SetPixelFormat`) is read but not honored (we always
serve X8R8G8B8); no VNC authentication; no `CopyRect`-based scroll optimization.
Contributions welcome.

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
