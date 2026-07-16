# scry

> Working name — rename freely.

**A framebuffer and a from-scratch VNC/RFB server in pure Common Lisp.** Draw into
an in-memory framebuffer with simple primitives, then serve it over the RFB
protocol (RFC 6143) so any VNC client can view — and drive — it. Clean-room: no
libvncserver, no FFI. The only platform dependency is SBCL's `sb-bsd-sockets` for
the default transport.

To *scry* is to see at a distance. That's what VNC is: it exports a framebuffer so
a remote client can watch it and send back keyboard and pointer events. This is
meant to give [modus](https://github.com/modus-lisp) — a bare-metal Lisp OS — a
remote display, developed and tested on SBCL first (so it drops onto modus once
its display path lands, without fighting bare-metal quirks in the meantime).

## Status & disclaimer

Early and small: RFB 3.8, `None` security, Raw encoding, keyboard/pointer input.
**Research / educational; not audited.** No warranty (see [LICENSE](LICENSE)).

Validated in-process by a self-test (an RFB client that reads the framebuffer back
and checks the pixels) and interoperated with an independent third-party RFB
client. See [Build & test](#build--test).

```lisp
(asdf:load-system "scry")

(let ((fb (scry:make-framebuffer 640 480 scry:+blue+)))
  (scry:fb-rect  fb 40 20 200 120 scry:+red+)
  (scry:fb-frame fb 0 0 640 480 scry:+white+ 2)
  (scry:serve fb 5900
              :on-key     (lambda (down keysym) (format t "key ~a ~a~%" down keysym))
              :on-pointer (lambda (buttons x y)  (format t "ptr ~a @ ~a,~a~%" buttons x y))))
;; then point any VNC client at  localhost:5900  (display :0)
```

## What it does

- **Framebuffer** — a row-major `(unsigned-byte 32)` buffer of `0x00RRGGBB`
  pixels, with clipped drawing: `fb-put` / `fb-get`, `fb-fill`, `fb-rect`,
  `fb-hline` / `fb-vline`, `fb-frame` (outline), `fb-blit` (compose), `rgb`.
- **RFB server** — the version + security handshake (`None`), `ServerInit`
  advertising a 32-bit X8R8G8B8 pixel format, `FramebufferUpdate` (Raw encoding)
  in response to update requests, and `KeyEvent` / `PointerEvent` dispatched to
  caller callbacks. Each client runs in its own thread; `:once` serves a single
  client (for tests).

## Not yet

Dirty-region tracking and the compact encodings (CopyRect, RRE, Hextile, ZRLE,
Tight) — v1 always sends the requested rect in full Raw, which every client
understands but is bandwidth-hungry; a client's format request
(`SetPixelFormat`) is read but not honored (we always serve X8R8G8B8); no VNC
authentication, no `CopyRect`-based scrolling, no text/font drawing yet, no
resize. Contributions welcome.

## Build & test

Pure Common Lisp; the only dependency is `sb-bsd-sockets`.

```lisp
(push #p"/path/to/scry/" asdf:*central-registry*)
(asdf:load-system "scry")
```

Run the self-test (exits non-zero on any failure):

```sh
./run-tests.sh
```

or from a REPL: `(asdf:load-system "scry/test")` then `(scry/test:run-tests)` —
returns `T` iff every check passes. It serves a known pattern to an in-process
RFB client and asserts the received pixels are exactly what was drawn.

## License

MIT — see [LICENSE](LICENSE).
