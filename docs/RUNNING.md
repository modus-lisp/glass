# Running a glass session

A *session* is an RFB server: a framebuffer, its sound, and its selection, exported
to any number of VNC clients. There are three tiers, from a self-check that needs
nothing to the full OPEN LOOK desktop.

## 1. The self-test — nothing but SBCL and cram

```sh
./run-tests.sh          # LISP=sbcl by default; exits non-zero on any failure
```

Serves a known pattern to an in-process RFB client and asserts the received pixels
are exactly what was drawn. No VNC client, no ports, no McCLIM.

## 2. A framebuffer over VNC

```lisp
(asdf:load-system "glass")
(let ((fb (glass:make-framebuffer 640 480 glass:+blue+)))
  (glass:fb-rect fb 40 20 200 120 glass:+red+)
  (glass:serve fb 5900))
```

Point any VNC client at `localhost:5900` (display `:0`). `:once` instead of the
default serves a single client and returns — that is what the tests use.

## 3. The OPEN LOOK desktop

```sh
sbcl --dynamic-space-size 4096 --eval '(require :asdf)' \
     --eval '(asdf:load-system "glass/wm")' \
     --eval '(glass.wm:serve-wm 5900 :width 1024 :height 700)'
```

Blocks, serving glass's window manager (`:glass/wm`) with its root menu of glass windows
on `localhost:5900`.  modus's `scripts/glass-desk.lisp` does the same from a source tree
(`DESK_PORT`, `DESK_SIZE`), and also loads Files (warren) and Browser (loom) when they are
there.

The McCLIM desktop this section used to start (`backend/inspect/serve-desktop.lisp`, with
its display-numbered ports and control socket) was removed on 2026-11-25.

## Or skip all of it

[kiln](https://github.com/modus-lisp/kiln) packages every prerequisite below —
SBCL, Quicklisp and all 38 sibling repos — into one container image:

```sh
kiln build && kiln vnc
```

The rest of this file is what that image is doing on your behalf, and what to do
if you would rather run the desktop directly.

## Prerequisites

- **SBCL.** The only platform dependency is `sb-bsd-sockets`; the desktop also uses
  `sb-thread`.
- **Quicklisp at `~/quicklisp`.** Every script under `inspect/` opens with `(load "~/quicklisp/setup.lisp")`. A Quicklisp
  installed anywhere else needs that line changed, or ASDF configured by hand.
- **The sibling repos on your ASDF path.** glass is one repo in a workspace of
  them; nothing vendors anything. The usual arrangement is a symlink per `.asd`
  into `~/quicklisp/local-projects/`.

Which sibling each system wants:

| system | needs | where it comes from |
| --- | --- | --- |
| `glass/fb`, `glass/clipboard` | — | — |
| `glass` | `cram` | sibling repo |
| `glass/test` | `chipz` | Quicklisp dist |
| `glass/vncauth` | `seal` | sibling repo |
| `glass/text`, `glass/term` | `scribe` | sibling repo |
| `glass/audio`, `glass/audio-stream`, `glass/mic-stream` | `reed` | sibling repo |
| `glass/speech` | `chord` + a voice model | sibling repo + model file |
| `glass/hearing` | `stave` + recognizer models | sibling repo + model files |
| desktop extras | `pigment`, `loom`, `warren` | sibling repos, all optional |


### Voice and ears

`:glass/speech` and `:glass/hearing` need model files that are not in any repo:

- `GLASS_VOICE` — path to the chord `.graph` to speak with (its `.bin` and config
  sit beside it). Also settable as `glass:*speech-voice*`.
- `GLASS_EARS` — directory holding stave's three `.graph` files and `tokens.txt`.
  Also settable as `glass:*hearing-models*`.

Both are optional everywhere: `serve-desktop.lisp` wraps them in `ignore-errors`
and prints what it found, and the gates under `inspect/` *skip* rather than fail
when the models are absent. A silent desktop is a working desktop.

## Known rough edges

These are the places a fresh checkout still needs hands. Tracked here so the list
shrinks rather than gets rediscovered.

1. **`~/quicklisp/setup.lisp` is hardcoded** in 54 scripts. A `LISP`-style
   environment variable, or falling back to an already-loaded ASDF, would let the
   scripts run against any Quicklisp (or none).
2. **Nothing links the sibling repos for a REPL.** The scripts under `inspect/`
   no longer need it — each registers the whole workspace
   with ASDF, derived from its own `*load-truename*` — but working from your own
   REPL still means wiring `local-projects/` (or a `CL_SOURCE_REGISTRY`) by hand,
   and the failure mode, a `SYSTEM-NOT-FOUND` deep in a quickload, does not say
   which repo is missing or where to get it.
3. **The `*wav*` fixture in the audio gates** points at a path inside stave's
   checkout, unlike the voice and ears models it has no environment-variable
   override.

*Fixed since this list was written:* the gates under `inspect/` used to name one container's absolute paths in their
`(asdf:load-asd ...)` calls, so they could not run from any other checkout. That
was not cosmetic — it is why a client-side ZRLE regression went unnoticed, since
`inspect/remote-gate.lisp` is exactly the test for it and could not be run. Every
such call now derives the `.asd` from the loading file's own `*load-truename*`,
and `climacs-gate.lisp`, which uses a path as *test data* for a directory
predicate, asks about this checkout's own root.

