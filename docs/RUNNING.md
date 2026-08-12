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
sbcl --control-stack-size 256 --dynamic-space-size 4096 \
     --load backend/inspect/serve-desktop.lisp
```

Blocks, serving a wallpapered workspace with the Apps menu (Calculator, Browser,
Inspector, Debugger, Image Viewer, Listener, …) and one terminal.

`GLASS_DISPLAY` picks the display number, X-style — **every port the desktop owns
is derived from it**, so a second desktop is one environment variable, not a fork
of the file:

| what | port | display 1 (default) |
| --- | --- | --- |
| VNC / RFB | `5900 + N` | 5901 |
| session audio | `5910 + N` | 5911 |
| control + eval socket | `4008 + N` | 4009 |

The control socket reads one form, evals it in `clim-glass`, and writes the printed
result — live perf and every knob, on the *running* desktop with no restart:

```sh
echo '(glass:perf-report)' | nc -q1 127.0.0.1 4009
```

The VNC password comes from `~/.glass-vnc-pass` if that file exists (create it
yourself, mode 600 — it is not in the repo). Absent, the server keeps the open
posture described in the README.

## Prerequisites

- **SBCL.** The only platform dependency is `sb-bsd-sockets`; the desktop also uses
  `sb-thread` and `sb-concurrency`.
- **Quicklisp at `~/quicklisp`.** Every script under `inspect/` and
  `backend/inspect/` opens with `(load "~/quicklisp/setup.lisp")`. A Quicklisp
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
| `mcclim-glass` (`backend/`) | `mcclim`, `mcclim-render` | Quicklisp dist |
| desktop extras | `pigment`, `loom`, `warren` | sibling repos, all optional |

`backend/mcclim-glass.asd` is **not** registered by `glass.asd` — the scripts under
`backend/inspect/` load it themselves, relative to their own `*load-truename*`. To
use the backend from your own REPL, `(asdf:load-asd "…/glass/backend/mcclim-glass.asd")`
first, or symlink that file into `local-projects/` too.

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
   and `backend/inspect/` no longer need it — each registers the whole workspace
   with ASDF, derived from its own `*load-truename*` — but working from your own
   REPL still means wiring `local-projects/` (or a `CL_SOURCE_REGISTRY`) by hand,
   and the failure mode, a `SYSTEM-NOT-FOUND` deep in a quickload, does not say
   which repo is missing or where to get it.
3. **`backend/mcclim-glass.asd` is invisible to ASDF** unless a script loads it.
   `glass.asd` does not reference it, so the same REPL caveat applies.
4. **`:glass/hearing` cannot be built from the public repos.** It depends on
   `stave`, and there is no `github.com/modus-lisp/stave` — the org's repo list
   does not include it and `api.github.com/repos/modus-lisp/stave` is a 404. So a
   full org sync still leaves this one system unbuildable. Either stave is private
   / unpublished, or the dependency needs to name where it actually comes from.
5. **`scribe` defines a `DEFLATE` package** that collides with the Quicklisp
   `deflate` system pulled in by McCLIM's PNG path. The load survives only because
   every script wraps it in `(handler-bind ((warning #'muffle-warning)) …)`; a
   plain `(asdf:load-system :mcclim-glass)` fails on the package-variance warning.
6. **The `*wav*` fixture in the audio gates** points at a path inside stave's
   checkout, unlike the voice and ears models it has no environment-variable
   override.
