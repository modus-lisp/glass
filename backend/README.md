# mcclim-glass — a McCLIM backend on glass

Run **McCLIM** applications over VNC with no X server. McCLIM's software
renderer (`mcclim-render`) already rasterizes every CLIM drawing operation —
anti-aliased fills, TrueType text, gradients — into an ARGB image; this backend
copies that image into a [glass](../) framebuffer and serves it over RFB, and
translates RFB keyboard/pointer input back into CLIM events. Pure Common Lisp
end to end (McCLIM, mcclim-render + deps, glass) — no FFI, no X.

It is the same shape as McCLIM's stock `clx-fb` backend, with the one
display-specific hook (`clx-fb`'s `image-mirror-put` = `xlib:put-image`) replaced
by a framebuffer copy, and X's event source replaced by glass's `on-key` /
`on-pointer` callbacks feeding a CLIM event queue.

## Use

```lisp
(ql:quickload :mcclim)            ; and mcclim-render, glass on your ASDF path
(asdf:load-system :mcclim-glass)

(clim-glass:run-frame 'my-frame :port 5900 :width 800 :height 600)
;; then point any VNC client at localhost:5900
```

`run-frame` makes an application frame on a `:glass` port and runs its top-level
loop (blocking). The port starts an RFB server on the first paint and the CLIM
event-loop thread that drives it.

## An OPEN LOOK desktop

`(clim-glass:run-wm SPECS &key port width height background)` runs a tiny
**OPEN LOOK** window manager over VNC: a Sun-teal (or image/SVG) workspace with
decorated windows — title bars with the abbreviated-menu button, drag/raise,
functional resize and close, a right-click workspace root menu — compositing
McCLIM application frames, PTY **terminals**, and a **browser** side by side. Each
`spec` is a window: `(FRAME-CLASS …)`, `(:terminal …)`, `(:browse URL …)`,
`(:inspect FORM)` (Clouseau), `(:edit …)` (Climacs), … The compositor adds:

- **Damage-tracked, coalesced compositing** — recomposite + re-encode only the
  region that changed; a burst of McCLIM repaints coalesces to one composite per
  tick.
- **Adaptive drag** — opaque with `CopyRect` while the connection keeps up;
  switches to a **wireframe** outline when the socket send-queue backs up (a big
  window on a no-`CopyRect` client like macOS), snapping into place on release.
- **Live perf + control socket** — standing per-frame counters and every knob
  tunable on the running server (see `inspect/serve-desktop.lisp`, which also
  opens a bare-TCP eval socket: `echo '(glass:perf-report)' | nc -q1 127.0.0.1 4009`).

## Multi-seat: several people, one session

`(clim-glass::add-wm-seat PORT :port-num N :width W :height H)` attaches a **second
person** to a running desktop — a screen of their own on RFB port `N`, with their own
pointer, keyboard, focus, open menu, clipboard, and **arrangement of the same windows**.

The axis is: **what runs** is shared, **who watches** is per-seat. The applications and
each window's *content* framebuffer are shared, and so is each window's *size* — size is
what an application lays its content out to, so a per-seat size would be a per-seat
layout, which is a second copy of the application. Position and stacking carry no such
consequence, so they are per-seat: two people can hold the same window in different
places, with different things in front of it, and click, drag and type independently.

A seat's view of a window is **copy-on-write** against the window's own slots
(`wm-surface-x`, `wm-mirror-x`, `wm-window-z`), so a seat that has never moved a
particular window has no record for it and reads those — a seat joining a running
session sees the desktop as it stands, and a one-seat session carries no view records at
all. One seat is **primary** and writes through to those slots, because McCLIM's sheet
geometry is single-valued and somebody has to own the position it believes in.

**McCLIM windows are one consolidated seat**, and this is a documented seam rather than
a bug: a CLIM port has one pointer and one keyboard focus, so the last seat to *press a
button* inside a CLIM window drives it, and another seat's bare pointer motion is
dropped rather than dragging CLIM's one pointer around under their hands. Everything the
*window manager* does — see, move, raise, lower, resize, close — stays per-seat for CLIM
windows too. Native glass surfaces (terminals, the browser, warren, a nested remote
desktop) carry no such assumption and are per-seat all the way down.

Cursors need no code: glass sends the cursor *shape* once as an RFB pseudo-encoding and
each viewer draws it at its own pointer. Audio and the microphone are **not** per-seat
yet — the mix reaches a listener over a separate socket, so that is work in
`glass/audio-stream` and `glass/mic-stream`, not in the compositor.

`backend/inspect/seat-gate.lisp` holds two seats on one session and checks all of the
above from the pixels.

Also a **message-port** backend (`clim-glass:make-message-port`) + **compositor**
that run each app as an isolated actor drawing to a shared display over a mailbox —
an unmodified CLIM frame across an actor boundary — the shape glass takes toward
modus.

## Status

Working over VNC (validated with **TigerVNC** and **macOS Screen Sharing**): live
frames at their laid-out size, incremental (dirty-tile) updates, pointer +
keyboard input, **desktop resize both ways** (`DesktopSize` / `SetDesktopSize`),
and the OPEN LOOK desktop above. Headless in-process proofs under `inspect/`
(`interactive`, `compositor-proof`, `mcclim-damage-proof`, `adaptive-drag-proof`,
`handshake33-proof`, …) read the framebuffer back over RFB, or the compositor
state directly, and verify behavior.

Not yet: a pointer cursor shape; multiple top-level windows of one frame
(menus/dialogs) still share the single framebuffer; command `:keystroke`
accelerators without an interactor pane (standard CLIM — key events are delivered;
a frame with an interactor gets accelerators for free).
