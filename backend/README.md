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

## Status

Proof of concept. Working: a live frame renders over VNC at its laid-out size,
incremental (dirty-tile) updates, and both pointer and keyboard input driving
real state changes (see `inspect/interactive.lisp`, a headless end-to-end test
that reads the framebuffer back over RFB and verifies the app reacted).

Not yet: window/desktop resize after the first paint (the fb size is fixed once
the client negotiates it — needs the DesktopSize pseudo-encoding), a pointer
cursor, multiple top-level windows (menus/dialogs share the single framebuffer),
and command `:keystroke` accelerators without an interactor pane (standard CLIM —
key events are delivered; a frame with an interactor gets accelerators for free).
