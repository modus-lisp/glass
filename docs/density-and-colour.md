# Density and colour — HiDPI now, HDR not yet, and how not to foreclose it

A design note, written before it is built, in the shape `seats-and-transports.md` and
`carriers-and-authentication.md` use: the argument first, the refusals recorded, and the
things we are choosing *not* to do said out loud so nobody has to re-derive them later.

**Status:** design, nothing built. Written from reading the code, with the specific facts
below checked rather than assumed.

---

## What glass believes today

Four facts, all verified:

| | |
|---|---|
| a framebuffer is | `(simple-array (unsigned-byte 32))`, X8R8G8B8 — 8 bits a channel |
| the only notion of density is | `+glass-dpi+ 96`, one global `defparameter` |
| what it is used for | CLIM graft unit conversion (`:inches`, `:millimeters`) and nothing else |
| the colour space is | **not named anywhere**; sRGB is an unstated assumption |

`+glass-dpi+`'s own docstring says "*Assumed* pixel density". It is honest about being a
guess, and it is a guess made once for the whole session rather than per screen — which is
the thing HiDPI breaks.

## The good news, which is structural

**glass does not blit fixed bitmaps. It rasterises vectors.** gesso builds paths and fills
them antialiased; scribe renders glyphs at a given `ppem` — pixels per em, passed in
explicitly and already threaded through the terminal, the REPL and the tab bar.

That means HiDPI is *not* a rendering problem here, which is the part that is expensive in
most systems. Double the ppem and glyphs are genuinely sharper, not upscaled. Double a
stroke width and the curve is re-rasterised, not stretched. Nothing needs a second asset at
@2x, because there are no assets.

**And the argument has already been made once in this codebase, correctly.** The seat's
wallpaper carries this comment:

> The wallpaper is per-seat because it is rasterised AT THE SCREEN SIZE: a phone seat and a
> desktop seat looking at the same session want the same image and not the same pixels.
> The PATH is the session's taste; these are one seat's pixels.

That is the whole HiDPI design, already stated and already implemented — for exactly one
asset. Density is the same observation applied to everything else a seat draws.

## Where the scale lives

**On the seat, not on the session, and not on the framebuffer.**

Not the session, for the reason the wallpaper comment gives: two people watching one
session from a phone at 3x and a laptop at 2x want the same desktop and different pixels.
A session-wide scale would make one of them wrong, and which one is wrong would depend on
who connected first.

Not the framebuffer either, and this is the part worth being careful about. **The
framebuffer stays in device pixels and knows nothing about scale.** The tempting move —
introducing a "logical pixel" that everything is expressed in — infects every call site in
the compositor with a conversion, and the conversions are where the off-by-one bugs live.
A framebuffer is a rectangle of real pixels; that is a good definition and it should not
change. The seat says how big a pixel *means*, and only layout consults it.

So: `seat-scale`, a rational (2, 3, 3/2 — fractional scaling is real and 1.5 is common on
Linux), defaulting to 1, beside `screen-w` and `screen-h` which are already there.

## What actually has to change, honestly ordered

The rasterising is nearly free. The work is in the constants, and there are more of them
than one would like.

1. **Text.** Every `ppem` becomes `(* ppem scale)` — 32 mentions across `src/` and
   `backend/`. Cheap, mechanical, and the single biggest visible win, because this is what
   "sharp" means to a reader.

   Verified rather than assumed, since the whole "nearly free" argument rests on it:
   `scribe:rasterize-glyph` scales the *outline* by `ppem/upem` out of font units and
   grid-fits at the target size. A bigger ppem is a genuine re-rasterisation, not a cached
   bitmap scaled up. Hinting and sub-pixel positioning both happen at the target size too.
2. **Layout constants.** `+wm-titleh+` (18 uses), the 8-px title-bar insets, the 4-px
   button offsets, the `(+ 40 c)` window cascade. Each is a device-pixel number that has to
   become `(round (* scale n))`. Mechanical, broad, and the bulk of the diff — the part
   that gets harder to retrofit the longer it waits, which is the argument for sooner.

   Counted, because the size of this decides everything: `wm.lisp` has 106 `fb-` drawing
   calls containing a bare number, inside an upper bound of 232 integer literals in the
   2..400 range (which still includes colour components and indices). So the real figure is
   somewhere under a hundred and above a few dozen — an afternoon or two of careful work,
   not a week, and not something to fear.
3. **`+glass-dpi+` becomes per-seat.** CLIM asks a graft how many inches wide it is; with a
   scale, the answer differs per seat, and the constant is already the wrong shape for that
   (it is a global, and grafts belong to a port).
4. **Hit-testing and input.** Pointer coordinates arrive from RFB in device pixels, which
   stays correct and needs nothing. But anything that compares a pointer position against a
   layout constant is comparing against a number that just changed.
5. **The wallpaper** already does the right thing and needs nothing. It is the worked
   example to copy.

## The cost nobody can design away

**RFB has no concept of scale.** SetDesktopSize and ExtendedDesktopSize carry width and
height in pixels and nothing else — there is no DPI field, and no encoding negotiates one.
A 2x seat is simply four times the pixels on the wire.

That is not a reason to avoid HiDPI; it is the reason the scale must be **per seat**. A
local SDL window on a Retina panel wants 2x and can afford it — the pixels never leave the
process. A phone on a hotel network wants a small desktop at 1x far more than it wants a
sharp one, and today it has no way to say so because the concept does not exist.

Related, and worth stating because it is counter-intuitive: on the local viewer, HiDPI is
what makes the picture *cheaper* to be correct about, not dearer. Without
`SDL_WINDOW_ALLOW_HIGHDPI` the window server upscales our backing store 2x, so we are
already paying for a 2x panel and merely declining to use it.

## HDR — not crazy, but four layers away

Not a silly question. It is a modern reality, and the reason to say "not yet" is specific
rather than a shrug: **four independent layers assume 8-bit sRGB, and widening any one of
them alone buys nothing.**

1. **The framebuffer** is a packed `(unsigned-byte 32)`. HDR needs ≥10 bits a channel, so
   this becomes a different element type and every fast path that touches pixels as u32 is
   rewritten.
2. **RFB** cannot carry it. The pixel format negotiation tops out at 8 bits per channel in
   every implementation that exists, and there is no transfer-function field to say what the
   numbers mean.
3. **VP8**, on the video-primary path, is 8-bit and Rec.601. HDR over that path means VP9 or
   AV1 with 10-bit profiles — a different encoder, not a flag.
4. **No colour space is named**, so there is nothing for a second one to be different *from*.

Doubling the framebuffer's memory and slowing every blit, to feed a transport that cannot
carry the result to a codec that cannot encode it, is the definition of premature.

### The one cheap thing that keeps the door open

**Name the colour space.** Today sRGB is an assumption living in nobody's head in
particular; `rgb`'s docstring says "an X8R8G8B8 pixel from 8-bit R, G, B" and says nothing
about what those numbers mean. Writing "these are sRGB, non-linear, 8 bits" costs one
docstring and is the prerequisite for ever having a second answer — you cannot convert
between colour spaces when one of them is unstated.

Two things already point the right way and should be kept that way:

- **Pixel construction is behind `rgb`.** Call sites say `(glass:rgb 61 122 138)`, not a
  packed integer. The representation can change without touching them — which is exactly
  the property a future widening needs. New code should keep using it and not hand-pack.
- **gesso already distinguishes linear light from device space.** `*straight-composite*`
  exists precisely because image blits must composite in device (gamma) space while the
  normal path uses scribe's linear-light coverage blend. That distinction is the hard
  conceptual half of colour management and it is already made, deliberately, with the
  reasoning written down.

So the honest HDR plan is: *do nothing, but stop being silent*. The gap between here and
HDR is four layers of real work; the gap between here and being *able to reason about it*
is one docstring.

## What I would build first

In order, each useful on its own:

1. `seat-scale`, defaulting to 1, plumbed to ppem only. Text gets sharp; nothing else
   moves. Small, self-contained, and immediately visible.
2. `SDL_WINDOW_ALLOW_HIGHDPI` in glass-sdl, with the seat's scale set from the ratio of
   drawable pixels to window points. This is where the concept earns its keep, and it makes
   the local viewer render at the panel's real resolution instead of being upscaled.
3. The layout constants, as one mechanical pass.
4. Per-seat `+glass-dpi+`, so CLIM's graft answers per screen.

Not planned: any change to the framebuffer's element type, any RFB extension, any codec
work. Those are what HDR would need, and HDR is not what this is for yet.

## Not checked

Written from reading, with two things checked because the argument depended on them:
`scribe:rasterize-glyph` really does re-rasterise from outlines at the requested ppem, and
the layout-constant count in `wm.lisp` is bounded above by 232 and realistically well under
a hundred.

Not checked: that fractional scales (3/2) survive the rounding without seams — the
compositor works in integers and a half-pixel window edge has to land somewhere, which is
where fractional scaling goes wrong in every system that has tried it. Nor that McCLIM's
own geometry, which has its own idea of what a pixel is, tolerates a graft whose DPI is not
96. Both are worth a spike before step 3 rather than after.
