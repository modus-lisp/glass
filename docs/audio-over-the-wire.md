# Handoff: the box's mix, heard from another process

## Where this stands

`:glass/audio` (src/audio.lisp, gate inspect/audio-gate.lisp, 29/0) is done and
correct: one mixer per session, its own 20 ms clock, N subscribers each with a
private cursor + resampler, so an 8 kHz WebRTC peer and a 48 kHz listener hear
the same mix. Its header argues at length that the mixer is deliberately
*outside* any transport, because a mixer inside one transport is a mixer only
that transport's clients can hear.

That argument is currently false in practice. The only mixer running on this box
lives in the **gateway** process (webrtc-data/demo/glass-webrtc/gateway-nostr.lisp
:341-390), which builds its own, registers `AUDIO_MP3` on it, and subscribes one
sink per peer. The glass desktop serving :3 — the desktop the gateway captures
video from, pid 3701235, launched from `/home/claude/warren/desktop-5903.lisp`,
control socket 4013 — has no mixer at all: `glass/audio` is not loaded in it.

So the box's picture comes from the desktop and the box's sound comes from
somewhere else. Anything that is not the WebRTC gateway (a second gateway, a
recorder, a future VNC-side audio path, the desktop's own UI sounds) would build
a *third* mixer and we would be exactly where the header says not to be.

**The job: put the mixer where the desktop is, and give other processes a way to
subscribe across the process boundary.**

## Decisions already made (so they don't get re-derived)

**Push, not pull.** The sink contract is "ask, cheaply, never block". Across a
socket a pull becomes a round trip on a 20 ms clock — 50 RTTs/sec, and through
the eval control socket it would be 50 connections/sec through a single-form
REPL. Invert it: the server already owns a clock (the mixer's timer thread), so
it writes a frame per tick and the socket carries them. TCP backpressure plus the
existing per-sink trim is the flow control. The client spins a reader thread
filling a queue and its source thunk pops from that queue — which is literally
what `webrtc-media:start-audio`'s docstring demands of a source ("read from a
queue a capture thread fills, do not wait on a device").

**A new file and a new system, not audio.lisp.** `src/audio-net.lisp`,
`:glass/audio-net`, `:depends-on ("glass/audio" "sb-bsd-sockets")`. `:glass/audio`
stays socket-free and modus-portable; audio-net is the SBCL seam, the same
relationship `:glass` has to `:glass/fb`. Putting a socket in audio.lisp would
falsify its own header in the first file.

**Wire format, fixed-size frames, no per-frame header.** Handshake is one line
each way so it stays pokeable with `nc`:

    client -> "glass-audio 1 <rate> <frame-samples> <name>\n"
    server -> "ok <rate> <frame-samples>\n"   |   "err <reason>\n"
    then    -> raw little-endian s16 mono, exactly <frame-samples> per frame,
               one frame per mixer tick, forever.

The frame size is fixed by the handshake, so a length prefix carries nothing. A
sequence number also carries nothing: over TCP the frames either arrive in order
or the connection is dead, and there is no retransmit to ask for. Stats belong on
the control socket (`mixer-report`), not in the audio stream.

**A frame the sink couldn't fill is written as silence, not skipped.** Same
reason the RTP sender does it: on a fixed-size stream the *count* of frames is
the clock, so a skipped frame is a timing error the client cannot see. NIL from
`sink-next-frame` means write zeros and move on.

**Bind 127.0.0.1.** This is a same-box seam. The mix is the desktop's audio;
binding 0.0.0.0 makes it audible to the LAN with no auth. Port **4014** is free
(4006-4009 and 4013 are taken).

**The client belongs in glass too**, not in the gateway — anything on the box may
want to listen. Suggested shape, mirroring the local API so a consumer can swap
between them with one line:

    (glass:serve-audio mixer &key (port 4014) (address "127.0.0.1")) => stop-thunk
    (glass:connect-audio &key host port rate frame-samples name)
        => (values source-thunk stop-thunk report-thunk)

`source-thunk` obeys reed's contract exactly: next frame, or NIL meaning ask
again. The client needs its own bounded cushion with the same argument as
`%sink-trim` (audio.lisp:286) — a reader slower than the sender accumulates
latency without bound, and for live audio that is worse than a gap. Two frames of
cushion, drop the oldest past four; correct by one frame, not a lump (that was
commit 298efcf's lesson).

**The MP3 moves to the desktop.** The box plays music; peers hear the box. So
`desktop-5903.lisp` gains the mixer, the looping source, and `serve-audio`, and
the gateway loses `%mp3-loop-source` + the source registration in `ensure-mixer`.

**The gateway keeps a local-mixer fallback.** If `connect-audio` fails, build the
local mixer as today. That is what keeps the gateway testable standalone against
a box with no audio port, and it is three lines.

**The per-peer connect beep does not survive as-is.** Today the gateway does
`mixer-play` on its own mixer, so the tone is per-peer. `mixer-play` on the box's
mixer would beep at *every* listener, which is wrong for what is really private
feedback. Drop it in remote mode. If it is wanted back, the composable answer is a
small local mixer in the gateway with two sources — the remote stream thunk and
the tone — which costs one more 20 ms clock and 20 ms of latency. Note it; don't
build it unasked.

## Hazard the gate must actually pin down

A listener that stops reading. Its socket buffer fills, the server's
`write-sequence` on that connection blocks, that connection's thread stops
draining its sink. Per-connection threads mean it wedges only itself — but
confirm that: **the other listener and the mixer must be provably unaffected**,
and the stalled connection should be dropped (and its sink unsubscribed) rather
than left wedged forever. That is the single behaviour most likely to be wrong on
the first pass, and it is the whole reason the mixer exists.

Alongside it, `inspect/audio-net-gate.lisp` in the style of audio-gate.lisp
(`check`/`check-that`, goertzel tone detection, nonzero exit on failure) owes:
a tone put in one end comes out the other at the client's requested rate; two
clients hear the same frames; disconnect and server-stop leave `sinks=()` behind
in `mixer-report`; the client thunk returns NIL and never signals; latency does
not accumulate over a sustained run.

## Landing it on the live desktop

The :3 desktop has been up since Jul 31 and people use it — restarting it kills
the session. It hot-loads over 4013, and I verified from the running image that
both `reed` and `glass/audio` are findable there (`asdf:find-system` => T T), so
the load will resolve:

    echo '(progn (asdf:load-system "glass/audio-net") ...)' | nc -q2 127.0.0.1 4013

Two things about that socket (desktop-5903.lisp:31-56): it reads exactly **one
form**, so everything goes in a single `(progn ...)`, and it evals in
**CLIM-GLASS**, so any `defvar` lands in that package.

Then edit `desktop-5903.lisp` as well, so a restart doesn't silently lose the
sound. Both, not either — the hot-load is what avoids downtime, the file edit is
what makes it true tomorrow.

## Not this job

Audio over RFB. glass's rfb.lisp implements two pseudo-encodings, DesktopSize
(-223) and Cursor (-239), and no audio one. The relevant client-side fact: the
noVNC we serve has no audio support (nothing in core/encodings.js, no QEMU
entries), and neither TigerVNC nor macOS Screen Sharing appear to either. So
building the QEMU audio pseudo-encoding buys nothing until there is a viewer for
it. Doing *this* job is what makes that a small change later, which is the point.
