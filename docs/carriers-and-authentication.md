# Carriers and authentication — three axes: delivery, carrier, authentication

A design note, written before it is built, in the same shape as `seats-and-transports.md`:
the argument first, the refusals recorded, and the things we are choosing *not* to do said
out loud so nobody has to re-derive them later. That note separated a session from the wire
that serves it; this one separates the wire from how you are told about it and how you are
let in.

**Status:** design, nothing built. Written after an evening inside the WebRTC path's
failure modes, and a conversation that took the interesting turn away from "add Tor" and
toward "stop conflating three things".

---

## The observation

`--nostr=WHO` has been doing three jobs under one name:

1. **deliver** — DM the location and a credential to WHO
2. **authenticate** — put WHO on the ACL, and accept a NIP-07 signature as them
3. **carry** — negotiate a WebRTC data channel by gift-wrapped SDP over relays

Those are independent. You can want a nostr DM whether the address it carries is an
onion, a LAN address, or an SDP offer. You can want NIP-07 whether the bytes arrive by
data channel or WebSocket. And you can want a direct LAN connection with no relays in
it at all while still being told about it by DM.

So:

| axis | the question | options |
|---|---|---|
| **delivery** | how does the client learn the address + credential? | nostr DM (npub / NIP-05); printed locally; QR |
| **carrier** | how do bytes flow? | `:tcp` (local/LAN), `:tor` (onion service), `:webrtc` (relay-negotiated hole punch) |
| **authentication** | how does the box decide you may in? | one-time code; enrolled device key; NIP-07 signature vs the ACL; VNC password |

### They are only accidentally coupled today

This is the part that matters, and it is a security argument rather than a taxonomy
one.

`glass:admission-admit (pubkey code)` takes a **claimed** pubkey and no signature. That
is sound right now *only* because of where the pubkey comes from: the gateway unwraps a
NIP-59 gift wrap, so the sender is cryptographically verified before admission is ever
asked. The old gateway says so plainly — *"the sender is the VERIFIED seal signer from
unwrap-giftwrap … so an allowlist hit is a real cryptographic identity."*

In other words **authentication currently rides the carrier.** Change the carrier and
the allowlist quietly becomes an honour system: a WebSocket carries whatever the client
says it is.

- **OTC** survives any carrier — a bearer credential, single-use, TTL'd, HMAC'd with
  the session key. Holding it *is* the proof. It is the path that bootstraps enrolment.
- **Allowlist / NIP-07** and **enrolled device** do not survive, unless the client is
  made to prove possession.

The fix is the thing that also makes the axes genuinely independent:

1. box sends a nonce on connect
2. client signs it — NIP-07 for the user's key, the stored device key on reconnect
3. box verifies the signature against the claimed pubkey
4. **then** `admission-admit`

`glass` needs no change. The gateway's obligation moves from "unwrap" to "verify", and
becomes the same obligation on every carrier. Arguably stronger than today: a wrap
proves the sender per *message*; a challenge proves it per *connection*, which is what
a long-lived stream actually wants.

---

## Carrier: one abstraction, three backends

`cl-transport` already has the right shape. `EXPOSE on-connection :backend X` means
"arrange inbound reachability and hand me each stream", with backends registered by
`REGISTER-LISTENER` (`:tcp` built in, `:frp` provided by cl-frpc as a worked example).

What the gateway actually needs from a carrier is **a reliable ordered byte stream plus
a way to become reachable**. All three provide that — including WebRTC, whose data
channel is reliable-ordered SCTP, i.e. a stream.

```
   :tcp    ─┐
   :tor    ─┼──▶ expose ──▶ stream ──▶ [ nonce + signature ] ──▶ seat, RFB, audio, mic
   :webrtc ─┘
```

Everything above the stream stops caring. `--onion` versus `--nostr` collapses into a
`:backend` keyword, and running both at once is two `EXPOSE`s.

### What exists

| piece | state |
|---|---|
| Tor client, 3-hop circuits, SOCKS5 | ✅ `cl-tor` |
| `.onion` **dialing** (v3 client) | ✅ verified live |
| `.onion` **hosting** (v3 service) | ✅ `run-service`, "validated against stock Tor" |
| circuit → binary Lisp stream | ✅ `cl-tor/src/gray-stream.lisp` |
| outbound `:tor` transport registered | ✅ `cl-tor/src/tor-backend.lisp` |
| inbound abstraction (`EXPOSE`) | ✅ `cl-transport/src/inbound.lisp` |
| **inbound `:tor` backend** | ❌ nobody registered one |
| **`:webrtc` backend** (wrap what exists) | ❌ the signalling + ICE currently lives in the gateway |
| HTTP / WebSocket | ✅ `seal/http`, `seal/websocket` |
| noVNC over WebSocket | ✅ its *native* transport |
| admission, OTC, enrolment | ✅ `glass`, already carrier-independent |

### The exclusion

**WebRTC over Tor does not work in a browser.** Two independent blockers: Tor carries
TCP only, while ICE and SCTP-over-DTLS want UDP; and Tor Browser disables WebRTC
outright (`media.peerconnection.enabled = false`) precisely because it is a
proxy-bypass and fingerprinting vector. The browser you need for `.onion` is the browser
that will not do WebRTC. They are alternatives, not layers.

### Where the asymmetry actually lives

Not in the gateway — in **the client**. noVNC over a data channel for `:webrtc`, noVNC
over WebSocket for `:tor` and `:tcp`. Same protocol, different carrier. The payload and
control stream-ids (104, 100) are a data-channel-ism that a WebSocket path expresses
differently, and the payload transfer may not be needed at all when the page can simply
be served.

---

## Why Tor is worth having, and why it is not the default

The current path fails in the least legible way available: every layer reports healthy
and the screen never arrives. Tonight that cost hours — both ends behind Cloudflare
WARP, 66 candidates gathered, none pairing, `DTLS: no ICE peer within 20.0s`. The
diagnostics that finally explained it (`0 private, 0 public, 21 mDNS`) had to be written
first.

An onion service removes the class rather than improving the odds: no candidate
gathering, no reflexive address, no relay allocation, no NAT traversal, no pairing to
fail. Both ends dial *out*; the rendezvous is the network's job.

**But the browser must be a Tor client** — Tor Browser, or Onion Browser on iOS. The
link that works in Safari today will not. That is the whole objection and it is enough
to keep WebRTC as the default. The two fail in opposite conditions: WebRTC is excellent
on a permissive network and hopeless behind symmetric NAT plus a VPN; Tor does not care
about either and is merely slow.

---

## Build order

**1. `:tor` inbound backend** — small, self-contained, useful to anything in the org.
Mirrors `dial-over-tor` in the same file: `run-service` with a handler that wraps
`(circ sid)` in the gray stream that already exists, hands it to `on-connection`, and
returns a closer.

The onion identity should be **per session**, minted beside the nsec in
`~/.kiln/sessions/<name>/`, so a session has two names that are both readings of keys it
owns and `--resume` keeps the address as it keeps the npub.

**2. Challenge–response auth** — see above. Do not skip; this is the piece that makes
the axes independent instead of accidentally coupled.

**3. Carrier-agnostic gateway** — `EXPOSE`, serve the page, bridge WebSocket ⇄ the
seat's RFB socket. Seats are ephemeral and per-connection: a connection gets its own
seat, sized to the client's viewport via `SetDesktopSize`, culled when it goes.

**4. `:webrtc` backend** (optional, later) — wrap the existing signalling + ICE as a
backend so the current path stops being special. Worth doing for symmetry; nothing
depends on it.

## Flags

Keep the convenient one-liner, make it sugar over the axes:

```sh
kiln run --nostr=ynniv@ynniv.com               # tell by DM, ACL = them, default carrier
kiln run --nostr=ynniv@ynniv.com --via=tor     # same delivery + auth, onion carrier
kiln local --vnc-socket --via=tcp              # no relays at all; print the link here
```

`--nostr=WHO` keeps meaning *deliver to WHO and let WHO in*. `--via` picks the carrier.
Auth follows from what is configured — OTC always, ACL when a WHO is named, enrolment
on first success.

## Trade-offs to expect

- **Latency.** Three hops each way; typing will be perceptible. RFB rectangles tolerate
  this far better than VP8 at 150 KB/s with Tor's jitter, so the video path probably
  *goes away* on that carrier — the right trade on a high-latency link, and it removes
  the capture from the picture.
- **Onion addresses are not credentials.** 56 unguessable characters that will be DM'd
  and pasted. Reachability is not authorisation, which is exactly why piece 2 is not
  optional.
- **Startup cost.** `run-service` establishes intro points and publishes a descriptor
  before the address answers — seconds to tens of seconds. Mint and publish at session
  start, not on first connect.

## Not checked

Written from reading, not running. `cl-tor`'s hosting is documented as validated against
stock Tor, but nothing here exercised it; the handler contract (`circ`, `sid`) was read
from source, and "the existing gray stream wraps a service-side stream as cleanly as a
client-side one" is an assumption. First step is a bare onion service that serves one
line of text, fetched from a Tor Browser — before any of this touches kiln.
