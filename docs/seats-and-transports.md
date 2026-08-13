# Seats and transports — what you connect to, and what carries it

A design note, written before it is built, in the shape `warp/DESIGN.md` uses: the argument first,
the refusals recorded, and the things we are choosing *not* to do said out loud so nobody has to
re-derive them later.

## The mistake this corrects

glass's top-level entry point is `run-wm`, and its own docstring says *"Run a mini OPEN LOOK desktop
over VNC"*. A seat's `port-num` defaults to 5900. So **running a session and exposing it on a wire
are the same call**, and there is no way to have a session that serves nothing.

That conflation is why:

- **`:5903` currently listens on `0.0.0.0` with no authentication.** Not by decision — there is no
  other posture available. The session is the listener.
- **Setting `~/.glass-vnc-pass` breaks video.** `glass-capture.lisp` speaks RFB security type None
  and has no VNC-auth implementation, so turning on a password kills the VP8 capture while the
  browser's bridged RFB keeps working. The *secure* configuration is the broken one, and it is
  broken only because capture is an RFB client of the same public listener as everyone else.
- **Two gateways on one npub both answer every offer.** They subscribe to the same filter and share
  no dedup state, so one phone gets two sessions from two processes.

Every one of those is the same error in a different costume, and it is the error this stack has now
made four times: **the shared thing was conflated with one consumer's view of it.** The compositor
(paint once, composite per seat), the mixer (pull once, sum per seat), warp's rule 8 (query once,
diff per consumer) — and now the session, conflated with one wire out of it.

## The model

> **A seat is what you connect to. A transport is what carries it. They are different things and
> they have different identities.**

- A **session** is the running Lisp image: the applications, the windows, their content.
- A **seat** is one person's place at it: their own screen, pointer, keyboard, focus, z-order,
  clipboard, mix — already true today (rule 8's other half).
- A **transport** is a wire that reaches a seat: the WebRTC gateway, an RFB listener, a native
  client, a token stream to an agent.

**Serving is a seat's decision, not the session's.** The default is no listener at all. A seat that
wants VNC opens one, on a port it chooses, with a credential it chooses. A seat that only wants
WebRTC never opens a socket a stranger can reach.

## Seats have keypairs

A seat gets an **npub of its own**. Not the transport's, though nothing stops them being the same
key today.

The immediate objection is that no code needs it yet, and that is true. The argument is that
identity is what makes everything else expressible:

- **Addressing.** "DM this npub and get a link" is the interaction we already have, at the level it
  should have been all along — you are asking for a way in to *a seat*, not to a wire.
- **Durability across transports.** A seat reachable over WebRTC today and a native client tomorrow
  is the same seat. Transport keys become approximately ephemeral: rotate one and nothing about
  what you are connecting to changes. (We rotated the box key on 2026-08-12 and it invalidated
  every link and every enrolment, because the key *was* the destination.)
- **Authentication, if we want it.** A seat that signs its requests is a seat the session can
  verify, which is what would let a seat live in another process — or on another machine — without
  the trust boundary being "whoever can reach port 5915".
- **A real subject for authorization.** warp's rule 6 enforces commands against an `invoker`, which
  is today the keyword `:allowlist` or `:device`. With seat identity the invoker is a *principal*,
  and "who did this" becomes answerable in the command log rather than inferable.

### Seat identity is not person identity

Keep these apart or they collapse and take a useful distinction with them:

- The **seat's** key says *which place this is*. It belongs to the session's configuration and
  persists across whoever is sitting there.
- The **person's** key — today's `NOSTR_ALLOW` entries and device enrolments — says *who may sit in
  it*.

Both are npubs; they answer different questions. Conflating them makes "the owner sat down at the
guest seat" unsayable, and that is a thing people do.

### Three keys, and one we can probably drop

- **transport key** — signalling only. Each gateway needs its own, which is what makes two gateways
  possible at all: each answers only its own offers.
- **seat key** — the destination.
- **session/box key** — what we have today, and it may be *redundant*: if seats are what you address,
  the "box npub" is just the home seat's. Dropping it is not a goal in itself, but a session key
  that names nothing you can connect to is a key with no job.

## What this fixes, concretely

| today | after |
|---|---|
| `:5903` open on `0.0.0.0`, no auth, no alternative | no listener unless a seat opens one |
| VNC password breaks VP8 capture | capture is internal to the seat; it never authenticates |
| two gateways race on one npub | each gateway fronts one seat with its own transport key |
| admission is session-wide | per seat: owner and guest can differ, which is what warp's `invoker` wants |
| a rotated key invalidates everything | rotating a transport key changes nothing about the destination |

## Deliberately not doing

- **Not building seat-signed requests yet.** Give seats identity now because retrofitting identity
  is a migration and adding verification later is a feature. Say where the signature would go; do
  not add a signing step to a loopback call that has no attacker today.
- **Not making seats remote.** The model permits it and that is the point; nothing in this note is
  an argument for doing it.
- **Not one key per person per seat.** A seat is a place, not a session-of-a-person.
- **Not touching how seats composite.** Rule 8's split stands unchanged; this is about who may
  reach a seat, not how it draws.

## The migration, and its honest cost

`run-wm` is called by every launcher (`warren/desktop-5903.lisp`, `backend/inspect/serve-desktop.lisp`,
the gates). Splitting "run a session" from "expose a seat" changes the one call everything makes,
and its name stops being true. The launchers keep doing both, in two calls instead of one.

There is also a **bootstrap** question with no clever answer: a seat with no transport cannot be
reached, so the first seat's transport has to be arranged locally, by the launcher. That is fine —
it is the same reason `desktop-5903.lisp` exists — but it means "no listener by default" is a
default, not an invariant.
