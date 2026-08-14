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

## What met the code

Written before the code, and the code disagreed in four places. Recorded here rather than quietly
fixed above, because a note that edits itself to have been right teaches nothing.

**1. A seat's `port-num` was never the conflation.** The argument above puts two things side by
side — `run-wm` serves, and `port-num` defaults to 5900 — as though they were one fault. They are
not. Nothing has ever listened because a seat exists: `add-seat` opens no socket, and
`seat-gate.lisp` has been running two seats with no sockets at all since seats were built. The
default port is a *setting* — the port this seat serves on **if** it serves, and the number the
audio ports are derived from — and it is still 5900 after this change. The whole conflation was one
line, `start-glass-server` inside `run-wm`, and that is the line that moved.

**2. Closing a listener is not `socket-close`, and this is the trap the model hides.** The note
says a seat "opens one" and treats closing as the same act backwards. It is not. A thread parked in
`accept()` holds the open file description, so `socket-close` drops this process's descriptor and
the kernel goes on listening: the port stays bound, `ss -ltn` still shows it, and a client still
connects. A seat that closed its transport that way would have *believed* it had stopped serving
while remaining reachable — the exact posture this note exists to abolish, now with a slot saying
otherwise. `shutdown()` first is what the parked accept notices. See `GLASS:CLOSE-LISTENER`; the
gate asks the operating system rather than the object, which is why it caught it.

**3. `run-wm`'s name did not have to stop being true, and the launchers do not need two calls.**
The migration section predicts that cost and offers to pay it. It is avoidable and it was not paid:
"run a session and expose the home seat" is a perfectly honest description of a convenience, so
`run-wm` keeps it, keeps its argument list, keeps its ordering (the listener still comes up before
the first window is spawned, which is observable to anybody connecting during startup), and
`desktop-5903.lisp` needs **no diff at all**. The split is underneath — `MAKE-WM-SESSION`,
`START-WM-SESSION`, `RUN-WM-LOOP`, and `RUN-SESSION` for a session that serves nothing — where a
caller who wants the two things separately can reach them and a caller who wants the old one is not
made to care.

**4. A seat's keyboard belonged to the seat, not to its listener.** `SEAT-INJECTOR` — the callback
anything typing *for* this seat uses, dictation included — was made by `START-SEAT-SERVER`, which
was fine while a seat and a listener arrived together and is wrong the moment they do not: a seat
that serves nothing would have had no hands, and a seat with two transports would have grown two
keyboards. It is made by `ADD-SEAT` now. Nothing in the note predicts this, and it is the one place
where separating the two exposed something that was already leaning on their being one.

### Built, and not built

Built: seats have identities (opaque in core, minted and persisted by `:glass/nostr`, keyed by seat
name so a seat survives a restart as the same destination), and serving is a seat's decision
(`OPEN-SEAT-TRANSPORT` / `CLOSE-SEAT-TRANSPORT`, a seat may hold several or none).

Not built, and still wanted, in the order the note argues for them: **per-seat admission** (the
`invoker` becoming a principal), and **capture internal to the seat** (which is what makes the VNC
password stop breaking video). Nothing above should block either. Still deliberately refused:
signing and verification — the places a signature would go are marked in `backend/seat.lisp` and
`src/nostr.lisp` and nothing else about them is built.

The bind address is now a parameter (`CLIM-GLASS:*SEAT-BIND-ADDRESS*`, and `:address` on `run-wm`
and `open-seat-transport`) whose default is unchanged: `0.0.0.0`, every interface, exactly what
glass has always bound. Closing the hole the note opens with is one word — `:address "127.0.0.1"`
in a launcher — and it is deliberately left to whoever knows what is pointed at the box.

## What met the code again: a wire that is a *file*

A second round, against the same note, and it moved one of its conclusions.

**5. `:address "127.0.0.1"` is not the one word that closes the hole.** The note ends by saying
the exposure it opens with — `:5903` on every interface, no authentication — is closed by binding
loopback, and calls that the operator's decision. Binding loopback is real and it was the right
first move, but it is not access control: **`127.0.0.1` is reachable by every process of every uid
on the box.** A shared build account, a compromised service, a container in the same network
namespace — all of them can connect to the screen, the mix, the microphone, admission, and to
`:4013`, which is an unauthenticated `EVAL`. The note's table says "no listener unless a seat opens
one"; what it could not yet say is *and when a seat does open one, who may reach it.*

A **UNIX-domain transport** is that missing column, and it is a sibling of the TCP one rather than
a replacement — `CLIM-GLASS:OPEN-SEAT-TRANSPORT` `:KIND :RFB-UNIX`, `GLASS:OPEN-LISTENER`,
`GLASS:*PEER-POLICY*`, one `LISTENER` class with two subclasses, and every protocol above it (RFB,
the mix, the microphone, admission, the control socket) untouched because all of them are a stream
and none of them ever asked what carried it. A seat may hold one of each at the same time.

**6. The note deferred authentication, and the kernel was already offering it.** "Deliberately not
doing: seat-signed requests… do not add a signing step to a loopback call that has no attacker
today." That reasoning still holds for signatures. But it framed authentication as something that
must be *built* — a key, a signature, a verification step — and on a UNIX socket it is something
you can simply **ask for**: `SO_PEERCRED` hands the server the connecting process's uid, gid and
pid, filled in by the kernel at `connect()` and unforgeable by the peer. `GLASS:PEER-CREDENTIALS`
reads it (a raw `getsockopt` through `sb-alien`; `sb-bsd-sockets` exposes `SO_PASSCRED` and not
this). It is not seat identity and it does not replace it — it says *which process*, not *which
person* — but it is a real principal, it needs no crypto anywhere near core glass, and it turns the
anonymous `peer` in a log line into `uid=1001 pid=2658990`. The signature still goes where the note
says it goes; there is now something underneath it.

**7. The trap in point 2 was documented and not finished.** `CLOSE-LISTENER` exists because
`SOCKET-CLOSE` alone leaves the kernel listening — and `STOP-AUDIO-STREAM`, `STOP-MIC-STREAM` and
`STOP-ADMISSION-SERVICE` were all still calling bare `SOCKET-CLOSE` on a socket with a thread
parked in `accept()` on it. Three copies of the bug the note had already caught once, in files
nobody re-read after catching it. They call `CLOSE-LISTENER` now. Writing the fix down is not the
same as applying it, and a note that records a trap should be read as a search pattern.

**And the UNIX version of that same trap:** the socket *file* outlives the socket. `bind()` refuses
a path that exists — `EADDRINUSE`, whether or not anything is behind it, and there is no
`SO_REUSEADDR` for this — so a server killed with `-9` leaves a file that makes the *next* start
fail. Closing unlinks it (only if the inode is still the one we created, or a listener closing late
would delete its successor's file), and opening probes first: `connect()` returning `ECONNREFUSED`
means stale, and stale gets removed. The exit hook is a courtesy; the probe at `bind` is the
mechanism, because a `SIGKILL` is exactly the case an exit hook cannot cover.

**Where the files live, and why:** `$GLASS_RUNTIME_DIR`, else `$XDG_RUNTIME_DIR/glass/` **if we
own it**, else `~/.glass/run/`. The ownership check is not defensive habit — on this box
`XDG_RUNTIME_DIR` is `/run/user/0`, root's, inherited from whatever started the session, while
`/run/user/1001` does not exist. Directory mode `0700`, socket mode `0600`, and the `chmod` happens
**between `bind()` and `listen()`**: a bound socket that is not yet listening refuses every connect,
so the window in which the mode is whatever the umask made it is a window with nothing to connect
to. `/tmp` was refused: world-writable parent, and a tmp-cleaner that removes an idle socket file
takes the listener with it.

**Nothing about the default moved.** `*SEAT-TRANSPORT-KIND*` is `:RFB`, a TCP port; the audio, mic
and admission services still take `:PORT`; `TCP-LISTEN` still returns a raw socket because callers
outside this tree hold what it returns. Switching a live desktop over is a deployment decision and
it is one word in one launcher — which is the same shape, and the same refusal to make it for
somebody else, as the bind address before it.

## What met the code a third time: a credential belongs to a *wire*

A third round, and it took the note's own **work plan** away from it.

**8. "Capture internal to the seat" was never what would fix the password, and it is not needed.**
The note twice names the same remedy — the table's *"capture is internal to the seat; it never
authenticates"*, and the closing list's *"capture internal to the seat (which is what makes the VNC
password stop breaking video)"*. That is an architecture change: the RFB client that feeds VP8 would
stop being a client and become part of the session. It is not what fixed it. **`GLASS:*VNC-PASSWORD*`
was one special variable read inside `HANDSHAKE`, so a credential was a property of the SESSION, and
every listener inherited it at once.** Making it a property of the LISTENER — `SERVE`'s `:PASSWORD`,
`OPEN-SEAT-TRANSPORT`'s `:PASSWORD`, one slot on `SEAT-TRANSPORT` — is the whole of the fix, and the
capture stays exactly what it was: an ordinary external RFB client, speaking security type None, on a
wire that asks it for nothing. The desktop now demands a password on a TCP port and offers None on a
socket file **in the same process, in the same second**, and the gate proves it by running
webrtc-data's own `glass-capture.lisp` and encoding a real VP8 keyframe out of what it captured while
a wrong password is being refused on the port.

This is the fourth costume of the same error, in a note written to catalogue that error: the
compositor, the mixer, warp's rule 8, the session-conflated-with-one-wire — **and the credential,
conflated with the session.** The note diagnosed the disease and then prescribed surgery on the
symptom.

**9. The note asserted a capability the code could not express.** *"A seat that wants VNC opens one,
on a port it chooses, with a credential it chooses."* The port was true from the moment transports
existed. The credential was never true and could not be made true by any caller: `SERVE` had no
password argument at all, so "a credential it chooses" was a sentence with nothing under it. Written
before the code, it read as a description of the model; read after, it was a promise. It is the
argument that ought to have been checked first, precisely because it was the one nobody had to
implement to make the section compile.

**10. The socket file inherited the session password too — the same bug, inside the fix.** Point 5
introduces the UNIX transport as the strong door, and it was: mode 0600, `SO_PEERCRED`, no key
material. But the handshake over it read the same global, so an operator who set `~/.glass-vnc-pass`
would have demanded a DES password **on the wire the capture comes in on** and broken video exactly
as before — through the mechanism the note had just built to be immune to it. `:RFB-UNIX` now forces
the credential to `NIL` and says so if you pass one: a socket file's access control is the filesystem
and the peer's uid, checked by the kernel, which is stronger than DES over a plaintext stream and is
not fooled by somebody who watched one handshake. Point 7 called a recorded trap "a search pattern";
so is a recorded *conflation*, and this one was sitting inside the paragraph that fixed the last one.

**11. And the bind address is not always the operator's call.** Twice the note refuses to choose an
address for somebody — `*SEAT-BIND-ADDRESS*` (point 4's coda) and `*SEAT-TRANSPORT-KIND*` (point 5's).
Both refusals stand, and both are about a **launcher**, where a person wrote the call and knows the
box. A root-menu item has nobody to ask. So `SERVE-SEAT-VNC` — the one thing that picks an address on
a person's behalf — is also the one thing that must not pick an exposing one on their behalf: its
default is `0.0.0.0` **and it overrules that to `127.0.0.1` whenever there is no credential to go with
it**, including the case where the image has no DES verifier and a password would only reject
everybody. `OPEN-SEAT-TRANSPORT` underneath it still binds precisely what it is told.

`0.0.0.0` is the default there, and not by inheritance: with a UNIX socket already present, a
loopback-only VNC port is a *worse copy of a door that already exists* — everything local is better
served by the socket file, and the only thing a port adds is somebody on another machine. A feature
whose default made it useless would be a feature nobody turns on.

### Still not built

**Per-seat admission** (the `invoker` becoming a principal) is unchanged and still wanted. **Capture
internal to the seat** is struck from the list: it was on it for a reason that no longer exists, and
nothing else has asked for it.
