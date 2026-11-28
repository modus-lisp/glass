#!/usr/bin/env python3
"""The web desk's test client, not modus and not glass: HTTP, then RFB over a WebSocket, and a
plain TCP VNC viewer at the same time.

    python3 -I glass/test/web-desk-client.py HOST WEB_PORT RFB_PORT [GLASS_WEBRTC_NOVNC_DIR]

Checks (each prints ok/FAIL; exit 0 only if all pass):
  GET /                      200, text/html, imports ./core/rfb.js
  GET /core/rfb.js           200, text/javascript, byte-identical to the vendored file
  GET of ../, %2e%2e, a file outside core/ and vendor/, a foreign Host -> refused (403)
  a WebSocket from a foreign Origin -> refused
  ws://HOST:WEB_PORT/websockify, subprotocol binary: RFB 3.8, security None, ClientInit,
  ServerInit, SetEncodings Raw, a full FramebufferUpdateRequest decoded rectangle by rectangle
  to the whole screen, a PointerEvent, a ping answered, and a clean close (1000 both ways);
  a TCP viewer on RFB_PORT doing the same handshake and full update while the WebSocket viewer
  is connected.
"""
import asyncio, os, posixpath, re, socket, struct, sys, zlib

import websockets

HOST, WEB, RFB = sys.argv[1], int(sys.argv[2]), int(sys.argv[3])
NOVNC = sys.argv[4] if len(sys.argv) > 4 else None
fails = 0
passes = 0


def check(name, ok, detail=""):
    global fails, passes
    if ok:
        passes += 1
        print(f"  ok    {name}" + (f"  ({detail})" if detail else ""))
    else:
        fails += 1
        print(f"  FAIL  {name}" + (f"  ({detail})" if detail else ""))
    sys.stdout.flush()


def http(target, host=None, extra=""):
    """One raw request (no client-side path normalisation); (status, headers, body)."""
    s = socket.create_connection((HOST, WEB), timeout=20)
    host = host or f"{HOST}:{WEB}"
    s.sendall(f"GET {target} HTTP/1.1\r\nHost: {host}\r\n{extra}\r\n".encode())
    data = b""
    while True:
        b = s.recv(65536)
        if not b:
            break
        data += b
    s.close()
    head, _, body = data.partition(b"\r\n\r\n")
    lines = head.decode("latin-1").split("\r\n")
    status = int(lines[0].split()[1])
    headers = {}
    for l in lines[1:]:
        k, _, v = l.partition(":")
        headers[k.strip().lower()] = v.strip()
    return status, headers, body


class Reader:
    """Exact reads over a WebSocket (frames are not RFB messages) or a TCP stream."""
    def __init__(self, ws=None, r=None):
        self.ws, self.r, self.buf = ws, r, b""

    async def read(self, n):
        if self.r is not None:
            return await self.r.readexactly(n)
        while len(self.buf) < n:
            m = await asyncio.wait_for(self.ws.recv(), 30)
            if isinstance(m, str):
                raise RuntimeError("a text frame")
            self.buf += m
        out, self.buf = self.buf[:n], self.buf[n:]
        return out


async def rfb_session(label, rd, send, encodings=(0,)):
    """RFB 3.8 with security None, then a full Raw update.  Returns (w, h, pixels-covered, nonzero)."""
    v = await rd.read(12)
    check(f"{label}: ProtocolVersion", v == b"RFB 003.008\n", repr(v))
    await send(b"RFB 003.008\n")
    n = (await rd.read(1))[0]
    types = await rd.read(n)
    check(f"{label}: security None offered", 1 in types, repr(list(types)))
    await send(bytes([1]))
    res = struct.unpack(">I", await rd.read(4))[0]
    check(f"{label}: SecurityResult OK", res == 0)
    await send(bytes([1]))                                   # ClientInit, shared
    w, h = struct.unpack(">HH", await rd.read(4))
    pf = await rd.read(16)
    bpp = pf[0]
    nlen = struct.unpack(">I", await rd.read(4))[0]
    name = (await rd.read(nlen)).decode("utf-8", "replace")
    check(f"{label}: ServerInit", w > 0 and h > 0 and bpp in (8, 16, 32), f"{w}x{h} {bpp}bpp {name!r}")
    await send(struct.pack(">BxH", 2, len(encodings)) + b"".join(struct.pack(">i", e) for e in encodings))
    await send(struct.pack(">BBHHHH", 3, 0, 0, 0, w, h))    # full FramebufferUpdateRequest
    covered = nonzero = 0
    rects = 0
    z = zlib.decompressobj()                                # ZRLE's one zlib stream
    inflated = 0
    while covered < w * h:
        t = (await rd.read(1))[0]
        if t == 2:                                           # Bell
            continue
        if t == 3:                                           # ServerCutText
            await rd.read(3)
            ln = struct.unpack(">I", await rd.read(4))[0]
            await rd.read(ln)
            continue
        if t != 0:
            check(f"{label}: server message type", False, f"unexpected type {t}")
            return w, h, covered, nonzero
        await rd.read(1)
        nr = struct.unpack(">H", await rd.read(2))[0]
        for _ in range(nr):
            x, y, rw, rh, enc = struct.unpack(">HHHHi", await rd.read(12))
            if enc not in encodings:
                check(f"{label}: rectangle encoding", False, f"encoding {enc} (asked for {encodings})")
                return w, h, covered, nonzero
            if enc == 16:                                    # ZRLE: a length, then zlib data
                ln = struct.unpack(">I", await rd.read(4))[0]
                inflated += len(z.decompress(await rd.read(ln)))
                covered += rw * rh
                rects += 1
                continue
            px = await rd.read(rw * rh * bpp // 8)
            covered += rw * rh
            nonzero += sum(1 for i in range(0, len(px), bpp // 8) if any(px[i:i + bpp // 8]))
            rects += 1
    check(f"{label}: full update decoded", covered == w * h,
          f"{rects} rectangle(s) of {encodings}, {covered} of {w * h} pixels, {nonzero} non-zero")
    if 16 in encodings:
        check(f"{label}: ZRLE's zlib stream inflates", inflated > 0, f"{inflated} bytes of tiles")
    else:
        check(f"{label}: the screen is not blank", nonzero > 0)
    await send(struct.pack(">BBHH", 5, 0, w // 2, h // 2))  # PointerEvent: a move
    await send(struct.pack(">BBHH", 5, 0, w // 3, h // 3))
    return w, h, covered, nonzero


async def ws_viewer(ready, done):
    url = f"ws://{HOST}:{WEB}/websockify"
    async with websockets.connect(url, subprotocols=["binary"], max_size=None,
                                  origin=f"http://{HOST}:{WEB}", compression=None) as ws:
        check("ws: upgraded, subprotocol binary", ws.subprotocol == "binary", repr(ws.subprotocol))
        rd = Reader(ws=ws)
        # every client message of more than 3 bytes goes as a FRAGMENTED message (a first frame
        # and a continuation), which the server must reassemble
        async def send(b):
            if len(b) > 3:
                await ws.send([b[:2], b[2:]])
            else:
                await ws.send(b)
        await rfb_session("ws", rd, send)
        ready.set()
        await done.wait()
        pong = await ws.ping(b"glass?")
        await asyncio.wait_for(pong, 10)
        check("ws: ping answered with a pong", True)
        # an incremental update request after the pointer moved: the server answers it
        await ws.send(struct.pack(">BBHHHH", 3, 1, 0, 0, 1, 1))
        await ws.close(1000, "done")
        rcvd = getattr(ws, "close_rcvd", None) or getattr(getattr(ws, "protocol", None), "close_rcvd", None)
        sent = getattr(ws, "close_sent", None) or getattr(getattr(ws, "protocol", None), "close_sent", None)
        check("ws: closed cleanly", sent is not None and sent.code == 1000 and rcvd is not None
              and rcvd.code == 1000, f"sent {sent.code if sent else None}, received "
              f"{rcvd.code if rcvd else None}")


async def tcp_viewer(ready, done):
    await ready.wait()
    r, w = await asyncio.open_connection(HOST, RFB)
    async def send(b):
        w.write(b)
        await w.drain()
    await rfb_session("tcp", Reader(r=r), send)
    w.close()
    await w.wait_closed()
    done.set()


async def main():
    st, hd, body = http("/")
    check("GET / is the desk's page", st == 200 and hd.get("content-type", "").startswith("text/html")
          and b"./core/rfb.js" in body, f"{st} {hd.get('content-type')} {len(body)} bytes")
    st, hd, body = http("/core/rfb.js")
    ct = hd.get("content-type", "")
    same = True
    if NOVNC:
        with open(os.path.join(NOVNC, "core", "rfb.js"), "rb") as f:
            same = f.read() == body
    check("GET /core/rfb.js as text/javascript", st == 200 and ct.startswith("text/javascript") and same,
          f"{st} {ct} {len(body)} bytes{', identical to the vendored file' if NOVNC and same else ''}")
    st, hd, body = http("/vendor/pako/lib/zlib/inflate.js")
    check("GET a vendor/ module", st == 200 and hd.get("content-type", "").startswith("text/javascript"),
          f"{st} {len(body)} bytes")
    for target in ["/../VENDORED.md", "/core/../../glass/glass.asd", "/core/%2e%2e/LICENSE.txt",
                   "/core/./rfb.js", "/LICENSE.txt", "/core/..", "/vendor/../../../../etc/passwd"]:
        st, _, _ = http(target)
        check(f"GET {target} refused", st == 403, str(st))
    # every module the page's import of core/rfb.js pulls in, transitively, as the browser would
    seen, todo, bad = set(), ["/core/rfb.js"], []
    while todo:
        t = todo.pop()
        if t in seen:
            continue
        seen.add(t)
        st, hd, body = http(t)
        if st != 200 or not hd.get("content-type", "").startswith("text/javascript"):
            bad.append(f"{t} {st} {hd.get('content-type')}")
            continue
        for m in re.finditer(rb"""(?:^|\n)\s*(?:import|export)\b[^'";]*?\bfrom\s*['"]([^'"]+)['"]""", body):
            todo.append(posixpath.normpath(posixpath.join(posixpath.dirname(t), m.group(1).decode())))
    check("every module rfb.js imports is served as JavaScript", not bad and len(seen) > 20,
          f"{len(seen)} modules" + (f"; {bad}" if bad else ""))
    st, _, _ = http("/core/nope.js")
    check("GET a missing file: 404", st == 404, str(st))
    st, _, _ = http("/", host="desk.attacker.example")
    check("a foreign Host header is refused (DNS rebinding)", st == 403, str(st))
    try:
        async with websockets.connect(f"ws://{HOST}:{WEB}/websockify", origin="http://attacker.example",
                                      subprotocols=["binary"]):
            check("a foreign Origin's WebSocket is refused", False, "it was accepted")
    except Exception as e:          # InvalidStatus (or the legacy client's InvalidStatusCode)
        check("a foreign Origin's WebSocket is refused", "403" in str(e), str(e))
    ready, done = asyncio.Event(), asyncio.Event()
    await asyncio.wait_for(asyncio.gather(ws_viewer(ready, done), tcp_viewer(ready, done)), 180)
    # noVNC asks for ZRLE first: a second WebSocket viewer, ZRLE then Raw
    async with websockets.connect(f"ws://{HOST}:{WEB}/websockify", subprotocols=["binary"],
                                  max_size=None, origin=f"http://{HOST}:{WEB}", compression=None) as ws:
        await rfb_session("ws-zrle", Reader(ws=ws), lambda b: ws.send(b), encodings=(16, 0))
    # and a page still loads after the viewers have gone
    st, _, _ = http("/")
    check("GET / again after the viewers left", st == 200, str(st))


try:
    asyncio.run(main())
except Exception as e:
    check("the run", False, f"{type(e).__name__}: {e}")
print(f"web desk client: {passes} passed, {fails} failed")
sys.exit(1 if fails else 0)
