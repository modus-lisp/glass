#!/bin/sh
# test/run-web-desk.sh -- the desk in a web browser, end to end, on one Lisp:
#
#   glass/test/run-web-desk.sh TREE LISP...
#   e.g. glass/test/run-web-desk.sh ~/src/modus-lisp sbcl --dynamic-space-size 8192
#        glass/test/run-web-desk.sh ~/src/modus-lisp modus --dynamic-space-size 8192
#
# TREE holds the repositories side by side (modus/, glass/, glass-webrtc/, ...).  Starts
# modus/scripts/glass-desk.lisp on 127.0.0.1 with ephemeral ports (--port 0 --web-port 0) and
# --no-open, waits for it to say where it listens, runs test/web-desk-client.py against both
# ports, stops the desk and checks that nothing is left listening.  Exit 0 only if every check
# passed.  The log goes to $WEB_DESK_LOG (default: a file in $TMPDIR).
set -u
TREE=$1; shift
HERE=$(cd "$(dirname "$0")" && pwd)
LOG=${WEB_DESK_LOG:-${TMPDIR:-/tmp}/web-desk.$$.log}
"$@" --script "$TREE/modus/scripts/glass-desk.lisp" --port 0 --web-port 0 --no-open >"$LOG" 2>&1 &
PID=$!
i=0
until grep -q "desk: in a web browser" "$LOG"; do
  if ! kill -0 $PID 2>/dev/null || [ $i -ge 1200 ]; then
    echo "run-web-desk: the desk did not come up (log: $LOG)"; kill -9 $PID 2>/dev/null; exit 2
  fi
  i=$((i + 1)); sleep 1
done
RFB=$(sed -n 's/^desk: VNC viewers: .*:\([0-9]*\)$/\1/p' "$LOG" | head -1)
WEB=$(sed -n 's|^desk: in a web browser: http://.*:\([0-9]*\)/$|\1|p' "$LOG" | head -1)
echo "run-web-desk: up after ${i}s, RFB on $RFB, web on $WEB"
python3 "$HERE/web-desk-client.py" 127.0.0.1 "$WEB" "$RFB" "$TREE/glass-webrtc/novnc"
RC=$?
kill -9 $PID 2>/dev/null; wait $PID 2>/dev/null
sleep 1
if ss -ltn | grep -qE "127\.0\.0\.1:($RFB|$WEB) "; then
  echo "run-web-desk: FAIL: a port is still listening"; RC=3
fi
exit $RC
