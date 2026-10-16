#!/bin/sh
# demo/desk.sh — the glass desk (Clock, Notes, Paint, Media) served over VNC.
#
#   demo/desk.sh            serve on 127.0.0.1:5910; point a VNC viewer at it
#   LISP=sbcl demo/desk.sh  the same on SBCL
#
# On modus (the default) the first run loads everything and saves a core next to nothing you
# own -- ~/.cache/glass/desk.core -- so later runs start in about a second.  The core is rebuilt
# when the modus binary or any glass source is newer than it.
#
# env: LISP (modus|sbcl), MODUS (the modus binary; default `modus' on PATH, else ~/modus-main/modus),
#      DESK_PORT, DESK_ADDRESS, DESK_SIZE (WxH), DESK_MEDIA -- see demo/desk-run.lisp.
set -eu
here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
export DESK_DEMO_DIR="$here/"

if [ "${LISP:-modus}" = sbcl ]; then
  exec sbcl --dynamic-space-size 4096 --non-interactive --load "$here/desk-run.lisp"
fi

modus=${MODUS:-$(command -v modus || echo "$HOME/modus-main/modus")}
[ -x "$modus" ] || { echo "desk: no modus binary at $modus (set MODUS)" >&2; exit 1; }
export DESK_QL_ADAPTER="$(dirname "$modus")/modus-quicklisp/real-quicklisp.lisp"
cache=${XDG_CACHE_HOME:-$HOME/.cache}/glass
export DESK_CORE="$cache/desk.core"
mkdir -p "$cache"

stale=no
if [ ! -f "$DESK_CORE" ] || [ "$modus" -nt "$DESK_CORE" ]; then stale=yes
elif [ -n "$(find "$here/.." -name '*.lisp' -newer "$DESK_CORE" 2>/dev/null | head -1)" ]; then stale=yes
fi
if [ $stale = yes ]; then
  echo "desk: building $DESK_CORE (once; a minute or two)..." >&2
  rm -f "$DESK_CORE"
  "$modus" --script "$here/desk-world.lisp" >"$cache/desk-build.log" 2>&1 || true
  [ -f "$DESK_CORE" ] || { echo "desk: the core was not saved -- see $cache/desk-build.log" >&2; exit 1; }
fi
exec "$modus" --core "$DESK_CORE" --script "$here/desk-run.lisp"
