#!/bin/sh
# run-tests.sh — load glass + run its RFB self-test. Exits 0 iff every check passes.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
LISP=${LISP:-sbcl}
exec "$LISP" --non-interactive --disable-debugger \
  --eval '(require :asdf)' \
  --eval "(push #p\"$ROOT/\" asdf:*central-registry*)" \
  --eval '(handler-bind ((warning (function muffle-warning))) (asdf:load-system "glass/test"))' \
  --eval '(uiop:quit (if (funcall (read-from-string "glass/test:run-tests")) 0 1))'
