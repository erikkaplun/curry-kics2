#!/bin/sh

# Start interactive read-eval-print loop for KiCS2

# The installation directory of KiCS2
KICS2HOME=`echo KICS2HOME must be defined here!`

REPL="$KICS2HOME/bin/.local/kics2i"
if [ ! -x "$REPL" ] ; then
  echo "ERROR: executable '$REPL' not found!" >&2
  echo "Run: cd $KICS2HOME && make" >&2
  exit 1
fi

# use readline wrapper rlwrap if it is installed and we have tty as stdin:
USERLWRAP=no
if tty -s ; then
  RLWRAP=`which rlwrap`
  if [ -x "$RLWRAP" ] ; then
    USERLWRAP=yes
  fi
fi

if [ "$1" = "--noreadline" ] ; then
  shift
  USERLWRAP=no
fi

if [ $USERLWRAP = yes ] ; then
  exec rlwrap -c -f "$KICS2HOME/tools/rlwrap" "$REPL" ${1+"$@"}
else
  exec "$REPL" ${1+"$@"}
fi
