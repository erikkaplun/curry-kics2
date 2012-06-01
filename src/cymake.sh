#!/bin/sh
# shell script to call the front end for Curry

# The installation directory of KiCS2
KICS2HOME=`echo KICS2HOME must be defined here!`

# define appropriate 'cymake' executable:
CYMAKE="$KICS2HOME/mccparser/bin/cymake"
if [ ! -x "$CYMAKE" ] ; then
  # try cymake of local installation
  CYMAKE="$KICS2HOME/bin/.local/cymake"
fi
if [ ! -x "$CYMAKE" ] ; then
  # try cymake of cabal installation
  CYMAKE="$HOME/.cabal/bin/cymake"
fi
if [ ! -x "$CYMAKE" ] ; then
  # try cymake of PAKCS
  CYMAKE="$(dirname `which pakcs`)/../mccparser/bin/cymake"
fi
if [ ! -x "$CYMAKE" ] ; then
  echo "ERROR: Curry parser not found!" >&2
  exit 1
fi

# define KICS2RC
KICS2RC=$HOME/.kics2rc

# Prepare flags for MCC parser:
MCCEXTENDED=
MCCOVERLAPWARN=
if [ -f "$KICS2RC" ] ; then
  # check flag "curryextensions" in kics2rc:
  for i in `sed -n '/^curryextensions=/p' < "$KICS2RC"`
  do 
    if [ xx`expr $i : '.*=\(.*\)'` = xxyes ] ; then
      MCCEXTENDED="--extended"
    fi
  done
  # check flag "warnoverlapping" in kics2rc:
  for i in `sed -n '/^warnoverlapping=/p' < "$KICS2RC"`
  do 
    if [ xx`expr $i : '.*=\(.*\)'` = xxno ] ; then
      MCCOVERLAPWARN="--no-overlap-warn"
    fi
  done
else
  # if there is no rc file, we use the extensions since they
  # are necessary for bootstrapping the system
  MCCEXTENDED="--extended"
fi

"$CYMAKE" $MCCEXTENDED $MCCOVERLAPWARN ${1+"$@"}
