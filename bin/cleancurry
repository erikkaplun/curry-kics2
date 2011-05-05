#!/bin/sh
# delete all auxiliary files of all Curry programs in a directory

RECURSIVE=no
if [ "$1" = "-r" ] ; then
  RECURSIVE=yes
  shift
fi

PROG=
if [ $# = 1 ] ; then
  # remove possible suffix:
  PROG=`expr $1 : '\(.*\)\.lcurry' \| $1`
  PROG=`expr $PROG : '\(.*\)\.curry' \| $PROG`
  shift
fi

if [ $# != 0 ]
then
  echo "Usage: $0 [-r] [<prog>]" >&2
  echo "-r: apply this command recursively to all subdirectories" >&2
  echo "<prog>: remove only auxiliary Curry files for program <prog>" >&2
  exit 1
fi

if [ -z "$PROG" ] ; then
  CURRYFILES="*.curry *.lcurry"
else
  CURRYFILES="$PROG.curry $PROG.lcurry"
fi

for F in $CURRYFILES
do
  if [ "$F" != "*.curry" -a "$F" != "*.lcurry" ] ; then
    F=`expr $F : '\(.*\)\.lcurry' \| $F`
    F=`expr $F : '\(.*\)\.curry' \| $F`
    if [ -x "$F" ] ; then
      rm -f "$F"  # remove executable main program
    fi
    FDIR=`dirname $F`
    FBASE=`basename $F`
    CURRYDIR="$FDIR/.curry"
    KICS2DIR="$CURRYDIR/kics2"
    if [ -d "$KICS2DIR" ] ; then
      # delete Haskell's main program:
      rm -f $KICS2DIR/Main $KICS2DIR/Main.hs $KICS2DIR/Main.hi $KICS2DIR/Main.o
      # delete translated Haskell files:
      TransF="$KICS2DIR/Curry_$FBASE"
      rm -f $TransF.hs $TransF.hi $TransF.o $TransF.nda $TransF.info
      KICS2DIRFILES=`ls -A $KICS2DIR`
      if [ -z "$KICS2DIRFILES" ] ; then # .curry/kics2 directory is empty
        rmdir $KICS2DIR
      fi
    fi
    # delete also auxiliary files of PAKCS to be upward compatible:
    rm -f "$F.state"
    PAKCSDIR="$CURRYDIR/pakcs"
    if [ -d $PAKCSDIR ] ; then
      rm -f $PAKCSDIR/$FBASE.pl $PAKCSDIR/$FBASE.po
      PAKCSDIRFILES=`ls -A $PAKCSDIR`
      if [ -z "$PAKCSDIRFILES" ] ; then # .curry/pakcs directory is empty
        rmdir "$PAKCSDIR"
      fi
    fi
    CURRYDIRF=$FDIR/.curry/$FBASE
    if [ -d $CURRYDIR ] ; then
      rm -f $CURRYDIRF.fcy $CURRYDIRF.fint $CURRYDIRF.acy $CURRYDIRF.uacy
      CURRYDIRFILES=`ls -A $CURRYDIR`
      if [ -z "$CURRYDIRFILES" ] ; then # .curry directory is empty
        rmdir $CURRYDIR
      fi
    fi
    rm -f -r COOSYLOGS
  fi
done

if [ $RECURSIVE = yes ]
then
  PATHNAME=`(cd "\`dirname \"$0\"\`" > /dev/null ; pwd)`
  for i in *
  do
    if test -d "$i"
    then
      (cd "$i" ; "$PATHNAME/`basename \"$0\"`" -r)
    fi
  done
fi
