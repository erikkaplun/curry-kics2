#!/bin/bash

TESTCASE=$1

../idc -q -i ../lib $TESTCASE.curry
ghc --make -i./.curry/kics2:..:../lib/.curry/kics2:../idsupplyioref Main$TESTCASE.hs
ghc --make Test$TESTCASE.hs
exec ./Test$TESTCASE