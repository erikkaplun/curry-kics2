#!/bin/bash

TESTCASE=$1

../idc -q -i ../lib $TESTCASE.curry
ghc --make -i..:../lib:../idsupplyioref Main$TESTCASE.hs
ghc --make Test$TESTCASE.hs
exec ./Test$TESTCASE