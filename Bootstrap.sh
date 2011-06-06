#!/bin/sh

# Bootstrap KiCS2 compiler

# make everything clean:
make clean

# Create kics2 via PAKCS
make Compile.state
make REPL.state

# Create REPL via KiCS2:
make REPLexec

# Compile KiCS2 compiler by PAKCS/KiCS2 compiler:
./idc -v 3  -i lib -i lib/meta Compile.curry

# Compile generated compiler with ghc:
ghc -O2 --make -v1 -XMultiParamTypeClasses -XFlexibleInstances -XRelaxedPolyRec  -iruntime:runtime/idsupplyinteger:./.curry/kics2/:lib/.curry/kics2/:lib/meta/.curry/kics2/ CompileBoot.hs

# Replace PAKCS/KiCS2 compiler by bootstrapped compiler:
cp -p CompileBoot idc

# Now, kics2 should work with the bootstrapped compiler, i.e., you can
# install the complete system:
make
