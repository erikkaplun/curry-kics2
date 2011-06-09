#!/bin/sh

# Bootstrap KiCS2 compiler

# make everything clean:
make clean

# Create kics2 via PAKCS (stage 1)
make Compile.state

# Create kics2 via kics2 (stage 2)
make Compile

# Create REPL via KiCS2, stage 2
make REPL

# Now, kics2 should work with the bootstrapped compiler, i.e., you can
# install the complete system:
make
