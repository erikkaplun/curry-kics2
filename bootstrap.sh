#!/bin/sh
LOGFILE=bootstrap.log

# Bootstrap KiCS2 compiler

# make everything clean:
make clean

# start logging
echo "Bootstrapping started at `date`" > $LOGFILE

# Create kics2 via PAKCS (stage 1)
echo "Compiling stage 1" >> $LOGFILE
make Compile.state 2>&1 | tee -a $LOGFILE

# Create kics2 via kics2 (stage 2)
echo "Compiling stage 2" >> $LOGFILE
make Compile 2>&1 | tee -a $LOGFILE

# Create REPL via KiCS2, stage 2
echo "Compiling REPL" >> $LOGFILE
make REPL 2>&1 | tee -a $LOGFILE

echo "Bootstrapping finished at  `date`" >> $LOGFILE

# Now, kics2 should work with the bootstrapped compiler, i.e., you can
# install the complete system:
make
