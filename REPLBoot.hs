-- idc -v 3  -ilib:lib/meta REPL.curry

module Main where
import Basics
import Curry_REPL
main = evalDIO d_C_main

-- ghc -O2 --make -v1 -XMultiParamTypeClasses -XFlexibleInstances -XRelaxedPolyRec  -iruntime:runtime/idsupplyinteger:./.curry/kics2/:lib/.curry/kics2/:lib/meta/.curry/kics2/ REPLBoot.hs
