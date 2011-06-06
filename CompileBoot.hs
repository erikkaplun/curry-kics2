-- /home/mh/home/curry/idcompiler/idc -v 3  -i /home/mh/home/curry/idcompiler/lib -i /home/mh/home/curry/idcompiler/lib/meta Compile.curry

module Main where
import Basics
import Curry_Compile
main = evalDIO d_C_main

-- ghc -O2 --make -v1 -XMultiParamTypeClasses -XFlexibleInstances -XRelaxedPolyRec  -i/home/mh/home/curry/idcompiler/runtime:/home/mh/home/curry/idcompiler/runtime/idsupplyinteger:./.curry/kics2/:/home/mh/home/curry/idcompiler/lib/.curry/kics2/:/home/mh/home/curry/idcompiler/lib/meta/.curry/kics2/ Compile.hs
