module IOFree where

act = getLine >>= \s -> unknown =:= s `seq` return 42

double x = x >> x >>= print
