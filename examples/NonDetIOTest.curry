-- KiCS : IO: True
-- PAKCS: IO: True
-- MCC  : no output
test1 = return True

-- KiCS : IO: True
-- PAKCS: IO: True
-- MCC  : Error: cannot duplicate the world
test2 = return True ? failed

-- KiCS : IO: True
-- PAKCS: IO: True
-- MCC  : Error: cannot duplicate the world
test3 = failed ? return True

-- KiCS : IO: True
-- PAKCS: IO: True
--        IO: False
--        ERROR: non-determinism in I/O actions occurred!
-- MCC  : Error: cannot duplicate the world
test4 = return True ? return False

-- KiCS : Prelude.failed
--        <interactive>: program error
-- PAKCS: No more solutions.
-- MCC  : Error: failed
test5 = return True >>= failed

-- KiCS : Test6 output
--        IO: True
-- PAKCS: Test6 output
--        IO: True
-- MCC  : Test6 output
--        Error: cannot duplicate the world
test6 = putStrLn "Test6 output" >> (failed ? return True)

-- KiCS : Test7 output
--        IO: True
-- PAKCS: Test7 output
--        IO: True
-- MCC  : Test7 output
--        Error: cannot duplicate the world
test7 = putStrLn "Test7 output" >> (return True ? failed)

-- KiCS : Test8 output
--        IO: True
-- PAKCS: Test8 output
--        IO: True
--        IO: False
-- MCC  : Test8 output
--        Error: cannot duplicate the world
test8 = putStrLn "Test8 output" >> (return True ? return False)

-- KiCS : Test9 output
--        Prelude.failed
--        <interactive>: program error
-- PAKCS: Test9 output
--        No more solutions.
-- MCC  : Test9 output
--        Error: failed
test9 = putStrLn "Test9 output" >>=  \_ -> failed

test10 = (putStrLn "Test10 output 1" >> failed) ? (putStrLn "Test10 output 2" >> return True)

test11 = (putStrLn "Test11 output 1" >> return True) ? (putStrLn "Test11 output 2" >> failed)

test12 = x =:= True &> putStrLn (show x) where x free

test13 = putStrLn "Test13 output" >> (x =:= True &> putStrLn (show x)) where x free

test14 = putStrLn (show (not x)) where x free
