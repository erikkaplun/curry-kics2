

test1 = return True

test2 = return True ? failed

test3 = failed ? return True

test4 = return True ? return False

test5 = return True >>= failed

test6 = putStrLn "Test6 output" >> (failed ? return True)

test7 = putStrLn "Test7 output" >> (return True ? failed) 

test8 = putStrLn "Test8 output" >> (return True ? return False)

test9 = putStrLn "Test9 output" >>=  \_ -> failed

test10 = (putStrLn "Test10 output 1" >> failed) ? (putStrLn "Test10 output 2" >> return True)

test11 = x =:= True &> putStrLn (show x) where x free

test12 = putStrLn (show (not x)) where x free  
