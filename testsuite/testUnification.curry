-- Testing unification of free variables:

import Assertion

-- simple types
test0 = assertValues "test0" (True =:= True)  [success]
test1 = assertValues "test1" (True =:= False) []
test2 = assertValues "test2" (x =:= True) [success] where x free
test3 =  assertValues "test3" (x =:= True &> x) [True] where x free
test4 =  assertValues "test4" (x =:= True &> [x,x]) [[True,True]] where x free
test5 =  assertValues "test5" ([x =:= True &> x,x])  [[True,True]] where x free
test6 =  assertValues "test6" ([x,x =:= True &> x]) [[True,True]] where x free

uni :: Bool -> Bool -> Success
uni x y = x =:= y

test7 =  assertValues "test7" (uni x y) [success] where x,y free
test8 =   assertValues "test8" (x =:= y    
      &> y=:= True
      &> [x,y]) [[True,True]]   where x,y free

test9 =   assertValues "test9" (x =:= y  
      &> x=:= True
      &> [x,y]) [[True,True]]   where x,y free

test10 =   assertValues "test10" (x =:= y
       &> x =:= z
       &> y=:=False &> [x,y])[[False,False]]
  where x,y,z free

test11 =   assertValues "test11" (x =:= y     
       &> x =:= z
       &> x=:=False
       &> [z,x,y])[[False,False,False]]   where x,y,z free

test12 = assertValues "test12" ( x =:= y     
       &> x =:= z
       &> z=:=False &> [x,z,y]) [[False,False,False]]
  where x,y,z free

test13 =   assertValues "test13" (x =:= y      
       &> x =:= z
       &> z=:=False
       &> y=:=False
       &> [x,y,z]) [[False,False,False]]   where x,y,z free

test14 =  assertValues "test14" (x=:=y &> y=:=False &> x) [False] where x,y free
test15 =  assertValues "test15" (x=:=(y?True) &> y=:=False &> x) [False,True] where x,y free

-- complex types

test16 =  assertValues "test16" (x=:=[] &> True:x) [[True]]  where x free
test17 =  assertValues "test17" (x=:=[True] &> x) [[True]] where x free

test18 =  assertValues "test18" (x=:=y &> (y=:= [True] &> x)) [[True]] where x,y free

f [False] = success
test19 =  assertValues "test19" (x=:=y &> y=:= [True] &> f x &> x) [] where x,y free

g [True] = success
test20 =  assertValues "test20" (x=:=y &> y=:= [True] &> g x &> x) [[True]] where x,y free


test21 =  assertValues "test21" (x=:=(y?[False]) &> y=:=[True] &> x)[[True],[False]] where x,y free

uni2 :: [Bool] -> [Bool] -> Success
uni2 x y = x =:= y

test22 =  assertValues "test22" (uni2 x [y]) [success] where x,y free
--test23 =  assertValues "test23" (uni2 x [y] &> x)  [y] where x,y free TODO: how to test this?
test24 =  assertValues "test24" (x =:= [y] &> y=:=True &> x) [[True]] where x,y free
-- test25 =  assertValues "test25" (x =:= (y:z) &> x=:=(False:z1:z2) &> z2 =:=[] &> x) [[False,z1]]
--   where x,y,z,z1,z2 free TODO: how to test this?


test26 =  assertValues "test26" (x =:= [y?True] &> y=:=False &> x) [[False],[True]] where x,y free

test27 =  assertValues "test27" ([x,True,z]=:=[False,y,y] &> [x,y,z]) [[False,True,True]]
  where x,y,z free

test28 = assertValues "test28" (x =:= (y =:= [True] &> y) &> x)[[True]] where x, y free

test29 = assertValues "test29" (x =:= (True:(y =:= [] &> y)) &> x) [[True]] where x, y free

test30 = assertValues "test30" (x =:= [True] &> y =:= [False] &> x =:= y) [] where x , y free

test31 = assertValues "test31" (x =:= [] &> y ++ [False] =:= x) [] where x, y free
test32 = assertValues "test32" (x =:= [] &> y1:(y2 ++ [False]) =:= x) [] where x, y1, y2 free
test33 = assertValues "test33" (x =:= [] &> (y2 ++ [False]) =:= x) [] where x, y2 free
test34 = assertValues "test34" (x =:= [] &> y1:[False] =:= x) [] where x, y1 free
-- This call should fail due to occure check
-- An occure check is not yet implemented
-- test35 = assertValues "test35" (ones =:= 1:ones &> head ones) [] where ones free