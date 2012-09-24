-- Testing fcase expressions:

import Assertion

fNot x = fcase x of
          False -> True
          True  -> False

testNot1 = assertValues "fNot1" (fNot False) [True]

testNot2 = assertValues "fNot2" (fNot _) [True,False]


aBool = fcase () of
          _ -> True
          _ -> False

testABool = assertValues "aBool" aBool [True,False]


firstOrSecond zs =
  fcase zs of
    (x:_)    -> x
    (_:y:_)  -> y

testFS = assertValues "firstOrSecond" (firstOrSecond [1,2,3]) [1,2]
