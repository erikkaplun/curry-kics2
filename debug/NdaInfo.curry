module NdaInfo where

data TypeName
  = ConsName1 Int
  | ConsName2 (Int -> Int)
  | ConsName3 (IO Int)
  | ConsName4 (IO (Int -> Int))

data Type2 = OtherCons TypeName

funcName1 :: TypeName -> TypeName
funcName1 = id
