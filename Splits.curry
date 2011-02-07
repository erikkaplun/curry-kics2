module Splits (mkSplits) where

mkSplits :: Int -> [Int] -> (Int, [(Int,Int,Int)])
mkSplits n [] = (n,[])
mkSplits n xs@(_:_) = case splits n xs of
  Left sps -> sps
  Right x  -> (x,[])

splits :: Int -> [Int] -> Either (Int, [(Int,Int,Int)]) Int
splits _ []    = error "nothing to split"
splits _ [x]   = Right x
splits i [x,y] = Left (i,[(i,x,y)])
splits i xs@(_:_:_:_) = case half xs of
   (ys,zs) -> case splits i ys of
     Left (i',sps1) -> case splits i' zs of
       Left (i'',sps2) -> Left (i''+1,(i''+1,i',i''):sps1 ++ sps2)
       _ -> error "right list should be longer"

     Right x -> case splits i zs of
       Left (i',sps) -> Left (i'+1,(i'+1,x,i'):sps)
       _ -> error "right list should be longer"

-- split a list into two halfs
half :: [a] -> ([a],[a])
half xs = splitAt (div (length xs) 2) xs

