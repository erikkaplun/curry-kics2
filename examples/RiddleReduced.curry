module RiddleReduced where

import AllSolutions

data Country = Norway | England | Spain | Japan

data Color = Red | Yellow | Blue | Green | Ivory

data Pet = Dog | Horse | Snails | Fox | Zebra

data Smoke = Winston | Kools | Chesterfield | LuckyStrike | Parliaments


rightOf :: a -> a -> [a] -> Success
rightOf x y (h1:h2:hs) = (x =:= h1 & y =:= h2) ? rightOf x y (h2:hs)

nextTo :: a -> a -> [a] -> Success
nextTo x y = rightOf x y
nextTo x y = rightOf y x

member :: a -> [a] -> Success
member x (y:ys) = x =:= y ? member x ys


zebra | member (England, unknown, Kools) houses
      & rightOf (unknown, Green,  Parliaments)
                (England, Ivory, Kools)
                houses
      & member (England, unknown,  Kools) houses
      & nextTo (unknown, unknown, Chesterfield)
               (unknown, unknown, unknown)
               houses
      & member (Japan, Green,  Parliaments) houses
      & nextTo (Norway, Red, Chesterfield)
               (Spain, Blue,  Winston)
               houses
      = houses
  where
   houses = [(Norway,unknown,unknown),
             (unknown, unknown, unknown),
             (unknown, unknown , unknown),
             unknown]

zebraCaps = getAllValues zebra