module RiddleProlog where

data Country = Norway | England | Spain | Ukraine | Japan

data Color = Red | Yellow | Blue | Green | Ivory

data Pet = Dog | Horse | Snails | Fox | Zebra

data Smoke = Winston | Kools | Chesterfield | LuckyStrike | Parliaments

data Drink = Coffee | Tea | Milk | OrangeJuice | Water


rightOf :: a -> a -> [a] -> Success
rightOf x y (h1:h2:hs) = (x =:= h1 & y =:= h2) ? rightOf x y (h2:hs)

nextTo :: a -> a -> [a] -> Success
nextTo x y = rightOf x y
nextTo x y = rightOf y x

member :: a -> [a] -> Success
member x (y:ys) = x =:= y ? member x ys

zebra :: (Country,Country)
zebra | member (England, Red, unknown, unknown, unknown) houses
      & member (Spain, unknown, Dog, unknown, unknown) houses
      & member (unknown, Green, unknown, unknown, Coffee) houses
      & member (Ukraine, unknown, unknown, unknown, Tea) houses
      & rightOf (unknown, Green, unknown, unknown, unknown)
                (unknown, Ivory, unknown, unknown, unknown)
                houses
      & member (unknown, unknown, Snails, Winston, unknown) houses
      & member (unknown, Yellow, unknown, Kools, unknown) houses
      & nextTo (unknown, unknown, unknown, Chesterfield, unknown)
               (unknown, unknown, Fox, unknown, unknown)
               houses
      & nextTo (unknown, unknown, unknown, Kools, unknown)
               (unknown, unknown, Horse, unknown, unknown)
               houses
      & member (unknown, unknown, unknown, LuckyStrike, OrangeJuice) houses
      & member (Japan, unknown, unknown, Parliaments, unknown) houses
      & nextTo (Norway, unknown, unknown, unknown, unknown)
               (unknown, Blue, unknown, unknown, unknown)
               houses
      & member (zebraOwner, unknown, Zebra, unknown, unknown) houses
      & member (waterDrinker, unknown, unknown, unknown, Water) houses
      = (zebraOwner,waterDrinker)
  where
   zebraOwner = unknown
   waterDrinker = unknown
   houses = [(Norway,unknown,unknown,unknown,unknown),
             unknown,
             (unknown,unknown,unknown,unknown,Milk),
             unknown,
             unknown]

main = zebra
