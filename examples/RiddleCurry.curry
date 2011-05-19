module RiddleCurry where

import AllSolutions

infix 3 &&&
infix 4 `is`

data Country = Norway | England | Spain | Ukraine | Japan

data Color = Red | Yellow | Blue | Green | Ivory

data Pet = Dog | Horse | Snails | Fox | Zebra

data Smoke = Winston | Kools | Chesterfield | LuckyStrike | Parliaments

data Drink = Coffee | Tea | Milk | OrangeJuice | Water

type House = (Country,Color,Pet,Smoke,Drink)

country :: House -> Country
country (c,_,_,_,_) = c

color :: House -> Color
color (_,c,_,_,_) = c

pet :: House -> Pet
pet (_,_,p,_,_) = p

smoke :: House -> Smoke
smoke (_,_,_,s,_) = s

drink :: House -> Drink
drink (_,_,_,_,d) = d

rightOf :: (a -> Success) -> (a -> Success) -> [a] -> Success
rightOf p1 p2 (h1:h2:hs) = (p1 h1 & p2 h2) ? rightOf p1 p2 (h2:hs)

nextTo :: (a -> Success) -> (a -> Success) -> [a] -> Success
nextTo p1 p2 = rightOf p1 p2
nextTo p1 p2 = rightOf p2 p1

atPosition :: Int -> (a -> Success) -> [a] -> Success
atPosition i p xs = p (xs!!i)

(&&&) :: (a -> Success) -> (a -> Success) -> a -> Success
(p1 &&& p2) x = p1 x & p2 x

any' :: (a -> Success) -> [a] -> Success
any' p (x:xs) = p x ? any' p xs

all' :: (a -> Success) -> [a] -> Success
all' _ [] = success
all' p (x:xs) = p x & all' p xs

is :: (House -> a) -> a -> House -> Success
is p x h = x=:=p h

constraints :: [[House] -> Success]
constraints =
  [ atPosition 0 (country `is` Norway)
  , atPosition 2 (drink `is` Milk)
  , rightOf (color `is` Green) (color `is` Ivory)
  , nextTo (smoke `is` Chesterfield) (pet `is` Fox)
  , nextTo (smoke `is` Kools) (pet `is` Horse)
  , nextTo (country `is` Norway) (color `is` Blue)
  , any' (country `is` England &&& color `is` Red)
  , any' (country `is` Spain &&& pet `is` Dog)
  , any' (color `is` Green &&& drink `is` Coffee)
  , any' (country `is` Ukraine &&& drink `is` Tea)
  , any' (pet `is` Snails &&& smoke `is` Winston)
  , any' (color `is` Yellow &&& smoke `is` Kools)
  , any' (smoke `is` LuckyStrike &&& drink `is` OrangeJuice)
  , any' (country `is` Japan &&& smoke `is` Parliaments)
  , any' (pet `is` Zebra)
  , any' (drink `is`  Water)]

isSolution :: [House] -> Success
isSolution houses = all' ($houses) constraints

solutions :: [House]
solutions | isSolution house = house
  where
   house = [unknown,unknown,unknown,unknown,unknown]

-- [(Norway,Yellow,Fox,Kools,Free),(Ukraine,Blue,Horse,Chesterfield,Tea)
-- ,(England,Red,Snails,Winston,Milk),(Japan,Green,Free,Parliaments,Coffee)
-- ,(Spain,Ivory,Dog,LuckyStrike,OrangeJuice)]
main = solutions

mainCaps = getAllValues solutions
