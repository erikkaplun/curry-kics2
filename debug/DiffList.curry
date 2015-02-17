-- Computations (reverse and quicksort) with difference lists.

-- Infix operator for writing difference lists:
infixr 4 #

-- A difference lists consists of a head and a tail:
data DList a = [a] # [a]

-- Transform a difference list into a standard list:
fromDList :: DList a -> [a]
--fromDList (xs#[]) = xs
fromDList (xs#ys) | ys=:=[] = xs

-- Concatenation of difference lists:
appD :: DList a -> DList a -> DList a
appD (a#b) (m#n) | b=:=m  = (a#n)

-- Reverse with difference lists:
revD :: [a] -> DList a
revD []     = (m#m)                     where m free
revD (x:xs) = appD (revD xs) (x:m # m)  where m free

main0 :: [Int]
main0 = fromDList (revD [1..100])

main1 :: Int -> [()]
main1 n = fromDList (revD (take n (repeat ())))

-- unilist :: [a] -> [a]
-- unilist []     | x =:= [] = x where x free
-- unilist (x:xs) | ys =:= x : un

chain :: Int -> () -> Success
chain n x | n <= 0    = x =:= ()
          | otherwise = x =:= y &> chain (n - 1) y
          where y free
