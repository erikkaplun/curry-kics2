import RandomTest
import Array
import Integer
import Maybe

array = listToErrorArray 

allEntries n a = map (a!) [0 .. n]

test1 = eq "initialize and retrieve from array"
           id
           (\xs -> allEntries (length xs - 1) (array xs))

test2 = test "update already initialized positions" upTest

upTest nums = 
  let maxidx = length nums - 1
      startArray = array (map Just nums)
      rndChanges = map (abs . flip mod maxidx) (take (div (maxidx+1) 3) nums)
      newArray   =  startArray // zip rndChanges (repeat Nothing)
   in compare rndChanges 0 nums (allEntries maxidx newArray)

compare _ _ [] [] = True
compare nos n (_:xs) (Nothing:ys) = elem n nos && compare nos (n+1) xs ys
compare nos n (x:xs) (Just y:ys) = x==y && compare nos (n+1) xs ys
