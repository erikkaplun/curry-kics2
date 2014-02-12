---------------------------------------------------------------------------------
-- Test the extension of PAKCS for function patterns as described in:
-- Sergio Antoy, Michael Hanus:
-- Declarative Programming with Function Patterns
-- Proceedings of the International Symposium on Logic-based Program Synthesis
-- and Transformation (LOPSTR'05), appeared in Springer LNCS 3901, 2006
-- http://www-ps.informatik.uni-kiel.de/~mh/papers/LOPSTR05.html
--
-- Note: this requires the setting "curryextensions=yes" in
--  ~/.kics2rc or ~/.pakcsrc
--------------------------------------------------------------------------------

import Maybe

import Assertion
import AllSolutions

import SearchTree
someValue = head . allValuesDFS . someSearchTree

--------------------------------------------------------------------------------
-- define operation last by a function pattern:
last :: [a] -> a
last (_++[x]) = x

test1 = assertEqual  "last1" (last (map (+1) [1..200])) 201

test2 = assertValues "last2" (last (take 10000 (repeat failed) ++ [1])) [1]

--------------------------------------------------------------------------------
-- define a palindrome constraint:
pali :: [a] -> Success
pali (xs ++ reverse xs) = success
--pali l | xs ++ reverse xs =:<= l = success      where xs free

test3 = assertEqual "palindrome1" (pali "otto") success
test4 = assertValues "palindrome2" (pali "toto") []

--------------------------------------------------------------------------------
-- define tree transformations and search by function patterns:
data Exp = Lit Int | Var [Char] | Add Exp Exp | Mul Exp Exp

evalTo e = Add (Lit 0) e
         ? Add e (Lit 0)
         ? Mul (Lit 1) e
         ? Mul e (Lit 1)

replace _         []    x = x
replace (Add l r) (1:p) x = Add (replace l p x) r
replace (Add l r) (2:p) x = Add l (replace r p x)
replace (Mul l r) (1:p) x = Mul (replace l p x) r
replace (Mul l r) (2:p) x = Mul l (replace r p x)

simplify :: Exp -> Exp
simplify (replace c p (evalTo x)) = replace c p x
--simplify e | (replace c p (evalTo x)) =:<= e  = replace c p x  where c,p,x free

-- Apply a transformation to some data structure as long as it is defined:
transformAll :: (a -> a) -> a -> IO a
transformAll trans term =
   (getOneValue (trans term)) >>= maybe (return term) (transformAll trans)


test5 = assertEqual "simplify1" (simplify (Mul (Lit 1) (Var "x")))
                                (Var "x")
test6 = assertIO    "simplify2" (transformAll simplify exp) (Var "x")

exp = Mul (Lit 1) (Add (Var "x") (Lit 0))

exp_n n e = if n==0 then e else Add (exp_n (n-1) e) (exp_n (n-1) e)

bigexp | e =:= exp_n 8 exp = e  where e free

-- return some variable occurring in an expression:
varInExp :: Exp -> String
varInExp (replace _ _ (Var v)) = v
--varInExp e | (replace c p (Var v)) =:<= e = v   where c,p,v free

getVarsInExp :: Exp -> IO [String]
getVarsInExp e = getAllValues (varInExp e)

test7 = assertIO "vars" (getVarsInExp bigexp >>= return . length) 256


--------------------------------------------------------------------------------
-- Dijsktra's Dutch National Flag problem with function patterns

data Color = Red | White | Blue

solve (x++[White]++y++[Red  ]++z) = solve (x++[Red]++y++[White]++z)
solve (x++[Blue ]++y++[Red  ]++z) = solve (x++[Red]++y++[Blue]++z)
solve (x++[Blue ]++y++[White]++z) = solve (x++[White]++y++[Blue]++z)
solve flag | isDutchFlag flag = flag
 where isDutchFlag (uni Red ++ uni White ++ uni Blue) = success
       --isDutchFlag flag | uni Red ++ uni White ++ uni Blue =:<= flag = success
       uni _ = []
       uni color = color : uni color

test8 = assertEqual "Dutch Flag"
                    (someValue (solve [White,Red,White,Blue,Red,Blue,White]))
                    [Red,Red,White,White,White,Blue,Blue]

--------------------------------------------------------------------------------

test9 = assertValues "test9" (y =:<= True & x =:= True & x =:= y) [success]
  where x, y free

test10 = assertValues "test10"
                      (y =:<= [True] &> x =:<= ([]?[False]) &> x=:=y &> x) []
  where x,y free

test11 = assertValues "test11"
                      (y =:<= [True] &> x =:<= ([False]?[]) &> x=:=y &> x) []
  where x,y free

mkSamePair x = (x,x)

fpair (mkSamePair x) = x

gpair (mkSamePair x) y = y

test12 = assertValues "test12" (isJust (fpair (y, gpair (y, Just failed) z))) []
  where y, z free


-------------------------------------------------------------------------------
-- Function Pattern tests by Michael
-------------------------------------------------------------------------------

data Nat = O | S Nat

g x y = (x,y)

pair x | g y y =:<= x = success where y free

test13 = assertValues "test13" (pair (0,1)) []

test14 = assertValues "test14" (pair (0,0)) [success]

-- This call should fail due to an occure check
-- However, an occure check is not yet implemented
-- test15 = assertValues "test15"  (pair (x, S x)) []
--   where x free 

test16 = assertValues "test16" (pair (x,failed)) []
  where x free 

f x | g (const 0 y) y =:<= x = success where y free

test17 = assertValues "test17" (f (x,failed)) [success]
  where x free 

h x | g (id y) y =:<= x = success where y free

test18 = assertValues "test18" (h (x,failed)) []
  where x free

