--------------------------------------------------------------------------------
-- define operation last by a function pattern:
last :: [a] -> a
last (_++[x]) = x

test1 = (last (map (+1) [1..200])) --> 201

test2 = (last (take 10000 (repeat failed) ++ [1])) --> [1]

--------------------------------------------------------------------------------
-- define a palindrome constraint:
pali :: [a] -> Success
pali (xs ++ reverse xs) = success

test3 = (pali [True,False,False,True]) --> success
test4 = (pali [True,False]) --> fail

--------------------------------------------------------------------------------
-- define tree transformations and search by function patterns:
data Exp = Lit Nat | Var VarName | Add Exp Exp | Mul Exp Exp

data Nat = Z | S Nat
data VarName = X1 | X2 | X3
data Position = Lt | Rt

evalTo e = Add (Lit Z) e
         ? Add e (Lit Z)
         ? Mul (Lit (S Z)) e
         ? Mul e (Lit (S Z))

replace _         []    x = x
replace (Add l r) (Lt:p) x = Add (replace l p x) r
replace (Add l r) (Rt:p) x = Add l (replace r p x)
replace (Mul l r) (Lt:p) x = Mul (replace l p x) r
replace (Mul l r) (Rt:p) x = Mul l (replace r p x)

simplify :: Exp -> Exp
simplify (replace c p (evalTo x)) = replace c p x

-- Apply a transformation to some data structure as long as it is defined:
--transformAll :: (a -> a) -> a -> IO a
--transformAll trans term =
--   (getOneValue (trans term)) >>= maybe (return term) (transformAll trans) 


test5 = (simplify (Mul (Lit (S Z)) (Var X1))) --> (Var X1)
--test6 = (transformAll simplify exp) --> (Var X1)

{-
exp = Mul (Lit 1) (Add (Var "x") (Lit 0))

exp_n n e = if n==0 then e else Add (exp_n (n-1) e) (exp_n (n-1) e)

bigexp | e =:= exp_n 8 exp = e  where e free
-}

-- return some variable occurring in an expression:
varInExp :: Exp -> VarName
varInExp (replace _ _ (Var v)) = v
--varInExp e | (replace c p (Var v)) =:<= e = v   where c,p,v free

test7 = varInExp (Mul (Var X2) (Var X1)) --> X2 or X1
{-
getVarsInExp :: Exp -> IO [String]
getVarsInExp e = getAllValues (varInExp e)

test7 = AssertIO "vars" (getVarsInExp bigexp >>= return . length) 256
-}

--------------------------------------------------------------------------------
-- Dijsktra's Dutch National Flag problem with function patterns

data Color = Red | White | Blue

solve (x++[White]++y++[Red  ]++z) = solve (x++[Red]++y++[White]++z)
solve (x++[Blue ]++y++[Red  ]++z) = solve (x++[Red]++y++[Blue]++z)
solve (x++[Blue ]++y++[White]++z) = solve (x++[White]++y++[Blue]++z)
solve flag | isDutchFlag flag = flag

isDutchFlag (uni Red ++ uni White ++ uni Blue) = success

uni _ = []
uni color = color : uni color

test8 = (solve [White,Red,White,Blue,Red,Blue,White])
        -->            [Red,Red,White,White,White,Blue,Blue]

--------------------------------------------------------------------------------
