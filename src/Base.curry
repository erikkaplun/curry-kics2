module Base where

data NDClass
  = D   -- deterministic function
  | ND  -- non-deterministic function

data HOClass
 = FO   -- first order function or constructor
 | HO   -- higher order function or constructor
 | HORes Int --function that yields a function
             --with the given arity as result
             --and is not higher order for
             -- other reasons
