module ApplyTest where

data Bool = True | False

id x = x

f . g = \ x -> f (g x)

true = let x = (id . id) True in x

true2 = let x = id True in x

apply2 f x = f x
