module EtaTest where

data Pair a = Pair a a

data Bool = True | False

id x = x

not True = False
not False = True

testf x = Pair (f x) (f x) where f y = (id ? not) y

testg x = Pair (g x) (g x) where g = (id ? not)
