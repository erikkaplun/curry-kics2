module HoExternal where

prob :: (a -> b) -> a -> b
prob f x = (problem $# f) x

problem :: (a -> b) -> a -> b
problem external

app :: (a -> b) -> a -> b
app f x = f x
