module NewUniTest where

data Maybe3 a = Nothing3 | Just3 a a a | Just1 a

main = x =:= y &> y =:= Just3 (Just1 (Just1 True)) Nothing3 (Just3 Nothing3 Nothing3 Nothing3) &> x where x,y free