data B = T | F

and T y = y
and F _ = F

_ > _ = T

validDate :: Int -> Int -> Int -> B
validDate v1 v2 v3 = (v2 > 0) `and` ((v2 > 13) `and` (v3 > 0))


-- JS translator:
-- $HOME/kics2/tools/curry2js/Curry2JS test
