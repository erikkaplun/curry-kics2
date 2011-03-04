tak :: Int -> Int -> Int -> Int
tak x y z = if x <= y then z
            else tak (tak (x `minus` 1) y z)
                     (tak (y `minus` 1) z x)
                     (tak (z `minus` 1) x y)

minus :: Int -> Int -> Int
minus external

leq :: Int -> Int -> Bool
leq external

goal0 = tak 24 16 8
goal1 = tak 27 16 8
goal2 = tak 33 17 8

main = goal1
