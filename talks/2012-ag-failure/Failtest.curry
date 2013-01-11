head (x : _ ) = x
tail (_ : xs) = xs

length []       = 0
length (_ : xs) = 1 + length xs

nth xs n = if n == 0 then head xs else nth (tail xs) (n - 1)

last xs = nth xs (length xs)

f x = last [1 .. 1000000 * x]