
-- f :: [a -> a]
--f = [id]

-- g :: Int
--g = head f 3 ? 4


h 3 = id . id

k = h 3 4 ? 5