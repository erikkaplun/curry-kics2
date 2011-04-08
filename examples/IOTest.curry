module IOTest where

hello = putStrLn "Hello World!"

ndHello = putStrLn "Hello World!" ? putStrLn "Hi World!"

-- TODO
flipTwoChars = do
  putStr "Please enter two chars: "
  c1 <- getChar
  c2 <- getChar
  putStrLn [c2, c1]

leakTest = mapIO_ (const (putStr "")) [1..10000000]


writeFileTest = writeFile "Test.txt" (makeString 500000)
  where makeString n | n == 0    = ""
                     | otherwise = 'a': makeString (n - 1)

readFileTest = readFile "Test.txt" >>= return . length



main = readFileTest