module IOTest where

hello = putStrLn "Hello World!"

ndHello = putStrLn "Hello World!" ? putStrLn "Hi World!"

flipTwoChars = do
  putStr "Please enter two chars: "
  c1 <- getChar
  c2 <- getChar
  putStrLn [c2, c1]
