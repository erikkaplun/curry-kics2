module FailTrace (inspectTrace) where

import Control.Monad (unless)
import Data.Char     (isDigit, toLower)
import Data.List     (intercalate)
import System.IO     (hFlush, stdin, stdout, hGetEcho, hSetEcho
                     , hGetBuffering, hSetBuffering, BufferMode (..))

import FailInfo

-- augmented calls

type CallStack = [ACall]
type AArg = (Bool, String)

data ACall = ACall String [AArg]

augmentCall :: Call -> ACall
augmentCall (f, args) = ACall f $ zip (repeat False) args

-- failTrace

inspectTrace :: FailInfo -> IO ()
inspectTrace (FailInfo calls cause) = do
  putStrLn "A failure occured for the following reason:"
  putStrLn cause
  blank
  if null calls
    then putStrLn "No call stack is available. Try enabling the trace option."
    else do
    k <- askUserKey "yn\n" "Do you want to see the call stack? [Y(es)/n(o)] "
    case toLower k of
      'n' -> return ()
      _   -> do
      let stack = map augmentCall calls
      printStack stack >> inspect stack

printStack :: CallStack -> IO ()
printStack stack = do
  putStrLn "\nCall stack:"
  mapM_ putStrLn $ zipWith format [0 :: Int ..] stack
  where format n call = show n ++ ": " ++ showCall call

showCall :: ACall -> String
showCall (ACall f args) = intercalate " " (f : map showArg args)
  where showArg (eval, arg) = if eval then arg else "_"

inspect :: CallStack -> IO ()
inspect cs = help >> go cs
  where
  help = do
    blank
    putStrLn "You can evaluate the arguments of the calls if you wish. Type"
    putStrLn "  h          to show this information"
    putStrLn "  s          to show the stack again"
    putStrLn "  a          to evaluate all arguments"
    putStrLn "  <n>        to evaluate all arguments of the n-th call"
    putStrLn "  <n>.<m>    to evaluate the m-th argument of the n-th call"
    putStrLn "  q          exits inspection"
    blank
  go stack = do
    putStr "What do you like to inspect?: "
    hFlush stdout
    input <- getLine
    case input of
      "q"                          -> return ()
      "h"                          -> help >> go stack
      "s"                          -> printStack stack >> go stack
      "a"                          -> printStack (evalStack stack)
      _ | isNumber n && m == ""    -> printCall (read n) stack >>= go
        | isNumber n && isNumber m -> printArg (read n) (read m) stack >>= go
        | otherwise                -> putStrLn "Sorry, I didn't understand you." >> go stack
          where (n, m) = let (x, y) = break (== '.') input
                        in (x, drop 1 y)

printCall :: Int -> CallStack -> IO CallStack
printCall n stack
  | n < 0 || n >= length stack = putStrLn "no such call" >> return stack
  | otherwise                  = do
    let stack' = evalCallAt n stack
    putStrLn (showCall (stack' !! n))
    return stack'

printArg :: Int -> Int -> CallStack -> IO CallStack
printArg n m stack
  | n < 0 || n >= length stack = putStrLn "no such call"     >> return stack
  | m < 0 || m >= length args  = putStrLn "no such argument" >> return stack
  | otherwise                  = do
    let stack' = evalArgAt n m stack
    putStrLn (showCall (stack' !! n))
    return stack'
  where ACall _ args = stack !! n

updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt n f xs = case splitAt n xs of
  (ys, []    ) -> ys
  (ys, z : zs) -> ys ++ f z : zs

evalStack :: CallStack -> CallStack
evalStack = map evalCall

evalCall :: ACall -> ACall
evalCall (ACall f args) = ACall f (map evalArg args)

evalArg :: AArg -> AArg
evalArg (_, arg) = (True, arg)

evalCallAt :: Int -> [ACall] -> [ACall]
evalCallAt n = updateAt n evalCall

evalArgAt :: Int -> Int -> [ACall] -> [ACall]
evalArgAt n m = updateAt n updCall
  where updCall (ACall f args) = ACall f (updateAt m evalArg args)

isNumber :: String -> Bool
isNumber s = not (null s) && all isDigit s

askUserKey :: String -> String -> IO Char
askUserKey accept prompt = do
  putStr prompt >> hFlush stdout
  echo <- hGetEcho stdin
  buf  <- hGetBuffering stdin
  hSetEcho stdin False  -- to avoid interference with rlwrap
  hSetBuffering stdin NoBuffering
  c <- getChar
  putChar c
  hSetEcho stdin echo
  hSetBuffering stdin buf
  unless (c == '\n') (putChar '\n')
  if c `elem` accept then return c else askUserKey accept prompt

blank :: IO ()
blank = putStrLn ""
