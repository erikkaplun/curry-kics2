------------------------------------------------------------------------------
--- Library containing unsafe operations.
--- These operations should be carefully used (e.g., for testing or debugging).
--- These operations should not be used in application programs!
---
--- @author Michael Hanus
--- @version April 2005
------------------------------------------------------------------------------

module Unsafe(unsafePerformIO,trace)
 where

--- Performs and hides an I/O action in a computation (use with care!).
unsafePerformIO :: IO a -> a
unsafePerformIO external

--- Prints the first argument as a side effect and behaves as identity on the
--- second argument.
trace :: String -> a -> a
trace s x = unsafePerformIO (putStr s >> return x)

