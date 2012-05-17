-- Support for safe execution of actions

module SafeExec where

import qualified Control.Exception as C

-- Execute an action but print possible errors instead of raising them:
safeExec :: IO () -> IO ()
safeExec act =
  C.catch act
          (\e -> do let err = show (e :: C.SomeException)
                    putStrLn ("ERROR: " ++ err)
                    return ())
