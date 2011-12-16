{-# LANGUAGE CPP, ForeignFunctionInterface, MultiParamTypeClasses #-}
import qualified Curry_Prelude as CP

import Control.Exception as C (IOException, handle)
import Network.BSD (getHostName)
import System.Cmd
import System.CPUTime (getCPUTime)
import System.Environment
import System.Exit

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import System.Win32.Process
#else
import System.Posix.Process (getProcessID)
#endif

-- #endimport - do not remove this line!

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
foreign import stdcall unsafe "windows.h GetCurrentProcessId"
  getProcessID :: IO ProcessId
#endif

external_d_C_getCPUTime :: ConstStore -> CP.C_IO CP.C_Int
external_d_C_getCPUTime _ = toCurry (getCPUTime >>= return . (`div` (10 ^ 9)))

external_d_C_getElapsedTime :: ConstStore -> CP.C_IO CP.C_Int
external_d_C_getElapsedTime _ = toCurry (return 0 :: IO Int)

external_d_C_getArgs :: ConstStore -> CP.C_IO (CP.OP_List CP.C_String)
external_d_C_getArgs _ = toCurry getArgs

external_d_C_prim_getEnviron :: CP.C_String -> ConstStore -> CP.C_IO CP.C_String
external_d_C_prim_getEnviron str _ =
  toCurry (handle handleIOException . getEnv) str
  where
  handleIOException :: IOException -> IO String
  handleIOException _ = return ""

external_d_C_getHostname :: ConstStore -> CP.C_IO CP.C_String
external_d_C_getHostname _ = toCurry getHostName

external_d_C_getPID :: ConstStore -> CP.C_IO CP.C_Int
external_d_C_getPID _ = toCurry $ do
  pid <- getProcessID
  return (fromIntegral pid :: Int)

external_d_C_getProgName :: ConstStore -> CP.C_IO CP.C_String
external_d_C_getProgName _ = toCurry getProgName

external_d_C_prim_system :: CP.C_String -> ConstStore -> CP.C_IO CP.C_Int
external_d_C_prim_system str _ = toCurry system str

instance ConvertCurryHaskell CP.C_Int ExitCode where
  toCurry ExitSuccess     = toCurry (0 :: Int)
  toCurry (ExitFailure i) = toCurry i

  fromCurry j = let i = fromCurry j :: Int
                in if i == 0 then ExitSuccess else ExitFailure i

external_d_C_prim_exitWith :: CP.Curry a => CP.C_Int -> ConstStore -> CP.C_IO a
external_d_C_prim_exitWith c _ = fromIO (exitWith (fromCurry c))

external_d_C_prim_sleep :: CP.C_Int -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_sleep x _ =
  toCurry (\i -> system ("sleep "++show (i :: Int)) >> return ()) x -- TODO

external_d_C_isWindows :: ConstStore -> CP.C_Bool
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
external_d_C_isWindows _ = CP.C_True
#else
external_d_C_isWindows _ = CP.C_False
#endif
