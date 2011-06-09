{-# LANGUAGE MagicHash, MultiParamTypeClasses #-}
import System
import System.Posix.Process(getProcessID)
import System.CPUTime(getCPUTime)
import Network.BSD(getHostName)
import qualified Curry_Prelude as CP

external_d_C_getCPUTime :: CP.C_IO CP.C_Int
external_d_C_getCPUTime =
  fromIO (getCPUTime >>= return . toCurry . (`div` 1000000000))

external_d_C_getElapsedTime :: CP.C_IO CP.C_Int
external_d_C_getElapsedTime =
  fromHaskellIO0 (return (0 :: Int))

external_d_C_getArgs :: CP.C_IO (CP.OP_List CP.C_String)
external_d_C_getArgs = fromHaskellIO0 getArgs

external_d_C_prim_getEnviron :: CP.C_String -> CP.C_IO CP.C_String
external_d_C_prim_getEnviron =
  fromHaskellIO1 (\var -> getEnv var `catch` (\_ -> return ""))

external_d_C_getHostname :: CP.C_IO CP.C_String
external_d_C_getHostname = fromHaskellIO0 getHostName

external_d_C_getPID :: CP.C_IO CP.C_Int
external_d_C_getPID = fromIO (do pid <- getProcessID
                                 return (toCurry (fromIntegral pid :: Int))
                             )

external_d_C_getProgName :: CP.C_IO CP.C_String
external_d_C_getProgName = fromIO (getProgName >>= return . toCurry)

external_d_C_prim_system :: CP.C_String -> CP.C_IO CP.C_Int
external_d_C_prim_system = fromHaskellIO1 system

instance ConvertCurryHaskell CP.C_Int ExitCode where
  toCurry ExitSuccess     = toCurry (0::Int)
  toCurry (ExitFailure i) = toCurry i

  fromCurry j = let i = fromCurry j :: Int
                 in if i==0 then ExitSuccess else ExitFailure i

external_d_C_prim_exitWith :: CP.Curry a => CP.C_Int -> CP.C_IO a
external_d_C_prim_exitWith c = fromIO (exitWith (fromCurry c))

external_d_C_prim_sleep :: CP.C_Int -> CP.C_IO CP.OP_Unit
external_d_C_prim_sleep =
  fromHaskellIO1 (\i -> system ("sleep "++show (i :: Int)) >> return ())
