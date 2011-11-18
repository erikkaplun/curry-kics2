{-# LANGUAGE MagicHash, MultiParamTypeClasses #-}
import System
import System.Posix.Process(getProcessID)
import System.CPUTime(getCPUTime)
import Network.BSD(getHostName)
import qualified Curry_Prelude as CP

external_d_C_getCPUTime :: ConstStore -> CP.C_IO CP.C_Int
external_d_C_getCPUTime _ =
  fromIO (getCPUTime >>= return . toCurry . (`div` 1000000000))

external_d_C_getElapsedTime :: ConstStore -> CP.C_IO CP.C_Int
external_d_C_getElapsedTime _ =
  fromHaskellIO0 (return (0 :: Int))

external_d_C_getArgs :: ConstStore -> CP.C_IO (CP.OP_List CP.C_String)
external_d_C_getArgs _ = fromHaskellIO0 getArgs

external_d_C_prim_getEnviron :: CP.C_String -> ConstStore -> CP.C_IO CP.C_String
external_d_C_prim_getEnviron str _ =
  fromHaskellIO1 (\var -> getEnv var `catch` (\_ -> return "")) str

external_d_C_getHostname :: ConstStore -> CP.C_IO CP.C_String
external_d_C_getHostname _ = fromHaskellIO0 getHostName

external_d_C_getPID :: ConstStore -> CP.C_IO CP.C_Int
external_d_C_getPID _ = fromIO (do pid <- getProcessID
                                   return (toCurry (fromIntegral pid :: Int))
                               )

external_d_C_getProgName :: ConstStore -> CP.C_IO CP.C_String
external_d_C_getProgName _ = fromIO (getProgName >>= return . toCurry)

external_d_C_prim_system :: CP.C_String -> ConstStore -> CP.C_IO CP.C_Int
external_d_C_prim_system str _ = fromHaskellIO1 system str

instance ConvertCurryHaskell CP.C_Int ExitCode where
  toCurry ExitSuccess     = toCurry (0::Int)
  toCurry (ExitFailure i) = toCurry i

  fromCurry j = let i = fromCurry j :: Int
                 in if i==0 then ExitSuccess else ExitFailure i

external_d_C_prim_exitWith :: CP.Curry a => CP.C_Int -> ConstStore -> CP.C_IO a
external_d_C_prim_exitWith c _ = fromIO (exitWith (fromCurry c))

external_d_C_prim_sleep :: CP.C_Int -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_sleep x _ =
  fromHaskellIO1 (\i -> system ("sleep "++show (i :: Int)) >> return ()) x
