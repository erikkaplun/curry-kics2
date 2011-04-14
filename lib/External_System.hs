{-# LANGUAGE MagicHash #-}
import System
import System.Posix.Process(getProcessID)
import System.CPUTime(getCPUTime)
import Network.BSD(getHostName)

external_d_C_getCPUTime :: C_IO C_Int
external_d_C_getCPUTime =
  fromIO (getCPUTime >>= return . toCurry . (`div` 1000000000))

external_d_C_getElapsedTime :: C_IO C_Int
external_d_C_getElapsedTime =
  fromHaskellIO0 (return (0 :: Int))

external_d_C_getArgs :: C_IO (OP_List C_String)
external_d_C_getArgs = fromHaskellIO0 getArgs

external_d_C_prim_getEnviron :: C_String -> C_IO C_String
external_d_C_prim_getEnviron =
  fromHaskellIO1 (\var -> getEnv var `catch` (\_ -> return ""))

external_d_C_getHostname :: C_IO C_String
external_d_C_getHostname = fromHaskellIO0 getHostName

external_d_C_getPID :: C_IO C_Int
external_d_C_getPID = fromIO (do pid <- getProcessID
                                 return (toCurry (fromIntegral pid :: Int))
                             )

external_d_C_getProgName :: C_IO C_String
external_d_C_getProgName = fromIO (getProgName >>= return . toCurry)

external_d_C_prim_system :: C_String -> C_IO C_Int
external_d_C_prim_system = fromHaskellIO1 system

instance ConvertCurryHaskell C_Int ExitCode where
  toCurry ExitSuccess     = toCurry (0::Int)
  toCurry (ExitFailure i) = toCurry i

  fromCurry j = let i = fromCurry j :: Int
                 in if i==0 then ExitSuccess else ExitFailure i

external_d_C_prim_exitWith :: Curry a => C_Int -> C_IO a
external_d_C_prim_exitWith c = fromIO (exitWith (fromCurry c))

external_d_C_prim_sleep :: C_Int -> C_IO OP_Unit
external_d_C_prim_sleep =
  fromHaskellIO1 (\i -> system ("sleep "++show (i :: Int)) >> return ())
