import System.Mem (performGC)
import System.CPUTime
import qualified Curry_Prelude as CP

external_d_C_getProcessInfos ::
            CP.C_IO (CP.OP_List (CP.OP_Tuple2 C_ProcessInfo CP.C_Int))
external_d_C_getProcessInfos = fromIO $ do
  t <- getCPUTime
  return (CP.OP_Cons (CP.OP_Tuple2 C_RunTime (toCurry (t `div` (10^9))))
                     CP.OP_List)

external_d_C_garbageCollectorOff :: CP.C_IO CP.OP_Unit
external_d_C_garbageCollectorOff = fromHaskellIO0 (return ()) -- not supported

external_d_C_garbageCollectorOn :: CP.C_IO CP.OP_Unit
external_d_C_garbageCollectorOn = fromHaskellIO0 (return ()) -- not supported

external_d_C_garbageCollect :: CP.C_IO CP.OP_Unit
external_d_C_garbageCollect = fromHaskellIO0 performGC
