import System.Mem (performGC)
import System.CPUTime
import qualified Curry_Prelude as CP

external_d_C_getProcessInfos ::
            ConstStore -> CP.C_IO (CP.OP_List (CP.OP_Tuple2 C_ProcessInfo CP.C_Int))
external_d_C_getProcessInfos _ = fromIO $ do
  t <- getCPUTime
  return (CP.OP_Cons (CP.OP_Tuple2 C_RunTime (toCurry (t `div` (10^9))))
                     CP.OP_List)

external_d_C_garbageCollectorOff :: ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_garbageCollectorOff _ = fromHaskellIO0 (return ()) -- not supported

external_d_C_garbageCollectorOn :: ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_garbageCollectorOn _ = fromHaskellIO0 (return ()) -- not supported

external_d_C_garbageCollect :: ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_garbageCollect _ = fromHaskellIO0 performGC
