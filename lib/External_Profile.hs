import System.Mem (performGC)
import System.CPUTime

external_d_C_getProcessInfos :: C_IO (OP_List (OP_Tuple2 C_ProcessInfo C_Int))
external_d_C_getProcessInfos = fromIO $ do
  t <- getCPUTime
  return (OP_Cons (OP_Tuple2 C_RunTime (toCurry (t `div` (10^9)))) OP_List)

external_d_C_garbageCollectorOff :: C_IO OP_Unit
external_d_C_garbageCollectorOff = fromHaskellIO0 (return ()) -- not supported

external_d_C_garbageCollectorOn :: C_IO OP_Unit
external_d_C_garbageCollectorOn = fromHaskellIO0 (return ()) -- not supported

external_d_C_garbageCollect :: C_IO OP_Unit
external_d_C_garbageCollect = fromHaskellIO0 performGC
