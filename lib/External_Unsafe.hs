import System.IO.Unsafe(unsafePerformIO)
import qualified Curry_Prelude as CP

external_d_C_unsafePerformIO :: CP.C_IO a -> a
external_d_C_unsafePerformIO = unsafePerformIO . toIO

-----------------------------------------------------------------------