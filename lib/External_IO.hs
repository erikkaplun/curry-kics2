import System.IO
import Control.Concurrent
import qualified Curry_Prelude as CP

type C_Handle = PrimData CurryHandle

instance ConvertCurryHaskell C_IOMode IOMode where
  toCurry ReadMode   = C_ReadMode
  toCurry WriteMode  = C_WriteMode
  toCurry AppendMode = C_AppendMode

  fromCurry C_ReadMode   = ReadMode
  fromCurry C_WriteMode  = WriteMode
  fromCurry C_AppendMode = AppendMode
  fromCurry _            = error "IOMode data with no ground term occurred"

instance ConvertCurryHaskell C_SeekMode SeekMode where
  toCurry AbsoluteSeek = C_AbsoluteSeek
  toCurry RelativeSeek = C_RelativeSeek
  toCurry SeekFromEnd  = C_SeekFromEnd

  fromCurry C_AbsoluteSeek = AbsoluteSeek
  fromCurry C_RelativeSeek = RelativeSeek
  fromCurry C_SeekFromEnd  = SeekFromEnd
  fromCurry _            = error "SeekMode data with no ground term occurred"


external_d_C_stdin :: C_Handle
external_d_C_stdin = PrimData (OneHandle stdin)

external_d_C_stdout :: C_Handle
external_d_C_stdout = PrimData (OneHandle stdout)

external_d_C_stderr :: C_Handle
external_d_C_stderr = PrimData (OneHandle stderr)

external_d_C_prim_openFile :: CP.OP_List CP.C_Char -> C_IOMode
                           -> CP.C_IO C_Handle
external_d_C_prim_openFile =
  fromHaskellIO2 (\s m -> openFile s m >>= return . OneHandle)

external_d_C_prim_hClose :: C_Handle -> CP.C_IO CP.OP_Unit
external_d_C_prim_hClose = fromHaskellIO1
  (\ch -> case ch of OneHandle h       -> hClose h
                     InOutHandle h1 h2 -> hClose h1 >> hClose h2)

external_d_C_prim_hFlush :: C_Handle -> CP.C_IO CP.OP_Unit
external_d_C_prim_hFlush = fromHaskellIO1 (hFlush . outputHandle)

external_d_C_prim_hIsEOF :: C_Handle -> CP.C_IO CP.C_Bool
external_d_C_prim_hIsEOF = fromHaskellIO1 (hIsEOF . inputHandle)

external_d_C_prim_hSeek :: C_Handle -> C_SeekMode -> CP.C_Int
                        -> CP.C_IO CP.OP_Unit
external_d_C_prim_hSeek = fromHaskellIO3 (\h -> hSeek (inputHandle h))


external_d_C_prim_hWaitForInput :: C_Handle -> CP.C_Int -> CP.C_IO CP.C_Bool
external_d_C_prim_hWaitForInput =
  fromHaskellIO2 (\h -> myhWaitForInput (inputHandle h))

myhWaitForInput :: Handle -> Int -> IO Bool
myhWaitForInput h i =
  if i < 0 
  then hIsEOF h >>= return . not
  else hWaitForInput h i 


external_d_C_prim_hWaitForInputs :: CP.OP_List C_Handle -> CP.C_Int
                                 -> CP.C_IO CP.C_Int
external_d_C_prim_hWaitForInputs = fromHaskellIO2 selectHandle

selectHandle :: [CurryHandle] -> Int -> IO Int
selectHandle handles t = do
  mvar <- newEmptyMVar
  threads <- mapM (\ (i,h) -> forkIO (waitOnHandle (inputHandle h) i t mvar)) 
                  (zip [0..] handles)
  inspectRes (length handles) mvar threads

inspectRes :: Int -> MVar (Maybe Int) -> [ThreadId] ->  IO Int
inspectRes 0 _    _       = return (-1)
inspectRes n mvar threads = do
  res <- readMVar mvar
  case res of 
    Nothing -> inspectRes (n-1) mvar threads
    Just v  -> mapM_ killThread threads >> return v

waitOnHandle :: Handle -> Int -> Int -> MVar (Maybe Int) -> IO ()
waitOnHandle h v t mvar = do
   	    ready <- myhWaitForInput h t
  	    putMVar mvar (if ready then Just v else Nothing)


external_d_C_prim_hWaitForInputsOrMsg ::
 CP.Curry a => CP.OP_List C_Handle -> CP.OP_List a
            -> CP.C_IO (CP.C_Either CP.C_Int (CP.OP_List a))
external_d_C_prim_hWaitForInputsOrMsg = error "hWaitForInputsOrMsg undefined"

external_d_C_prim_hGetChar :: C_Handle -> CP.C_IO CP.C_Char
external_d_C_prim_hGetChar = fromHaskellIO1 (hGetChar . inputHandle)

external_d_C_prim_hPutChar :: C_Handle -> CP.C_Char -> CP.C_IO CP.OP_Unit
external_d_C_prim_hPutChar = fromHaskellIO2 (hPutChar . outputHandle)

external_d_C_prim_hIsReadable :: C_Handle -> CP.C_IO CP.C_Bool
external_d_C_prim_hIsReadable = fromHaskellIO1 (hIsReadable . inputHandle)

external_d_C_prim_hIsWritable :: C_Handle -> CP.C_IO CP.C_Bool
external_d_C_prim_hIsWritable = fromHaskellIO1 (hIsWritable . outputHandle)

