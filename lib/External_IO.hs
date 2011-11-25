{-# LANGUAGE MultiParamTypeClasses #-}
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


external_d_C_stdin :: ConstStore -> C_Handle
external_d_C_stdin _ = PrimData (OneHandle stdin)

external_d_C_stdout :: ConstStore -> C_Handle
external_d_C_stdout _ = PrimData (OneHandle stdout)

external_d_C_stderr :: ConstStore -> C_Handle
external_d_C_stderr _ = PrimData (OneHandle stderr)

external_d_C_prim_openFile :: CP.OP_List CP.C_Char -> C_IOMode
                           -> ConstStore -> CP.C_IO C_Handle
external_d_C_prim_openFile fn  mode _ =
  fromHaskellIO2 (\s m -> openFile s m >>= return . OneHandle) fn mode

external_d_C_prim_hClose :: C_Handle -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_hClose handle _ = fromHaskellIO1
  (\ch -> case ch of OneHandle h       -> hClose h
                     InOutHandle h1 h2 -> hClose h1 >> hClose h2) handle

external_d_C_prim_hFlush :: C_Handle -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_hFlush h _ = fromHaskellIO1 (hFlush . outputHandle) h

external_d_C_prim_hIsEOF :: C_Handle -> ConstStore -> CP.C_IO CP.C_Bool
external_d_C_prim_hIsEOF h _ = fromHaskellIO1 (hIsEOF . inputHandle) h

external_d_C_prim_hSeek :: C_Handle -> C_SeekMode -> CP.C_Int
                        -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_hSeek handle  mode i _ = fromHaskellIO3 (\h -> hSeek (inputHandle h)) handle mode i


external_d_C_prim_hWaitForInput :: C_Handle -> CP.C_Int -> ConstStore -> CP.C_IO CP.C_Bool
external_d_C_prim_hWaitForInput handle i _=
  fromHaskellIO2 (\h -> myhWaitForInput (inputHandle h)) handle i

myhWaitForInput :: Handle -> Int -> IO Bool
myhWaitForInput h i =
  if i < 0
  then hIsEOF h >>= return . not
  else hWaitForInput h i


external_d_C_prim_hWaitForInputs :: CP.OP_List C_Handle -> CP.C_Int
                                 -> ConstStore -> CP.C_IO CP.C_Int
external_d_C_prim_hWaitForInputs hs i _ = fromHaskellIO2 selectHandle hs i

selectHandle :: [CurryHandle] -> Int -> IO Int
selectHandle handles t = do
  mvar <- newEmptyMVar
  threads <- mapM (\ (i,h) -> forkIO (waitOnHandle (inputHandle h) i t mvar))
                  (zip [0..] handles)
  inspectRes (length handles) mvar threads

inspectRes :: Int -> MVar (Maybe Int) -> [ThreadId] -> IO Int
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
            -> ConstStore -> CP.C_IO (CP.C_Either CP.C_Int (CP.OP_List a))
external_d_C_prim_hWaitForInputsOrMsg = error "hWaitForInputsOrMsg undefined"

external_d_C_prim_hGetChar :: C_Handle -> ConstStore -> CP.C_IO CP.C_Char
external_d_C_prim_hGetChar h _ = fromHaskellIO1 (hGetChar . inputHandle) h

external_d_C_prim_hPutChar :: C_Handle -> CP.C_Char -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_hPutChar h c _ = fromHaskellIO2 (hPutChar . outputHandle) h c

external_d_C_prim_hIsReadable :: C_Handle -> ConstStore -> CP.C_IO CP.C_Bool
external_d_C_prim_hIsReadable h _ = fromHaskellIO1 (hIsReadable . inputHandle) h

external_d_C_prim_hIsWritable :: C_Handle -> ConstStore -> CP.C_IO CP.C_Bool
external_d_C_prim_hIsWritable h _ = fromHaskellIO1 (hIsWritable . outputHandle) h

