import System.Time
import System.Directory
import System.IO
import qualified Curry_Prelude as CP

external_d_C_prim_doesFileExist :: CP.C_String -> ConstStore -> CP.C_IO CP.C_Bool
external_d_C_prim_doesFileExist s _ = fromHaskellIO1 doesFileExist s

external_d_C_prim_doesDirectoryExist :: CP.C_String -> ConstStore -> CP.C_IO CP.C_Bool
external_d_C_prim_doesDirectoryExist s _ = fromHaskellIO1 doesDirectoryExist s

external_d_C_prim_fileSize :: CP.C_String -> ConstStore -> CP.C_IO CP.C_Int
external_d_C_prim_fileSize s _ = fromHaskellIO1
  (\f -> do h <- openFile f ReadMode 
            i <- hFileSize h
            hClose h
            return i
  ) s

external_d_C_prim_getModificationTime :: CP.C_String -> ConstStore
                                      -> CP.C_IO Curry_Time.C_ClockTime
external_d_C_prim_getModificationTime s _ = fromHaskellIO1 getModificationTime s

external_d_C_getCurrentDirectory :: ConstStore -> CP.C_IO (CP.C_String)
external_d_C_getCurrentDirectory _ =
  fromHaskellIO0 System.Directory.getCurrentDirectory

external_d_C_prim_setCurrentDirectory :: CP.C_String -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_setCurrentDirectory s _ = fromHaskellIO1 setCurrentDirectory s

external_d_C_prim_getDirectoryContents :: CP.C_String -> ConstStore
                                       -> CP.C_IO (CP.OP_List (CP.C_String))
external_d_C_prim_getDirectoryContents s _ = fromHaskellIO1 getDirectoryContents s

external_d_C_prim_createDirectory :: CP.C_String -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_createDirectory s _ = fromHaskellIO1 createDirectory s

external_d_C_prim_removeFile :: CP.C_String -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_removeFile s _ = fromHaskellIO1 removeFile s

external_d_C_prim_removeDirectory :: CP.C_String -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_removeDirectory s _ = fromHaskellIO1 removeDirectory s

external_d_C_prim_renameFile :: CP.C_String -> CP.C_String
                             -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_renameFile s1 s2 _ = fromHaskellIO2 renameFile s1 s2

external_d_C_prim_renameDirectory :: CP.C_String -> CP.C_String
                                  -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_renameDirectory s1 s2 _ = fromHaskellIO2 renameDirectory s1 s2

