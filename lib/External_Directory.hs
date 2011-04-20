import System.Time
import System.Directory
import System.IO
import qualified Curry_Prelude as CP

external_d_C_prim_doesFileExist :: CP.C_String -> CP.C_IO CP.C_Bool
external_d_C_prim_doesFileExist = fromHaskellIO1 doesFileExist

external_d_C_prim_doesDirectoryExist :: CP.C_String -> CP.C_IO CP.C_Bool
external_d_C_prim_doesDirectoryExist = fromHaskellIO1 doesDirectoryExist

external_d_C_prim_fileSize :: CP.C_String -> CP.C_IO CP.C_Int
external_d_C_prim_fileSize = fromHaskellIO1
  (\f -> do h <- openFile f ReadMode 
            i <- hFileSize h
            hClose h
            return i
  )

external_d_C_prim_getModificationTime :: CP.C_String
                                      -> CP.C_IO Curry_Time.C_ClockTime
external_d_C_prim_getModificationTime = fromHaskellIO1 getModificationTime

external_d_C_getCurrentDirectory :: CP.C_IO (CP.C_String)
external_d_C_getCurrentDirectory =
  fromHaskellIO0 System.Directory.getCurrentDirectory

external_d_C_prim_setCurrentDirectory :: CP.C_String -> CP.C_IO CP.OP_Unit
external_d_C_prim_setCurrentDirectory = fromHaskellIO1 setCurrentDirectory

external_d_C_prim_getDirectoryContents :: CP.C_String
                                       -> CP.C_IO (CP.OP_List (CP.C_String))
external_d_C_prim_getDirectoryContents = fromHaskellIO1 getDirectoryContents

external_d_C_prim_createDirectory :: CP.C_String -> CP.C_IO CP.OP_Unit
external_d_C_prim_createDirectory = fromHaskellIO1 createDirectory

external_d_C_prim_removeFile :: CP.C_String -> CP.C_IO CP.OP_Unit
external_d_C_prim_removeFile = fromHaskellIO1 removeFile

external_d_C_prim_removeDirectory :: CP.C_String -> CP.C_IO CP.OP_Unit
external_d_C_prim_removeDirectory = fromHaskellIO1 removeDirectory

external_d_C_prim_renameFile :: CP.C_String -> CP.C_String
                             -> CP.C_IO CP.OP_Unit
external_d_C_prim_renameFile = fromHaskellIO2 renameFile

external_d_C_prim_renameDirectory :: CP.C_String -> CP.C_String
                                  -> CP.C_IO CP.OP_Unit
external_d_C_prim_renameDirectory = fromHaskellIO2 renameDirectory

