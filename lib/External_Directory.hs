import System.Time
import System.Directory
import System.IO


external_d_C_prim_doesFileExist :: C_String -> C_IO C_Bool
external_d_C_prim_doesFileExist = fromHaskellIO1 doesFileExist

external_d_C_prim_doesDirectoryExist :: C_String -> C_IO C_Bool
external_d_C_prim_doesDirectoryExist = fromHaskellIO1 doesDirectoryExist

external_d_C_prim_fileSize :: C_String -> C_IO C_Int
external_d_C_prim_fileSize = fromHaskellIO1
  (\f -> do h <- openFile f ReadMode 
            i <- hFileSize h
            hClose h
            return i
  )

external_d_C_prim_getModificationTime :: C_String -> C_IO C_ClockTime
external_d_C_prim_getModificationTime = fromHaskellIO1 getModificationTime

external_d_C_getCurrentDirectory :: C_IO (C_String)
external_d_C_getCurrentDirectory =
  fromHaskellIO0 System.Directory.getCurrentDirectory

external_d_C_prim_setCurrentDirectory :: C_String -> C_IO OP_Unit
external_d_C_prim_setCurrentDirectory = fromHaskellIO1 setCurrentDirectory

external_d_C_prim_getDirectoryContents :: C_String -> C_IO (OP_List (C_String))
external_d_C_prim_getDirectoryContents = fromHaskellIO1 getDirectoryContents

external_d_C_prim_createDirectory :: C_String -> C_IO OP_Unit
external_d_C_prim_createDirectory = fromHaskellIO1 createDirectory

external_d_C_prim_removeFile :: C_String -> C_IO OP_Unit
external_d_C_prim_removeFile = fromHaskellIO1 removeFile

external_d_C_prim_removeDirectory :: C_String -> C_IO OP_Unit
external_d_C_prim_removeDirectory = fromHaskellIO1 removeDirectory

external_d_C_prim_renameFile :: C_String -> C_String -> C_IO OP_Unit
external_d_C_prim_renameFile = fromHaskellIO2 renameFile

external_d_C_prim_renameDirectory :: C_String -> C_String -> C_IO OP_Unit
external_d_C_prim_renameDirectory = fromHaskellIO2 renameDirectory

