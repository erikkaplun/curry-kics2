--- --------------------------------------------------------------------------
--- A collection of operations for dealing with file names
---
--- @author  Bjoern Peemoeller
--- @version July 2012
--- --------------------------------------------------------------------------
module Files
  ( -- File name modification
    withComponents, withDirectory, withBaseName, withExtension
    -- directory creation
  , createDirectoryIfMissing
    -- file creation
  , writeFileInDir, writeQTermFileInDir
    -- file deletion
  , removeFileIfExists
  ) where

import Directory
  ( createDirectory, doesDirectoryExist
  , doesFileExist, removeFile
  )
import FilePath
  ( extSeparator, pathSeparator, (</>), (<.>)
  , splitFileName, splitExtension, splitDirectories, takeDirectory
  )
import List         (last, scanl1)
import ReadShowTerm (writeQTermFile)

--- Apply functions to all parts of a file name
withComponents :: (String -> String) -- change path
               -> (String -> String) -- change base name
               -> (String -> String) -- change suffix
               -> String -> String
withComponents pf bf sf fn = pf path </> bf base <.> sf suffix
  where (path, bassfx) = splitFileName fn
        (base, suffix) = splitExtension bassfx

--- Apply a function to the directory component of a file path
withDirectory :: (String -> String) -> String -> String
withDirectory f fn = withComponents f id id fn

--- Apply a function to the base name component of a file path
withBaseName :: (String -> String) -> String -> String
withBaseName f fn = withComponents id f id fn

--- Apply a function to the extension component of a file path
withExtension :: (String -> String) -> String -> String
withExtension f fn =withComponents id id f fn

--- Create a directory if it does not exist. The flag signals if all missing
--- parent directories should also be created
createDirectoryIfMissing :: Bool -> String -> IO ()
createDirectoryIfMissing createParents path
  | createParents = createDirs parents
  | otherwise     = createDirs [last parents]
  where
    parents = scanl1 (</>) $ splitDirectories $ path

    createDirs [] = done
    createDirs (dir:dirs) = do
      dde <- doesDirectoryExist dir
      unless dde $ createDirectory dir
      createDirs dirs

--- write the 'String' into a file where the file name may contain a path.
--- The corresponding directories are created first if missing.
writeFileInDir :: String -> String -> IO ()
writeFileInDir file content = do
  createDirectoryIfMissing True $ takeDirectory file
  writeFile file content

--- write the 'String' into a file where the file name may contain a path.
--- The corresponding directories are created first if missing.
writeQTermFileInDir :: String -> a -> IO ()
writeQTermFileInDir file content = do
  createDirectoryIfMissing True $ takeDirectory file
  writeQTermFile file content

removeFileIfExists :: String -> IO ()
removeFileIfExists file = do
  exists <- doesFileExist file
  when exists $ removeFile file
