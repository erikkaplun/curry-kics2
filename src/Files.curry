--- --------------------------------------------------------------------------
--- A collection of operations for dealing with file names
---
--- @author  Bjoern Peemoeller
--- @version July 2012
--- --------------------------------------------------------------------------
module Files
  ( -- File name modification
    withComponents, withDirectory, withBaseName, withExtension
    -- file creation
  , writeFileInDir, writeQTermFileInDir
    -- file deletion
  , removeFileIfExists, (</?>)
  ) where

import Directory
  ( createDirectory, createDirectoryIfMissing, doesDirectoryExist
  , doesFileExist, removeFile
  )
import FilePath
  ( FilePath, extSeparator, pathSeparator, joinPath, (</>), (<.>)
  , splitFileName, splitExtension, splitDirectories, takeDirectory
  )
import List         (isPrefixOf, last, scanl1)
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
withExtension f fn = withComponents id id f fn

--- write the 'String' into a file where the file name may contain a path.
--- The corresponding directories are created first if missing.
writeFileInDir :: String -> String -> IO ()
writeFileInDir file content = do
  createDirectoryIfMissing True $ takeDirectory file
  writeFile file content

--- write the 'String' into a file where the file name may contain a path.
--- The corresponding directories are created first if missing.
writeQTermFileInDir :: FilePath -> a -> IO ()
writeQTermFileInDir file content = do
  createDirectoryIfMissing True $ takeDirectory file
  writeQTermFile file content

--- This operation removes the specified file only if it exists.
removeFileIfExists :: FilePath -> IO ()
removeFileIfExists file = do
  exists <- doesFileExist file
  when exists $ removeFile file

--- `dir </?> subdir` appends the only the path components of `subdir` to `dir`
--- which are not already the suffix if `dir`. Examples:
---
--- `"debug"              </?> ".curry/kics2"` -> "debug/.curry/kics2"
--- `"debug/.curry"       </?> ".curry/kics2"` -> "debug/.curry/kics2"
--- `"debug/.curry/kics2" </?> ".curry/kics2"` -> "debug/.curry/kics2"
(</?>) :: FilePath -> FilePath -> FilePath
fn </?> sfx = joinPath $ reverse $ add (reverse $ splitDirectories sfx)
                                       (reverse $ splitDirectories fn)
  where
  add []         dirs = dirs
  add dir@(d:ds) dirs
    | dir `isPrefixOf` dirs = dirs
    | otherwise             = d : add ds dirs
