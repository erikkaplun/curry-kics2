-- ---------------------------------------------------------------------------
-- A collection of operations for dealing with file names
--
-- @author  Bjoern Peemoeller
-- @version June 2011
-- ---------------------------------------------------------------------------
module Files
  ( -- File name modification
    withComponents, withDirectory, withBaseName, withExtension
    -- combination and segmentation
  , (</>), (<.>), splitDirectories, dropTrailingPathSeparator
    -- directory creation
  , createDirectoryIfMissing
    -- file creation
  , writeFileInDir, writeQTermFileInDir
  ) where

import Directory (createDirectory, doesDirectoryExist)
import FileGoodies
import List (last)
import ReadShowTerm (writeQTermFile)
import Utils

-- Apply functions to all parts of a file name
withComponents :: (String -> String) -- change path
               -> (String -> String) -- change base name
               -> (String -> String) -- change suffix
               -> String -> String
withComponents pf bf sf fn = pf path </> bf base <.> sf suffix
  where (path, bassfx) = splitDirectoryBaseName fn
        (base, suffix) = splitBaseName bassfx

-- Apply a function to the directory component of a file path
withDirectory :: (String -> String) -> String -> String
withDirectory f fn = withComponents f id id fn

-- Apply a function to the base name component of a file path
withBaseName :: (String -> String) -> String -> String
withBaseName f fn = withComponents id f id fn

-- Apply a function to the extension component of a file path
withExtension :: (String -> String) -> String -> String
withExtension f fn =withComponents id id f fn

-- Combine two paths
(</>) :: String -> String -> String
dir </> subdir =  dropTrailing separatorChar dir
               ++ separatorChar : dropLeading separatorChar subdir

-- Combine a file name and an extension
(<.>) :: String -> String -> String
file <.> ext =  dropTrailing suffixSeparatorChar file
             ++ suffixSeparatorChar : dropLeading suffixSeparatorChar ext

dropTrailingPathSeparator :: String -> String
dropTrailingPathSeparator fn = dropTrailing separatorChar fn

-- Split a path into the list of directories and the base name as the last
-- element
splitDirectories :: String -> [String]
splitDirectories [] = []
splitDirectories (x:xs)
  | x == separatorChar = [separatorChar] : splitDirs xs
  | otherwise          = splitDirs (x:xs)
  where
    splitDirs [] = []
    splitDirs path@(_:_)
      | null dirs = [dir]
      | otherwise = dir : splitDirs (tail dirs)
      where (dir, dirs) = break (== separatorChar) path

-- Create a directory if it does not exist. The flag signals if all missing
-- parent directories should also be created
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

-- write the 'String' into a file where the file name may contain a path.
-- The corresponding directories are created first if missing.
writeFileInDir :: String -> String -> IO ()
writeFileInDir file content = do
  createDirectoryIfMissing True $ dirName file
  writeFile file content

-- write the 'String' into a file where the file name may contain a path.
-- The corresponding directories are created first if missing.
writeQTermFileInDir :: String -> a -> IO ()
writeQTermFileInDir file content = do
  createDirectoryIfMissing True $ dirName file
  writeQTermFile file content


-- helper

dropTrailing :: Char -> String -> String
dropTrailing c = reverse . dropWhile (== c) . reverse

dropLeading :: Char -> String -> String
dropLeading c = dropWhile (== c)
