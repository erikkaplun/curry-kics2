module Files where

import Directory (createDirectory, doesDirectoryExist)
import FileGoodies
import List (last)
import ReadShowTerm (writeQTermFile)
import Utils

-- file utils
withBaseName :: (String -> String) -> String -> String
withBaseName f fn = dirName fn
  ++ separatorChar : (f $ stripSuffix $ baseName fn)
  ++ suffixSeparatorChar : fileSuffix fn

withExtension :: (String -> String) -> String -> String
withExtension f fn = stripSuffix fn ++ f (fileSuffix fn)

withPath :: (String -> String) -> String -> String
withPath f fn = f path ++ base where (path, base) = splitDirectoryBaseName fn

(</>) :: String -> String -> String
dir </> subdir = dropTrailingPathSeparator dir
                 ++ separatorChar : dropWhile (== separatorChar) subdir

dropTrailingPathSeparator :: String -> String
dropTrailingPathSeparator = reverse . dropWhile (== separatorChar) . reverse

(<.>) :: String -> String -> String
file <.> extension = stripSuffix file ++ suffixSeparatorChar : extension

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
