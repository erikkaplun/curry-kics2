module Files where

import FileGoodies

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
