module Message where

import IO (hPutStrLn, stderr)

import CompilerOpts

putErrLn :: String -> IO ()
putErrLn msg = hPutStrLn stderr msg

showStatus :: Options -> String -> IO ()
showStatus opts msg = showLevel VerbStatus opts msg

showAnalysis :: Options -> String -> IO ()
showAnalysis opts msg = showLevel VerbAnalysis opts msg

showDetail :: Options -> String -> IO ()
showDetail opts msg = showLevel VerbDetails opts msg

showLevel :: Verbosity -> Options -> String -> IO ()
showLevel level opts msg = unless (opts :> optVerbosity < level)
                                  (putStrLn msg)
