-- This is a "benchmark" just to show some information
-- about the Curry distribution

import Distribution

runtimeSystem =
  putStrLn $ curryRuntime ++
  " Vers. " ++ show curryRuntimeMajorVersion ++ "." ++ show curryRuntimeMinorVersion

