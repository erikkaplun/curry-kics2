-- ---------------------------------------------------------------------------
-- This module contains the types and functions to collect information
-- about the origin of failures
-- ---------------------------------------------------------------------------
module FailInfo where

type Call  = (String, [String])
type Cause = String

data FailInfo = FailInfo
  { failTrace :: [Call]
  , failCause :: Cause
  } deriving (Eq, Show)

defFailInfo :: FailInfo
defFailInfo = FailInfo [] "Unspecified failure"

customFail :: String -> FailInfo
customFail msg = FailInfo [] msg

-- construct a failure due to incomplete pattern matching
consFail :: String -> String -> FailInfo
consFail fun arg = FailInfo [] msg
  where msg = fun ++ ": Pattern match failed for argument: " ++ arg

unificationFail :: String -> String -> FailInfo
unificationFail arg1 arg2 = FailInfo [] msg
  where msg = "Unification failed for terms `" ++ arg1 ++ "' and `" ++ arg2 ++ "'"

-- trace a failure in an argument position
traceFail :: String -> [String] -> FailInfo -> FailInfo
traceFail fun args fi = fi { failTrace = (fun, args) : failTrace fi }

nondetIO :: String -> FailInfo
nondetIO v = FailInfo [] msg
  where msg = "Non-determinism in IO occured for variable " ++ v
