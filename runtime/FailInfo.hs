-- ---------------------------------------------------------------------------
-- This module contains the types and functions to collect information
-- about the origin of failures
-- ---------------------------------------------------------------------------
module FailInfo
  ( FailInfo (..)
  , defFailInfo
  , customFail
  , consFail
  , traceFail
  ) where

import Data.List

data FailInfo = FailInfo
  { failTrace :: [String] }
  deriving (Eq, Show)

defFailInfo :: FailInfo
defFailInfo = FailInfo []

addTrace :: String -> FailInfo -> FailInfo
addTrace t fi = fi { failTrace = t : failTrace fi }

customFail :: String -> FailInfo
customFail msg = addTrace msg defFailInfo

consFail :: String -> [String] -> FailInfo
consFail fun args = addTrace (fmtCall fun args) defFailInfo

traceFail :: String -> [String] -> FailInfo -> FailInfo
traceFail fun args fi = addTrace (fmtCall fun args) fi

fmtCall ::  String -> [String] -> String
fmtCall fun args = intercalate " " (fun : map addParens args)
  where addParens s = '(' : s ++ ")"
