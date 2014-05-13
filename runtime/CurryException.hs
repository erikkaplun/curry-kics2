{-# LANGUAGE DeriveDataTypeable #-}
module CurryException where

import Control.Exception (Exception (..), throw)
import Data.Typeable     (Typeable)

throwFail :: String -> a
throwFail = throw . FailException

throwNondet :: String -> a
throwNondet = throw . NondetException

data CurryException
  = IOException       String
  | UserException     String
  | FailException     String
  | NondetException   String
  deriving (Show, Typeable)

instance Exception CurryException
