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
  deriving (Typeable)

instance Show CurryException where
  show (IOException     s) = "*** IOException: "     ++ s
  show (UserException   s) = "*** UserException: "   ++ s
  show (FailException   s) = "*** FailException: "   ++ s
  show (NondetException s) = "*** NondetException: " ++ s

instance Exception CurryException
