module TestHelper
  ( module Test.HUnit
  , testCmd
  ) where

import System
import System.Process
import Test.HUnit

testCmd cmd descr arg res = TestLabel descr $ TestCase $ do
  (ec, out, err) <- readProcessWithExitCode cmd [arg] []
  assertEqual ("exitcode of " ++ call) ExitSuccess ec
  assertEqual ("stderr of " ++ call) [] err
  assertEqual ("stdout of " ++ call) res (lines out)
  where call = cmd ++ ' ' : arg