import           ConfigTest
import           ArgsTest

import           Test.HUnit

import           System.Exit

runTests :: String -> Test -> IO ()
runTests label tests = do
  results <- runTestTT tests
  if errors results + failures results == 0
    then return ()
    else die ("Tests for " ++ label ++ "failed")


main :: IO ()
main = do
  runTests "config" configTests
  runTests "args"   argsTests
