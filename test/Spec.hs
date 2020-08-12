import           ConfigTest
import           ArgsTest

import           Test.HUnit

import           System.Exit

main :: IO ()
main = do
  results <- runTestTT
    (TestList [TestLabel "config" configTests, TestLabel "args" argsTests])
  if errors results + failures results == 0
    then exitSuccess
    else die "Tests failed"
