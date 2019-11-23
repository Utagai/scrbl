import ConfigTest

import Test.HUnit

import System.Exit

main :: IO ()
main = do
    results <- runTestTT configTests
    if (errors results + failures results == 0) then
        exitSuccess
    else
        die "Tests failed"