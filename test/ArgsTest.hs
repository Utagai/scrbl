{-# LANGUAGE OverloadedStrings #-}

module ArgsTest
  ( argsTests
  )
where

import           Test.HUnit
import           System.Directory
import           System.Environment

argTestCase :: [String] -> Test
argTestCase args = TestCase
  (do
    assertEqual "args are equal" args ["hello", "world"]
  )

argsTests = TestList
  [ TestLabel
    "noExtension"
    (argTestCase ["hello", "world"])
  ]
