{-# LANGUAGE OverloadedStrings #-}

module ArgsTest
  ( argsTests
  )
where

import           Test.HUnit
import           System.Directory
import           System.Environment

import           Args
import           ArgsInternal

-- We should identify any args list as help args if --help or -h is anywhere at
-- all in the args list.
isHelpArgsTestCase :: [String] -> Bool -> Test
isHelpArgsTestCase args expectedHelpArgs =
  TestCase (isHelpArgs args @?= expectedHelpArgs)

getConfigPathTestCase :: [String] -> Maybe String -> Test
getConfigPathTestCase args expectedPath =
  TestCase (getConfigPath args @?= expectedPath)

toArgsTestCase :: [String] -> Args -> Test
toArgsTestCase args expectedArg = TestCase (toArgs args @?= expectedArg)

helpArgTests = TestList
  [ TestLabel "lone --help is detected correctly"
              (isHelpArgsTestCase ["--help"] True)
  , TestLabel "lone -h is detected correctly" (isHelpArgsTestCase ["-h"] True)
  , TestLabel "--help as last argument is still considered help args"
              (isHelpArgsTestCase ["bar", "baz", "foo", "--help"] True)
  , TestLabel "--help as first argument is still considered help args"
              (isHelpArgsTestCase ["--help", "bar", "baz", "foo"] True)
  , TestLabel
    "--help as a middle argument is still considered help args"
    (isHelpArgsTestCase ["other", "stuff", "--help", "bar", "baz", "foo"] True)
  , TestLabel "empty argument list is not a help argument"
              (isHelpArgsTestCase [] False)
  , TestLabel "single non-help argument is not considered a help argument"
              (isHelpArgsTestCase ["foo"] False)
  , TestLabel "multi non-help arguments is not considered a help argument"
              (isHelpArgsTestCase ["foo", "bar", "baz"] False)
  ]

getConfigPathTests = TestList
  [ TestLabel "no config path is retrieved as Nothing"
              (getConfigPathTestCase ["foo", "bar"] Nothing)
  , TestLabel
    "singular config path via --config is picked up"
    (getConfigPathTestCase ["--config", "/foo/bar/baz"] (Just "/foo/bar/baz"))
  , TestLabel
    "singular config path via -c is picked up"
    (getConfigPathTestCase ["-c", "/foo/bar/baz"] (Just "/foo/bar/baz"))
  , TestLabel "config flag with nothing after it is considered nothing"
              (getConfigPathTestCase ["-c"] Nothing)
  , TestLabel
    "config flag with many arguments after it is still picked up"
    (getConfigPathTestCase ["-c", "/foo/bar/baz", "barf"] (Just "/foo/bar/baz"))
  , TestLabel
    "config flag with a non-filepath is still picked up because args is for parsing not validation"
    (getConfigPathTestCase ["-c", "NOT A FILEPATH", "barf"]
                           (Just "NOT A FILEPATH")
    )
  , TestLabel "empty argument list leads to nothing"
              (getConfigPathTestCase [] Nothing)
  ]

toArgsTests = TestList
  [ TestLabel "empty argument list leads to an empty args"
              (toArgsTestCase [] Args {configPath = Nothing, segments = []})
  , TestLabel
    "specifies config path but no segments"
    (toArgsTestCase ["--config", "/foo/bar/baz"]
                    Args {configPath = Just "/foo/bar/baz", segments = []}
    )
  , TestLabel
    "specifies segments but no config path"
    (toArgsTestCase
      ["notes", "about", "stuff"]
      Args {configPath = Nothing, segments = ["notes", "about", "stuff"]}
    )
  , TestLabel
    "specifies config and segments"
    (toArgsTestCase
      ["--config", "/foo/bar/baz", "notes", "about", "stuff"]
      Args
        { configPath = Just "/foo/bar/baz"
        , segments   = ["notes", "about", "stuff"]
        }
    )
  ]

argsTests = TestList
  [ TestLabel "isHelpArgs"    helpArgTests
  , TestLabel "getConfigPath" getConfigPathTests
  ]
