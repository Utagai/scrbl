{-# LANGUAGE OverloadedStrings #-}

module ConfigTest
  ( foo
  , configTests
  )
where

import           Test.HUnit
import           System.Directory

import           Config

foo :: Int -> Int
foo = (+ 2)

fooTest = TestCase (assertEqual "foo 3," 5 (foo 3))
simpleNoNested = TestCase
  (do
    eitherCfg <- getConfigAt "./rsrc/testdata/config/no_sync.json"
    case eitherCfg of
      Left err ->
        assertFailure
          $  "encountered error when opening configuration file: "
          ++ show err
      Right cfg -> assertEqual "read-in config," expectedCfg cfg
  )
 where
  expectedCfg = Config { base         = Just "./scrap/simple/"
                       , editor       = Just "vim"
                       , extension    = Just ".txt"
                       , accept_paths = Just True
                       , sync         = Nothing
                       }

simpleAll = TestCase
  (do
    eitherCfg <- getConfigAt "./rsrc/testdata/config/all.json"
    case eitherCfg of
      Left err ->
        assertFailure
          $  "encountered error when opening configuration file: "
          ++ show err
      Right cfg -> assertEqual "read-in config," expectedCfg cfg
  )
 where
  expectedCfg = Config
    { base         = Just "./scrap/simple/"
    , editor       = Just "vim"
    , extension    = Just ".txt"
    , accept_paths = Just True
    , sync         = Just Sync
                       { ssh = Just SSH { host    = "blah"
                                        , port    = 90
                                        , sshpath = "scrap/simple/"
                                        }
                       , local = Just Local { localpath = "./scrap/simple_backup/" }
                       }
    }

configTests = TestList
  [ TestLabel "fooTest"        fooTest
  , TestLabel "simpleNoNested" simpleNoNested
  , TestLabel "simpleAll"      simpleAll
  ]
