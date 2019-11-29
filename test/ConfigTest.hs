{-# LANGUAGE OverloadedStrings #-}

module ConfigTest
  ( configTests
  )
where

import           Test.HUnit
import           System.Directory

import           Config

cfgTestCase :: FilePath -> Config -> Test
cfgTestCase path expectedCfg = TestCase
  (do
    eitherCfg <- getConfigAt path
    case eitherCfg of
      Left err ->
        assertFailure
          $  "encountered error when opening configuration file: "
          ++ show err
      Right cfg -> assertEqual "read-in config," expectedCfg cfg
  )

type ErrorMessage = String
cfgFailTestCase :: FilePath -> ErrorMessage -> Test
cfgFailTestCase path expectedMsg = TestCase
  (do
    eitherCfg <- getConfigAt path
    case eitherCfg of
      Left  err -> assertEqual "failure from reading config" expectedMsg err
      Right cfg -> assertFailure "expected failure, but was successful"
  )

-- To be honest, these are really just testing that the Aeson library is acting
-- as it should... but I wrote these anyways to get experience writing Haskell
-- tests.
configTests = TestList
  [ TestLabel
    "noNested"
    (cfgTestCase
      "./rsrc/testdata/config/no_sync.json"
      Config { base      = Just "./scrap/simple/"
             , editor    = Just "vim"
             , extension = Just ".txt"
             , sync      = Nothing
             }
    )
  , TestLabel
    "allFields"
    (cfgTestCase
      "./rsrc/testdata/config/all.json"
      Config
        { base      = Just "./scrap/simple/"
        , editor    = Just "vim"
        , extension = Just ".txt"
        , sync      = Just Sync
                        { ssh = Just SSH { host    = "blah"
                                         , port    = 90
                                         , sshpath = "scrap/simple/"
                                         }
                        , local = Just Local { localpath = "./scrap/simple_backup/" }
                        }
        }
    )
  , TestLabel
    "partialSyncLocal"
    (cfgTestCase
      "./rsrc/testdata/config/partial_sync_local.json"
      Config
        { base      = Just "./scrap/simple/"
        , editor    = Just "vim"
        , extension = Just ".txt"
        , sync      = Just Sync
                        { ssh = Nothing
                        , local = Just Local { localpath = "./scrap/simple_backup/" }
                        }
        }
    )
  , TestLabel
    "partialSyncSSH"
    (cfgTestCase
      "./rsrc/testdata/config/partial_sync_ssh.json"
      Config
        { base      = Just "./scrap/simple/"
        , editor    = Just "vim"
        , extension = Just ".txt"
        , sync      = Just Sync
                        { ssh   = Just SSH { host    = "blah"
                                           , port    = 90
                                           , sshpath = "scrap/simple/"
                                           }
                        , local = Nothing
                        }
        }
    )
  , TestLabel
    "portDefaults"
    (cfgTestCase
      "./rsrc/testdata/config/sync_ssh_missing_port.json"
      Config
        { base      = Just "./scrap/simple/"
        , editor    = Just "vim"
        , extension = Just ".txt"
        , sync      = Just Sync
                        { ssh   = Just SSH { host    = "blah"
                                           , port    = 22
                                           , sshpath = "scrap/simple/"
                                           }
                        , local = Nothing
                        }
        }
    )
  , TestLabel
    "missingHostnameErrors"
    (cfgFailTestCase "./rsrc/testdata/config/sync_ssh_missing_hostname.json"
                     "Error in $.sync.ssh: key \"host\" not present"
    )
  , TestLabel
    "missingConfigFile"
    (cfgFailTestCase
      "./rsrc/testdata/config/does_not_exist.json"
      "./rsrc/testdata/config/does_not_exist.json: openBinaryFile: does not exist (No such file or directory)"
    )
  , TestLabel
    "badTypedConfig"
    (cfgFailTestCase
      "./rsrc/testdata/config/badly_typed.json"
      "Error in $.extension: expected String, but encountered Boolean"
    )
  , TestLabel
    "extraField"
    (cfgTestCase
      "./rsrc/testdata/config/no_sync.json"
      Config { base      = Just "./scrap/simple/"
             , editor    = Just "vim"
             , extension = Just ".txt"
             , sync      = Nothing
             }
    )
  ]
