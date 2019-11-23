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

noNested = cfgTestCase
  "./rsrc/testdata/config/no_sync.json"
  Config { base         = Just "./scrap/simple/"
         , editor       = Just "vim"
         , extension    = Just ".txt"
         , accept_paths = Just True
         , sync         = Nothing
         }

allFields = cfgTestCase
  "./rsrc/testdata/config/all.json"
  Config
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

partialSyncLocal = cfgTestCase
  "./rsrc/testdata/config/partial_sync_local.json"
  Config
    { base         = Just "./scrap/simple/"
    , editor       = Just "vim"
    , extension    = Just ".txt"
    , accept_paths = Just True
    , sync         = Just Sync
                       { ssh = Nothing
                       , local = Just Local { localpath = "./scrap/simple_backup/" }
                       }
    }

partialSyncSSH = cfgTestCase
  "./rsrc/testdata/config/partial_sync_ssh.json"
  Config
    { base         = Just "./scrap/simple/"
    , editor       = Just "vim"
    , extension    = Just ".txt"
    , accept_paths = Just True
    , sync         = Just Sync
                       { ssh   = Just SSH { host    = "blah"
                                          , port    = 90
                                          , sshpath = "scrap/simple/"
                                          }
                       , local = Nothing
                       }
    }

portDefaults = cfgTestCase
  "./rsrc/testdata/config/sync_ssh_missing_port.json"
  Config
    { base         = Just "./scrap/simple/"
    , editor       = Just "vim"
    , extension    = Just ".txt"
    , accept_paths = Just True
    , sync         = Just Sync
                       { ssh   = Just SSH { host    = "blah"
                                          , port    = 22
                                          , sshpath = "scrap/simple/"
                                          }
                       , local = Nothing
                       }
    }

missingHostnameErrors = cfgFailTestCase
  "./rsrc/testdata/config/sync_ssh_missing_hostname.json"
  "Error in $.sync.ssh: key \"host\" not present"

configTests = TestList
  [ TestLabel "noNested"              noNested
  , TestLabel "allFields"             allFields
  , TestLabel "partialSyncLocal"      partialSyncLocal
  , TestLabel "partialSyncSSH"        partialSyncSSH
  , TestLabel "portDefaults"          portDefaults
  , TestLabel "missingHostnameErrors" missingHostnameErrors
  ]
