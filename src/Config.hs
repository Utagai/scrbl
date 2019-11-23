{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Config
    ( Config
    , getConfig
    ) where

import Data.Text
import Control.Monad
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B

data SSH =
    SSH { host :: !Text
        , port :: !Text
        , path :: !Text
        } deriving (Show,Generic)

instance FromJSON SSH

data Local =
    Local { path :: !Text
          } deriving (Show,Generic)

instance FromJSON Local

data Sync =
    Sync { ssh :: SSH
         , local :: Local
         } deriving (Show,Generic)

instance FromJSON Sync

data Config =
    Config { base :: !(Maybe Text)
           , editor :: !(Maybe Text)
           , extension :: !(Maybe Text)
           , accept_paths :: Maybe Bool
           , sync :: Maybe Sync
           } deriving (Show,Generic)

instance FromJSON Config where
    parseJSON (Object v) =
        Config <$> v .:? "base"
               <*> v .:? "editor"
               <*> v .:? "extension"
               <*> v .:? "accept_paths"
               <*> v .:? "sync"

type ConfigPath = String

getConfig :: ConfigPath -> IO (Either String Config)
getConfig path = eitherDecode <$> B.readFile path