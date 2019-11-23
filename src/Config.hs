{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Config
    ( Config (..)
    , getConfigAt
    ) where

import Data.Text
import Control.Monad
import Control.Exception
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B

data SSH =
    SSH { host :: !Text
        , port :: !Text
        , path :: !Text
        } deriving (Show,Generic,Eq)

instance FromJSON SSH

data Local =
    Local { path :: !Text
          } deriving (Show,Generic,Eq)

instance FromJSON Local

data Sync =
    Sync { ssh :: SSH
         , local :: Local
         } deriving (Show,Generic,Eq)

instance FromJSON Sync

data Config =
    Config { base :: !(Maybe Text)
           , editor :: !(Maybe Text)
           , extension :: !(Maybe Text)
           , accept_paths :: Maybe Bool
           , sync :: Maybe Sync
           } deriving (Show,Generic,Eq)

instance FromJSON Config where
    parseJSON (Object v) =
        Config <$> v .:? "base"
               <*> v .:? "editor"
               <*> v .:? "extension"
               <*> v .:? "accept_paths"
               <*> v .:? "sync"

type ConfigPath = String

getConfigAt :: ConfigPath -> IO (Either String Config)
getConfigAt path = do
    eitherBytes <- try (B.readFile path) :: IO (Either IOException B.ByteString)
    case eitherBytes of
        Left err -> return $ Left (show err)
        Right bytes -> return . eitherDecode $ bytes
