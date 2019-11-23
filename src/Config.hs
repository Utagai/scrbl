{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config(..)
  , Sync(..)
  , SSH(..)
  , Local(..)
  , getConfigAt
  )
where

import qualified Data.Text                     as DT
import           Control.Monad
import           Control.Exception
import           Data.Aeson
import           GHC.Generics
import qualified Data.ByteString.Lazy          as B

data SSH =
    SSH { host :: DT.Text
        , port :: Int
        , sshpath :: DT.Text
        } deriving (Show,Generic,Eq)

instance FromJSON SSH where
  parseJSON (Object v) =
    SSH <$> v .: "host" <*> v .:? "port" .!= 22 <*> v .:? "path" .!= "."

newtype Local =
    Local { localpath :: DT.Text
          } deriving (Show,Generic,Eq)

instance FromJSON Local where
  parseJSON (Object v) = Local <$> v .: "path"

data Sync =
    Sync { ssh :: Maybe SSH
         , local :: Maybe Local
         } deriving (Show,Generic,Eq)

instance FromJSON Sync

data Config =
    Config { base :: Maybe DT.Text
           , editor :: Maybe DT.Text
           , extension :: Maybe DT.Text
           , accept_paths :: Maybe Bool
           , sync :: Maybe Sync
           } deriving (Show,Generic,Eq)

instance FromJSON Config where
  parseJSON (Object v) =
    Config
      <$> v
      .:? "base"
      <*> v
      .:? "editor"
      <*> v
      .:? "extension"
      <*> v
      .:? "accept_paths"
      <*> v
      .:? "sync"

type ConfigPath = String

getConfigAt :: ConfigPath -> IO (Either String Config)
getConfigAt path = do
  eitherBytes <- try (B.readFile path) :: IO (Either IOException B.ByteString)
  case eitherBytes of
    Left  err   -> return $ Left (show err)
    Right bytes -> return . eitherDecode $ bytes

defaultConfigLocations =
  ["~/.config/scrbl/scrbl.json", "~/.scrbl.json", "/etc/scrbl/scrbl.json"]

getConfig :: IO (Either String Config)
getConfig = head <$> successfulAttempts
 where
  configOpenAttempts =
    mapM getConfigAt defaultConfigLocations :: IO [Either String Config]
  isErr =
    (\case
      Left  err -> False
      Right cfg -> True
    ) :: Either String a -> Bool
  successfulAttempts =
    fmap (filter isErr) configOpenAttempts :: IO [Either String Config]
