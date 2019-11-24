{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config(..)
  , Sync(..)
  , SSH(..)
  , Local(..)
  , getConfigAt
  , getConfig
  )
where

import qualified Data.Text                     as DT
import           Control.Monad
import           Control.Exception
import           System.Directory
import           System.FilePath
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
    Config { base :: Maybe DT.Text -- TODO(may): We should probably allow this field to be interpolated and accept `~`.
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

getConfigAt :: FilePath -> IO (Either String Config)
getConfigAt path = do
  eitherBytes <- try (B.readFile path) :: IO (Either IOException B.ByteString)
  case eitherBytes of
    Left  err   -> return $ Left (show err)
    Right bytes -> return . eitherDecode $ bytes

homeDir = getHomeDirectory
dotConfigLocation = fmap (</> ".config/scrbl/scrbl.json") homeDir
dotfile = fmap (</> ".scrbl.json") homeDir
defaultConfigLocations =
  sequence [dotConfigLocation, dotfile, return "/etc/scrbl/scrbl.json"] -- TODO(may): We should also account for the config CLI argument.


-- TODO(may): This is kind of hard to test, since it inherently relies on the
-- environment for things like `~` and files that (don't) exist on the system.
-- The only good way to really test this and so robustly is to mandate that the
-- tests be run in an isolated/controlled environment, which is quite a bit of
-- work for just a single function.
getConfig :: IO (Either String Config)
getConfig = cfg
 where
   -- IO [FilePath] -> IO [Either String Config]
  configOpenAttempts =
    mapM getConfigAt =<< defaultConfigLocations :: IO [Either String Config]
  isErr =
    (\case
      Left  err -> False
      Right cfg -> True
    ) :: Either String a -> Bool
  successfulAttempts =
    fmap (filter isErr) configOpenAttempts :: IO [Either String Config]
  cfg =
    (\successes -> if null successes
        then return (Left "no viable configuration file found")
        else return (head successes)
      )
      =<< successfulAttempts
