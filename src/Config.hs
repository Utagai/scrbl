{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config(..)
  , baseString
  , SSH(..)
  , Local(..)
  , getConfigAt
  , getConfig
  )
where

import qualified Data.Text                     as DT
import           Data.Maybe
import           Control.Monad
import           Control.Exception
import           System.Directory
import           System.FilePath
import           Data.Aeson
import           GHC.Generics
import qualified Data.ByteString.Lazy          as B

data SSH =
    SSH { host :: String -- TODO(may): Do we actually need to use Text?
        , port :: Int
        , sshpath :: String
        } deriving (Show,Generic,Eq)

instance FromJSON SSH where
  parseJSON (Object v) =
    SSH <$> v .: "host" <*> v .:? "port" .!= 22 <*> v .:? "path" .!= "."

newtype Local =
    Local { localpath :: String
          } deriving (Show,Generic,Eq)

instance FromJSON Local where
  parseJSON (Object v) = Local <$> v .: "path"

data Config =
    Config { base :: Maybe String -- TODO(may): We should probably allow this field to be interpolated and accept `~`.
           , editor :: Maybe String -- TODO(may): We should default to $EDITOR.
           , extension :: Maybe String
           } deriving (Show,Generic,Eq)

baseString :: Config -> String
baseString cfg = fromMaybe "~/Documents/scrbl/" (base cfg)

instance FromJSON Config where
  parseJSON (Object v) =
    Config
      <$> v
      .:? "base"
      <*> v
      .:? "editor"
      <*> v
      .:? "extension"

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
  [dotConfigLocation, dotfile, return "/etc/scrbl/scrbl.json"] -- TODO(may): We should also account for the config CLI argument.


-- TODO(may): This is kind of hard to test fully, since it inherently relies on
-- the environment for things like `~` and files that (don't) exist on the
-- system.  The only good way to really test this and so robustly is to mandate
-- that the tests be run in an isolated/controlled environment, which is quite
-- a bit of work for just a single function.
getConfig :: Maybe String -> IO (Either String Config)
getConfig customPath = cfg
 where
  configLocations = case customPath of
    Nothing   -> defaultConfigLocations
    Just path -> return path : defaultConfigLocations
  configOpenAttempts =
    mapM getConfigAt =<< sequence configLocations :: IO [Either String Config]
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
