{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config(..)
  , baseString
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
import           System.Environment
import           Data.Aeson
import           GHC.Generics
import qualified Data.ByteString.Lazy          as B

data Config =
    Config { base :: Maybe String
           , editor :: Maybe String
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

-- We assume that the tilde ('~') will only appear at the beginning. This is
-- somewhat in-line with how bash handles ~, since it will only consider it
-- specially if it is at the beginning of a word, and our path should be a
-- single "word". There are some other special cases, but they come from the
-- language of bash and its intricacies, which do not apply here.
interpolateHome :: String -> IO String
interpolateHome ('~':'/':restOfBase) = (</> restOfBase) <$> getHomeDirectory
interpolateHome ('~':restOfBase) = (</> restOfBase) <$> getHomeDirectory
interpolateHome base = return base

interpolate :: Config -> IO Config
interpolate cfg = do
  interpolatedBase <- sequence (interpolateHome <$> basePath)
  interpolatedEditor <- case editorName of
                          Nothing -> getEnv "EDITOR"
                          Just e -> return e
  return cfg { base = interpolatedBase, editor = Just interpolatedEditor }
    where
      basePath = base cfg
      editorName = editor cfg

-- This is kind of hard to test fully, since it inherently relies on the
-- environment for things like `~` and files that (don't) exist on the system.
-- The only good way to really test this and so robustly is to mandate that the
-- tests be run in an isolated/controlled environment, which is quite a bit of
-- work for just a single function.
getConfig :: Maybe String -> IO (Either String Config)
getConfig customPath = interpolatedCfg
 where
  configLocations = case customPath of
    Nothing   -> defaultConfigLocations :: [IO String]
    Just path -> return path : defaultConfigLocations :: [IO String]
  configOpenAttempts =
    mapM (getConfigAt) =<< sequence configLocations :: IO [Either String Config]
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
  interpolatedCfg = sequence . ((<$>) interpolate) =<< cfg
