{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Config
    ( Config
    , getConfig
    ) where

import Data.Text
import Control.Monad
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B

data Config =
    Config { base :: !Text
           , editor :: !Text
           , extension :: !Text
           , accept_paths :: Bool
             } deriving (Show,Generic)

instance FromJSON Config

type ConfigPath = String

getConfig :: ConfigPath -> IO (Either String Config)
getConfig path = eitherDecode <$> (B.readFile path)