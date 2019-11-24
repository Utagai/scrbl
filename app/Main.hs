module Main where

import           Args
import           Config

import           System.Exit

config :: IO Config
config = do
  eitherCfg <- getConfig
  case eitherCfg of
    Left  err -> die ("Failed to load configuration: " ++ err)
    Right cfg -> return cfg

main :: IO ()
main = do
  args <- handleArgs
  cfg  <- config
  print cfg
  print args
