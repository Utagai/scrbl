module Main where

import           System.Exit

import           Args
import           Config

config :: IO Config
config = do
  eitherCfg <- getConfig
  case eitherCfg of
    Left  err -> die ("Failed to load configuration: " ++ err)
    Right cfg -> return cfg

main :: IO ()
main = do
  -- handleArgs will handle the work of exiting on bad args or on help.
  scribble <- handleArgs
  cfg      <- config
  print cfg
  print scribble
