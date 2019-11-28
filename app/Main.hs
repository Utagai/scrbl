module Main where

import           System.Exit

import           Args
import           Config

config :: Args -> IO Config
config args = do
  eitherCfg <- getConfig (configPath args)
  case eitherCfg of
    Left  err -> die ("Failed to load configuration: " ++ err)
    Right cfg -> return cfg

main :: IO ()
main = do
  -- handleArgs will handle the work of exiting on bad args or on help.
  args <- handleArgs
  cfg  <- config args
  print cfg
  print args
