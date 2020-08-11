module Main where

import           System.Exit
import           System.Process
import           System.FilePath
import           Data.Maybe

import           Args
import           Config
import           Scribble

config :: Args -> IO Config
config args = do
  eitherCfg <- getConfig (configPath args)
  case eitherCfg of
    Left  err -> die ("Failed to load configuration: " ++ err)
    Right cfg -> return cfg

openEditor :: String -> FilePath -> IO ()
openEditor editorName filepath = callCommand cmdString
  where cmdString = editorName ++ " " ++ filepath

main :: IO ()
main = do
  -- handleArgs will handle the work of exiting on bad args or on help.
  args <- handleArgs
  cfg  <- config args
  let scrbl = toScribble . segments $ args
  materialize scrbl (baseString cfg)
  let scrblFilePath = baseString cfg </> filepath scrbl
  openEditor (fromMaybe "nano" (editor cfg)) scrblFilePath
