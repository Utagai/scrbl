module Main where

import           Args
import           Config

main :: IO ()
main = do
  eitherCfg <- getConfig
  case eitherCfg of
    Left  err -> putStrLn ("Failed to load configuration: " ++ err)
    Right cfg -> print cfg
