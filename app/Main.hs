module Main where

import Args
import Config

main :: IO ()
main = do
    eitherCfg <- getConfigAt "./rsrc/testdata/config/simple_all.json"
    case eitherCfg of
        Left err -> putStrLn ("Failed to load configuration " ++ err)
        Right cfg -> print cfg
