module Main where

import Args
import Config

main :: IO ()
main = do
    eCfg <- getConfig "./rsrc/testdata/config/simple_all.json"
    case eCfg of
        Left err -> print err
        Right eCfg -> print eCfg
