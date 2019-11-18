module Main where

import Args

main :: IO ()
main = do
    handleArgs >>= print