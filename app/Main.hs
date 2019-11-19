module Main where

import Args

main :: IO ()
main = handleArgs >>= print
