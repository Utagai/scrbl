module Args
  ( handleArgs
  )
where

import           System.Environment
import           System.Exit

isHelpArg :: String -> Bool
isHelpArg arg = arg == "--help" || arg == "-h"

isHelpArgs :: [String] -> Bool
isHelpArgs = any isHelpArg

showHelpMsg :: IO [String]
showHelpMsg = do
  print "scrbl is for scribbling!"
  exitSuccess

handleArgs :: IO [String]
handleArgs = do
  args <- getArgs
  if isHelpArgs args then showHelpMsg else return . tail $ args
