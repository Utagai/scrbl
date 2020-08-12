module Args
  ( handleArgs
  , Args(..)
  )
where

import           System.Environment
import           System.Exit
import           Data.List

import           ArgsInternal

handleArgs :: IO Args
handleArgs = do
  args <- getArgs
  if isEmpty args
    then die "no arguments given"
    else if isHelpArgs args then showHelpMsg else return . toArgs $ args
  where isEmpty = null
