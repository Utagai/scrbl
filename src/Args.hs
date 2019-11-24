module Args
  ( handleArgs
  )
where

import           System.Environment
import           System.Exit
import           Data.List

import           Scribble

isHelpArg :: String -> Bool
isHelpArg arg = arg == "--help" || arg == "-h"

isHelpArgs :: [String] -> Bool
isHelpArgs = any isHelpArg

showHelpMsg :: IO a
showHelpMsg = do
  let separator = "\n"

  let
    usageLine
      = "Usage: scrbl [--help|-h] [--config CONFIG] [SEGMENT...] SCRIBBLE_NAME[.EXTENSION]"
  let
    description
      = "Creates notes (scribbles) at <base>/segment1/.../segmentn/SCRIBBLE_NAME.<EXTENSION>."
  let optionsHeader   = "Options:"
  let helpDescription = "\t -h, --help  \tPrints this usage information."
  let
    configDescription
      = "\t --config \tSpecifies a particular configuration location besides the default locations."
  let symbolsHeader = "Symbols:"
  let segmentDescription =
        "\t SEGMENT \tA segment of the path on top of the base directory."
  let
    scribbleDescription
      = "\t SCRIBBLE_NAME \tThe name for the scrible to create, will become the filename."
  let extensionDescription =
        "\t EXTENSION \tThe file extension of the scribble."
  let helpOutput = intercalate
        "\n"
        [ usageLine
        , description
        , separator
        , optionsHeader
        , helpDescription
        , configDescription
        , symbolsHeader
        , segmentDescription
        , scribbleDescription
        , extensionDescription
        ]
  putStrLn helpOutput
  exitSuccess

data Args =
    Args { configPath :: Maybe String
         , segments :: [String]
         } deriving (Show)

-- The following implementations for parsing the arguments could probably be
-- more efficient, but since I expect no ridiculous or nefarious invocations of
-- scrbl that involve billions of arguments, it shouldn't really matter and
-- therefore the simplicity and brevity are worth it.
getConfigPath :: [String] -> Maybe String
getConfigPath args = if configSpecified then config else Nothing
 where
  configSpecified = any (\arg -> arg == "-c" || arg == "--config") args
  config          = Just (args !! 1) -- Config comes right after

toArgs :: [String] -> Args
toArgs args = Args { configPath = getConfigPath, segments = segments }
  where segments = theRest

handleArgs :: IO [String]
handleArgs = do
  args <- getArgs
  if isEmpty args
    then die "no arguments given"
    else if isHelpArgs args then showHelpMsg else return args
  where isEmpty = null
