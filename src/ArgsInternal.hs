module ArgsInternal
  ( isHelpArg
  , isHelpArgs
  , showHelpMsg
  , getConfigPath
  , toArgs
  , Args(..)
  )
where


import           System.Environment
import           System.Exit
import           Data.List

data Args =
    Args { configPath :: Maybe String
         , segments :: [String]
         } deriving (Show, Eq)

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

-- The following implementations for parsing the arguments could probably be
-- more efficient, but since I expect no ridiculous or nefarious invocations of
-- scrbl that involve billions of arguments, it shouldn't really matter and
-- therefore the simplicity and brevity are worth it.
getConfigPath :: [String] -> Maybe String
getConfigPath args = if configSpecified then config else Nothing
 where
  configSpecified =
    length args > 1 && (head args == "--config" || head args == "-c")
  config = Just (args !! 1) -- Config comes right after if it exists.

toArgs :: [String] -> Args
toArgs args = Args {configPath = path, segments = segments}
 where
  path     = getConfigPath args
  segments = case path of
    Nothing -> args
    _       -> drop 2 args -- If a config path was found, then the first two arguments are not segments.
