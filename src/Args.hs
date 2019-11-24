module Args
  ( handleArgs
  )
where

import           System.Environment
import           System.Exit
import           Data.List

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
      = "Creates notes (scrbls) at <base>/segment1/.../segmentn/SCRIBBLE_NAME.<EXTENSION>."
  let optionsHeader   = "Options:"
  let helpDescription = "\t -h, --help  \tPrints this usage information."
  let
    configDescription
      = "\t --config \tSpecifies a particular configuration location besides the default locations."
  let symbolsHeader = "Symbols:"
  let segmentDescription =
        "\t SEGMENT \tA segment of the path on top of the base directory."
  let
    scrblDescription
      = "\t SCRIBBLE_NAME \tThe name for the scrbl to create, will become the filename."
  let extensionDescription = "\t EXTENSION \tThe file extension of the scrbl."
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
        , scrblDescription
        , extensionDescription
        ]
  putStrLn helpOutput
  exitSuccess

handleArgs :: IO [String]
handleArgs = do
  args <- getArgs
  if isHelpArgs args then showHelpMsg else return . tail $ args
