module Scribble
  ( Scribble
  , toScribble
  )
where

import           System.FilePath

newtype Scribble = Scribble FilePath deriving (Show)

stringsToFilePath :: [String] -> FilePath
stringsToFilePath = foldr (</>) ""

toScribble :: [String] -> Scribble
toScribble = Scribble . stringsToFilePath
