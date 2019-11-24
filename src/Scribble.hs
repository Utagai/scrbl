module Scribble
  ( Scribble
  , toScribble
  , materialize
  )
where

import           System.FilePath

newtype Scribble = Scribble FilePath deriving (Show)

stringsToFilePath :: [String] -> FilePath
stringsToFilePath = foldr (</>) ""

toScribble :: [String] -> Scribble
toScribble = Scribble . stringsToFilePath

filepath :: Scribble -> FilePath
filepath (Scribble fp) = fp

materialize :: Scribble -> IO ()
materialize scrbl = writeFile (filepath scrbl) ""
