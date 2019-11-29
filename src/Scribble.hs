module Scribble
  ( Scribble
  , toScribble
  , materialize
  , filepath
  )
where

import           System.FilePath
import           System.Directory

newtype Scribble = Scribble FilePath deriving (Show)

stringsToFilePath :: [String] -> FilePath
stringsToFilePath = foldr (</>) ""

toScribble :: [String] -> Scribble
toScribble = Scribble . stringsToFilePath

filepath :: Scribble -> FilePath
filepath (Scribble fp) = fp


materialize :: Scribble -> String -> IO ()
materialize scrbl base = do
  let (dirpath, filename) = splitFileName (filepath scrbl)
  createDirectoryIfMissing True (base </> dirpath)
  writeFile (base </> filepath scrbl) ""
