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
  let scrblPath = base </> filepath scrbl
  pathExists <- doesFileExist scrblPath
  if pathExists then return () else writeFile scrblPath ""
