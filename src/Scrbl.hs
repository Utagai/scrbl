module Scrbl
    ( Scribble
    ) where

data Scribble = Scribble [String]

toScribble :: [String] -> Scribble
toScribble segments = Scribble segments