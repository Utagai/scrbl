module Scrbl
  ( Scribble
  )
where

newtype Scribble = Scribble [String]

toScribble :: [String] -> Scribble
toScribble = Scribble
