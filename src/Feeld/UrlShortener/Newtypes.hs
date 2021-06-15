module Feeld.UrlShortener.Newtypes
  ( ShortUrl(..)
  ) where

import Data.Text (Text)

newtype ShortUrl = ShortUrl { _value :: Text }
  deriving (Eq, Ord, Show)
