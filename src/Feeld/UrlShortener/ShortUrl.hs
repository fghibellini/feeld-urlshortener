module Feeld.UrlShortener.ShortUrl
  ( genRandomShortUrl
  ) where

import Feeld.UrlShortener.Newtypes (ShortUrl(..))
import Control.Monad (replicateM)
import System.Random (randomRIO)
import Data.Text (Text, pack)

-- TODO put more thought into the random URL generation
-- the size and randomness should be good enough to be
-- able to generate a shortUrl and expect it to be unique (just like with UUIDs).
-- The actual uniqueness is tested by the DB (see Feeld.UrlShortener.API.StoreUrl.Handler).
genRandomShortUrl :: IO ShortUrl
genRandomShortUrl = do
  indexes <- replicateM 20 $ randomRIO (0, length alphabet - 1)
  pure $ ShortUrl $ pack ((\i -> alphabet !! i) <$> indexes)
  where
    alphabet = ['0'..'9'] <> ['a'..'z']

