{-# Language OverloadedStrings #-}

module Feeld.UrlShortener.API.StoreUrl.DB
  ( StoreUrlDBArgs(..)
  , storeUrlStmt
  ) where

import qualified Hasql.Statement as Hasql
import qualified Hasql.Encoders as Enc
import qualified Hasql.Decoders as Dec
import Data.Text (Text)
import Data.Int (Int64)
import Feeld.UrlShortener.Newtypes (ShortUrl(..))
import Data.Functor.Contravariant ((>$<))

data StoreUrlDBArgs
  = StoreUrlDBArgs
  { _shortUrl :: ShortUrl
  , _originalUrl :: Text
  }

storeUrlStmt :: Hasql.Statement StoreUrlDBArgs Int64
storeUrlStmt =
  let
    encoder = mconcat
      [ _originalUrl >$< Enc.param (Enc.nonNullable Enc.text)
      , _value . _shortUrl >$< Enc.param (Enc.nonNullable Enc.text)
      ]
    decoder = Dec.rowsAffected
  in Hasql.Statement "INSERT INTO urls (original_url, short_url) VALUES ($1,$2) ON CONFLICT ON CONSTRAINT urls_pkey DO NOTHING" encoder decoder True

