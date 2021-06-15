{-# Language OverloadedStrings #-}

module Feeld.UrlShortener.API.ResolveUrl.DB
  ( resolveUrlStmt
  ) where

import qualified Hasql.Statement as Hasql
import qualified Hasql.Encoders as Enc
import qualified Hasql.Decoders as Dec
import Data.Text (Text)

resolveUrlStmt :: Hasql.Statement Text (Maybe Text)
resolveUrlStmt =
  let
    encoder = Enc.param (Enc.nonNullable Enc.text)
    decoder = Dec.rowMaybe (Dec.column (Dec.nonNullable Dec.text))
  in Hasql.Statement "SELECT original_url FROM urls WHERE short_url = $1" encoder decoder True

