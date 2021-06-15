{-# Language BlockArguments #-}
{-# Language NamedFieldPuns #-}
{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Feeld.UrlShortener.API.StoreUrl.Types where

import Data.Text (Text)
import Data.Aeson
import Control.Lens
import GHC.Generics
import Data.Aeson.TH

data ShortenReq = ShortenReq { _url :: Text } deriving (Eq, Ord, Show)

-- sometimes I prefer to have hand-written JSON encoders/decoders
-- but on ShortenReq I show how to use TH to generate the instances
instance FromJSON ShortenReq where
  parseJSON = withObject "ShortenReq" \o -> ShortenReq <$> (o .: "url")

instance ToJSON ShortenReq where
  toJSON (ShortenReq { _url })= object [("url", String _url)]

data ShortenRes = ShortenRes { _shortened :: Text } deriving (Eq, Ord, Show, Generic)

makeLenses ''ShortenRes
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''ShortenRes
