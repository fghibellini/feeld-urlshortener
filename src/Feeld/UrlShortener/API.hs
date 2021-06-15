{-# Language DataKinds #-}
{-# Language TypeOperators #-}

module Feeld.UrlShortener.API
  ( API
  , server
  ) where

import qualified Feeld.UrlShortener.API.StoreUrl.Handler as StoreUrlHandler
import Feeld.UrlShortener.API.StoreUrl.Types (ShortenReq, ShortenRes)
import qualified Feeld.UrlShortener.API.ResolveUrl.Handler as ResolveUrlHandler
import Feeld.UrlShortener.Config (Config)

import Servant
import Hasql.Pool (Pool)
import Data.Text (Text)

type API
  = "shorten" :> ReqBody '[JSON] ShortenReq :> Post '[JSON] ShortenRes
  :<|> Capture "shortUrl" Text :> GetNoContent -- supports only GET (which I would assume is typical for URL shorteners)

server :: Pool -> Config -> Server API
server dbPool cfg = StoreUrlHandler.handler dbPool cfg :<|> ResolveUrlHandler.handler dbPool cfg
