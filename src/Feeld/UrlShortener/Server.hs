{-# Language DeriveGeneric #-}
{-# Language DeriveAnyClass #-}
{-# Language OverloadedStrings #-}
{-# Language NamedFieldPuns #-}

module Feeld.UrlShortener.Server
    ( runServer
    , application
    ) where

import Servant
import Data.Text (Text, pack)
import Data.Int (Int64)
import Network.Wai
import Network.Wai.Handler.Warp as Warp
import GHC.Generics
import Data.Aeson
import qualified Hasql.Connection as Hasql
import qualified Hasql.Statement as Hasql
import qualified Hasql.Encoders as Enc
import qualified Hasql.Decoders as Dec
import Hasql.Pool (Pool)
import qualified Hasql.Pool as Pool
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)

import Feeld.UrlShortener.API (API, server)
import Feeld.UrlShortener.Lib (withPool)
import Feeld.UrlShortener.Config (Config(..))


-- TODO
-- if we start to have more resources (e.g. caches, http clients, ...) to pass around we might create an Env type
-- that would hold all the resources and the config and probably use `ReaderT Env Handler` for our handlers
application :: Pool -> Config -> Application
application dbPool cfg = serve (Proxy :: Proxy API) (server dbPool cfg)

runServer :: Config -> IO ()
runServer cfg@Config { _dbPoolSize, _dbConnectionKeepAlive, _dbPostgresSettings, _port } =
    withPool poolSettings $ \pool -> Warp.run _port $ application pool cfg
  where
    poolSettings :: Pool.Settings
    poolSettings = (_dbPoolSize, _dbConnectionKeepAlive, _dbPostgresSettings)
