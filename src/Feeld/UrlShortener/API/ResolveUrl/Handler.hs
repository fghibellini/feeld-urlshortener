{-# Language OverloadedStrings #-}

module Feeld.UrlShortener.API.ResolveUrl.Handler
  ( handler
  ) where

import Hasql.Pool (Pool)
import qualified Hasql.Pool as Pool
import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as Tx
import Servant
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Encoding (encodeUtf8)
import Feeld.UrlShortener.Handler (throwUnexpectedServerError)
import Feeld.UrlShortener.Logging (logError)

import Feeld.UrlShortener.API.ResolveUrl.DB (resolveUrlStmt)
import Feeld.UrlShortener.Config (Config)


handler :: Pool -> Config -> Text -> Handler NoContent
handler dbPool cfg x = do
  dbRes <- liftIO $ Pool.use dbPool $ Tx.transaction Tx.ReadCommitted Tx.Read $ Tx.statement x resolveUrlStmt
  case dbRes of
    Right (Just x) -> throwError $ err301 { errHeaders = [("Location", encodeUtf8 x)] }
    Right Nothing -> throwError $ err404 { errBody = "URL not found!" }
    Left err -> do
      logError $ "DB error: " <> show err
      throwUnexpectedServerError
