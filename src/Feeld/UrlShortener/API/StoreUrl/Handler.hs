{-# Language NamedFieldPuns #-}
{-# Language OverloadedStrings #-}

module Feeld.UrlShortener.API.StoreUrl.Handler
  ( handler
  ) where

import Servant

import Feeld.UrlShortener.API.StoreUrl.DB (StoreUrlDBArgs(..), storeUrlStmt)
import Feeld.UrlShortener.API.StoreUrl.Types (ShortenRes(..), ShortenReq(..))
import Feeld.UrlShortener.Newtypes (ShortUrl(_value))
import Feeld.UrlShortener.ShortUrl (genRandomShortUrl)
import Hasql.Pool (Pool)
import qualified Hasql.Pool as Pool
import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as Tx
import Servant
import Data.Text (Text, unpack)
import Control.Monad.IO.Class (liftIO)
import Text.URI as URI
import Text.Megaparsec as MP
import Data.Void (Void)
import Feeld.UrlShortener.Handler (throwUnexpectedServerError)
import Feeld.UrlShortener.Logging (logError)
import Feeld.UrlShortener.Config (Config(Config, _baseUrl))

isValidURI :: Text -> Bool
isValidURI = either (const False) (const True) . MP.parse urlParser "<input>"
  where
    urlParser :: MP.Parsec Void Text URI.URI
    urlParser = URI.parser <* MP.eof

handler :: Pool -> Config -> ShortenReq -> Handler ShortenRes
handler dbPool Config { _baseUrl } ShortenReq { _url } = do
    if isValidURI _url
    then do
      storeUrl
    else do
      throwError $ err400 { errBody = "`url` must be a valid URL" }
  where
    -- the randomly generated short URL could already be in the DB which would cause a conflict on write
    -- on such occasion we simply try generating a new one
    storeUrl :: Handler ShortenRes
    storeUrl = go 1
      where
        go :: Int -> Handler ShortenRes
        go iterationNr | iterationNr > 5 = do
          logError $ "Could not insert new url even afer " <> show (iterationNr - 1) <> " attempts"
          throwUnexpectedServerError
        go iterationNr | otherwise = do
          res <- storeUrlAttempt
          case res of
            Left () -> go (iterationNr + 1)
            Right x -> pure x

    storeUrlAttempt :: Handler (Either () ShortenRes)
    storeUrlAttempt = do
      shortUrl <- liftIO genRandomShortUrl
      dbRes <- liftIO $ Pool.use dbPool $ Tx.transaction Tx.ReadCommitted Tx.Write $ Tx.statement (StoreUrlDBArgs { _shortUrl = shortUrl, _originalUrl = _url }) storeUrlStmt
      case dbRes of
        Right 0 -> pure $ Left () -- DB conflict on primary key
        Right 1 -> pure $ Right $ ShortenRes { _shortened = _baseUrl <> "/" <> _value shortUrl }
        Right affectedRows -> do
          logError $ "Unexpected number of affected rows: " <> show affectedRows <> ", this should never happen!"
          throwUnexpectedServerError
        Left err -> do
          logError $ "DB error: " <> show err
          throwUnexpectedServerError

