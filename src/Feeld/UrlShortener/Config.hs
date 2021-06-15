
module Feeld.UrlShortener.Config
  ( Config(..)
  , readConfig
  ) where

import Data.Time.Clock (NominalDiffTime)
import System.Environment (lookupEnv)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)

data Config = Config
  { _baseUrl :: Text -- TODO better type?
  , _port :: Int
  , _dbPoolSize :: Int -- maximum number of BD connections kept in the pool
  , _dbConnectionKeepAlive :: NominalDiffTime -- for how long will a connection be kept in the pool after it stops being used
  , _dbPostgresSettings :: ByteString -- postgres connection string
  }

-- TODO
-- this would probably read some kind of config file instead of environment variables
-- e.g. JSON, YAML, Toml, Dhall, ...
readConfig :: IO Config
readConfig = do
  -- TODO ideally add validation
  baseUrl <- pack . fromMaybe "http://localhost:8081" <$> lookupEnv "BASE_URL"
  port <- maybe 8081 read <$> lookupEnv "PORT"
  poolSize <- maybe 5 read <$> lookupEnv "DB_POOL_SIZE"
  keepAlive <- maybe 30 parseTime <$> lookupEnv "DB_CONNECTION_KEEP_ALIVE"
  pgSettings <- encodeUtf8 . pack . fromMaybe "" <$> lookupEnv "DB_PG_SETTINGS"
  pure $ Config
    { _baseUrl = baseUrl
    , _port = port
    , _dbPoolSize = poolSize
    , _dbConnectionKeepAlive = keepAlive
    , _dbPostgresSettings = pgSettings
    }
  where
    parseTime :: String -> NominalDiffTime
    parseTime str = undefined -- TODO parse stuff like 20s, 2m, 1h ...
