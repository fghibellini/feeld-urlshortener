module Feeld.UrlShortener.Logging
  ( logError
  ) where

-- TODO
-- Here we would use a logging library with different error levels
-- ideally one that logs JSON instead of simple text

import Control.Monad.IO.Class (MonadIO, liftIO)

logError :: MonadIO m => String -> m ()
logError = liftIO . putStrLn
