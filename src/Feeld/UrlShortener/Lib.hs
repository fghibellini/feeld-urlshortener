{-# Language BlockArguments #-}

module Feeld.UrlShortener.Lib
    ( withPool
    ) where

import Hasql.Pool (Pool)
import qualified Hasql.Pool as Pool
import Control.Exception (bracket)

withPool :: Pool.Settings -> (Pool -> IO a) -> IO a
withPool poolSettings = bracket (Pool.acquire poolSettings) Pool.release

