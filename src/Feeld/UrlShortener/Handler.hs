{-# Language OverloadedStrings #-}

module Feeld.UrlShortener.Handler
  ( throwUnexpectedServerError
  ) where

import Servant

-- TODO we probably want to send a JSON encoded response
throwUnexpectedServerError :: Handler a
throwUnexpectedServerError = throwError $ err500 { errBody = "Something went wrong" }
