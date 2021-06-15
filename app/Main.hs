module Main where

import Feeld.UrlShortener.Server
import Feeld.UrlShortener.Config (readConfig)

main :: IO ()
main = do
  config <- readConfig
  runServer config
