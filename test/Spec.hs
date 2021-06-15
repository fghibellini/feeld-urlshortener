{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language NamedFieldPuns #-}

import           Test.Hspec
import qualified Network.Wai.Handler.Warp         as Warp

import Feeld.UrlShortener.Lib (withPool)
import Feeld.UrlShortener.Server (application)
import Feeld.UrlShortener.API (API)
import Feeld.UrlShortener.API.StoreUrl.Types (ShortenReq(..), ShortenRes(..), shortened)
import Servant.Client hiding (responseHeaders)
import Servant
import Network.HTTP.Client       hiding (Proxy)
import Network.HTTP.Types
import Network.HTTP.Types.Status
import Data.Text (unpack)
import Control.Lens
import Data.Text.Lens (packed)
import qualified Control.Concurrent as C
import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Feeld.UrlShortener.Config (Config(..))
import Control.Monad.IO.Class (liftIO)


main :: IO ()
main = hspec spec

withApi :: IO () -> IO ()
withApi action =
  withPool (5, 10, "") $ \pool ->
    withWarpApplication (application pool config) action

  where
    withWarpApplication app action =
      bracket (liftIO $ C.forkIO $ Warp.run 8081 app)
        C.killThread
        (const action)

    config :: Config
    config = Config
      { _baseUrl = "http://localhost:8081"
      , _port = 8081
      , _dbPoolSize = 5
      , _dbConnectionKeepAlive = 10
      , _dbPostgresSettings = ""
      }

spec :: Spec
spec = around_ withApi $ do
    -- create a test client function
    let (shorten :<|> resolve) = client (Proxy :: Proxy API)
    -- create a servant-client ClientEnv
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv = mkClientEnv manager (baseUrl { baseUrlPort = 8081 })

    describe "Basic usage" $ do

        it "Can submit a url for shortening" $ do
            res <- runClientM (shorten $ ShortenReq { _url = "http://twitter.com/f_ghibellini" }) clientEnv
            case res of
              Right (ShortenRes { _shortened }) -> unpack _shortened `shouldStartWith` "http://localhost"
              Left err -> expectationFailure $ "The server returned an invalid response: " <> show err

        it "Can resolve a previously submitted url" $ do
            res <- runClientM (shorten $ ShortenReq { _url = "http://twitter.com/f_ghibellini" }) clientEnv
            let Just shortUrl = res ^? _Right . shortened . from packed
            req <- parseRequest shortUrl
            res2 <- httpNoBody (req { redirectCount = 0 })  manager
            responseStatus res2 `shouldBe` status301
            responseHeaders res2 ^.. folded . filtered (\(hName, _) -> hName == hLocation) . _2 `shouldBe` ["http://twitter.com/f_ghibellini" :: ByteString]

        it "Cannot resolve a URL that was not previously submitted" $ do
            req <- parseRequest "http://localhost:8081/abcdefg"
            res2 <- httpNoBody (req { redirectCount = 0 })  manager
            responseStatus res2 `shouldBe` status404

        it "Cannot submit an invalid url" $ do
            res <- runClientM (shorten $ ShortenReq { _url = "not a url" }) clientEnv
            case res of
              Right (ShortenRes { _shortened }) -> expectationFailure "Expected server to return 400 but received 200"
              Left err -> pure ()

        it "Two people can submit the same url and get different codes, both codes should work afterwards" $ do
            -- first person
            res1 <- runClientM (shorten $ ShortenReq { _url = "http://twitter.com/f_ghibellini" }) clientEnv
            let Just shortUrl1 = res1 ^? _Right . shortened . from packed
            -- second person
            res2 <- runClientM (shorten $ ShortenReq { _url = "http://twitter.com/f_ghibellini" }) clientEnv
            let Just shortUrl2 = res2 ^? _Right . shortened . from packed
            -- the two codes should be different
            shortUrl1 `shouldNotBe` shortUrl2
            -- check first code
            req1 <- parseRequest shortUrl1
            res3 <- httpNoBody (req1 { redirectCount = 0 })  manager
            responseStatus res3 `shouldBe` status301
            responseHeaders res3 ^.. folded . filtered (\(hName, _) -> hName == hLocation) . _2 `shouldBe` ["http://twitter.com/f_ghibellini" :: ByteString]
            -- check second code
            req2 <- parseRequest shortUrl2
            res4 <- httpNoBody (req2 { redirectCount = 0 })  manager
            responseStatus res4 `shouldBe` status301
            responseHeaders res4 ^.. folded . filtered (\(hName, _) -> hName == hLocation) . _2 `shouldBe` ["http://twitter.com/f_ghibellini" :: ByteString]

