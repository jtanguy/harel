{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
module Main where

import           Control.Monad              (replicateM)
import           Control.Monad.Trans
import           Control.Monad.Trans.Either
import           Data.Aeson
import qualified Data.ByteString.Char8      as B8
import           Data.Either
import           Data.Maybe
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TL
import qualified Database.Redis             as Redis
import           GHC.Generics
import           Lucid
import           Network.URI                (parseURI)
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.HTML.Lucid
import           System.Environment
import           System.Random              (randomRIO)
import           Text.Read                  hiding (lift)

alphabet = ['a'..'z']++['A'..'Z']++['0'..'9']
chooseFrom alph = (alph !!) <$> randomRIO (0,length alph -1)

genShort = replicateM 7 (chooseFrom alphabet)

newtype Url = Url { url :: Text } deriving (Show, Eq, Generic)

instance FromJSON Url
instance ToJSON Url

instance FromFormUrlEncoded Url where
  fromFormUrlEncoded = maybe (Left "No url field") ((>>= validateUrl) . Right . Url) . lookup "url"
    where
      validateUrl :: Url -> Either String Url
      validateUrl u = maybe (Left "Not a valid url") (Right . const u) . parseURI . T.unpack . url $ u

instance MimeRender PlainText Url where
  mimeRender _ = TL.encodeUtf8  . TL.fromStrict . url

newtype Short = Short { short :: Text } deriving (Show, Eq, Generic)

instance FromJSON Short
instance ToJSON Short

instance MimeRender PlainText Short where
  mimeRender _ = TL.encodeUtf8  . TL.fromStrict . short

instance FromText Short where
  fromText = Just . Short

data Home = Home
instance ToHtml Home where
  toHtml Home = doctypehtml_ $ do
    head_ $ do
      title_ "Harel - Url Shortener"
      meta_ [charset_ "utf-8"]
      link_ [rel_ "stylesheet", type_ "text/css",  href_ "http://groundfloor.neocities.org/default.css"]
    body_ $ do
      header_ $ do
        h1_ "Harel - Url Shortener"
        p_ $ do
          "Powered by "
          a_ [href_ "http://hackage.haskell.org/package/servant"] "servant"
          ", "
          a_ [href_ "http://hackage.haskell.org/package/hedis"] "hedis"
          " and "
          a_ [href_ "http://hackage.haskell.org/package/lucid"] "lucid"
      div_ [ style_ "width: 80%; margin: auto;"] $ do
        div_ [style_ "display: flex; flex-direction: row; align-items: flex-stretch"] $ do
          section_ [style_ "flex: 1 1 50%;",  class_ "input"] $ do
            h2_ "Paste your url here"
            form_ [action_ "/" , method_ "POST"] $ do
              input_ [ name_ "url" ]
              input_ [type_ "submit" , value_ "Shorten"]
          section_ [style_ "flex: 1 1 50%", class_ "output"] $ do
            h2_ "Your short id here"
            pre_ $ samp_ [id_ "short"] ""
        h2_ "You can also curl or httpie"
        kbd_ "http :8080 url=\"<your url>\""
        kbd_ "curl localhost:8080 -d url=\"<your url>\""
        h2_ "Changelog"
        p_ "Insert changelog here"
        term "script" [src_ "//code.jquery.com/jquery-1.11.3.min.js"] ""
      script_ "$(function(){ \
        \ $('form').submit(function(){ \
        \    this.preventDefault();\
        \    $.post($(this).attr('action'), $(this).serialize(), function(json) { \
        \         $('#short').html(json.short);\
        \             }, 'json');\
        \                   });\
        \                   });"
  toHtmlRaw = toHtml

type Harel = Get '[HTML] Home
        :<|> Header "Host" Text :> ReqBody '[FormUrlEncoded,JSON] Url :> Post '[PlainText, JSON] Short
        :<|> Header "Host" Text :> Capture "shortId" Short :> Get '[PlainText, JSON] Url

harel :: Redis.Connection -> Server Harel
harel conn = return Home :<|> getId :<|> retreiveUrl
  where
    getId (Just host) (Url url) = do
        s <- T.pack <$> lift genShort
        res <- lift $ Redis.runRedis conn $ Redis.hsetnx (T.encodeUtf8 host) (T.encodeUtf8 s) (T.encodeUtf8 url)
        case res of
          Right True -> return . Short $ (host <> "/" <> s)
          Right False -> getId (Just host) (Url url)
          Left _ -> left err500
    retreiveUrl (Just host) s  = do
        res <- lift $ Redis.runRedis conn $ Redis.hget (T.encodeUtf8 host) (T.encodeUtf8 . short $ s)
        case res of
          Right (Just url) -> return (Url (T.decodeUtf8 url))
          Right Nothing -> left err404
          Left _ -> left err500


main :: IO ()
main = do
  port <- maybe 8080 read <$> lookupEnv "PORT"
  connInfo <- Redis.ConnInfo <$> getEnv "REDIS_HOST"
                   <*> (Redis.PortNumber .fromInteger . read <$> getEnv "REDIS_PORT")
                   <*> (Just . B8.pack <$> getEnv "REDIS_PASSWORD")
                   <*> pure 0
                   <*> pure 10
                   <*> pure 30
  conn <- Redis.connect connInfo
  putStr "Hello world ! This is Harel, running on port "
  print port
  run port (serve (Proxy :: Proxy Harel) (harel conn))
