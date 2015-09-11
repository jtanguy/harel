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
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TL
import qualified Database.Redis             as Redis
import           GHC.Generics
import           Network.URI                (parseURI)
import           Network.Wai.Handler.Warp
import           System.Environment
import           System.Random              (randomRIO)
import           Text.Read                  hiding (lift)

import           Servant

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

type Short = Text

type Harel = ReqBody '[FormUrlEncoded, JSON] Url :> Post '[PlainText] Short
        :<|> Capture "shortId" Short :> Get '[PlainText, JSON] Url

harel :: Redis.Connection -> Server Harel
harel conn = getId :<|> retreiveUrl
  where
    getId (Url url) = do
        short <- T.pack <$> lift genShort
        lift $ Redis.runRedis conn $ Redis.set (T.encodeUtf8 short) (T.encodeUtf8 url)
        return short
    retreiveUrl short  = do
        res <- lift $ Redis.runRedis conn $ Redis.get (T.encodeUtf8 short)
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
