{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators   #-}

module API
  ( API
  , server
  ) where

import Protolude hiding (Handler)

import Data.Aeson (FromJSON, ToJSON)
import NeatInterpolation (text)
import Servant
  ( (:>)
  , (:<|>)(..)
  , Get
  , JSON
  , MimeRender(..)
  , Handler
  , Raw
  , Server
  )
import System.Random (randomIO)

import ContentTypes (HTML)
import Instrument (metrics)


data User = User
  { _userId        :: Int
  , _userFirstName :: Text
  , _userLastName  :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON User
instance ToJSON User

data RootPage = RootPage

type API =
  Get '[HTML] RootPage
  :<|> "users" :> Get '[JSON] [User]
  :<|> "metrics" :> Raw

server :: Server API
server = pure RootPage :<|> users :<|> metrics

users :: Handler [User]
users = liftIO $ simulateNormalCode [ User 1 "Isaac" "Newton"
                                    , User 2 "Albert" "Einstein"
                                    ]
  where
    simulateNormalCode x = do
      r <- randomIO :: IO Double
      if r < 0.05
        then panic "Credit expired. Insert coin to proceed."
        else pure x


instance MimeRender HTML RootPage where
  mimeRender _ _ =
    toS [text|
         <!doctype html>
         <html>
         <head><title>hello-prometheus-haskell</title></head>
         <body>
         <h1>hello-prometheus-haskell</h1>
         <ul>
         <li><a href="/users">users</a></li>
         <li><a href="/metrics"><code>/metrics</code></a></li>
         </ul>
         <p>
         Source code at <a href="https://github.com/jml/hello-world-haskell">https://github.com/jml/hello-world-haskell</a>
         </p>
         </body>
         <html>
         |]
