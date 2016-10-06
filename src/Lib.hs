{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    ) where

import Protolude

import Data.Aeson (FromJSON, ToJSON)
import NeatInterpolation (text)
import Network.HTTP.Media ((//), (/:))
import Network.Wai (Application)
import Network.Wai.Handler.Warp
  ( Port
  , Settings
  , defaultSettings
  , runSettings
  , setBeforeMainLoop
  , setPort
  )
import qualified Network.Wai.Middleware.RequestLogger as RL
import qualified Network.Wai.Middleware.Prometheus as Prom
import Servant
  ( (:>)
  , (:<|>)(..)
  , Accept(..)
  , Get
  , JSON
  , MimeRender(..)
  , Server
  , serve
  )


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

server :: Server API
server = pure RootPage :<|> pure users

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]


data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

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
         </body>
         <html>
         |]


startApp :: IO ()
startApp = runApp 8080 app

runApp :: Port -> Application -> IO ()
runApp port application =
  runSettings settings (middleware application)
  where
    settings = warpSettings port
    middleware = RL.logStdoutDev . Prom.prometheus Prom.def

-- | Generate warp settings from config
--
-- Serve from a port and print out where we're serving from.
warpSettings :: Port -> Settings
warpSettings port =
  setBeforeMainLoop printPort (setPort port' defaultSettings)
  where
    printPort = putText $ "hello-prometheus-haskell running at http://localhost:" <> show port' <> "/"
    port' = port


app :: Application
app = serve api server

api :: Proxy API
api = Proxy

