{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module API
  ( API
  , server
  ) where

import Protolude hiding (Handler)

import Control.Monad.Log (MonadLog, Severity(..), WithSeverity)
import Data.Aeson (FromJSON, ToJSON)
import qualified NeatInterpolation as NI
import Servant
       ((:>), (:<|>)(..), Get, JSON, MimeRender(..), Raw, Server)
import System.Random (randomIO)

import ContentTypes (HTML)
import Instrument (metrics)
import qualified Logging as Log

data User = User
  { _userId :: Int
  , _userFirstName :: Text
  , _userLastName :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON User

instance ToJSON User

data RootPage =
  RootPage

type API = Get '[HTML] RootPage :<|> "users" :> Get '[JSON] [User] :<|> "metrics" :> Raw

server :: Server API
server = pure RootPage :<|> Log.withLogging users :<|> metrics

users
  :: (MonadIO m, MonadLog (WithSeverity LText) m)
  => m [User]
users = simulateNormalCode [User 1 "Isaac" "Newton", User 2 "Albert" "Einstein"]
  where
    simulateNormalCode x = do
      r <- liftIO randomIO
      if r < (0.05 :: Double)
        then do
          Log.log Error ("Need more money" :: LText)
          panic "Credit expired. Insert coin to proceed."
        else pure x

instance MimeRender HTML RootPage where
  mimeRender _ _ =
    toS
      [NI.text|
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
