{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module {{ cookiecutter.module_name}}.API
  ( API
  , server
  ) where

import Protolude hiding (Handler)

import Control.Monad.Log (MonadLog, Severity(..), WithSeverity)
import Data.Aeson (FromJSON, ToJSON)
import qualified NeatInterpolation as NI
import Servant
       ((:>), (:<|>)(..), Get, JSON, MimeRender(..), Raw, Server)

import {{ cookiecutter.module_name }}.ContentTypes (HTML)
import {{ cookiecutter.module_name }}.Instrument (metrics)
import qualified {{ cookiecutter.module_name }}.Logging as Log

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
users = do
  Log.log Informational ("Example of logging" :: LText)
  pure [User 1 "Isaac" "Newton", User 2 "Albert" "Einstein"]

instance MimeRender HTML RootPage where
  mimeRender _ _ =
    toS
      [NI.text|
         <!doctype html>
         <html>
         <head><title>{{ cookiecutter.project_name }}</title></head>
         <body>
         <h1>{{ cookiecutter.project_name }}</h1>
         <ul>
         <li><a href="/users">users</a></li>
         <li><a href="/metrics"><code>/metrics</code></a></li>
         </ul>
         <p>
         Source code at <a href="https://github.com/{{ cookiecutter.github_username }}/{{ cookiecutter.project_name }}">https://github.com/{{ cookiecutter.github_username }}/{{ cookiecutter.project_name }}/</a>
         </p>
         </body>
         <html>
         |]
