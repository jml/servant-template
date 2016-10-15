{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

-- | API definition for {{ cookiecutter.project_name }}.
module {{ cookiecutter.module_name}}.API
  ( API
  , RootPage(..)
  , User(User)
  ) where

import Protolude

import Data.Aeson (FromJSON, ToJSON)
import qualified NeatInterpolation as NI
import Servant.API ((:>), (:<|>)(..), Get, JSON, MimeRender(..))

import {{ cookiecutter.module_name }}.API.Internal (HTML)

data User = User
  { _userId :: Int
  , _userFirstName :: Text
  , _userLastName :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON User

instance ToJSON User

-- | {{ cookiecutter.project_name }} API definition.
type API = Get '[HTML] RootPage :<|> "users" :> Get '[JSON] [User]

-- | Represents the root page of the service.
data RootPage =
  RootPage

-- | Very simple root HTML page. Replace this with your own simple page that
-- describes your API to other developers and sysadmins.
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
