{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

-- | API definition for {{ cookiecutter.project_name }}.
module {{ cookiecutter.module_name}}.API
  ( API
  , api
  , RootPage(..)
  , User(User)
  , Users(..)
  ) where

import Protolude

import Data.Aeson
       (FromJSON(..), ToJSON(..), Value(..), object, (.=), (.:))
import Data.Aeson.Types (typeMismatch)
import qualified NeatInterpolation as NI
import Servant.API ((:>), (:<|>)(..), Get, JSON, MimeRender(..))

import {{ cookiecutter.module_name }}.API.Internal (HTML)

-- | {{ cookiecutter.project_name }} API definition.
type API = Get '[HTML] RootPage :<|> "users" :> Get '[JSON] Users

-- | Value-level representation of API.
api :: Proxy API
api = Proxy

-- | Example object. Replace this with something relevant to your app.
data User = User
  { _userId :: Int
  , _userFirstName :: Text
  , _userLastName :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON User

instance ToJSON User

-- | Represents a list of users.
--
-- We have a newtype so we can be sure to return a JSON object.
newtype Users =
  Users [User]
  deriving (Eq, Show, Generic)

instance FromJSON Users where
  parseJSON (Object v) = Users <$> v .: "users"
  parseJSON x = typeMismatch "Users" x

instance ToJSON Users where
  toJSON (Users users) = object ["users" .= toJSON users]

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
