{-# LANGUAGE FlexibleContexts #-}

-- | Implementation of the {{ cookiecutter.project_name }} API.
module {{ cookiecutter.module_name }}.Server.Handlers
  ( server
  ) where

-- XXX: jml doesn't like the name "Handlers" for this, and isn't sure that it
-- should be in a submodule of Project.Server. Perhaps the code in
-- Project.Server (which is command-line processing, setting up logs &
-- monitoring, starting the HTTP server) should be in a different module.

import Protolude

import Control.Monad.Log (MonadLog, Severity(..), WithSeverity)
import Servant (Server, (:<|>)(..))

import {{ cookiecutter.module_name }}.API (API, RootPage(..), User(..))
import qualified {{ cookiecutter.module_name }}.Server.Logging as Log

-- | {{ cookiecutter.project_name }} API implementation.
server :: Server API
server = pure RootPage :<|> Log.withLogging users

-- | Example endpoint.
users
  :: (MonadIO m, MonadLog (WithSeverity LText) m)
  => m [User]
users = do
  Log.log Informational ("Example of logging" :: LText)
  pure [User 1 "Isaac" "Newton", User 2 "Albert" "Einstein"]
