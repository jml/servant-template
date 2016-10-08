{-# LANGUAGE DataKinds #-}

module Lib
    ( startApp
    ) where

import Protolude hiding (Handler)

import Control.Monad.Log (WithSeverity(..), Severity(..), logMessage, renderWithSeverity, runLoggingT)
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
import qualified Prometheus as Prom
import qualified Prometheus.Metric.GHC as Prom
import Servant (serve)
import Text.PrettyPrint.Leijen.Text (text)

import API (API, server)
import Instrument (instrumentApp, requestDuration)


startApp :: IO ()
startApp = runApp 8080 app

runApp :: Port -> Application -> IO ()
runApp port application = do
  requests <- Prom.registerIO requestDuration
  void $ Prom.register Prom.ghcMetrics
  runSettings settings (middleware requests application)
  where
    settings = warpSettings port
    middleware r = RL.logStdoutDev . instrumentApp r "hello_world"

-- | Generate warp settings from config
--
-- Serve from a port and print out where we're serving from.
warpSettings :: Port -> Settings
warpSettings port =
  setBeforeMainLoop printPort (setPort port defaultSettings)
  where
    printPort = runLoggingT (logMessage (WithSeverity Informational ("Listening on :" <> show port))) (print . renderWithSeverity text)

app :: Application
app = serve (Proxy :: Proxy API) server

