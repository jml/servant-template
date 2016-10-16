{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

-- | Serve the API as an HTTP server.
module {{ cookiecutter.module_name }}.Server
  ( server
  , startApp
  ) where

import Protolude

import Control.Monad.Log (Severity(..))
import qualified Data.List as List
import GHC.Stats (getGCStatsEnabled)
import Network.Wai.Handler.Warp
       (Port, Settings, defaultSettings, runSettings, setBeforeMainLoop,
        setPort)
import qualified Network.Wai.Middleware.RequestLogger as RL
import Options.Applicative
       (ParserInfo, auto, eitherReader, execParser, fullDesc, header,
        help, helper, info, long, metavar, option, progDesc, switch, value)
import qualified Prometheus as Prom
import qualified Prometheus.Metric.GHC as Prom
import Servant (serve)
import Text.PrettyPrint.Leijen.Text (int, text)

import {{ cookiecutter.module_name }}.API (api)
import {{ cookiecutter.module_name }}.Server.Handlers (server)
import {{ cookiecutter.module_name }}.Server.Instrument
       (defaultPrometheusSettings, prometheus, requestDuration)
import qualified {{ cookiecutter.module_name }}.Server.Logging as Log

-- | Configuration for the application.
data Config = Config
  { port :: Port
  , accessLogs :: AccessLogs
  , logLevel :: Severity
  , enableGhcMetrics :: Bool
  } deriving (Show)

-- | What level of access logs to show.
data AccessLogs
  = Disabled -- ^ Don't show access logs.
  | Enabled -- ^ Show Apache-style access logs.
  | DevMode -- ^ Show detailed, colorful access logs. Not suitable in production.
  deriving (Eq, Show)

-- | Run the service.
startApp :: IO ()
startApp = runApp =<< execParser options

options :: ParserInfo Config
options = info (helper <*> parser) description
  where
    parser =
      Config <$>
      option auto (fold [long "port", metavar "PORT", help "Port to listen on"]) <*>
      option
        (eitherReader parseAccessLogs)
        (fold
           [long "access-logs", help "How to log HTTP access", value Disabled]) <*>
      option
        (eitherReader
           (maybe (throwError (toS invalidLogLevel)) pure . Log.fromKeyword . toS))
        (fold
           [ long "log-level"
           , help "Minimum severity for log messages"
           , value Informational
           ]) <*>
      switch
        (fold
           [ long "ghc-metrics"
           , help "Export GHC metrics. Requires running with +RTS."
           ])
    invalidLogLevel = "Log level must be one of: " <> allLogLevels
    allLogLevels = fold . List.intersperse "," . map Log.toKeyword $ enumFrom minBound
    parseAccessLogs "none" = pure Disabled
    parseAccessLogs "basic" = pure Enabled
    parseAccessLogs "dev" = pure DevMode
    parseAccessLogs _ = throwError "One of 'none', 'basic', or 'dev'"
    description =
      fold
        [ fullDesc
        , progDesc "{{ cookiecutter.synopsis }}"
        , header "{{ cookiecutter.project_name }} - TODO fill this in"
        ]

runApp :: Config -> IO ()
runApp config@Config {..} = do
  requests <- Prom.registerIO requestDuration
  when enableGhcMetrics $
    do statsEnabled <- getGCStatsEnabled
       unless statsEnabled $
         Log.withLogging logLevel $
         Log.log
           Warning
           (text
              "Exporting GHC metrics but GC stats not enabled. Re-run with +RTS -T.")
       void $ Prom.register Prom.ghcMetrics
  runSettings settings (middleware requests)
  where
    settings = warpSettings config
    middleware r =
      logging . prometheus defaultPrometheusSettings r "{{ cookiecutter.metric_namespace }}" $ app
    logging =
      case accessLogs of
        Disabled -> identity
        Enabled -> RL.logStdout
        DevMode -> RL.logStdoutDev
    app = serve api (server logLevel)

-- | Generate warp settings from config
--
-- Serve from a port and print out where we're serving from.
warpSettings :: Config -> Settings
warpSettings Config {..} =
  setBeforeMainLoop
    (Log.withLogging logLevel printPort)
    (setPort port defaultSettings)
  where
    printPort = Log.log Informational (text "Listening on :" `mappend` int port)
