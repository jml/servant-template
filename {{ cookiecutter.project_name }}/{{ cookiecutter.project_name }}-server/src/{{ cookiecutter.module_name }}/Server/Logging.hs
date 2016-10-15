{-# LANGUAGE FlexibleContexts #-}

-- | Logging helpers for {{ cookiecutter.project_name }}.
module {{ cookiecutter.module_name }}.Server.Logging
  ( withLogging
  , log
  , fromKeyword
  , toKeyword
  ) where

import Protolude hiding (log)

import Control.Monad.Log
       (LoggingT, MonadLog, Severity(..), WithTimestamp(..), WithSeverity(..),
        logMessage, mapLogMessageM, renderWithSeverity,
        renderWithTimestamp, runLoggingT, timestamp)
import Data.Time.Format
       (defaultTimeLocale, formatTime, iso8601DateFormat)
import System.IO (hFlush)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

type LogM msg m a = LoggingT (WithSeverity msg) (LoggingT (WithTimestamp (WithSeverity msg)) m) a

-- | Take a bunch of logs with severity and print them to stdout with timestamps.
withLogging
  :: (MonadIO m, Pretty msg)
  => Severity -> LogM msg m a -> m a
withLogging severityThreshold body = runLoggingT (withTimestamps body) (printLogs severityThreshold)

withTimestamps
  :: (MonadIO m, MonadLog (WithTimestamp msg) m)
  => LoggingT msg m a -> m a
withTimestamps = mapLogMessageM timestamp

type Keyword = Text
fromKeyword :: Alternative m => Keyword -> m Severity
fromKeyword "emerg" = pure Emergency
fromKeyword "alert" = pure Alert
fromKeyword "crit" = pure Critical
fromKeyword "err" = pure Error
fromKeyword "warning" = pure Warning  -- A friend in need's a friend indeed.
fromKeyword "notice" = pure Notice
fromKeyword "info" = pure Informational
fromKeyword "debug" = pure Debug
fromKeyword _ = empty

toKeyword :: Severity -> Keyword
toKeyword Emergency = "emerg"
toKeyword Alert = "alert"
toKeyword Critical = "crit"
toKeyword Error = "err"
toKeyword Warning = "warning"
toKeyword Notice = "notice"
toKeyword Informational = "info"
toKeyword Debug = "debug"

printLogs
  :: (Pretty a, MonadIO m)
  => Severity -> WithTimestamp (WithSeverity a) -> m ()
printLogs severityThreshold message =
  when (severityThreshold >= msgSeverity (discardTimestamp message)) $ do
    print . renderWithTimestamp timeFormatter (renderWithSeverity pretty) $ message
    liftIO $ hFlush stdout
  where
    timeFormatter = formatTime defaultTimeLocale timeFormat
    timeFormat = iso8601DateFormat (Just "%H:%M:%S.%q")

-- | Convenience method to log with severity.
log
  :: MonadLog (WithSeverity a) m
  => Severity -> a -> m ()
log severity msg = logMessage (WithSeverity severity msg)
