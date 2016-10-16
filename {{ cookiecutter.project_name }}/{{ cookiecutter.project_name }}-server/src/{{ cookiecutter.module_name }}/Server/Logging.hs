{-# LANGUAGE FlexibleContexts #-}

-- | Logging helpers for {{ cookiecutter.project_name }}.
module {{ cookiecutter.module_name }}.Server.Logging
  ( LogM
  , withLogging
  , log
  , fromKeyword
  , toKeyword
  ) where

import Protolude hiding (log)

import Control.Monad.Catch (MonadMask)
import Control.Monad.Log
       (Handler, LoggingT, MonadLog, Severity(..), WithTimestamp(..),
        WithSeverity(..), defaultBatchingOptions, logMessage,
        mapLogMessageM, renderWithSeverity, renderWithTimestamp,
        runLoggingT, timestamp, withFDHandler)
import Data.Time.Format
       (defaultTimeLocale, formatTime, iso8601DateFormat)
import Text.PrettyPrint.Leijen.Text (Doc, Pretty(..))

type LogM msg m = LoggingT (WithSeverity msg) (LoggingT (WithTimestamp (WithSeverity msg)) m)

-- | Take a bunch of logs with severity and print them to stdout with timestamps.
withLogging
  :: (MonadMask m, MonadIO m, Pretty msg)
  => Severity -> LogM msg m a -> m a
withLogging severityThreshold body =
  withFDHandler defaultBatchingOptions stdout 0.4 80 $
  \stdoutHandler ->
     runLoggingT
       (withTimestamps body)
       (printLogs severityThreshold stdoutHandler)

withTimestamps
  :: (MonadIO m, MonadLog (WithTimestamp msg) m)
  => LoggingT msg m a -> m a
withTimestamps = mapLogMessageM timestamp

type Keyword = Text

fromKeyword
  :: Alternative m
  => Keyword -> m Severity
fromKeyword "emerg" = pure Emergency
fromKeyword "alert" = pure Alert
fromKeyword "crit" = pure Critical
fromKeyword "err" = pure Error
fromKeyword "error" = pure Error
fromKeyword "warning" = pure Warning -- A friend in need's a friend indeed.
fromKeyword "warn" = pure Warning
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
  => Severity -> Handler m Doc -> WithTimestamp (WithSeverity a) -> m ()
printLogs severityThreshold handler message =
  when (severityThreshold >= msgSeverity (discardTimestamp message)) $
  handler . renderWithTimestamp timeFormatter (renderWithSeverity pretty) $ message
  where
    timeFormatter = formatTime defaultTimeLocale timeFormat
    timeFormat = iso8601DateFormat (Just "%H:%M:%S.%q")

-- | Convenience method to log with severity.
log
  :: MonadLog (WithSeverity a) m
  => Severity -> a -> m ()
log severity msg = logMessage (WithSeverity severity msg)
