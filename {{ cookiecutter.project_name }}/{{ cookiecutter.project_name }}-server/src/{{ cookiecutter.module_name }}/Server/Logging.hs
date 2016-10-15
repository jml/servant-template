{-# LANGUAGE FlexibleContexts #-}

-- | Logging helpers for {{ cookiecutter.project_name }}.
module {{ cookiecutter.module_name }}.Server.Logging
  ( withLogging
  , log
  ) where

import Protolude hiding (log)

import Control.Monad.Log
       (LoggingT, MonadLog, Severity(..), WithTimestamp, WithSeverity(..),
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
  => LogM msg m a -> m a
withLogging body = runLoggingT (withTimestamps body) printLogs

withTimestamps
  :: (MonadIO m, MonadLog (WithTimestamp msg) m)
  => LoggingT msg m a -> m a
withTimestamps = mapLogMessageM timestamp

printLogs
  :: (Pretty a, MonadIO m)
  => WithTimestamp (WithSeverity a) -> m ()
printLogs logs = do
  print . renderWithTimestamp timeFormatter (renderWithSeverity pretty) $ logs
  liftIO $ hFlush stdout
  where
    timeFormatter = formatTime defaultTimeLocale timeFormat
    timeFormat = iso8601DateFormat (Just "%H:%M:%S.%q")

-- | Convenience method to log with severity.
log
  :: MonadLog (WithSeverity a) m
  => Severity -> a -> m ()
log severity msg = logMessage (WithSeverity severity msg)
