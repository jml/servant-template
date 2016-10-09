{-# LANGUAGE FlexibleContexts #-}

module {{ cookiecutter.module_name }}.Logging
  ( withLogging
  , log
  , info
  ) where

import Protolude hiding (log)

import Control.Monad.Log
       (LoggingT, MonadLog, Severity(..), WithTimestamp, WithSeverity(..),
        logMessage, mapLogMessageM, renderWithSeverity,
        renderWithTimestamp, runLoggingT, timestamp)
import Data.Time.Format
       (defaultTimeLocale, formatTime, iso8601DateFormat)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

type LogM msg m a = LoggingT (WithSeverity msg) (LoggingT (WithTimestamp (WithSeverity msg)) m) a

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
printLogs =
  print . renderWithTimestamp timeFormatter (renderWithSeverity pretty)
  where
    timeFormatter = formatTime defaultTimeLocale timeFormat
    timeFormat = iso8601DateFormat (Just "%H:%M:%S.%q")

log
  :: MonadLog (WithSeverity a) m
  => Severity -> a -> m ()
log severity msg = logMessage (WithSeverity severity msg)

info
  :: MonadLog (WithSeverity a) m
  => a -> m ()
info = log Informational
