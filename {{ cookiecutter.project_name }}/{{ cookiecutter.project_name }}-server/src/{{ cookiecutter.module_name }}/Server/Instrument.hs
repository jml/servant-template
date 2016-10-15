-- | Prometheus instrumentation for {{ cookiecutter.project_name }}.
module {{ cookiecutter.module_name }}.Server.Instrument
  ( metrics
  , requestDuration
  , instrumentApp
  , prometheus
  , PrometheusSettings(..)
  , defaultPrometheusSettings
  ) where

import Protolude

import Data.ByteString.Builder (byteString)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Prometheus as Prom

-- | Settings that control the behavior of the Prometheus middleware.
data PrometheusSettings = PrometheusSettings {
  prometheusEndPoint :: [Text]
  -- ^ The path that will be used for exporting metrics. The default value
  -- is ["metrics"] which corresponds to the path /metrics.
  , prometheusHandlerName :: Maybe Text
  -- ^ The name of the handler used to record metrics about the Prometheus
  -- endpoint. If Nothing, then we won't record any.
}

-- | Default settings for Prometheus. Serve metrics at /metrics and record
-- latency information for that endpoint with 'handler="metrics"'.
defaultPrometheusSettings :: PrometheusSettings
defaultPrometheusSettings = PrometheusSettings ["metrics"] (Just "metrics")

-- | Core information about HTTP requests:
--
-- Labels:
-- * handler: the name of the application
-- * method: the HTTP method requested
-- * status_code: the HTTP response code
--
-- Actual metric is the latency of the request.
type RequestDuration = Prom.Metric (Prom.Vector Prom.Label3 Prom.Summary)

requestDuration :: IO RequestDuration
requestDuration =
  Prom.vector ("handler", "method", "status_code") $
  Prom.summary info Prom.defaultQuantiles
  where
    info =
      Prom.Info
        "http_request_duration_seconds"
        "The HTTP request latencies in microseconds."

-- | Instrument a WAI app with the default WAI metrics.
instrumentApp
  :: RequestDuration -- ^ The metric to instrument
  -> Text -- ^ The label used to identify this app
  -> Wai.Application -- ^ The app to instrument
  -> Wai.Application -- ^ The instrumented app
instrumentApp metric handler app req respond = do
  start <- getCurrentTime
  app
    req
    (\res -> do
       recordResult start (HTTP.statusCode (Wai.responseStatus res))
       respond res) `onException`
    recordResult start (500 :: Integer)
  where
    recordResult start statusCode = do
      end <- getCurrentTime
      let latency = fromRational $ toRational (end `diffUTCTime` start)
      Prom.withLabel (toS handler, method, status) (Prom.observe latency) metric
      where
        method = toS (Wai.requestMethod req)
        status = show statusCode

-- | Instrument an app with Prometheus and export metrics from the configured
-- handler.
prometheus
  :: PrometheusSettings -- ^ How we're going to use Prometheus
  -> RequestDuration -- ^ A metric to instrument with request information
  -> Text -- ^ The label used to identify the app
  -> Wai.Middleware
prometheus PrometheusSettings{..} duration appName app req respond
  = if Wai.requestMethod req == HTTP.methodGet
       && Wai.pathInfo req == prometheusEndPoint
    then
      case prometheusHandlerName of
        Nothing -> respondWithMetrics respond
        Just name -> instrumentApp duration name (const respondWithMetrics) req respond
    else
      instrumentApp duration appName app req respond

-- | Application that serves the Prometheus /metrics page regardless of what
-- was requested.
metrics :: Wai.Application
metrics = const respondWithMetrics

respondWithMetrics :: (Wai.Response -> IO Wai.ResponseReceived)
                   -> IO Wai.ResponseReceived
respondWithMetrics respond = do
  content <- Prom.exportMetricsAsText
  respond $ Wai.responseBuilder HTTP.status200 headers $ byteString content
  where
    headers = [(HTTP.hContentType, "text/plain; version=0.0.4")]
