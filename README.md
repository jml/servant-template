# hello-prometheus-haskell

Simple example of a web app instrumented with Prometheus metrics.


## TODO

- wai-middleware-prometheus
  - uses summaries for instrumenting queries, which are not aggregatable
  - doesn't have status codes or method
- defaults to `app` namespace, which isn't great for running more than one
  thing
- example of custom prometheus metric
