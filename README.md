# hello-prometheus-haskell

Simple example of a web app instrumented with Prometheus metrics.

Run with `+RTS -T` to get GHC metrics (e.g. garbage collection).

## TODO

- wai-middleware-prometheus
  - uses summaries for instrumenting queries, which are not aggregatable
- example of custom prometheus metric

## Template project TODO

- [ ] Actually make it a template project (using cookiecutter?)
- [ ] Manual metrics increment in handler
- [ ] circle CI
- [ ] docker image make target
- [ ] Internal haddock documentation
- [ ] Tests
  - [ ] servant-aeson-hspec
  - [ ] servant-quickcheck
- [ ] Top-level module

### Tweaks

- too many decimals in time format output
- name of app being instrumented
- use optparse-applicative to get completion
