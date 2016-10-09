# {{ cookiecutter.project_name }}

{{cookiecutter.synopsis}}

## About this template

This is a template for making HTTP APIs with Servant that starts off by being
very close to production ready.

Specifically:

* logging by
  using [logging-effect](http://hackage.haskell.org/package/logging-effect)
* Prometheus instrumentation
  using
  [prometheus-client](https://hackage.haskell.org/package/prometheus-client)
* command-line parsing
  with
  [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative)

It makes a couple of opinionated decisions:

* GHC 8.0 only
* Built with Stack
* Uses `package.yaml`
  (from [hpack](https://hackage.haskell.org/package/hpack)) to configure
  dependencies etc.
* [Protolude](https://github.com/sdiehl/protolude) as the Prelude

Note that the Cabal file is not checked in to this cookiecutter template. 

Uses
[Johan Tibbe's style guide](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md),
enforced by [hindent](https://github.com/chrisdone/hindent)

## TODO

### Management

- [ ] More Haddock documentation
- [ ] Add CircleCI / Travis tests
- [ ] Makefile
  - [ ] Run & enforce hindent
  - [ ] Run & enforce hlint
  - [ ] Make a Docker image
- [ ] Embrace optparse-applicative's completion

### Code improvements

- [ ] Put everything in a top-level module
- [ ] Add a basic test suite
  - [ ] servant-aeson-hspec
  - [ ] servant-quickcheck
- [ ] Reader monad to pass in metrics
- [ ] Switch to [prometheus](https://hackage.haskell.org/package/prometheus)
  library if it ever gets `Vector` support:
  https://github.com/LukeHoersten/prometheus/issues/1
