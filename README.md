# servant-template

A [cookiecutter]({{ cookiecutter.project_name }}) template for
(almost) production-ready Servant servers.

## Usage

```
$ pip install --user cookiecutter
$ cookiecutter gh:jml/servant-template
```

## Features

Things you get with this template:

* logging using [logging-effect](http://hackage.haskell.org/package/logging-effect)
* Prometheus instrumentation using [prometheus-client](https://hackage.haskell.org/package/prometheus-client)
* command-line parsing with [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative)

It makes a couple of opinionated decisions:

* GHC 8.0 only
* Built with Stack
* Uses `package.yaml`
  (from [hpack](https://hackage.haskell.org/package/hpack)) to configure
  dependencies etc.
* [Protolude](https://github.com/sdiehl/protolude) as the Prelude
* The root page of the service has a simple HTML page intended for use by
  developers
* The hackage name is the repo name is the project name

Note that the Cabal file is not checked in to this cookiecutter template.

Uses [Johan Tibbe's style guide](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md), enforced by [hindent](https://github.com/chrisdone/hindent)

## TODO

### Management

- [ ] More Haddock documentation
- [ ] Add CircleCI / Travis tests
- [ ] Makefile
  - [ ] Run & enforce hindent
  - [ ] Run & enforce hlint
  - [ ] Make a Docker image
- [ ] Remove custom logging-effect dependency once new features have been
  released

### Code improvements

- [ ] Add a basic test suite
  - [ ] servant-aeson-hspec
  - [ ] servant-quickcheck
- [ ] Reader monad to pass in metrics
- [ ] Switch to [prometheus](https://hackage.haskell.org/package/prometheus)
  library if it ever gets `Vector` support:
  https://github.com/LukeHoersten/prometheus/issues/1
- [ ] Embrace optparse-applicative's completion
