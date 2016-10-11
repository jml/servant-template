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

It makes a few opinionated decisions:

* GHC 8.0 only
* Built with Stack
* Uses `package.yaml` (from [hpack](https://hackage.haskell.org/package/hpack)) to configure dependencies etc.
* [Protolude](https://github.com/sdiehl/protolude) as the Prelude
* The root page of the service has a simple HTML page intended for use by developers
* The hackage name is the repo name is the project name
* Uses CircleCI as its default CI tool
* Enforces hindent-formatted code in CI

Many of these opinions are lightly held. If this template would be more useful to you with some of them changed, please [file an issue](https://github.com/jml/servant-template/issues/new) or submit a PR.

Note that the Cabal file is not checked in to this cookiecutter template.

Uses [Johan Tibbe's style guide](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md), enforced by [hindent](https://github.com/chrisdone/hindent)

You can build a Docker image with `make image`.

## TODO

### Management

- [ ] Remove custom logging-effect dependency once new features have been released
- [ ] Make the Docker image actually work (libstdc++)

### Code improvements

- [ ] Add a basic test suite
  - [ ] servant-aeson-hspec
  - [ ] servant-quickcheck
- [ ] Reader monad to pass in metrics
- [ ] Switch to [prometheus](https://hackage.haskell.org/package/prometheus) library if it ever gets `Vector` support: https://github.com/LukeHoersten/prometheus/issues/1
- [ ] Embrace optparse-applicative's completion
- [ ] Move `/metrics` out of the API and into the top-level Server.hs
