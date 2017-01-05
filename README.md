# servant-template

[![Circle CI](https://circleci.com/gh/jml/servant-template/tree/master.svg?style=shield)](https://circleci.com/gh/jml/servant-template/tree/master)

A [cookiecutter](https://github.com/audreyr/cookiecutter) template for (almost) production-ready Servant servers.

My goal is to reduce friction for people starting working on Haskell REST API servers by putting together a few best practices in a way that's not intrusive or prescriptive.

## Usage

```
$ pip install --user cookiecutter
$ cookiecutter gh:jml/servant-template
```

Once the template is created, you can build things with `stack` or use the
provided `Makefile`.

Your API definition goes in the `YourApp.API` module and the application logic in the `YourApp.Server.Handlers` module.

## Features

Things you get with this template:

* Logging using [logging-effect](http://hackage.haskell.org/package/logging-effect)
* Prometheus instrumentation using [prometheus-client](https://hackage.haskell.org/package/prometheus-client)
* Command-line parsing with [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative)
* Tests with [servant-quickcheck](http://hackage.haskell.org/package/servant-quickcheck)
* Make target to build a Docker image tagged with the Git revision ID.

## Choices

This cookiecutter project makes a few opinionated decisions:

* GHC 8.0 only
* Built with Stack
* Uses `package.yaml` (from [hpack](https://hackage.haskell.org/package/hpack)) to configure dependencies etc.
* [Protolude](https://github.com/sdiehl/protolude) as the Prelude
* The root page of the service has a simple HTML page intended for use by developers
* There is only one executable: the binary that launches the server
* The hackage name is the repo name is the project name is the executable name
* Uses CircleCI as its default CI tool
* Enforces hindent-formatted code in CI
* The code that defines the API is in a separate library to the code that implements it

Many of these opinions are lightly held. If this template would be more useful to you with some of them changed, please [file an issue](https://github.com/jml/servant-template/issues/new) or submit a PR.

## Notes

Note that the Cabal file is not checked in to this cookiecutter template.

Uses [Johan Tibbe's style guide](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md), enforced by [hindent](https://github.com/chrisdone/hindent)

You can build a Docker image with `make image`.
