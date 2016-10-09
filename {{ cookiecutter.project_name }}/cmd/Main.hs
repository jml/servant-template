module Main
  ( main
  ) where

import Protolude

import {{ cookiecutter.module_name }}.Server (startApp)

main :: IO ()
main = startApp
