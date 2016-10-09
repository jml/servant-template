module Main
  ( main
  ) where

import Protolude

import {{ cookiecutter.module_name }}.Lib (startApp)

main :: IO ()
main = startApp
