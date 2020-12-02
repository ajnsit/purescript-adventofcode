module Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Year2020.Day1.Main as Day1
import Year2020.Day2.Main as Day2

main :: Effect Unit
main = do
  log "--------------------- Day 1 -------------------------"
  Day1.main
  log "--------------------- Day 2 -------------------------"
  Day2.main
