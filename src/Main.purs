module Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Year2020.Day1.Main as Day1
import Year2020.Day2.Main as Day2
import Year2020.Day3.Main as Day3
import Year2020.Day4.Main as Day4
import Year2020.Day5.Main as Day5

main :: Effect Unit
main = do
  log "--------------------- Day 1 -------------------------"
  Day1.main
  log "--------------------- Day 2 -------------------------"
  Day2.main
  log "--------------------- Day 3 -------------------------"
  Day3.main
  log "--------------------- Day 4 -------------------------"
  Day4.main
  log "--------------------- Day 5 -------------------------"
  Day5.main
