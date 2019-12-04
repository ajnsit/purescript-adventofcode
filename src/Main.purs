module Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Year2019.Day1.One as Day11
import Year2019.Day1.Two as Day12
import Year2019.Day2.One as Day21
import Year2019.Day2.Two as Day22
import Year2019.Day3.One as Day31
import Year2019.Day3.Two as Day32
import Year2019.Day4.One as Day41
import Year2019.Day4.Two as Day42

main :: Effect Unit
main = do
  log "--------------------- Day 1 -------------------------"
  Day11.main
  Day12.main
  log "--------------------- Day 2 -------------------------"
  Day21.main
  Day22.main
  log "--------------------- Day 3 -------------------------"
  Day31.main
  Day32.main
  log "--------------------- Day 3 -------------------------"
  Day41.main
  Day42.main
