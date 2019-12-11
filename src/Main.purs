module Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Year2019.Day1.One as Day11
import Year2019.Day1.Two as Day12
import Year2019.Day11.One as Day111
import Year2019.Day11.Two as Day112
import Year2019.Day2.One as Day21
import Year2019.Day2.Two as Day22
import Year2019.Day3.One as Day31
import Year2019.Day3.Two as Day32
import Year2019.Day4.One as Day41
import Year2019.Day4.Two as Day42
import Year2019.Day5.One as Day51
import Year2019.Day5.Two as Day52
import Year2019.Day6.One as Day61
import Year2019.Day6.Two as Day62
import Year2019.Day7.One as Day71
import Year2019.Day7.Two as Day72
import Year2019.Day8.One as Day81
import Year2019.Day8.Two as Day82
import Year2019.Day9.One as Day91
import Year2019.Day9.Two as Day92

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
  log "--------------------- Day 4 -------------------------"
  Day41.main
  Day42.main
  log "--------------------- Day 5 -------------------------"
  Day51.main
  Day52.main
  log "--------------------- Day 6 -------------------------"
  Day61.main
  Day62.main
  log "--------------------- Day 7 -------------------------"
  Day71.main
  Day72.main
  log "--------------------- Day 8 -------------------------"
  Day81.main
  Day82.main
  log "--------------------- Day 9 -------------------------"
  Day91.main
  Day92.main
  -- log "--------------------- Day 10 ------------------------"
  -- Day101.main
  -- Day102.main
  log "--------------------- Day 11 ------------------------"
  Day111.main
  Day112.main
