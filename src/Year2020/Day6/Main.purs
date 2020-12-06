module Year2020.Day6.Main where

import Control.Bind (bind, discard)
import Control.Category ((<<<))
import Data.Array as A
import Data.BooleanAlgebra (not)
import Data.Enum (enumFromTo)
import Data.Foldable (foldl, sum)
import Data.Function (($))
import Data.Functor (map)
import Data.Semigroup ((<>))
import Data.Set (Set)
import Data.Set as Set
import Data.Show (show)
import Data.String as S
import Data.String.Pattern (Pattern(..))
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)
import Util.Input (readInputParagraphs)

chars :: Array S.CodePoint
chars = map S.codePointFromChar $ enumFromTo 'a' 'z'

main :: Effect Unit
main = do
  groups <- readInputParagraphs "src/Year2020/Day6/input"
  let sets1 = map (Set.fromFoldable <<< A.filter (\c -> c `A.elem` chars) <<< S.toCodePointArray) groups
  let sets2 = map (map (Set.fromFoldable) <<< map (A.filter (\c -> c `A.elem` chars)) <<< map S.toCodePointArray <<< S.split (Pattern "\n")) groups
  log $ "Part 1: " <> (show $ sum $ map Set.size sets1)
  log $ "Part 2: " <> (show $ sum $ map toCount sets2)

toCount :: Array (Set S.CodePoint) -> Int
toCount = Set.size <<< foldl Set.intersection (Set.fromFoldable chars) <<< A.filter (not <<< Set.isEmpty)
