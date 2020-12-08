module Year2020.Day06 where

import Data.Function (($))
import Data.Semigroup ((<>))
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)
import Control.Category ((<<<))
import Data.Array as A
import Data.BooleanAlgebra (not)
import Data.Enum (enumFromTo)
import Data.Foldable (foldl, sum)
import Data.Functor (map)
import Data.Set (Set)
import Data.Set as Set
import Data.Show (show)
import Data.String as S
import Data.String.Pattern (Pattern(..))

--------------------------------------------------------------------------------
-- Write your solutions here

part1 :: String -> Effect Unit
part1 input = do
  let groups = S.split (Pattern "\n\n") input
  let sets1 = map (Set.fromFoldable <<< A.filter (\c -> c `A.elem` chars) <<< S.toCodePointArray) groups
  log $ "Part 1 ==> " <> (show $ sum $ map Set.size sets1)

part2 :: String -> Effect Unit
part2 input = do
  let groups = S.split (Pattern "\n\n") input
  let sets2 = map (map (Set.fromFoldable) <<< map (A.filter (\c -> c `A.elem` chars)) <<< map S.toCodePointArray <<< S.split (Pattern "\n")) groups
  log $ "Part 2 ==> " <> (show $ sum $ map toCount sets2)

--------------------------------------------------------------------------------


chars :: Array S.CodePoint
chars = map S.codePointFromChar $ enumFromTo 'a' 'z'

toCount :: Array (Set S.CodePoint) -> Int
toCount = Set.size <<< foldl Set.intersection (Set.fromFoldable chars) <<< A.filter (not <<< Set.isEmpty)
