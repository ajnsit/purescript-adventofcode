module Year2021.Day07 where

import AOC.Lib (parseInt10)
import Control.Apply ((<*>))
import Control.Bind ((=<<))
import Control.Category ((<<<))
import Data.Array ((!!))
import Data.Array as A
import Data.CommutativeRing ((*), (+))
import Data.EuclideanRing ((-), (/))
import Data.Foldable (maximum, minimum, minimumBy, sum)
import Data.Function (on, ($))
import Data.Functor (map, (<$>))
import Data.Maybe (Maybe)
import Data.Ord (class Ord, abs, compare)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String as S
import Data.String.Pattern (Pattern(..))
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)

--------------------------------------------------------------------------------
-- Write your solutions here

part1 :: String -> Effect Unit
part1 input = do
  let crabs = A.mapMaybe parseInt10 $ S.split (Pattern ",") input
      result = fuelTo1 crabs <$> median crabs
  log $ "Part 1 ==> " <> show result

part2 :: String -> Effect Unit
part2 input = do
  let crabs = A.mapMaybe parseInt10 $ S.split (Pattern ",") input
      pos = minimumBy (compare `on` (fuelTo2 crabs)) =<< (A.range <$> minimum crabs <*> maximum crabs)
      result = fuelTo2 crabs <$> pos
  log $ "Part 2 ==> " <> show result

--------------------------------------------------------------------------------

median :: forall a. Ord a => Array a -> Maybe a
median arr = arr' !! (A.length arr' / 2)
  where arr' = A.sort arr

distanceTo :: Int -> Int -> Int
distanceTo pos orig = abs (orig - pos)

fuelTo1 :: Array Int -> Int -> Int
fuelTo1 arr pos = sum $ map (distanceTo pos) arr

fuelTo2 :: Array Int -> Int -> Int
fuelTo2 arr pos = sum $ map (sumFromZero <<< distanceTo pos) arr

sumFromZero :: Int -> Int
sumFromZero n = (n * (n + 1)) / 2
