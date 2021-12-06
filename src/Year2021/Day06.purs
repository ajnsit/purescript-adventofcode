module Year2021.Day06 where

import AOC.Lib (intToBigNumber, parseInt10)
import Control.Category ((<<<), (>>>))
import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.BigNumber as BN
import Data.CommutativeRing ((+))
import Data.EuclideanRing ((-))
import Data.Foldable (foldl, sum)
import Data.Function (applyN, ($))
import Data.Functor (map)
import Data.Map (Map)
import Data.Map as Map
import Data.Ord ((>=))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String as S
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)

--------------------------------------------------------------------------------
-- Write your solutions here

part1 :: String -> Effect Unit
part1 input = do
  let fish = A.mapMaybe parseInt10 $ S.split (Pattern ",") input
  let result = generate 80 fish
  log $ "Part 1 ==> " <> show result

part2 :: String -> Effect Unit
part2 input = do
  let fish = A.mapMaybe parseInt10 $ S.split (Pattern ",") input
  let result = generate 256 fish
  log $ "Part 2 ==> " <> show result

--------------------------------------------------------------------------------

type Fish = Int
type FishCount = Map Fish BN.BigNumber

-- step :: Fish -> Array Fish
-- step age
--   | age >= 1 = [age - 1]
--   | otherwise = [8, 6]
--
-- generate :: Int -> Array Int -> Array Int
-- generate = applyN (A.concatMap step)

fishToCount :: Array Int -> Map Int BN.BigNumber
fishToCount arr = Map.fromFoldable $ map (\e -> Tuple (NEA.head e) (intToBigNumber $ NEA.length e)) (A.group $ A.sort arr)

step :: FishCount -> Array FishCount
step = Map.toUnfoldable >>> map \ (Tuple age num) ->
  if age >= 1
    then Map.singleton (age - 1) num
    else Map.fromFoldable [Tuple 8 num, Tuple 6 num]

count :: Array FishCount -> FishCount
count arr = foldl (Map.unionWith (+)) Map.empty arr

generateCount :: Int -> Array Int -> FishCount
generateCount gens fish = applyN (count <<< step) gens (fishToCount fish)

generate :: Int -> Array Int -> BN.BigNumber
generate i = sum <<< Map.values <<< generateCount i
