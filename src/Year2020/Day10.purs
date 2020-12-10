module Year2020.Day10 (part1, part2) where

import AOC.Lib (groupBySeq, intToBigNumber, parseInt10)
import Control.Applicative (pure)
import Control.Apply ((<*>))
import Control.Bind (bind, discard)
import Control.Category ((<<<))
import Control.MonadPlus (guard)
import Data.Array ((!!))
import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.BigNumber (BigNumber)
import Data.CommutativeRing ((*), (+))
import Data.Eq ((/=), (==))
import Data.EuclideanRing ((-))
import Data.Field (negate)
import Data.Foldable (maximum, product)
import Data.Function (($))
import Data.Functor (map, (<$>))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Ord ((<), (<=))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String.Utils (lines)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)

--------------------------------------------------------------------------------
-- Write your solutions here

part1 :: String -> Effect Unit
part1 input = do
  let adapters = A.sort $ groundAndDevice $ A.mapMaybe parseInt10 (lines input)
  let diffs = diff <$> tails adapters
  log $ "Part 1 ==> " <> show ((count 1 diffs) * (count 3 diffs))

part2 :: String -> Effect Unit
part2 input = do
  let adapters = A.sort $ groundAndDevice $ A.mapMaybe parseInt10 (lines input)
  let diffs = dropLast 2 $ diff <$> tails adapters
  log $ "Part 2 ==> " <> (show $ product $ map countVariations $ A.group diffs)


--------------------------------------------------------------------------------

noMoreThan2Consecutive :: Array Int -> Boolean
noMoreThan2Consecutive arr = A.all (\x -> NEA.length x < 3) $
  groupBySeq (\a b -> b-a == 1 ) arr

-- Number of variations in a consecutive run of adapters (separated by 1s)
-- For any other separations (namely 3s) we return 1 (as no more rearrangements are possible)
countVariations :: NEA.NonEmptyArray Int -> BigNumber
countVariations arr = intToBigNumber $ if x.head /= 1 then 1 else A.length (go (NEA.length arr))
  where
  x = NEA.uncons arr
  -- n adapters separated by 1
  -- We can remove any number of adapters as long as no more than 3 are consecutive
  -- Return the Array of Array of indices to remove
  go :: Int -> Array (Array Int)
  go n = go' (A.range 0 (n-1))
  go' :: Array Int -> Array (Array Int)
  go' indices =
    if A.length indices <= 1
       then [indices]
       else do
         -- Pick one index
         index <- indices
         -- Pick variations for all the remaining *trailing* array
         variation <- go' (A.slice (index+1) (A.length indices) indices)
         let res = A.cons index variation
         guard (noMoreThan2Consecutive res)
         pure res

dropLast :: forall a. Int -> Array a -> Array a
dropLast x = A.slice 0 (negate x)

groundAndDevice :: Array Int -> Array Int
groundAndDevice arr = A.cons 0 (maybe arr (A.snoc arr <<< (_+3)) (maximum arr))

diff :: Array Int -> Int
diff arr = fromMaybe (negate 1) $ (-) <$> arr!!1 <*> arr!!0

count :: Int -> Array Int -> Int
count x arr = A.length $ A.filter (_==x) arr

tails :: forall a. Array a -> Array (Array a)
tails arr = case A.uncons arr of
  Nothing -> A.singleton arr
  Just x -> A.cons arr (tails x.tail)
