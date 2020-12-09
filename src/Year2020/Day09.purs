module Year2020.Day09 where

import AOC.Lib (parseInt10)
import Control.Applicative ((<*>))
import Data.Array as A
import Data.Boolean (otherwise)
import Data.CommutativeRing ((+))
import Data.Eq ((==))
import Data.EuclideanRing ((-))
import Data.Field (negate)
import Data.Foldable (maximum, minimum, sum)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..))
import Data.Ord ((<))
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
  let numbers = A.mapMaybe parseInt10 $ lines input
  log $ "Part 1 ==> " <> (show $ step numbers)

part2 :: String -> Effect Unit
part2 input = do
  let numbers = A.mapMaybe parseInt10 $ lines input
  let target = step numbers
  log $ "Part 2 ==> " <> (show $ step2 target numbers)

--------------------------------------------------------------------------------

step2 :: Int -> Array Int -> Maybe Int
step2 target arr = go 0 2
  where
  go start end
    | end - start < 2 = go 0 (end+1)
    | otherwise =
       let range = A.slice start end arr
       in if target == sum range
          then (+) <$> minimum range <*> maximum range
          else go (start+1) end

step :: Array Int -> Int
step arr =
  let prev = A.take 25 arr
      next = A.head $ A.drop 25 arr
  in case next of
    Nothing -> negate 1
    Just x -> case ensure x prev of
      false -> x
      true -> step (A.drop 1 arr)

ensure :: Int -> Array Int -> Boolean
ensure target = go
  where
    go arr = case A.uncons arr of
      Nothing -> false
      Just {head, tail} ->
        let next = target - head
        in if next `A.elem` tail
           then true
           else go tail
