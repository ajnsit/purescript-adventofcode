module Year2020.Day01 where

import AOC.Lib (parseInt10)
import Data.Array as A
import Data.Foldable (product)
import Data.Function (($))
import Data.Maybe (Maybe(..))
import Data.Ring ((-))
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
  let entries = A.mapMaybe parseInt10 $ lines input
  let numberPair = findPair 2020 entries
  log $ "Part 1 ==> The numbers are " <> show numberPair <> " and the product is " <> show (product numberPair)

part2 :: String -> Effect Unit
part2 input = do
  let entries = A.mapMaybe parseInt10 $ lines input
  let numberTriple = findTriple 2020 entries
  log $ "Part 2 ==> The numbers are " <> show numberTriple <> " and the product is " <> show (product numberTriple)

--------------------------------------------------------------------------------


findPair :: Int -> Array Int -> Array Int
findPair target = go
  where
    go arr = case A.uncons arr of
      Nothing -> []
      Just {head, tail} ->
        let next = target - head
        in if next `A.elem` tail
           then [head, next]
           else go tail

findTriple :: Int -> Array Int -> Array Int
findTriple target = go
  where
  go arr = case A.uncons arr of
    Nothing -> []
    Just {head, tail} ->
      let newTarget = target - head
          matching = findPair newTarget tail
      in case A.uncons matching of
        Nothing -> go tail
        Just vals -> A.cons head matching
