module Year2020.Day1.Main where

import Control.Bind (bind, discard)
import Control.Category ((<<<))
import Data.Array as A
import Data.Foldable (product)
import Data.Function (($))
import Data.Functor (map, (<$>))
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Ring ((-))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)
import Util.Input (readInputLines)
import Util.Parse (unsafeParseInt10)

main :: Effect Unit
main = do
  entries <- map (round <<< unsafeParseInt10) <$> readInputLines "src/Year2020/Day1/input"
  let numberPair = findPair 2020 entries
  log $ "Part 1 ==> The numbers are " <> show numberPair <> " and the product is " <> show (product numberPair)
  let numberTriple = findTriple 2020 entries
  log $ "Part 2 ==> The numbers are " <> show numberTriple <> " and the product is " <> show (product numberTriple)

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
