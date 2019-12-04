module Year2019.Day4.Two where

import Control.Alternative (pure)
import Control.Bind (bind)
import Data.Array as A
import Data.Boolean (otherwise)
import Data.BooleanAlgebra ((||))
import Data.Eq ((==))
import Data.EuclideanRing ((-))
import Data.Field (div, mod, (+))
import Data.Function (($))
import Data.Maybe (fromMaybe)
import Data.Ord ((<))
import Data.Show (show)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)

main :: Effect Unit
main = do
  log $ show $ A.length $ A.filter matches input

input :: Array Int
input = 137683 A... 596253

digits :: Int -> Array Int
digits i
  | i < 10 = [i]
  | otherwise = A.snoc (digits $ i `div` 10) (i `mod` 10)

matches :: Int -> Boolean
matches i = matches' (digits i) false 0

matches' :: Array Int -> Boolean -> Int -> Boolean
matches' arr hasDouble i = fromMaybe hasDouble do
  curr <- A.index arr i
  next <- A.index arr (i+1)
  if curr == next
  then do
    let same = A.length $ A.takeWhile (_==curr) (A.drop i arr)
    pure $ matches' arr (hasDouble || same == 2) (i+same-1)
  else
    if curr < next
    then pure $ matches' arr hasDouble (i+1)
    else pure false
