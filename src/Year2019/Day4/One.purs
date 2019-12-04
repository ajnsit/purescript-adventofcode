module Year2019.Day4.One where

import Data.Array as A
import Data.Boolean (otherwise)
import Data.Eq ((==))
import Data.Field (div, mod)
import Data.Function (($))
import Data.Maybe (Maybe(..))
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
matches i = matches' false (digits i)
matches' :: Boolean -> Array Int -> Boolean
matches' hasDouble ds = case A.uncons ds of
  Nothing -> hasDouble
  Just r -> case A.uncons r.tail of
    Nothing -> hasDouble
    Just r' ->
      if r.head == r'.head
      then matches' true r.tail
      else if r.head < r'.head
           then matches' hasDouble r.tail
           else false
