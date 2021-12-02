module Year2021.Day01 where

import AOC.Lib (parseInt10)
import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Array as A
import Data.CommutativeRing ((+))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (maybe)
import Data.Ord ((<))
import Data.Ring ((-))
import Data.Show (show)
import Data.String.Utils (lines)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)


--------------------------------------------------------------------------------
-- Write your solutions here

part1 :: String -> Effect Unit
part1 input = do
  let entries = A.mapMaybe parseInt10 $ lines input
  log $ maybe "ERROR" show do
    diffs <- A.zipWith (-) entries <$> A.tail entries
    pure $ A.length $ A.filter (_ < 0) diffs -- `<` because we did `zip prev next`

part2 :: String -> Effect Unit
part2 input = do
  let entries = A.mapMaybe parseInt10 $ lines input
  log $ maybe "ERROR" show do
    t1 <- A.tail entries
    t2 <- A.tail t1
    let e1 = A.zip entries t1
    let e2 = A.zipWith (\ (Tuple a b) c -> a+b+c) e1 t2
    diffs <- A.zipWith (-) e2 <$> A.tail e2
    pure $ A.length $ A.filter (_ < 0) diffs

--------------------------------------------------------------------------------
