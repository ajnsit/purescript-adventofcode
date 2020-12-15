module Year2020.Day15 where

import AOC.Stream as Stream
import Data.Array ((!!))
import Data.Array as A
import Data.Boolean (otherwise)
import Data.EuclideanRing ((-))
import Data.Field (negate)
import Data.Function (($))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord ((<), (>=))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)

--------------------------------------------------------------------------------
-- Write your solutions here

input :: Array Int
input = [14,3,1,0,9,5]
l :: Int
l = A.length input

part1 :: String -> Effect Unit
part1 _ = do
  let target = 2020
  let result = Stream.fold (step target) initStateCounter (Stream.inf 0 1)
  log $ "Part 1 ==> " <> show result.st.prev

part2 :: String -> Effect Unit
part2 _ = do
  let target = 30000000
  let result = Stream.fold (step target) initStateCounter (Stream.inf 0 1)
  log $ "Part 2 ==> " <> show result.st.prev

--------------------------------------------------------------------------------

type State = {prev2::Int, prev::Int, lasts::Map.Map Int Int}
type StateCounter = {st::State, iter::Int}

emit :: State -> Int -> Int -> State
emit s idx n = {prev2:s.prev, prev: n, lasts: Map.insert s.prev (idx-1) s.lasts }

get :: State -> Int -> State
get s idx
  | idx < l = emit s idx (fromMaybe 0 (input!!idx))
  | otherwise =
    case Map.lookup s.prev s.lasts of
      Nothing -> emit s idx 0
      Just x -> emit s idx (idx-x-1)

initStateCounter :: StateCounter
initStateCounter = {st: {prev2:negate 1, prev:negate 1, lasts: Map.empty}, iter:0}

step :: Int -> StateCounter -> Int -> Maybe StateCounter
step target = go
  where
  go x i =
    if i >= target then Nothing
    else Just $ x {st = get x.st i, iter = i}
