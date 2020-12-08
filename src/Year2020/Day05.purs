module Year2020.Day05 where

import Control.Applicative ((<*>))
import Control.Bind (bind)
import Control.Category ((<<<))
import Data.Array as A
import Data.Boolean (otherwise)
import Data.CommutativeRing ((*), (+))
import Data.Eq ((==))
import Data.EuclideanRing ((/))
import Data.Foldable (maximum)
import Data.Function (($))
import Data.Functor (map, (<$>))
import Data.Int (ceil, floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Semigroup ((<>))
import Data.Set (difference, empty, findMax, findMin, fromFoldable) as S
import Data.Show (show)
import Data.String (codePointAt, codePointFromChar) as S
import Data.String.Utils (lines)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)

--------------------------------------------------------------------------------
-- Write your solutions here

part1 :: String -> Effect Unit
part1 input = do
  let seatids = lines input
  let ids = A.mapMaybe (map posToId <<< pathToPos) seatids
  log $ "Part 1 ==> Maximum id: " <> (show $ maximum ids)

part2 :: String -> Effect Unit
part2 input = do
  let seatids = lines input
  let ids = A.mapMaybe (map posToId <<< pathToPos) seatids
  let sids = S.fromFoldable ids
  let sarr = fromMaybe S.empty $ map S.fromFoldable (A.range <$> S.findMin sids <*> S.findMax sids)
  log $ "Part 2 ==> Missing ids: " <> (show $ S.difference sarr sids)

--------------------------------------------------------------------------------


type Pos = {row::Int, col::Int}
type Range = {lower::Int, higher::Int}
data Segment = Higher | Lower

posToId :: Pos -> Int
posToId x = (x.row * 8) + x.col

pathToPos :: String -> Maybe Pos
pathToPos s = mkPos <$> binSearch s 'F' {lower:0, higher:127} 0 <*> binSearch s 'L' {lower:0, higher:7} 7

mkPos :: Int -> Int -> Pos
mkPos row col = {row, col}

binSearch :: String -> Char -> Range -> Int -> Maybe Int
binSearch s lc r i = do
  c <- S.codePointAt i s
  let r' = binIterate r (segment c)
  maybe (binSearch s lc r' (i+1)) Just $ binFound r'
  where
    segment c
      | c == S.codePointFromChar lc = Lower
      | otherwise = Higher

binIterate :: Range -> Segment -> Range
binIterate r Lower = r {higher = (floor (toNumber (r.lower + r.higher) / 2.0)) }
binIterate r Higher = r {lower = (ceil (toNumber (r.lower + r.higher) / 2.0)) }

binFound :: Range -> Maybe Int
binFound r
  | r.higher == r.lower = Just r.higher
  | otherwise = Nothing
