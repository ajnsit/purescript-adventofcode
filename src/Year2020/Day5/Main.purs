module Year2020.Day5.Main where

import Control.Applicative ((<*>))
import Control.Bind (bind, discard)
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
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)
import Util.Input (readInputLines)

main :: Effect Unit
main = do
  seatids <- readInputLines "src/Year2020/Day5/input"
  -- let seatids = ["BFFFBBFRRR", "FFFBBBFRRR", "BBFFBBFRLL"]
  -- log $ show $ seatids
  -- log $ show $ map pathToPos seatids
  let ids = A.mapMaybe (map posToId <<< pathToPos) seatids
  log $ "Maximum id: " <> (show $ maximum ids)
  -- log $ show $ map (_ `mod` 10) $ A.sort ids
  let sids = S.fromFoldable ids
  let sarr = fromMaybe S.empty $ map S.fromFoldable (A.range <$> S.findMin sids <*> S.findMax sids)
  log $ "Missing ids: " <> (show $ S.difference sarr sids)

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
