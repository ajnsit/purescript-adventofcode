module Year2021.Day05 where

import AOC.Lib (eitherToMaybe, parseInt10)
import Control.Alternative (empty)
import Control.Applicative (pure)
import Control.Bind (bind, discard, (=<<))
import Control.Category ((<<<))
import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.Boolean (otherwise)
import Data.BooleanAlgebra ((||))
import Data.Eq ((==))
import Data.EuclideanRing ((-))
import Data.Function (($))
import Data.Maybe (Maybe)
import Data.Ord (abs, (>))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String.Regex as SR
import Data.String.Regex.Flags as RF
import Data.String.Utils (lines)
import Data.Traversable (sequence)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Class.Console (log)

--------------------------------------------------------------------------------
-- Write your solutions here

part1 :: String -> Effect Unit
part1 input = do
  let
    vents = A.mapMaybe parseVent $ lines input
    result = ndups $ A.concatMap expandHorzOrVertVent vents
  log $ "Part 1 ==> " <> show result

part2 :: String -> Effect Unit
part2 input = do
  let
    vents = A.mapMaybe parseVent $ lines input
    result = ndups $ A.concatMap expandVent vents
  log $ "Part 2 ==> " <> show result

--------------------------------------------------------------------------------

type Point = {x :: Int, y :: Int}
type Vent = {p1 :: Point, p2 :: Point}

parseVent :: String -> Maybe Vent
parseVent s = do
  instrRegex <- eitherToMaybe $ SR.regex "(\\d+),(\\d+) -> (\\d+),(\\d+)" RF.noFlags
  nem <- SR.match instrRegex s
  let m' = NEA.tail nem
  m <- sequence m'
  vx1 <- parseInt10 =<< A.index m 0
  vy1 <- parseInt10 =<< A.index m 1
  vx2 <- parseInt10 =<< A.index m 2
  vy2 <- parseInt10 =<< A.index m 3
  pure {p1:{x:vx1, y:vy1}, p2:{x:vx2, y:vy2}}

isHorzOrVert :: Vent -> Boolean
isHorzOrVert v = v.p1.x == v.p2.x || v.p1.y == v.p2.y

isDiag :: Vent -> Boolean
isDiag v = abs (v.p1.x - v.p2.x) == abs (v.p1.y - v.p2.y)

expandVent :: Vent -> Array Point
expandVent v
  | isDiag v = expandDiagVent v
  | isHorzOrVert v = expandHorzOrVertVent v
  | otherwise = []

expandHorzOrVertVent :: Vent -> Array Point
expandHorzOrVertVent v = do
  if isHorzOrVert v then pure unit else empty
  x <- A.range v.p1.x v.p2.x
  y <- A.range v.p1.y v.p2.y
  pure {x,y}

expandDiagVent :: Vent -> Array Point
expandDiagVent v = A.zipWith (\x y -> {x,y}) (A.range v.p1.x v.p2.x) (A.range v.p1.y v.p2.y)

ndups :: Array Point -> Int
ndups = A.length <<< A.filter ((_ > 1) <<< NEA.length) <<< A.group <<< A.sort
