module Year2019.Day3.Two where

import Control.Alt ((<$>))
import Control.Alternative (pure, (<*>))
import Control.Bind (bind)
import Control.Category ((<<<))
import Data.Array as A
import Data.Boolean (otherwise)
import Data.BooleanAlgebra ((&&))
import Data.Either (Either(..))
import Data.Eq ((/=), (==))
import Data.Field (negate, (+))
import Data.Foldable (minimumBy)
import Data.Function (($))
import Data.Functor (map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (Ordering(..), abs, compare, signum)
import Data.Ordering (Ordering)
import Data.Ring ((-))
import Data.Show (class Show, show)
import Data.String (split)
import Data.String.CodeUnits as S
import Data.String.Pattern (Pattern(..))
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)
import Util.Input (readInputLines)
import Util.Parse (parseInt10)

main :: Effect Unit
main = do
  inpStr <- readInputLines "inputs/2019/3/1"
  let wires = map (vectorsToSegments <<< A.catMaybes <<< map readVector <<< split (Pattern ",")) inpStr
  let intersections = fromMaybe [] (wireIntersections <$> A.index wires 0 <*> A.index wires 1)
  log $ show $ map (_.steps) $ minimumBy cmpIntersection intersections

wireIntersections :: Wire -> Wire -> Array Intersection
wireIntersections w1 w2 = do
  s1 <- w1
  s2 <- w2
  case intersectSegment s1 s2 of
    Nothing -> []
    Just x -> [x]

data Dir = L | R | U | D
instance showDir :: Show Dir where
  show L = "L"
  show R = "R"
  show U = "U"
  show D = "D"

readDir :: String -> Maybe Dir
readDir "L" = Just L
readDir "R" = Just R
readDir "U" = Just U
readDir "D" = Just D
readDir _ = Nothing

type Point = {x::Int, y::Int}
type Vector = {dir::Dir, len::Int}
type Path = Array Vector
-- Also captures the steps taken to reach the origin of this segment
type Segment = {vec::Vector, orig::Point, steps :: Int}
type Wire = Array Segment

{-
-- Alternative representation of a segment
-- Up/Right is +, Down/Left is -
-- Negative length represents -X, or -Y
type LineSegment = {orig::Point, isVert :: Boolean, len::Int}
-}

-- Partially order points by manhattan distance from origin
-- instance eqPoint :: Eq Point where
--   eq p1 p2 = p1.x + p1.y == p2.x + p2.y
cmpPoint :: Point -> Point -> Ordering
cmpPoint p1 p2 = compare (manhattan p1) (manhattan p2)

manhattan :: Point -> Int
manhattan p = abs p.x + abs p.y

subPoint :: Point -> Point -> Point
subPoint p1 p2 = {x:p1.x - p2.x, y:p1.y-p2.y}

distance :: Point -> Point -> Int
distance p1 p2 = manhattan (subPoint p1 p2)

minPoint :: Point -> Point -> Point
minPoint x y =
  case cmpPoint x y of
    LT -> x
    EQ -> x
    GT -> y

readVector :: String -> Maybe Vector
readVector s = do
  x <- S.charAt 0 s
  dir <- readDir (S.singleton x)
  len <- parseInt10 (S.drop 1 s)
  pure {dir:dir, len:len}

xdelta :: Vector -> Int
xdelta v = case v.dir of
  L -> negate v.len
  R -> v.len
  _ -> 0


ydelta :: Vector -> Int
ydelta v = case v.dir of
  U -> v.len
  D -> negate v.len
  _ -> 0

move :: Point -> Vector -> Point
move p v =
  { x: p.x + xdelta v
  , y: p.y + ydelta v
  }

vectorsToSegments :: Array Vector -> Array Segment
vectorsToSegments vs =
  let ret = A.foldl f {prev:[], orig:{x:0,y:0}, steps:0} vs
  in ret.prev
  where
    f r v = {prev:A.snoc r.prev {orig:r.orig, vec:v, steps:r.steps}, orig: move r.orig v, steps:r.steps+v.len}


isVert :: Dir -> Boolean
isVert U = true
isVert D = true
isVert _ = false

isHorz :: Dir -> Boolean
isHorz L = true
isHorz R = true
isHorz _ = false

{-
segmentToLineSegment :: Segment -> LineSegment
segmentToLineSegment s = case s.vec.dir of
  L -> {orig: s.orig, isVert: false, len: negate s.vec.len}
  R -> {orig: s.orig, isVert: false, len: s.vec.len}
  D -> {orig: s.orig, isVert: true, len: negate s.vec.len}
  U -> {orig: s.orig, isVert: true, len: s.vec.len}
-}

endPt :: Segment -> Point
endPt s =
  { x: s.orig.x + xdelta s.vec
  , y: s.orig.y + ydelta s.vec
  }

-- An infinite line
-- Either X=<something> or Y=<something>
type Line = Either Int Int

-- Gets the infinite line on which the segment lies
underline :: Segment -> Line
underline s
  | isVert s.vec.dir = Left s.orig.x
  | otherwise = Right s.orig.y

-- Either the intersection point
-- Or whether they are the same line
intersectLine :: Line -> Line -> Either Point Boolean
intersectLine (Left x) (Right y) = Left {x:x,y:y}
intersectLine (Right y) (Left x) = Left {x:x,y:y}
intersectLine (Left x1) (Left x2) = Right (x1 == x2)
intersectLine (Right y1) (Right y2) = Right (y1 == y2)

-- Does a number lie on a number line segment
-- Here the point represents x1, and x2, on the same axis. Not x,y.
liesOn1 :: Point -> Int -> Boolean
liesOn1 p n = signum (p.x - n) /= signum (p.y - n)

-- Does a point lie on a segment
-- *IMPORTANT NOTE*: This assumes that the point lies on the underlying line
liesOn2 :: Point -> Segment -> Boolean
liesOn2 p s
  | isVert s.vec.dir = liesOn1 {x:s.orig.y, y:(endPt s).y} p.y
  | otherwise = liesOn1 {x:s.orig.x, y:(endPt s).x} p.x

-- Intersection point, plus the sum of steps the wires took to get here
type Intersection = {point::Point, steps::Int}

-- Compare intersections by the number of steps it took
cmpIntersection :: Intersection -> Intersection -> Ordering
cmpIntersection i1 i2 = compare i1.steps i2.steps

-- Do two segments intersect
intersectSegment :: Segment -> Segment -> Maybe Intersection
intersectSegment s1 s2 =
  case potentialPt of
    -- No intersection
    Right false -> Nothing
    -- Many possible intersections -- TODO: HANDLE
    Right true -> Nothing
    -- One possible intersection
    -- Check if it lies on both segments
    Left p ->
      if liesOn2 p s1 && liesOn2 p s2
      then
        let l = s1.steps + distance s1.orig p + s2.steps + distance s2.orig p
        in Just {point:p, steps:l}
      else Nothing
  where
    potentialPt = intersectLine l1 l2
    l1 = underline s1
    l2 = underline s2
