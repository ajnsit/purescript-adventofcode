module Year2019.Day10.Two where

import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Category ((<<<))
import Data.Array ((!!))
import Data.Array as A
import Data.Boolean (otherwise)
import Data.BooleanAlgebra (not, (&&))
import Data.CommutativeRing ((*), (+))
import Data.Eq ((/=), (==))
import Data.EuclideanRing ((-))
import Data.Foldable (maximumBy)
import Data.Function (($))
import Data.Int (toNumber)
import Data.Ord (comparing, signum, (<), (<=), (>=))
import Data.Ring (negate)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String.CodeUnits as SC
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)
import Math (atan2, pi)
import Util.Input (readInputLines)

main :: Effect Unit
main = do
  lines <- readInputLines "inputs/2019/10/1"
  let asteroids = getAsteroids lines
  let maybeBest = maximumBy (comparing $ numberVisible asteroids) asteroids
  log $ show do
    best <- maybeBest
    {x:x,y:y} <- (sortedByRotation asteroids best)!!199
    pure (x*100 + y)

type Point = {x::Int, y::Int}

getAsteroids :: Array String -> Array Point
getAsteroids =
  A.concat <<< A.mapWithIndex \i ->
    A.concat
      <<< A.mapWithIndex (\j c -> if c == '#' then [{x:j, y:i}] else [])
      <<< SC.toCharArray

blocks :: Point -> Point -> Point -> Boolean
blocks o x1 x2 =
  signum (o.x - x1.x) == signum (x1.x - x2.x)
  && signum (o.y - x1.y) == signum (x1.y - x2.y)
  && (o.x - x1.x)*(x1.y - x2.y) == (x1.x - x2.x)*(o.y - x1.y)

isInvisible :: Array Point -> Point -> Point -> Boolean
isInvisible asteroids o p = A.any (\x -> x /= p && x /= o && blocks o x p) asteroids

numberVisible :: Array Point -> Point -> Int
numberVisible asteroids o = includingItself - 1
  where
    includingItself = A.length $ A.filter (not <<< isInvisible asteroids o) asteroids

visibles :: Array Point -> Point -> Array Point
visibles asteroids o = A.filter (\x -> x /= o && not (isInvisible asteroids o x)) asteroids

sortByAngleTo :: Point -> Array Point -> Array Point
sortByAngleTo = A.sortWith <<< angleTo

-- Atan2 is the angle between the positive y axis and a point `(x, y)`
-- We negate to move in the opposite direction (clockwise)
angleTo :: Point -> Point -> Number
angleTo o p = negate $ atan2 (toNumber $ p.x-o.x) (toNumber $ p.y-o.y)

normalize :: Number -> Number
normalize angle
  | angle >= 2.0*pi = angle - 2.0*pi
  | angle < 0.0 = angle + 2.0*pi
  | otherwise = angle

sortedByRotation :: Array Point -> Point -> Array Point
sortedByRotation asteroids o =
  first <>
  -- If we made no progress in this pass, abort
  -- This is because `o` itself will always be in `rest`
  (if A.length first <= 0
   then []
   else sortedByRotation rest o)
  where
    first = sortByAngleTo o (visibles asteroids o)
    rest = A.difference asteroids first
