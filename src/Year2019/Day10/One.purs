module Year2019.Day10.One where

import Control.Bind (bind)
import Control.Category ((<<<))
import Data.Array as A
import Data.BooleanAlgebra (not, (&&))
import Data.CommutativeRing ((*))
import Data.Eq ((/=), (==))
import Data.EuclideanRing ((-))
import Data.Foldable (maximum)
import Data.Function (($))
import Data.Functor (map)
import Data.Ord (signum)
import Data.Show (show)
import Data.String.CodeUnits as SC
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)
import Util.Input (readInputLines)

main :: Effect Unit
main = do
  lines <- readInputLines "inputs/2019/10/1"
  let asteroids = getAsteroids lines
  log $ show $ maximum $ map (numberVisible asteroids) asteroids

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
