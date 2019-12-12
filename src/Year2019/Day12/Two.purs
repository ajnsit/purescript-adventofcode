module Year2019.Day12.Two where

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Category ((<<<))
import Control.MonadZero (guard)
import Data.Array as A
import Data.Boolean (otherwise)
import Data.CommutativeRing (add, one, zero)
import Data.DivisionRing (sub)
import Data.Eq (class Eq, (/=), (==))
import Data.EuclideanRing (lcm)
import Data.Field ((+))
import Data.Function (($))
import Data.Functor (map, (<$>))
import Data.Map as M
import Data.Maybe (Maybe)
import Data.Ord ((<))
import Data.Ring ((-))
import Data.Show (show)
import Data.String as S
import Data.String.CodeUnits as CS
import Data.Traversable (traverse)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)
import Util.BigNum (BigNumber, parseBigInt)
import Util.Input (readInputLines)
import Util.Util (fromInt, pipeline)

main :: Effect Unit
main = do
  inpStr <- readInputLines "inputs/2019/12/1"
  let moons = A.catMaybes $ map (map atRest <<< parseVec) inpStr
  let interval a = computeInterval simulate $ map (axis a) moons
  log $ show $ lcm (interval X) (lcm (interval Y) (interval Z))

computeInterval :: forall a. Eq a => (a -> a) -> a -> BigNumber
computeInterval f orig =
  let go i a = if a==orig then i else go (add i one) (f a)
  in go one (f orig)

data Axis = X | Y | Z

type Vec =
  { x::BigNumber
  , y::BigNumber
  , z::BigNumber
  }

type Moon =
  { pos::Vec
  , vel::Vec
  }

type MoonAxis =
  { pos::BigNumber
  , vel::BigNumber
  }

atRest :: Vec -> Moon
atRest v = {pos:v, vel:{x:fromInt 0,y:fromInt 0,z:fromInt 0}}

axis :: Axis -> Moon -> MoonAxis
axis X m = {pos:m.pos.x, vel:m.vel.x}
axis Y m = {pos:m.pos.y, vel:m.vel.y}
axis Z m = {pos:m.pos.z, vel:m.vel.z}

simulate :: Array MoonAxis -> Array MoonAxis
simulate moons = map (\m -> simulateMoon moons m) moons

simulateMoon :: Array MoonAxis -> MoonAxis -> MoonAxis
simulateMoon rest moon =
  applyVel $ pipeline moon $ map applyVDelta vdeltas
  where
    vdeltas = map (grav moon) rest

applyVel :: MoonAxis -> MoonAxis
applyVel m = m{pos=m.pos+m.vel}

grav :: MoonAxis -> MoonAxis -> BigNumber
grav t s = signumz (s.pos - t.pos)

-- Signum, but maps 0 to 0, and works with big numbers
signumz :: BigNumber -> BigNumber
signumz x
  | x == zero = zero
  | x < zero = sub zero one
  | otherwise = one

applyVDelta :: BigNumber -> MoonAxis -> MoonAxis
applyVDelta v m = m{vel=v + m.vel}

parseVec :: String -> Maybe Vec
parseVec s' = do
  leftParen <- CS.charAt 0 s'
  guard $ leftParen == '<'
  let s = S.takeWhile (_ /= S.codePointFromChar '>') s'
  let partsStr = S.split (S.Pattern ", ") (S.drop 1 s)
  m <- M.unions <$> traverse parseVecComponent partsStr
  x <- M.lookup "x" m
  y <- M.lookup "y" m
  z <- M.lookup "z" m
  pure {x:x,y:y,z:z}

parseVecComponent :: String -> Maybe (M.Map String BigNumber)
parseVecComponent s = do
  let k = S.takeWhile (_ /= S.codePointFromChar '=') s
  let s' = S.dropWhile (_ /= S.codePointFromChar '=') s
  eq <- CS.charAt 0 s'
  guard $ eq == '='
  v <- parseBigInt (S.drop 1 s')
  pure (M.singleton k v)
