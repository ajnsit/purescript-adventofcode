module Year2019.Day12.One where

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Category ((<<<))
import Control.MonadZero (guard)
import Data.Array as A
import Data.Boolean (otherwise)
import Data.CommutativeRing ((*))
import Data.DivisionRing (negate)
import Data.Eq ((/=), (==))
import Data.Field ((+))
import Data.Foldable (sum)
import Data.Function (($))
import Data.Functor (map, (<$>))
import Data.Map as M
import Data.Maybe (Maybe)
import Data.Ord (abs, (<))
import Data.Ring ((-))
import Data.Show (show)
import Data.String as S
import Data.String.CodeUnits as CS
import Data.Traversable (traverse)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)
import Util.Input (readInputLines)
import Util.Parse (parseInt10)
import Util.Util (iterate, pipeline)

main :: Effect Unit
main = do
  inpStr <- readInputLines "inputs/2019/12/1"
  let moons = A.catMaybes $ map (map atRest <<< parseVec) inpStr
  log $ show $ energy $ iterate 1000 simulate moons

type Vec =
  { x::Int
  , y::Int
  , z::Int
  }

type Moon =
  { pos::Vec
  , vel::Vec
  }

atRest :: Vec -> Moon
atRest v = {pos:v, vel:{x:0,y:0,z:0}}

energy :: Array Moon -> Int
energy = sum <<< map energyMoon

energyMoon :: Moon -> Int
energyMoon m = pot * kin
  where
    pot = abs m.pos.x + abs m.pos.y + abs m.pos.z
    kin = abs m.vel.x + abs m.vel.y + abs m.vel.z

simulate :: Array Moon -> Array Moon
simulate moons = map (\m -> simulateMoon moons m) moons

simulateMoon :: Array Moon -> Moon -> Moon
simulateMoon rest moon =
  applyVel $ pipeline moon $ map applyVDelta vdeltas
  where
    vdeltas = map (grav moon) rest

applyVel :: Moon -> Moon
applyVel m =
  m{pos=
    { x:m.pos.x+m.vel.x
    , y:m.pos.y+m.vel.y
    , z:m.pos.z+m.vel.z
    }
   }

grav :: Moon -> Moon -> Vec
grav t s =
  { x: signumz (s.pos.x - t.pos.x)
  , y: signumz (s.pos.y - t.pos.y)
  , z: signumz (s.pos.z - t.pos.z)
  }

-- Signum, but maps 0 to 0, and works with big numbers
signumz :: Int -> Int
signumz x
  | x == 0 = 0
  | x < 0 = negate 1
  | otherwise = 1

applyVDelta :: Vec -> Moon -> Moon
applyVDelta v m = m{vel=addVec v m.vel}

addVec :: Vec -> Vec -> Vec
addVec {x:x1,y:y1,z:z1} {x:x2,y:y2,z:z2} = {x:x1+x2,y:y1+y2,z:z1+z2}

parseVec :: String -> Maybe Vec
parseVec s = do
  leftParen <- CS.charAt 0 s
  guard $ leftParen == '<'
  let partsStr = S.split (S.Pattern ", ") (S.drop 1 s)
  m <- M.unions <$> traverse parseVecComponent partsStr
  x <- M.lookup "x" m
  y <- M.lookup "y" m
  z <- M.lookup "z" m
  pure {x:x,y:y,z:z}

parseVecComponent :: String -> Maybe (M.Map String Int)
parseVecComponent s = do
  let k = S.takeWhile (_ /= S.codePointFromChar '=') s
  let s' = S.dropWhile (_ /= S.codePointFromChar '=') s
  eq <- CS.charAt 0 s'
  guard $ eq == '='
  v <- parseInt10 (S.drop 1 s')
  pure (M.singleton k v)
