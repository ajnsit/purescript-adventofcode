module Year2019.Day6.Two where

import Control.Alternative (pure)
import Control.Bind (bind)
import Data.Array as A
import Data.Eq ((==))
import Data.EuclideanRing ((-))
import Data.Function (($))
import Data.Functor (map)
import Data.Map as M
import Data.Maybe (Maybe, fromMaybe)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..), fst)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)
import Util.Input (readInputLines)

main :: Effect Unit
main = do
  inpStr <- readInputLines "inputs/2019/6/1"
  let orbits = A.catMaybes $ map readOrbit inpStr
  let path = fromMaybe [] $ pathToX (mkSunMap orbits) "YOU" "SAN"
  log $ show $ A.length path - 3

readOrbit :: String -> Maybe Orbit
readOrbit s = do
  sun <- A.index parts 0
  planet <- A.index parts 1
  pure $ Orbit sun planet
  where
    parts = split (Pattern ")") s

-- Sun, Planet
data Orbit = Orbit String String
instance showOrbit :: Show Orbit where
  show (Orbit sun planet) = "{" <> sun <> ">" <> planet <> "}"

type SunMap = M.Map String String

addSunOrbit :: SunMap -> Orbit -> SunMap
addSunOrbit m (Orbit s p) = M.insert p s m

mkSunMap :: Array Orbit -> SunMap
mkSunMap = A.foldl addSunOrbit M.empty

pathFromCom :: SunMap -> String -> Maybe (Array String)
pathFromCom = go []
  where
  go path m "COM" = pure (A.cons "COM" path)
  go path m s = do
    sun <- M.lookup s m
    go (A.cons s path) m sun

pathToX :: SunMap -> String -> String -> Maybe (Array String)
pathToX m s1 s2 = do
  path1 <- pathFromCom m s1
  path2 <- pathFromCom m s2
  commonAncestor <- map fst $ A.last $ A.takeWhile (\(Tuple a b) -> a == b) (A.zip path1 path2)
  pure $
    -- From s1 to the common ancestor
    A.reverse (path1 `A.difference` path2)
    -- Common ancestor
    <> [commonAncestor]
    -- From common ancestor to s2
    <> path2 `A.difference` path1
