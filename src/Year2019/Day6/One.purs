module Year2019.Day6.One where

import Control.Alternative (pure)
import Control.Bind (bind)
import Control.Category ((<<<))
import Data.Array as A
import Data.Field ((+))
import Data.Foldable (sum)
import Data.Function (($))
import Data.Functor (map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)
import Util.Input (readInputLines)

main :: Effect Unit
main = do
  inpStr <- readInputLines "inputs/2019/6/1"
  let orbits = A.catMaybes $ map readOrbit inpStr
  log $ show $ orbitCount $ orbitsToNodes orbits

-- Sun, Planet
data Orbit = Orbit String String
instance showOrbit :: Show Orbit where
  show (Orbit sun planet) = "{" <> sun <> ">" <> planet <> "}"

type OrbitMap = M.Map String (Array String)

-- I could have done it with just the orbitalmap instead of this tree
-- oh well, hindsight
data OrbitNode = OrbitNode
  { sun :: String, planets :: Array OrbitNode, orbitCount :: Int }
instance showOrbitNode :: Show OrbitNode where
  show (OrbitNode x) = x.sun <> "[" <> show x.orbitCount <> "]" <> "{" <> show x.planets <> "}"

orbitCount :: OrbitNode -> Int
orbitCount (OrbitNode n) = n.orbitCount + sum (map orbitCount n.planets)

orbitsToNodes :: Array Orbit -> OrbitNode
orbitsToNodes = orbitMapToNodes <<< A.foldl addOrbit M.empty

addOrbit :: OrbitMap -> Orbit -> OrbitMap
addOrbit m (Orbit s p) = case M.lookup s m of
  Nothing -> M.insert s [p] m
  Just ps -> M.insert s (A.snoc ps p) m

orbitMapToNodes :: OrbitMap -> OrbitNode
orbitMapToNodes m = orbitMapToNodes' m 0 "COM"
orbitMapToNodes' :: OrbitMap -> Int -> String -> OrbitNode
orbitMapToNodes' m count sname =
  let planets = fromMaybe [] $ M.lookup sname m
  in OrbitNode {sun:sname, planets: map (orbitMapToNodes' m (count+1)) planets, orbitCount: count}

readOrbit :: String -> Maybe Orbit
readOrbit s = do
  sun <- A.index parts 0
  planet <- A.index parts 1
  pure $ Orbit sun planet
  where
    parts = split (Pattern ")") s
