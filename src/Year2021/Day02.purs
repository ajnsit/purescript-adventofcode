module Year2021.Day02 where

import AOC.Lib (eitherToMaybe, parseInt10)
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.CommutativeRing ((*), (+))
import Data.EuclideanRing ((-))
import Data.Foldable (foldl)
import Data.Function (($))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.String.Regex as SR
import Data.String.Regex.Flags as RF
import Data.String.Utils (lines)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)

--------------------------------------------------------------------------------
-- Write your solutions here

part1 :: String -> Effect Unit
part1 input = do
  let
    entries = A.mapMaybe parseMovement $ lines input
    finalPos = foldl updateMovement (Pos 0 0) entries
  log "PART 1 ------"
  log $ "Final Pos = " <> show finalPos
  log $ "Result = " <> show (case finalPos of Pos x y -> x * y)

part2 :: String -> Effect Unit
part2 input = do
  let
    entries = A.mapMaybe parseMovement $ lines input
    Tuple finalAim finalPos = foldl updateAimMovement (Tuple 0 (Pos 0 0)) entries
  log "PART 2 ------"
  log $ "Final Pos = " <> show finalPos
  log $ "Result = " <> show (case finalPos of Pos x y -> x * y)

--------------------------------------------------------------------------------

data Movement = Up Int | Down Int | Forward Int
instance showMovement :: Show Movement where
  show (Up i) = "Up " <> show i
  show (Down i) = "Down " <> show i
  show (Forward i) = "Forward " <> show i

data Pos = Pos Int Int
instance showPos :: Show Pos where
  show (Pos x y) = "Pos " <> show x <> " " <> show y

updateMovement :: Pos -> Movement -> Pos
updateMovement (Pos depth horz) = case _ of
  Up x -> Pos (depth - x) horz
  Down x -> Pos (depth + x) horz
  Forward x -> Pos depth (horz + x)

type Aim = Int
updateAimMovement :: Tuple Aim Pos -> Movement -> Tuple Aim Pos
updateAimMovement (Tuple aim (Pos depth horz)) = case _ of
  Up x -> Tuple (aim - x) $ Pos depth horz
  Down x -> Tuple (aim + x) $ Pos depth horz
  Forward x -> Tuple aim $ Pos (depth + (aim * x)) (horz + x)

regexFlags :: RF.RegexFlags
regexFlags = RF.noFlags

parseMovement :: String -> Maybe Movement
parseMovement s = do
  instrRegex <- eitherToMaybe $ SR.regex "(up|down|forward) (\\d+)" regexFlags
  nem <- SR.match instrRegex s
  let m' = NEA.tail nem
  m <- sequence m'
  sdir <- A.index m 0
  sint <- A.index m 1
  int <- parseInt10 sint
  case sdir of
    "up" -> pure $ Up int
    "down" -> pure $ Down int
    "forward" -> pure $ Forward int
    _ -> Nothing
