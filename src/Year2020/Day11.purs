module Year2020.Day11 where

import AOC.Lib (countMatching, untilStable)
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Category (identity, (<<<))
import Data.Array as A
import Data.Boolean (otherwise)
import Data.CommutativeRing ((+))
import Data.Eq (class Eq, (==))
import Data.EuclideanRing ((-))
import Data.Foldable (sum)
import Data.Function (($))
import Data.Functor (map, (<$>))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord ((>=))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.String.CodePoints as S
import Data.String.Utils (lines)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)

--------------------------------------------------------------------------------
-- Write your solutions here

part1 :: String -> Effect Unit
part1 input = do
  let area = parseArea input
  let res = untilStable step area
  log $ "Part 1 ==> " <> show (nmatching Taken res)

part2 :: String -> Effect Unit
part2 input = do
  let area = parseArea input
  let res = untilStable step2 area
  log $ "Part 2 ==> " <> show (nmatching Taken res)

--------------------------------------------------------------------------------

type Pos = {x::Int, y::Int}
data Seat = Empty | Taken | Floor
type Area = Array (Array Seat)

instance showSeat :: Show Seat where
  show Empty = "L"
  show Taken = "#"
  show Floor = "."

instance eqSeat :: Eq Seat where
  eq Empty Empty = true
  eq Taken Taken = true
  eq Floor Floor = true
  eq _ _ = false

nmatching :: Seat -> Area -> Int
nmatching s area = sum $ map (countMatching (_==s)) area

getPos :: Pos -> Area -> Maybe Seat
getPos {x,y} area = do
  row <- A.index area x
  A.index row y

parseArea :: String -> Area
parseArea s = A.mapMaybe charToSeat <$> rows
  where
  rows = S.toCodePointArray <$> lines s
  charToSeat c
    | c == S.codePointFromChar 'L' = Just Empty
    | c == S.codePointFromChar '#' = Just Taken
    | c == S.codePointFromChar '.' = Just Floor
    | otherwise = Nothing

step :: Area -> Area
step area = stepRow <$> nrows
  where
  stepRow x = A.mapMaybe (stepCell x) ncols
  stepCell x y = do
    s <- getPos {x,y} area
    let nbrs = A.length $ A.mapMaybe (\p -> isOccupied (getPos p area)) (neighbors {x,y})
    pure $ case s,nbrs of
      Empty,0 -> Taken
      Taken,n -> if n >= 4 then Empty else Taken
      e,_ -> e
  isOccupied (Just Taken) = Just Taken
  isOccupied _ = Nothing
  nrows = A.range 0 (A.length area - 1)
  ncols = fromMaybe [] $ A.range 0 <<< A.length <$> A.head area

step2 :: Area -> Area
step2 area = stepRow <$> nrows
  where
  stepRow x = A.mapMaybe (stepCell x) ncols
  stepCell x y = do
    s <- getPos {x,y} area
    let nbrs = countMatching (_==Taken) (neighborsInEveryDirection {x,y} area)
    pure $ case s,nbrs of
      Empty,0 -> Taken
      Taken,n -> if n >= 5 then Empty else Taken
      e,_ -> e
  isOccupied (Just Taken) = Just Taken
  isOccupied _ = Nothing
  nrows = A.range 0 (A.length area - 1)
  ncols = fromMaybe [] $ A.range 0 <<< A.length <$> A.head area

neighbors :: Pos -> Array Pos
neighbors {x,y} =[{x:x+1,y:y+1}, {x:x+1,y:y}, {x:x+1,y:y-1}
                 ,{x:x,y:y+1}               , {x:x,y:y-1}
                 ,{x:x-1,y:y+1}, {x:x-1,y:y}, {x:x-1,y:y-1}
                 ]

-- Direction = offset from prev pos
type Dir = {x::Int->Int, y::Int->Int}
one :: Int -> Int
one = (_+1)
zer :: Int -> Int
zer = identity
neg :: Int -> Int
neg = (_-1)

findSeatInDir :: Pos -> Dir -> Area -> Maybe Seat
findSeatInDir {x,y} {x:f,y:g} area = do
  let pos = {x:f x, y: g y}
  s <- getPos pos area
  if s == Taken
  then pure Taken
  else if s == Empty
       then pure Empty
       else findSeatInDir pos {x:f,y:g} area

neighborsInEveryDirection :: Pos -> Area -> Array Seat
neighborsInEveryDirection {x,y} area =
  A.mapMaybe (\dir -> findSeatInDir {x,y} dir area) dirs
  where
  dirs =
    [{x:one,y:one}, {x:one,y:zer}, {x:one,y:neg}
    ,{x:zer,y:one}               , {x:zer,y:neg}
    ,{x:neg,y:one}, {x:neg,y:zer}, {x:neg,y:neg}
    ]
