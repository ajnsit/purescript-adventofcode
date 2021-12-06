module Year2021.Day03 where

import AOC.Lib (until)

import Control.Apply ((<*>))
import Control.Category ((<<<))
import Data.Array as A
import Data.CommutativeRing ((*), (+))
import Data.Eq ((==))
import Data.EuclideanRing ((-))
import Data.Function ((#), ($))
import Data.Functor (map, (<$>))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Ord ((<), (<=), (>=))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String.CodeUnits as String
import Data.String.Utils (lines)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)

--------------------------------------------------------------------------------
-- Write your solutions here

part1 :: String -> Effect Unit
part1 input = do
  let
    entries = A.mapMaybe parseBinary $ lines input
    indices = A.range 0 <$> ((_-1) <<< A.length) <$> A.head entries
    digits = map (\i -> bitsAt i entries) <$> indices
    gamma = map most <$> digits
    epsilon = map least <$> digits
    result = multBinary <$> gamma <*> epsilon
  log $ "Part 1 ==> " <> show result

part2 :: String -> Effect Unit
part2 input = do
  let
    entries = A.mapMaybe parseBinary $ lines input
    oxygenGeneratorRating = filterBy most entries
    co2ScrubberRating = filterBy least entries
    result = multBinary <$> oxygenGeneratorRating <*> co2ScrubberRating
  log $ "Part 2 ==> " <> show result

--------------------------------------------------------------------------------

type Binary = Array Boolean
parseBinary :: String -> Maybe Binary
parseBinary = traverse toBit <<< String.toCharArray

toBit :: Char -> Maybe Boolean
toBit '0' = Just false
toBit '1' = Just true
toBit _ = Nothing

fromBit :: Boolean -> Char
fromBit false = '0'
fromBit true = '1'

multBinary :: Binary -> Binary -> Maybe Int
multBinary xs ys = (*) <$> fromBinary xs <*> fromBinary ys

fromBinary :: Binary -> Maybe Int
fromBinary = Int.fromStringAs Int.binary <<< String.fromCharArray <<< map fromBit

bitsAt :: forall a. Int -> Array (Array a) -> Array a
bitsAt i = A.mapMaybe (\arr -> A.index arr i)

-- most :: Array Binary -> Maybe Boolean
-- most arr = A.head =<< maximumBy (comparing A.length) arr

most :: Binary -> Boolean
most b = A.length (ones b) >= A.length (zeroes b)
  -- fst <- A.head $ A.groupBy ((==) `on` A.length) $ A.sortBy (flip (comparing A.length)) [zeroes b, ones b]
  -- if NEA.length fst > 1 then Just true else A.head (NEA.head fst)

-- least :: Array Binary -> Maybe Boolean
-- least arr = A.head =<< minimumBy (comparing A.length) arr

least :: Binary -> Boolean
least b = A.length (ones b) < A.length (zeroes b)
  -- fst <- A.head $ A.groupBy ((==) `on` A.length) $ A.sortBy (comparing A.length) [zeroes b, ones b]
  -- if NEA.length fst > 1 then Just false else A.head (NEA.head fst)

ones :: Binary -> Binary
ones = A.filter (_ == true)

zeroes :: Binary -> Binary
zeroes = A.filter (_ == false)

filterBy :: (Binary -> Boolean) -> Array Binary -> Array Binary
filterBy pick entries = fst $ until p f (Tuple entries 0)
  where
  p = (_ <= 1) <<< A.length <<< fst
  f (Tuple nums idx) = Tuple (filterByIdx pick idx nums) (idx+1)

filterByIdx :: (Binary -> Boolean) -> Int -> Array Binary -> Array Binary
filterByIdx pick index entries =
  let bits = bitsAt index entries
  in entries # A.filter (\arr -> A.index arr index == Just (pick bits))
