module Year2021.Day08 where

import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Category ((<<<))
import Data.Array as A
import Data.BooleanAlgebra ((||))
import Data.CommutativeRing ((*), (+))
import Data.Eq ((==))
import Data.Foldable (foldl, sum)
import Data.Function (($))
import Data.Functor (map, (<$>))
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String as S
import Data.String.CodeUnits as SC
import Data.String.Pattern (Pattern(..))
import Data.String.Utils (lines, words)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)

--------------------------------------------------------------------------------
-- Write your solutions here

part1 :: String -> Effect Unit
part1 input = do
  let
    inps = map (map words <<< S.split (Pattern "|")) (lines input)
    inp = A.mapMaybe (_ A.!! 1) inps
    result = A.length $ A.filter (\s -> S.length s `A.elem` uniqueNumberOfSegments) (A.concat inp)
  log $ "Part 1 ==> " <> show result

part2 :: String -> Effect Unit
part2 input = do
  let
    inps = map (map words <<< S.split (Pattern "|")) (lines input)
    result = sum $ decodeValid inps
  log $ "Part 2 ==> " <> show result

--------------------------------------------------------------------------------

validCombinationsForInput :: Array String -> Array (Map Char Int)
validCombinationsForInput inp =
  A.filter (isValidCombination inp) possibleCombinations

uniqueNumberOfSegments :: Array Int
uniqueNumberOfSegments = A.mapMaybe (A.index digitLengths) [1,4,7,8]

possibleCombinations :: Array (Map Char Int)
possibleCombinations = do
  a <- A.range 0 6
  b <- A.filter (_ `A.notElem` [a]) $ A.range 0 6
  c <- A.filter (_ `A.notElem` [a,b]) $ A.range 0 6
  d <- A.filter (_ `A.notElem` [a,b,c]) $ A.range 0 6
  e <- A.filter (_ `A.notElem` [a,b,c,d]) $ A.range 0 6
  f <- A.filter (_ `A.notElem` [a,b,c,d,e]) $ A.range 0 6
  g <- A.filter (_ `A.notElem` [a,b,c,d,e,f]) $ A.range 0 6
  pure $ M.fromFoldable
    [ Tuple 'a' a
    , Tuple 'b' b
    , Tuple 'c' c
    , Tuple 'd' d
    , Tuple 'e' e
    , Tuple 'f' f
    , Tuple 'g' g
    ]

digits :: Array (Array Int)
digits =
  [ [a,b,c,e,f,g] -- 0
  , [c,f] -- 1
  , [a,c,d,e,g] -- 2
  , [a,c,d,f,g] -- 3
  , [b,c,d,f] -- 4
  , [a,b,d,f,g] -- 5
  , [a,b,d,e,f,g] -- 6
  , [a,c,f] -- 7
  , [a,b,c,d,e,f,g] -- 8
  , [a,b,c,d,f,g] -- 9
  ]
  where
    a = 0
    b = 1
    c = 2
    d = 3
    e = 4
    f = 5
    g = 6

digitLengths :: Array Int
digitLengths = A.length <$> digits

isValidCombination :: Array String -> Map Char Int -> Boolean
isValidCombination strs combination = A.all (isValidStr combination) strs

isValidStr :: Map Char Int -> String -> Boolean
isValidStr combination str = str == "" || A.elem digit digits
  where
  digit :: Array Int
  digit = A.sort $ A.mapMaybe
    (\k -> M.lookup k combination)
    (SC.toCharArray str)

toDigit :: Map Char Int -> String -> Maybe Int
toDigit comb str = A.findIndex (_ == digit) digits
  where
  digit :: Array Int
  digit = A.sort $ A.mapMaybe
    (\k -> M.lookup k comb)
    (SC.toCharArray str)

decodeValid :: Array (Array (Array String)) -> Array Int
decodeValid = A.mapMaybe \inp -> do
  combs <- validCombinationsForInput <$> A.head inp
  comb <- A.head combs
  digsArr <- A.mapMaybe (toDigit comb) <$> inp A.!! 1
  pure (toDecimal digsArr)

toDecimal :: Array Int -> Int
toDecimal = foldl f 0
  where f n i = n * 10 + i
