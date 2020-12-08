module Year2020.Day02 where

import AOC.Lib (parseInt10, splitFirst)
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Category ((<<<))
import Data.Array as A
import Data.BooleanAlgebra ((&&))
import Data.CommutativeRing ((+))
import Data.Eq ((==))
import Data.Foldable (foldl)
import Data.Function (($))
import Data.Functor (map)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (class Ord, (<=), (>=))
import Data.Ring ((-))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String as S
import Data.String.Pattern (Pattern(..))
import Data.String.Utils (lines)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)


--------------------------------------------------------------------------------
-- Write your solutions here

part1 :: String -> Effect Unit
part1 input = do
  let entries = A.mapMaybe readPolicyAndPwd $ lines input
  log $ "Part 1 ==> Number of valid entries with policy 1: " <> (show $ A.length $ A.filter valid entries)

part2 :: String -> Effect Unit
part2 input = do
  let entries = A.mapMaybe readPolicyAndPwd $ lines input
  log $ "Part 2 ==> Number of valid entries with policy 2: " <> (show $ A.length $ A.filter valid2 entries)

--------------------------------------------------------------------------------

type PolicyAndPwd =
  { pwd :: String
  , policy :: Policy
  }

type Policy =
  { ch :: S.CodePoint
  , min :: Int
  , max :: Int
  }

readPolicyAndPwd :: String -> Maybe PolicyAndPwd
readPolicyAndPwd s = do
  {left:spolicy, right:spwd} <- splitFirst (Pattern ": ") s
  policy <- readPolicy spolicy
  pure {pwd: spwd, policy: policy}

readPolicy :: String -> Maybe Policy
readPolicy s = do
  {left:range, right:chr} <- splitFirst (Pattern " ") s
  {left:smin, right:smax} <- splitFirst (Pattern "-") range
  min <- parseInt10 smin
  max <- parseInt10 smax
  x <- S.uncons chr
  pure {ch: x.head, min: min, max: max}

valid :: PolicyAndPwd -> Boolean
valid pp =
  let n = getCount pp.policy.ch (charCounts pp.pwd)
  in (n <= pp.policy.max && n >= pp.policy.min)

valid2 :: PolicyAndPwd -> Boolean
valid2 pp =
  let matchingChars = A.filter (_ == pp.policy.ch) chars
      chars = A.catMaybes $ map charAt [pp.policy.min, pp.policy.max]
      -- IMPORTANT! 1-indexed
      charAt n = S.codePointAt (n-1) pp.pwd
  in A.length matchingChars == 1

getCount :: forall a. Ord a => a -> Map a Int -> Int
getCount a m = fromMaybe 0 (M.lookup a m)

charCounts :: String -> Map S.CodePoint Int
charCounts = foldl (\m c -> M.alter f c m) M.empty <<< S.toCodePointArray
  where
  f Nothing = Just 1
  f (Just v) = Just (v+1)
