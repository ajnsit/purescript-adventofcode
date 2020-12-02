module Year2020.Day2.Main where

import Control.Applicative (pure)
import Control.Bind (bind, discard)
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
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)
import Util.Input (readInputLinesReader)
import Util.Parse (parseInt10)
import Util.Util (splitFirst)

main :: Effect Unit
main = do
  entries <- readInputLinesReader readPolicyAndPwd "src/Year2020/Day2/input"
  log $ "Number of valid entries with policy 1: " <> (show $ A.length $ A.filter valid entries)
  log $ "Number of valid entries with policy 2: " <> (show $ A.length $ A.filter valid2 entries)

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
