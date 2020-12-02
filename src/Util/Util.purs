module Util.Util where

import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Array as A
import Data.Boolean (otherwise)
import Data.Either (Either(..))
import Data.EuclideanRing ((-))
import Data.Foldable (foldl)
import Data.Maybe (Maybe)
import Data.Ord ((>))
import Data.Show (show)
import Data.String (split)
import Data.String.Pattern (Pattern)
import Unsafe.Coerce (unsafeCoerce)
import Util.BigNum (BigNumber, parseBigNumber)

pipeline :: forall a. a -> Array (a -> a) -> a
pipeline a fs = foldl (\a' f -> f a') a fs

iterate :: forall a. Int -> (a -> a) -> a -> a
iterate i f a
  | i > 0 = iterate (i-1) f (f a)
  | otherwise = a

-- HACKY!
fromInt :: Int -> BigNumber
fromInt x = case parseBigNumber (show x) of
  Left _ -> unsafeCoerce "INVALID BIGNUM!"
  Right y -> y

-- Split by a pattern once and return left and right params
splitFirst :: Pattern -> String -> Maybe {left::String, right::String}
splitFirst p s = do
  let arr = split p s
  x <- A.uncons arr
  y <- A.uncons x.tail
  pure {left:x.head, right: y.head}
