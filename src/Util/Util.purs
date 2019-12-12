module Util.Util where

import Data.Boolean (otherwise)
import Data.CommutativeRing ((+))
import Data.Either (Either(..))
import Data.Eq (class Eq, (==))
import Data.EuclideanRing ((-))
import Data.Foldable (foldl)
import Data.Ord ((>))
import Data.Show (show)
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
