module Year2020.Day13 where

import AOC.Lib (intToBigNumber, parseInt10)
import AOC.Stream as Stream
import Control.Bind ((=<<))
import Data.Array as A
import Data.BigNumber as BN
import Data.Boolean (otherwise)
import Data.CommutativeRing ((*), (+))
import Data.Eq ((==))
import Data.EuclideanRing (mod)
import Data.Field (negate)
import Data.Foldable (foldl)
import Data.Function (($))
import Data.Functor (map)
import Data.Maybe (fromMaybe)
import Data.Ord ((<))
import Data.Ring ((-))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String as S
import Data.String.Utils (lines)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
-- Write your solutions here

part1 :: String -> Effect Unit
part1 input = do
  let res = lines input
  let earliest = fromMaybe 0 $ parseInt10 =<< A.head res
  let buses = A.mapMaybe parseInt10 $ S.split (S.Pattern ",") $ fromMaybe "" $ A.head $ A.drop 1 res
  let times = map (\b -> Tuple b (minTime earliest b)) buses
  let x = fromMaybe (unsafeCoerce "NO MIN TIME") $ A.head $ A.sortWith Tuple.snd times
  let y = Tuple.fst x
  let z = Tuple.snd x
  log $ "Part 1 ==> " <> show (y * (z - earliest))
  where
  minTime earliest interval = earliest + interval - mod earliest interval

part2 :: String -> Effect Unit
part2 input = do
  let res = lines input
  let buses = map parseInt10 $ S.split (S.Pattern ",") $ fromMaybe "" $ A.head $ A.drop 1 res
  let busOffsets = A.mapMaybe (\ (Tuple i b) -> map (\b' -> Tuple (intToBigNumber b') (intToBigNumber i)) b) $ A.zip (A.range 0 (A.length buses)) buses
  let one = intToBigNumber 1
  log $ "Part 2 ==> " <> show (Tuple.snd $ foldl matchBus (Tuple (negate one) (negate one)) busOffsets)

--------------------------------------------------------------------------------

matchBus :: (Tuple BN.BigNumber BN.BigNumber) -> (Tuple BN.BigNumber BN.BigNumber) -> (Tuple BN.BigNumber BN.BigNumber)
matchBus (Tuple b1 o1) (Tuple b2 o2)
  | b1 < (intToBigNumber 0) = Tuple b2 o2
  | otherwise =
    Tuple (b1*b2) $ Stream.find (\t -> ((t + o2) `mod` b2) == zer) $ Stream.inf o1 b1
  where zer = intToBigNumber 0
