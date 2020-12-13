module AOC.Lib where

import Control.Applicative (pure)
import Control.Bind (bind, discard, (=<<), (>>=))
import Control.Monad.ST as ST
import Control.Monad.ST.Ref as STRef
import Data.Array ((!!))
import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.ST as STA
import Data.Array.ST.Iterator as STAI
import Data.BigNumber (BigNumber, parseBigNumber)
import Data.Boolean (otherwise)
import Data.BooleanAlgebra (not)
import Data.CommutativeRing ((+))
import Data.Either (Either(..))
import Data.Eq (class Eq, (==))
import Data.EuclideanRing ((-))
import Data.Foldable (foldl)
import Data.Function (($))
import Data.Functor (void, (<$>))
import Data.Int (round)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NaturalTransformation (type (~>))
import Data.Ord ((<=), (>))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String as S
import Data.String.Pattern (Pattern)
import Data.Unit (Unit)
import Effect (Effect)
import Global (isNaN)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Unsafe.Coerce (unsafeCoerce)


--------------------------------------------------------------------------------
-- Utilities

-- | Apply a function repeatedly a specified number of times
iterate :: forall a. Int -> (a -> a) -> a -> a
iterate i f a
  | i > 0 = iterate (i-1) f (f a)
  | otherwise = a

-- | Successively run an array of functions on a value
pipeline :: forall a. a -> Array (a -> a) -> a
pipeline a fs = foldl (\a' f -> f a') a fs

-- | Apply a function until fixpoint
untilStable :: forall a. Eq a => (a -> a) -> a -> a
untilStable f a =
  let a' = f a
  in if a == a'
     then a
     else untilStable f a'

-- | Convert an either into a maybe
eitherToMaybe :: forall e a. Either e a -> Maybe a
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right a) = Just a

--------------------------------------------------------------------------------
-- Bignumber

-- HACKY!
-- | Convert an integer into a bignumber
intToBigNumber :: Int -> BigNumber
intToBigNumber x = case parseBigNumber (show x) of
  Left _ -> unsafeCoerce "IMPOSSIBLE: fromInt: INVALID BIGNUM!"
  Right y -> y

--------------------------------------------------------------------------------
-- Array Utilities

-- | Make an array with the specified start point,
-- | specified offset between elements, and the specified number of elements
mkArr :: Int -> Int -> Int -> Array Int
mkArr start offset items = go start items []
  where
  go curr i arr
    | i <= 0 = arr
    | otherwise = go (curr+offset) (items-1) (A.cons curr arr)

-- | Returns all final segments of the argument, longest first
tails :: forall a. Array a -> Array (Array a)
tails arr = case A.uncons arr of
  Nothing -> A.singleton arr
  Just x -> A.cons arr (tails x.tail)

-- | Count the number of elements in an array that match a predicate
countMatching :: forall a. (a -> Boolean) -> Array a -> Int
countMatching p = go 0
  where
  go n arr = case A.uncons arr of
    Nothing -> n
    Just x -> if p x.head then go (n+1) x.tail else go n x.tail

-- | Split an array into two parts:
-- |
-- | 1. the longest initial subarray for which all elements return a Just value
-- |    for the supplied function, and returns the values mapped to the Just values
-- | 2. the remaining elements
spanMap :: forall a b. (a -> Maybe b) -> Array a -> {init :: Array b, rest :: Array a}
spanMap p arr = go 0 []
  where
  go i prev = case A.index arr i of
    Nothing -> {init: prev, rest: []}
    Just x -> case p x of
      Nothing ->
        { init: prev
        , rest:
            if i == 0
              then arr
              else A.slice i (A.length arr) arr
        }
      Just b -> go (i + 1) (A.snoc prev b)

-- | Delete all the given indices from the array
-- | Skips over any invalid indices
deleteAll :: forall a. Array Int -> Array a -> Array a
deleteAll indices arr =
  foldl (\arr' x -> fromMaybe arr (A.deleteAt x arr')) arr indices

-- | Like Array.groupBy, except it only even compares consecutive elements
-- | As an example where Array.groupBy would not work - this groupBY can be used to group together runs of consecutive numbers
-- |   groupBySeq (\a b -> b-a == 1 ) [1,2,3,5,6,9,11,13,14] = [[1,2,3],[5,6],[9],[11],[13,14]]
-- | Most of this implementation was copied verbatim from the functions in `Data.Array`
groupBySeq :: forall a. (a -> a -> Boolean) -> Array a -> Array (NonEmptyArray a)
groupBySeq op xs =
  ST.run do
    result <- STA.empty
    iter <- STAI.iterator (xs !! _)
    STAI.iterate iter \x -> void do
      sub <- STA.empty
      cmp <- STRef.new x
      _ <- STA.push x sub
      pushWhile (\z -> do
                       b <- runOp op cmp z
                       _ <- STRef.write z cmp
                       pure b
                ) iter sub
      grp <- STA.unsafeFreeze sub
      STA.push ((unsafeCoerce :: Array ~> NonEmptyArray) grp) result
    STA.unsafeFreeze result
  where
  pushWhile :: forall r. (a -> ST.ST r Boolean) -> STAI.Iterator r a -> STA.STArray r a -> ST.ST r Unit
  pushWhile p iter array = do
    break <- STRef.new false
    ST.while (not <$> STRef.read break) do
      mx <- STAI.peek iter
      case mx of
        Just x -> do
          b <- p x
          if b then do
            _ <- STA.push x array
            void $ STAI.next iter
          else
            void $ STRef.write true break
        _ ->
          void $ STRef.write true break

  runOp :: forall r. (a -> a -> Boolean) -> STRef.STRef r a -> a -> ST.ST r Boolean
  runOp f r a = do
    b <- STRef.read r
    pure (f b a)

--------------------------------------------------------------------------------
-- Text splitting

-- | Split by a pattern once and return left and right params
-- | To make multiple splits, use `Data.String.split`
splitFirst :: Pattern -> String -> Maybe {left::String, right::String}
splitFirst p s = do
  let arr = S.split p s
  x <- A.uncons arr
  y <- A.uncons x.tail
  pure {left:x.head, right: y.head}

-- | Split a string into chunks of fixed length
chunk :: Int -> String -> Array String
chunk len contents
  | S.length contents <= 0 = []
  | otherwise =
      let res = S.splitAt len contents
      in A.cons res.before (chunk len res.after)

--------------------------------------------------------------------------------
-- Getting the input

foreign import getInputDirectory :: Effect String

-- | The location of the input file, given a year and day
inputFileLocationYearDay :: String -> String -> Effect String
inputFileLocationYearDay y d = do
  inputDirectory <- getInputDirectory
  pure $ inputDirectory <> "/year" <> y <> "/day" <> d

-- | Read the entire input file into a single string, given a year and day
readInputYearDay :: String -> String -> Effect String
readInputYearDay year day = readTextFile UTF8 =<< inputFileLocationYearDay year day

-- | Write the input file, given a year and day
writeInputYearDay :: String -> String -> String -> Effect Unit
writeInputYearDay year day contents = do
  loc <- inputFileLocationYearDay year day
  writeTextFile UTF8 loc contents


--------------------------------------------------------------------------------
-- Parsing

-- These return `Number` instead of `Int` because they can return `NaN`
foreign import unsafeParseIntBase :: String -> Int -> Number
foreign import unsafeParseInt10 :: String -> Number
foreign import unsafeParseFloat :: String -> Number

-- | Turn NaN's into Nothing
preventNaN :: Number -> Maybe Number
preventNaN n
  | isNaN n = Nothing
  | otherwise = Just n

-- | Parse an integer in specified base
parseIntBaseN :: Int -> String -> Maybe Int
parseIntBaseN n s = round <$> preventNaN (unsafeParseIntBase s n)

-- | Parse an integer
parseInt10 :: String -> Maybe Int
parseInt10 s = round <$> preventNaN (unsafeParseInt10 s)

-- | Parse a float
parseFloat :: String -> Maybe Number
parseFloat s = preventNaN (unsafeParseFloat s)
