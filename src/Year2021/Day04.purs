module Year2021.Day04 where

import AOC.Lib (chunksOf, parseInt10, untilEither)
import Control.Category ((<<<))
import Data.Array as A
import Data.BooleanAlgebra (not, (&&), (||))
import Data.CommutativeRing ((*))
import Data.Either (Either(..))
import Data.Eq (class Eq, (==))
import Data.EuclideanRing ((-))
import Data.Foldable (foldMap)
import Data.Function ((#), ($))
import Data.Functor (map, (<$>))
import Data.Functor.Compose (Compose(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String as S
import Data.String.Pattern (Pattern(..))
import Data.String.Utils (lines, words)
import Data.Tuple (Tuple(..), snd)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)

--------------------------------------------------------------------------------
-- Write your solutions here

part1 :: String -> Effect Unit
part1 input = do
  let
    lins = lines input
    numline = fromMaybe "" $ A.head lins
    nums = A.mapMaybe parseInt10 $ S.split (Pattern ",") numline
    boardlines = A.drop 2 lins
    boards' = map (A.mapMaybe parseInt10 <<< words) <<< A.take 5 <$> chunksOf 6 boardlines
    boards = map fromInitial boards'
    result = winScore <$> winState nums boards
  log $ "Part 1 ==> " <> show result

part2 :: String -> Effect Unit
part2 input = do
  let
    lins = lines input
    numline = fromMaybe "" $ A.head lins
    nums = A.mapMaybe parseInt10 $ S.split (Pattern ",") numline
    boardlines = A.drop 2 lins
    boards' = map (A.mapMaybe parseInt10 <<< words) <<< A.take 5 <$> chunksOf 6 boardlines
    boards = map fromInitial boards'
    result = winScore <$> loseState nums boards
  log $ "Part 2 ==> " <> show result

--------------------------------------------------------------------------------

type Marked a = Tuple a Boolean
type Row a = Array (Marked a)
type Board a = Array (Row a)

unmarked :: forall a. a -> Marked a
unmarked a = Tuple a false

markIf :: forall a. Eq a => a -> Marked a -> Marked a
markIf a (Tuple b m) = if a == b then (Tuple b true) else (Tuple b m)

fromInitial :: forall a. Array (Array a) -> Board a
fromInitial = map (map unmarked)

step :: forall a. Eq a => a -> Board a -> Board a
step = map <<< map <<< markIf

isMarked :: forall a. Marked a -> Boolean
isMarked = snd

isWin :: forall a. Board a -> Boolean
isWin board = horz || vert
  where
  horz = board # A.any (A.all isMarked)
  vert = board # transpose # A.any (A.all isMarked)

elemsAt :: forall a. Int -> Array (Array a) -> Array a
elemsAt i = A.mapMaybe (\arr -> A.index arr i)

transpose :: forall a. Array (Array a) -> Array (Array a)
transpose arr = map (\i -> elemsAt i arr) (A.range 0 (len - 1))
  where
  len = fromMaybe 0 $ A.length <$> A.head arr

unmarkedSum :: Board Int -> Int
unmarkedSum = unwrap <<< foldMap (\ (Tuple x m) -> Additive $ if m then 0 else x) <<< Compose

winScore :: { board :: Board Int, last :: Int } -> Int
winScore {board, last} = unmarkedSum board * last

winState :: Array Int -> Array (Board Int) -> Maybe { board :: Board Int, last :: Int }
winState n b = untilEither step' { boards: b, nums: n }
  where
  step' { boards, nums } = case A.uncons nums of
    Nothing -> Right Nothing
    Just x -> do
      let newboards = map (step x.head) boards
      case A.find isWin newboards of
        Nothing -> Left { boards: newboards, nums: x.tail }
        Just winBoard -> Right (Just { board: winBoard, last: x.head })

loseState :: Array Int -> Array (Board Int) -> Maybe { board :: Board Int, last :: Int }
loseState n b = untilEither step' { boards: b, nums: n }
  where
  step' { boards, nums } =
    case A.uncons nums of
      Nothing -> Right Nothing
      Just x -> do
        let newboards = map (step x.head) boards
        case A.uncons newboards of
          Nothing -> Right Nothing
          Just y ->
            if A.null y.tail && isWin y.head
              then Right (Just { board: y.head, last: x.head })
              else Left { boards: A.filter (not <<< isWin) newboards, nums: x.tail }
