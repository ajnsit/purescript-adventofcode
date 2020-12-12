module Year2020.Day12 where

import AOC.Lib (parseInt10)
import Control.Bind ((=<<))
import Control.Category ((<<<))
import Data.Array as A
import Data.CommutativeRing ((*), (+))
import Data.EuclideanRing (mod, (-))
import Data.Field (negate)
import Data.Foldable (foldl)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (abs)
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
  let instrs = A.mapMaybe parseInstr $ lines input
  -- let unparseables = A.filter isNothing $ map parseInstr $ lines input
  let finalSt = foldl move initSt instrs
  log $ "Part 1 ==> " <> (show $ abs finalSt.x + abs finalSt.y)

part2 :: String -> Effect Unit
part2 input = do
  let instrs = A.mapMaybe parseInstr $ lines input
  let finalSt = foldl move2 initSt instrs
  -- finalSt <- foldl (\mst instr -> do
  --                         st <- mst
  --                         let st' = move st instr
  --                         log $ show st'
  --                         pure st'
  --                         ) (pure initSt) instrs
  log $ "Part 2 ==> " <> (show $ abs finalSt.x + abs finalSt.y)

--------------------------------------------------------------------------------

initSt :: State
initSt = {dir:D0, x:0, y:0, wp:{x:10,y:1}}

parseInstr :: String -> Maybe Instr
parseInstr s =
  let x = S.take 1 s
  in match x =<< (parseInt10 $ S.drop 1 s)

match :: String -> Int -> Maybe Instr
match "L" d = L <$> intToDir d
match "R" d = R <$> intToDir d
match "F" d = Just $ F d
match "N" d = Just $ N d
match "S" d = Just $ S d
match "E" d = Just $ E d
match "W" d = Just $ W d
-- NOP
match s d = Nothing

data Instr = L Dir | R Dir | N Amount | S Amount | E Amount | W Amount | F Amount

instance showInstr :: Show Instr where
  show (L d) = "L" <> show d
  show (R d) = "R" <> show d
  show (F d) = "F" <> show d
  show (N d) = "N" <> show d
  show (S d) = "S" <> show d
  show (E d) = "E" <> show d
  show (W d) = "W" <> show d

instance showDir :: Show Dir where
  show dir = show (dirToInt dir)

-- Clockwise angles from start position (East)
data Dir = D0 | D90 | D180 | D270
type Amount = Int

dirToInt :: Dir -> Int
dirToInt D0 = 0
dirToInt D90 = 90
dirToInt D180 = 180
dirToInt D270 = 270

intToDir :: Int -> Maybe Dir
intToDir = intToDir' <<< mkMod
  where
  mkMod x = mod x 360
  intToDir' 0 = Just D0
  intToDir' 90 = Just D90
  intToDir' 180 = Just D180
  intToDir' 270 = Just D270
  intToDir' _ = Nothing

dirToOffset :: Dir -> Amount -> {x::Int, y::Int}
dirToOffset D0 x = {x, y:0}
dirToOffset D90 y = {x:0, y:negate y}
dirToOffset D180 x = {x: negate x, y:0}
dirToOffset D270 y = {x:0, y}

addDir :: Dir -> Dir -> Dir
addDir d1 d2 = fromMaybe d1 $ intToDir $ dirToInt d1 + dirToInt d2

subDir :: Dir -> Dir -> Dir
subDir d1 d2 = fromMaybe d1 $ intToDir $ dirToInt d1 - dirToInt d2

type State = {dir::Dir, x::Int, y::Int, wp::{x::Int, y::Int}}

type Pos = {x::Int, y::Int}

rotate :: Pos -> Dir -> Pos
rotate pos D0 = pos
rotate pos D90 = {y:negate pos.x, x:pos.y}
rotate pos D180 = {y:negate pos.y, x:negate pos.x}
rotate pos D270 = {x:negate pos.y, y:pos.x}

move :: State -> Instr -> State
move st (L newdir) = st {dir = subDir st.dir newdir}
move st (R newdir) = st {dir = addDir st.dir newdir}
move st (F amount) =
  let offset = dirToOffset st.dir amount
  in st {x=st.x+offset.x, y=st.y+offset.y}
move st (N amount) =
  let offset = {x:0, y:amount}
  in st {x=st.x+offset.x, y=st.y+offset.y}
move st (S amount) =
  let offset = {x:0, y:negate amount}
  in st {x=st.x+offset.x, y=st.y+offset.y}
move st (E amount) =
  let offset = {x:amount, y:0}
  in st {x=st.x+offset.x, y=st.y+offset.y}
move st (W amount) =
  let offset = {x:negate amount, y:0}
  in st {x=st.x+offset.x, y=st.y+offset.y}

move2 :: State -> Instr -> State
move2 st (L d) = st {wp = rotate st.wp (subDir D0 d)}
move2 st (R d) = st {wp = rotate st.wp d}
move2 st (F amount) =
  let offset = {x:st.wp.x*amount, y:st.wp.y*amount}
  in st {x=st.x+offset.x, y=st.y+offset.y}
move2 st (N amount) =
  let offset = {x:0, y:amount}
  in st {wp = {x:st.wp.x+offset.x, y:st.wp.y+offset.y}}
move2 st (S amount) =
  let offset = {x:0, y:negate amount}
  in st {wp = {x:st.wp.x+offset.x, y:st.wp.y+offset.y}}
move2 st (E amount) =
  let offset = {x:amount, y:0}
  in st {wp = {x:st.wp.x+offset.x, y:st.wp.y+offset.y}}
move2 st (W amount) =
  let offset = {x:negate amount, y:0}
  in st {wp = {x:st.wp.x+offset.x, y:st.wp.y+offset.y}}
