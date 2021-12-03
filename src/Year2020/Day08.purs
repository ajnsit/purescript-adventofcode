module Year2020.Day08 where

import AOC.Lib (eitherToMaybe, parseInt10)
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Category ((<<<))
import Data.Array ((!!))
import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.Boolean (otherwise)
import Data.CommutativeRing ((+))
import Data.Eq ((==))
import Data.EuclideanRing ((-))
import Data.Field (negate)
import Data.Function (($))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Set as Set
import Data.Show (class Show, show)
import Data.String.Regex as SR
import Data.String.Regex.Flags as RF
import Data.String.Utils (lines)
import Data.Traversable (sequence)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)

--------------------------------------------------------------------------------
-- Write your solutions here

part1 :: String -> Effect Unit
part1 input = do
  let instrs = A.mapMaybe parseInstr $ lines input
  log $ "Part 1 ==> " <> (show $ run instrs)

part2 :: String -> Effect Unit
part2 input = do
  let instrs = A.mapMaybe parseInstr $ lines input
  log $ "Part 2 ==> " <> (show $ run2 instrs)

--------------------------------------------------------------------------------

data Instr = Acc Int | Jmp Int | Nop Int

instance showInstr :: Show Instr where
  show (Acc i) = "acc " <> show i
  show (Jmp i) = "jmp " <> show i
  show (Nop i) = "nop " <> show i

regexFlags :: RF.RegexFlags
regexFlags = RF.noFlags

parseInstr :: String -> Maybe Instr
parseInstr s = do
  instrRegex <- eitherToMaybe $ SR.regex "(acc|nop|jmp) (\\+|-)(\\d+)" regexFlags
  nem <- SR.match instrRegex s
  let m' = NEA.tail nem
  m <- sequence m'
  scmd <- A.index m 0
  ssign <- A.index m 1
  sint <- A.index m 2
  int' <- parseInt10 sint
  let int = if ssign == "-" then negate int' else int'
  pure $ case scmd of
    "acc" -> Acc int
    "nop" -> Nop int
    "jmp" -> Jmp int
    _ -> Nop int

type State =
  { ip :: Int
  , acc :: Int
  }

-- Nothing means out of bounds (end of file)
step :: Array Instr -> State -> Maybe State
step instrs st = do
  instr <- A.index instrs st.ip
  case instr of
    Acc i -> pure $ st {ip= st.ip+1, acc= st.acc+i}
    Jmp i -> pure $ st {ip= st.ip+i}
    Nop _ -> pure $ st {ip= st.ip+1}

mutate :: Int -> Array Instr -> Maybe (Array Instr)
mutate i arr = do
  instr <- arr !! i
  instr' <- mutateInstr instr
  A.updateAt i instr' arr
  where
  mutateInstr (Jmp n) = Just $ Nop n
  mutateInstr (Nop n) = Just $ Jmp n
  mutateInstr instr = Nothing

allMutations :: Array Instr -> Array (Array Instr)
allMutations instrs =
  A.mapMaybe (\i -> mutate i instrs) (A.range 0 (A.length instrs - 1))

data Terminate = EOF Int | Rerun Int
instance showTerminate :: Show Terminate where
  show (EOF i) = "EOF<" <> show i <> ">"
  show (Rerun i) = "Rerun<" <> show i <> ">"

run :: Array Instr -> Terminate
run instrs = go {ip:0, acc:0} Set.empty
  where
  go st prev
    | st.ip `Set.member` prev = Rerun st.acc
    | otherwise =
        let mst' = step instrs st
        in case mst' of
          Nothing -> EOF st.acc
          Just st' -> go st' (Set.insert st.ip prev)

run2 :: Array Instr -> Array Int
run2 instrs =
  A.mapMaybe (keepEOFs <<< run) (allMutations instrs)
  where
  keepEOFs (EOF i) = Just i
  keepEOFs _ = Nothing
