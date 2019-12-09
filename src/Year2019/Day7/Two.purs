module Year2019.Day7.Two where

import Control.Alternative (pure)
import Control.Bind (bind, discard, (>>=))
import Control.Category ((<<<))
import Control.MonadPlus (guard)
import Data.Array as A
import Data.Boolean (otherwise)
import Data.Eq ((/=), (==))
import Data.EuclideanRing (div, mod, (-))
import Data.Field ((*), (+))
import Data.Foldable (maximum)
import Data.Function (($))
import Data.Functor (map)
import Data.Int (pow)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord ((<), (>=))
import Data.Show (show)
import Data.String.Pattern (Pattern(..))
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)
import Unsafe.Coerce (unsafeCoerce)
import Util.Input (readInputSep)
import Util.Parse (parseInt10)

main :: Effect Unit
main = do
  inpStr <- readInputSep (Pattern ",") "inputs/2019/7/1"
  let program = map (fromMaybe 0 <<< parseInt10) inpStr
  let startStates = map (settingToStartState program) possibleSettings
  log $ show $ maximum $ map runAmplifierArray startStates

type AmpPhaseSettings = Array Int
type AmpState = {preInput::Array Int, program::Array Int, pointer::Int}
type AmpStates = Array AmpState
type AmpResult = {out::Int, state::AmpState}
type AmpResults = Array AmpResult

possibleSettings :: Array AmpPhaseSettings
possibleSettings = go []
  where
  go prev
    | A.length prev >= 5 = pure prev
    | otherwise = do
      i <- 5 A... 9
      guard $ A.notElem i prev
      go (A.snoc prev i)

validSettings :: AmpPhaseSettings -> Boolean
validSettings arr = A.length arr == A.length (A.nub arr)

settingToStartState :: Array Int -> AmpPhaseSettings -> AmpStates
settingToStartState program = map \s -> {preInput:[s], program:program, pointer:0}

runAmplifierArray :: AmpStates -> Int
runAmplifierArray = go 0
  where
  go prev states = case runAmplifierArrayOnce prev states of
    Nothing -> prev
    Just res -> case A.last res of
      Nothing -> prev
      Just r -> go r.out (map _.state res)

runAmplifierArrayOnce :: Int -> AmpStates -> Maybe AmpResults
runAmplifierArrayOnce i states = case A.uncons states of
  Nothing -> pure []
  Just x -> do
    first <- runAmplifier x.head i
    rest <- runAmplifierArrayOnce first.out x.tail
    pure (A.cons first rest)

runAmplifier :: AmpState -> Int -> Maybe AmpResult
runAmplifier st i = do
  out <- runIntCode {arr: st.program, ins:A.snoc st.preInput i, outs:[], pointer:st.pointer}
  o <- A.head out.outs
  pure {out:o, state:st{program=out.arr, pointer=out.pointer, preInput=[]}}


-- INTCODE RUNNER BELOW

data ParameterMode = Position | Immediate

toParameterMode :: Int -> ParameterMode
toParameterMode 0 = Position
toParameterMode 1 = Immediate
toParameterMode _ = unsafeCoerce "WUT"

type Instruction =
  { op :: IntOp
  , parameterModes :: Array ParameterMode
  }

data IntOp
  = Plus Int Int Int
  | Mult Int Int Int
  | Input Int
  | Output Int
  | JumpIfTrue Int Int
  | JumpIfFalse Int Int
  | LessThan Int Int Int
  | Equals Int Int Int
  | Halt
  | Unknown

type ProgramStep =
  { arr :: Array Int
  , outs :: Array Int
  , ins :: Array Int
  , pointer :: Int
  }

runIntCode :: ProgramStep -> Maybe ProgramStep
runIntCode inp =
  if inp.pointer >= A.length inp.arr
  then Nothing
  else let currOp = readIntOp inp
           nextInp = runIntOp currOp inp
       in case currOp of
            -- Special case for output, since we need to feed it to the next amp
            Output _ -> nextInp
            _ -> nextInp >>= runIntCode

getParam :: Int -> ProgramStep -> Int -> Maybe Int
getParam modeInt inp x = do
  let mode = toParameterMode $ (modeInt `mod` (pow 10 x)) `div` (pow 10 (x-1))
  i <- case mode of
    Immediate -> pure (inp.pointer+x)
    Position -> A.index inp.arr (inp.pointer+x)
  pure i

getPositionalParam :: ProgramStep -> Int -> Maybe Int
getPositionalParam inp x = A.index inp.arr (inp.pointer+x)

readIntOp :: ProgramStep -> IntOp
readIntOp inp = fromMaybe Unknown do
  instr <- A.index inp.arr inp.pointer
  let opcode = instr `mod` 100
      modeInt = instr `div` 100
  case opcode of
    99 -> pure Halt
    1 -> do
      i1 <- getParam modeInt inp 1
      i2 <- getParam modeInt inp 2
      o  <- getPositionalParam inp 3
      pure $ Plus i1 i2 o
    2 -> do
      i1 <- getParam modeInt inp 1
      i2 <- getParam modeInt inp 2
      o  <- getPositionalParam inp 3
      pure $ Mult i1 i2 o
    3 -> do
      i1 <- getPositionalParam inp 1
      pure $ Input i1
    4 -> do
      i1 <- getParam modeInt inp 1
      pure $ Output i1
    5 -> do
      i1 <- getParam modeInt inp 1
      i2 <- getParam modeInt inp 2
      pure $ JumpIfTrue i1 i2
    6 -> do
      i1 <- getParam modeInt inp 1
      i2 <- getParam modeInt inp 2
      pure $ JumpIfFalse i1 i2
    7 -> do
      i1 <- getParam modeInt inp 1
      i2 <- getParam modeInt inp 2
      o  <- getPositionalParam inp 3
      pure $ LessThan i1 i2 o
    8 -> do
      i1 <- getParam modeInt inp 1
      i2 <- getParam modeInt inp 2
      o  <- getPositionalParam inp 3
      pure $ Equals i1 i2 o
    _ -> pure Unknown

runIntOp :: IntOp -> ProgramStep -> Maybe ProgramStep
runIntOp op inp = case op of
  Unknown -> Nothing
  Halt -> Nothing
  Plus i1' i2' o -> do
    i1 <- A.index inp.arr i1'
    i2 <- A.index inp.arr i2'
    out <- A.updateAt o (i1+i2) inp.arr
    pure inp{arr=out, pointer=inp.pointer+4}
  Mult i1' i2' o -> do
    i1 <- A.index inp.arr i1'
    i2 <- A.index inp.arr i2'
    out <- A.updateAt o (i1*i2) inp.arr
    pure inp{arr=out, pointer=inp.pointer+4}
  Input i1 -> do
    head <- A.head inp.ins
    tail <- A.tail inp.ins
    outArr <- A.updateAt i1 head inp.arr
    pure inp{arr=outArr, pointer=inp.pointer+2, ins=tail}
  Output i1 -> do
    o <- A.index inp.arr i1
    let outs = A.snoc inp.outs o
    pure inp{outs=outs, pointer=inp.pointer+2}
  JumpIfTrue i1' i2' -> do
    i1 <- A.index inp.arr i1'
    if i1 /= 0
    then do
      i2 <- A.index inp.arr i2'
      pure inp{pointer=i2}
    else pure inp{pointer=inp.pointer+3}
  JumpIfFalse i1' i2' -> do
    i1 <- A.index inp.arr i1'
    if i1 == 0
    then do
      i2 <- A.index inp.arr i2'
      pure inp{pointer=i2}
    else pure inp{pointer=inp.pointer+3}
  LessThan i1' i2' o -> do
    i1 <- A.index inp.arr i1'
    i2 <- A.index inp.arr i2'
    out <- A.updateAt o (if i1<i2 then 1 else 0) inp.arr
    pure inp{arr=out,pointer=inp.pointer+4}
  Equals i1' i2' o -> do
    i1 <- A.index inp.arr i1'
    i2 <- A.index inp.arr i2'
    out <- A.updateAt o (if i1==i2 then 1 else 0) inp.arr
    pure inp{arr=out,pointer=inp.pointer+4}
