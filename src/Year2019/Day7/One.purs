module Year2019.Day7.One where

import Control.Alternative (pure, (<*>))
import Control.Bind (bind)
import Control.Category ((<<<))
import Data.Array as A
import Data.Eq ((/=), (==))
import Data.EuclideanRing (div, mod, (-))
import Data.Field ((*), (+))
import Data.Foldable (maximum)
import Data.Function (($))
import Data.Functor (map, (<$>))
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
  let phases = 0 A... 4
  let settingsArr = A.filter validSettings $ Settings <$> phases <*> phases <*> phases <*> phases <*> phases
  log $ show $ maximum $ A.catMaybes $ map (runAmplifierArray program) settingsArr

data Settings = Settings Int Int Int Int Int

validSettings :: Settings -> Boolean
validSettings (Settings a b c d e) = 5 == A.length (A.nub [a,b,c,d,e])

runAmplifierArray :: Array Int -> Settings -> Maybe Int
runAmplifierArray program (Settings a b c d e) = do
  let i1 = 0
  i2 <- runAmplifier program [a, i1]
  i3 <- runAmplifier program [b, i2]
  i4 <- runAmplifier program [c, i3]
  i5 <- runAmplifier program [d, i4]
  runAmplifier program [e, i5]

runAmplifier :: Array Int -> Array Int -> Maybe Int
runAmplifier program inputs =
  let out = runIntCode {arr:program, ins:inputs, outs:[], pointer: 0}
  in A.head out.outs

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

runIntCode :: ProgramStep -> ProgramStep
runIntCode inp =
  if inp.pointer >= A.length inp.arr
  then inp
  else let currOp = readIntOp inp
           nextInp = runIntOp currOp inp
       in case currOp of
            Halt -> nextInp
            Unknown -> nextInp
            _ -> runIntCode nextInp

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

runIntOp :: IntOp -> ProgramStep -> ProgramStep
runIntOp op inp = fromMaybe inp $ case op of
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
