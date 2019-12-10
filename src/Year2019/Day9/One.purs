module Year2019.Day9.One where

import Control.Alternative (pure)
import Control.Bind (bind)
import Control.Category ((<<<))
import Data.Array as A
import Data.Boolean (otherwise)
import Data.Either (Either(..))
import Data.Eq ((/=), (==))
import Data.EuclideanRing (div, mod, (-))
import Data.Field ((*), (+))
import Data.Foldable (foldl)
import Data.Function (($))
import Data.Functor (map, (<$>))
import Data.Int (pow, round)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord ((<))
import Data.Semiring (one, zero)
import Data.Show (show)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)
import Foreign.Object as FO
import Unsafe.Coerce (unsafeCoerce)
import Util.BigNum (BigNumber, parseBigInt, parseBigNumber, toNumber)
import Util.Input (readInputSep)

main :: Effect Unit
main = do
  inpStr <- readInputSep (Pattern ",") "inputs/2019/9/1"
  let programArr = map (fromMaybe zero <<< parseBigInt) inpStr
  let program = foldl
                  (\o (Tuple i a) -> FO.insert (show i) a o)
                  FO.empty
                  (A.mapWithIndex Tuple programArr)
  log $ show $ runProgram 0 [fromInt 1] program

-- JS objects used as sparse arrays
type Memory = FO.Object BigNumber

runProgram :: Int -> Array BigNumber -> Memory -> Maybe (Array BigNumber)
runProgram pointer preInput program = do
  out <- runIntCode {arr: program, ins:preInput, outs:[], pointer:pointer, relative:0}
  pure out.outs


-- INTCODE RUNNER BELOW

data ParameterMode = Position | Immediate | Relative

toParameterMode :: Int -> ParameterMode
toParameterMode 0 = Position
toParameterMode 1 = Immediate
toParameterMode 2 = Relative
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
  | RelativeOffset Int
  | Halt
  | Unknown

type ProgramStep =
  { arr :: Memory
  , outs :: Array BigNumber
  , ins :: Array BigNumber
  , pointer :: Int
  , relative :: Int
  }

runIntCode :: ProgramStep -> Maybe ProgramStep
runIntCode inp = case maybeNextInp of
  Nothing -> pure inp
  Just nextInp -> runIntCode nextInp
  where
    currOp = readIntOp inp -- (spy "runIntCode" inp)
    maybeNextInp = runIntOp currOp inp

toInt :: BigNumber -> Int
toInt b = round $ toNumber b

getParam :: Int -> ProgramStep -> Int -> Maybe Int
getParam modeInt inp x = do
  let mode = toParameterMode $ (modeInt `mod` (pow 10 x)) `div` (pow 10 (x-1))
  i <- case mode of
    Immediate -> pure (inp.pointer+x)
    Position -> toInt <$> aIndex inp.arr (inp.pointer+x) -- getParam inp x
    Relative -> map (\y -> inp.relative + toInt y) $ aIndex inp.arr (inp.pointer+x)
  pure i

aIndex :: Memory -> Int -> Maybe BigNumber
aIndex m i
  | i < 0 = Nothing
  | otherwise = Just $ fromMaybe zero $ FO.lookup (show i) m

aUpdateAt :: Int -> BigNumber -> Memory -> Maybe Memory
aUpdateAt i a m = Just $ FO.insert (show i) a m

-- HACKY!
fromInt :: Int -> BigNumber
fromInt x = case parseBigNumber (show x) of
  Left _ -> unsafeCoerce "INVALID BIGNUM!"
  Right y -> y

readIntOp :: ProgramStep -> IntOp
readIntOp inp = fromMaybe Unknown do
  instrBig <- aIndex inp.arr inp.pointer
  let instr = toInt instrBig
  let opcode = instr `mod` 100
      modeInt = instr `div` 100
  case opcode of
    99 -> pure Halt
    1 -> do
      i1 <- getParam modeInt inp 1
      i2 <- getParam modeInt inp 2
      o  <- getParam modeInt inp 3
      pure $ Plus i1 i2 o
    2 -> do
      i1 <- getParam modeInt inp 1
      i2 <- getParam modeInt inp 2
      o  <- getParam modeInt inp 3
      pure $ Mult i1 i2 o
    3 -> do
      i1 <- getParam modeInt inp 1
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
      o  <- getParam modeInt inp 3
      pure $ LessThan i1 i2 o
    8 -> do
      i1 <- getParam modeInt inp 1
      i2 <- getParam modeInt inp 2
      o  <- getParam modeInt inp 3
      pure $ Equals i1 i2 o
    9 -> do
      i1 <- getParam modeInt inp 1
      pure $ RelativeOffset i1
    _ -> pure Unknown

runIntOp :: IntOp -> ProgramStep -> Maybe ProgramStep
runIntOp op inp = case op of
  Unknown -> Nothing
  Halt -> Nothing
  Plus i1' i2' o -> do
    i1 <- aIndex inp.arr i1'
    i2 <- aIndex inp.arr i2'
    out <- aUpdateAt o (i1+i2) inp.arr
    pure inp{arr=out, pointer=inp.pointer+4}
  Mult i1' i2' o -> do
    i1 <- aIndex inp.arr i1'
    i2 <- aIndex inp.arr i2'
    out <- aUpdateAt o (i1*i2) inp.arr
    pure inp{arr=out, pointer=inp.pointer+4}
  Input i1 -> do
    head <- A.head inp.ins
    tail <- A.tail inp.ins
    outArr <- aUpdateAt i1 head inp.arr
    pure inp{arr=outArr, pointer=inp.pointer+2, ins=tail}
  Output i1 -> do
    o <- aIndex inp.arr i1
    let outs = A.snoc inp.outs o
    pure inp{outs=outs, pointer=inp.pointer+2}
  JumpIfTrue i1' i2' -> do
    i1 <- toInt <$> aIndex inp.arr i1'
    if i1 /= 0
    then do
      i2 <- aIndex inp.arr i2'
      pure inp{pointer=toInt i2}
    else pure inp{pointer=inp.pointer+3}
  JumpIfFalse i1' i2' -> do
    i1 <- toInt <$> aIndex inp.arr i1'
    if i1 == 0
    then do
      i2 <- aIndex inp.arr i2'
      pure inp{pointer=toInt i2}
    else pure inp{pointer=inp.pointer+3}
  LessThan i1' i2' o -> do
    i1 <- aIndex inp.arr i1'
    i2 <- aIndex inp.arr i2'
    out <- aUpdateAt o (if i1<i2 then one else zero) inp.arr
    pure inp{arr=out,pointer=inp.pointer+4}
  Equals i1' i2' o -> do
    i1 <- aIndex inp.arr i1'
    i2 <- aIndex inp.arr i2'
    out <- aUpdateAt o (if i1==i2 then one else zero) inp.arr
    pure inp{arr=out,pointer=inp.pointer+4}
  RelativeOffset i1' -> do
    i1 <- aIndex inp.arr i1'
    pure inp{pointer=inp.pointer+2, relative=inp.relative + toInt i1}
