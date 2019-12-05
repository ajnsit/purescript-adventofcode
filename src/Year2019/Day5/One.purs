module Year2019.Day5.One where

import Control.Alternative (pure)
import Control.Bind (bind)
import Control.Category ((<<<))
import Data.Array as A
import Data.EuclideanRing (div, mod, (-))
import Data.Field ((*), (+))
import Data.Function (($))
import Data.Functor (map)
import Data.Int (pow)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord ((>=))
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
  inpStr <- readInputSep (Pattern ",") "inputs/2019/5/1"
  let test = map (fromMaybe 0 <<< parseInt10) inpStr
  log $ show $ _.outs $ go test

go :: Array Int -> ProgramStep
go inp = runIntCode {arr:inp, outs:[]} 0

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
  | Halt
  | Unknown

jumpBy :: IntOp -> Int
jumpBy (Plus _ _ _) = 4
jumpBy (Mult _ _ _) = 4
jumpBy (Input _)    = 2
jumpBy (Output _)   = 2
jumpBy Halt = 1
jumpBy Unknown = 1

type ProgramStep =
  { arr :: Array Int
  , outs :: Array Int
  }

runIntCode :: ProgramStep -> Int -> ProgramStep
runIntCode inp pointer =
  if pointer >= A.length inp.arr
  then inp
  else let currOp = readIntOp inp.arr pointer
           nextInp = runIntOp currOp inp
       in case currOp of
            Halt -> nextInp
            Unknown -> nextInp
            _ -> runIntCode nextInp (pointer+(jumpBy currOp))

getParam :: Int -> Array Int -> Int -> Int -> Maybe Int
getParam modeInt inp pointer x = do
  let mode = toParameterMode $ (modeInt `mod` (pow 10 x)) `div` (pow 10 (x-1))
  i <- case mode of
    Immediate -> pure (pointer+x)
    Position -> A.index inp (pointer+x)
  pure i

readIntOp :: Array Int -> Int -> IntOp
readIntOp inp pointer = fromMaybe Unknown do
  instr <- A.index inp pointer
  let opcode = instr `mod` 100
      modeInt = instr `div` 100
  case opcode of
    99 -> pure Halt
    3 -> do
      i1 <- A.index inp (pointer+1)
      pure $ Input i1
    4 -> do
      i1 <- getParam modeInt inp pointer 1
      pure $ Output i1
    1 -> do
      i1 <- getParam modeInt inp pointer 1
      i2 <- getParam modeInt inp pointer 2
      o  <- A.index inp (pointer+3)
      pure $ Plus i1 i2 o
    2 -> do
      i1 <- getParam modeInt inp pointer 1
      i2 <- getParam modeInt inp pointer 2
      o  <- A.index inp (pointer+3)
      pure $ Mult i1 i2 o
    _ -> pure Unknown

runIntOp :: IntOp -> ProgramStep -> ProgramStep
runIntOp op inp = fromMaybe inp $ case op of
  Unknown -> Nothing
  Halt -> Nothing
  Input i1 -> do
    -- RIGHT NOW: ONLY PROVIDE 1 AS INPUT
    outArr <- A.updateAt i1 1 inp.arr
    pure inp{arr=outArr}
  Output i1 -> do
    o <- A.index inp.arr i1
    let outs = A.snoc inp.outs o
    pure inp{outs=outs}
  Plus i1' i2' o -> do
    i1 <- A.index inp.arr i1'
    i2 <- A.index inp.arr i2'
    out <- A.updateAt o (i1+i2) inp.arr
    pure inp{arr=out}
  Mult i1' i2' o -> do
    i1 <- A.index inp.arr i1'
    i2 <- A.index inp.arr i2'
    out <- A.updateAt o (i1*i2) inp.arr
    pure inp{arr=out}
