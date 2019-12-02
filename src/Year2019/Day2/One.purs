module Year2019.Day2.One where

import Control.Alternative (pure)
import Control.Bind (bind)
import Control.Category ((<<<))
import Data.Array as A
import Data.Field ((*), (+))
import Data.Function (($))
import Data.Functor (map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord ((>=))
import Data.Show (show)
import Data.String.Pattern (Pattern(..))
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)
import Util.Input (readInputSep)
import Util.Parse (parseInt10)

main :: Effect Unit
main = do
  inpStr <- readInputSep (Pattern ",") "inputs/2019/2/1"
  let inp = map (fromMaybe 0 <<< parseInt10) inpStr
  log $ show do
    inp1 <- overwriteInput 12 2 (Just inp)
    let out = runIntCode inp1 0
    A.index out 0

data IntOp
  = Plus Int Int Int
  | Mult Int Int Int
  | Halt
  | Unknown

overwriteInput :: Int -> Int -> Maybe (Array Int) -> Maybe (Array Int)
overwriteInput i1 i2 inp = do
  inp1 <- inp
  inp2 <- A.updateAt 1 i1 inp1
  A.updateAt 2 i2 inp2

runIntCode :: Array Int -> Int -> Array Int
runIntCode inp pointer =
  if pointer >= A.length inp
  then inp
  else let currOp = readIntOp inp pointer
           nextInp = runIntOp currOp inp
       in runIntCode nextInp (pointer+4)

readIntOp :: Array Int -> Int -> IntOp
readIntOp inp pointer = fromMaybe Unknown do
  opcode <- A.index inp pointer
  case opcode of
    99 -> pure Halt
    _ -> do
      i1 <- A.index inp (pointer+1)
      i2 <- A.index inp (pointer+2)
      o  <- A.index inp (pointer+3)
      pure $ case opcode of
        1 -> Plus i1 i2 o
        2 -> Mult i1 i2 o
        _ -> Unknown

runIntOp :: IntOp -> Array Int -> Array Int
runIntOp op inp = case op of
  Unknown -> inp
  Halt -> inp
  Plus i1' i2' o ->
    let out = do
          i1 <- A.index inp i1'
          i2 <- A.index inp i2'
          A.updateAt o (i1+i2) inp
    in fromMaybe inp out
  Mult i1' i2' o ->
    let out = do
          i1 <- A.index inp i1'
          i2 <- A.index inp i2'
          A.updateAt o (i1*i2) inp
    in fromMaybe inp out
