module Year2019.Day2.Two where

import Control.Alternative (pure)
import Control.Bind (bind)
import Control.Category ((<<<))
import Data.Array as A
import Data.Eq ((==))
import Data.Field ((*), (+))
import Data.Function (($))
import Data.Functor (map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord ((>=))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.String.Pattern (Pattern(..))
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)
import Util.Input (readInputSep)
import Util.Parse (parseInt10)

main :: Effect Unit
main = do
  inpStr <- readInputSep (Pattern ",") "inputs/2019/2/1"
  let out = until inputVals success go $ map (fromMaybe 0 <<< parseInt10) inpStr
  log $ show $ map (\(Val x y) -> 100 * x + y) out

until :: forall val output. Array val -> (output -> Boolean) -> (val -> output -> output) -> output -> Maybe val
until vals cond f def = case A.uncons vals of
  Nothing -> Nothing
  Just ref ->
    let out = f ref.head def
    in if cond out then Just ref.head else until ref.tail cond f def

success :: Array Int -> Boolean
success inp = A.index inp 0 == Just 19690720

data Val = Val Int Int
instance showVal :: Show Val where
  show (Val x y) = "{" <> show x <> ", " <> show y <> "}"

inputVals :: Array Val
inputVals = do
  x <- (0 A... 100)
  y <- (0 A... 100)
  pure (Val x y)

go :: Val -> Array Int -> Array Int
go val inp =
  let inp1 = overwriteInput val inp
  in runIntCode inp1 0

data IntOp
  = Plus Int Int Int
  | Mult Int Int Int
  | Halt
  | Unknown

overwriteInput :: Val -> Array Int -> Array Int
overwriteInput (Val i1 i2) inp = fromMaybe inp do
  inp1 <- A.updateAt 1 i1 inp
  A.updateAt 2 i2 inp1

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
