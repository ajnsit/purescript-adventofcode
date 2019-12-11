module Year2019.Day11.Two where

import Control.Alternative (pure)
import Control.Bind (bind)
import Control.Category ((<<<))
import Data.Array as A
import Data.Boolean (otherwise)
import Data.Either (Either(..))
import Data.Eq ((/=), (==))
import Data.EuclideanRing (div, mod, (-))
import Data.Field ((*), (+))
import Data.Foldable (foldl, maximum, minimum)
import Data.Function (($))
import Data.Functor (map, (<$>))
import Data.Int (pow, round)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord ((<))
import Data.Semiring (one, zero)
import Data.Show (class Show, show)
import Data.String as S
import Data.String.CodeUnits as SC
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)
import Foreign.Object as FO
import Unsafe.Coerce (unsafeCoerce)
import Util.BigNum (BigNumber, parseBigInt, parseBigNumber, toNumber)
import Util.Input (readInputSep)
import Util.Parse (parseInt10)

main :: Effect Unit
main = do
  inpStr <- readInputSep (Pattern ",") "inputs/2019/11/1"
  let programArr = map (fromMaybe zero <<< parseBigInt) inpStr
  let program = foldl
                  (\o (Tuple i a) -> FO.insert (show i) a o)
                  FO.empty
                  (A.mapWithIndex Tuple programArr)
  log $ showCanvas $ fromMaybe FO.empty $ map _.state.canvas $ runProgram 0 program

-- JS objects used as sparse arrays
type Memory = FO.Object BigNumber

type Canvas = FO.Object (FO.Object Int)
data Dir = U|D|L|R
instance showDir :: Show Dir where
  show U = "U"
  show D = "D"
  show L = "L"
  show R = "R"
type Pos = {x::Int, y::Int, dir::Dir}
type Turtle = {canvas::Canvas, pos::Pos, toMove::Boolean}

objectDimension :: forall a. FO.Object a -> {min::Int, max::Int}
objectDimension x = {min:fromMaybe 0 (minimum i), max: fromMaybe 0 (maximum i)}
  where i = indices x

showCanvas :: Canvas -> String
showCanvas c = S.joinWith "\n" $ map (showRow dims <<< extractRow c) (A.sort i)
  where
  i = indices c
  xx = map (objectDimension <<< extractRow c) i
  dims = {min: fromMaybe 0 (minimum (map _.min xx)), max: fromMaybe 0 (maximum (map _.max xx))}

extractRow :: forall a. Show a => Canvas -> a -> FO.Object Int
extractRow x i = fromMaybe FO.empty $ FO.lookup (show i) x

showRow :: {min::Int, max::Int} -> FO.Object Int -> String
showRow dim x = SC.fromCharArray $ map dispPix $ map (\i -> fromMaybe 0 (FO.lookup (show i) x)) (dim.min A... dim.max)

indices :: forall a. FO.Object a -> Array Int
indices = A.catMaybes <<< map parseInt10 <<< FO.keys

dispPix :: Int -> Char
dispPix 0 = '░'
dispPix 1 = '█'
dispPix _ = ' '

initTurtle :: Turtle
initTurtle =
  { canvas: FO.singleton "0" (FO.singleton "0" 1)
  , pos:{x:0,y:0,dir:U}
  , toMove:false
  }

runProgram :: Int -> Memory -> Maybe ProgramStep -- Maybe (Array BigNumber)
runProgram pointer program = do
  out <- runIntCode
    { arr: program
    , state: initTurtle
    , inputFn:currentColor
    , outputFn:handleOut
    , pointer:pointer
    , relative:0
    }
  pure out -- .outs

currentColor :: Turtle -> BigNumber
currentColor {canvas:c, pos:p} = fromInt $ cIndex c p

-- Alternate between accepting a color, and a move
handleOut :: BigNumber -> Turtle -> Turtle
handleOut o t
  | t.toMove = t{pos=move (toInt o) t.pos, toMove=false}
  | otherwise = t{canvas=paint (toInt o) t.pos t.canvas, toMove=true}

leftTurn :: Dir -> Dir
leftTurn U = L
leftTurn L = D
leftTurn D = R
leftTurn R = U

rightTurn :: Dir -> Dir
rightTurn L = U
rightTurn D = L
rightTurn R = D
rightTurn U = R

forward :: Pos -> Pos
forward p = case p.dir of
  L -> p{x=p.x-1}
  R -> p{x=p.x+1}
  U -> p{y=p.y-1}
  D -> p{y=p.y+1}

move :: Int -> Pos -> Pos
move o p =
  let p' =
        case o of
          0 -> p{dir=leftTurn p.dir}
          _ -> p{dir=rightTurn p.dir}
  in forward p'

paint :: Int -> Pos -> Canvas -> Canvas
paint o p c = FO.insert (show p.y) newRow c
  where
  newRow = case FO.lookup (show p.y) c of
    Nothing -> FO.singleton (show p.x) o
    Just row -> FO.insert (show p.x) o row

black :: Int
black = 0
white :: Int
white = 1

cIndex :: Canvas -> Pos -> Int
cIndex c i = fromMaybe black do
  row <- FO.lookup (show i.y) c
  pix <- FO.lookup (show i.x) row
  pure pix

-- cUpdateAt :: Int -> BigNumber -> Memory -> Maybe Memory
-- cUpdateAt i a m = Just $ FO.insert (show i) a m

type ProgramStep = ProgramStepF Turtle


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

type ProgramStepF s =
  { arr :: Memory
  , state :: s
  , inputFn :: s -> BigNumber
  , outputFn :: BigNumber -> s -> s
  , pointer :: Int
  , relative :: Int
  }

runIntCode :: forall s. ProgramStepF s -> Maybe (ProgramStepF s)
runIntCode inp = case maybeNextInp of
  Nothing -> pure inp
  Just nextInp -> runIntCode nextInp
  where
    currOp = readIntOp inp -- (spy "runIntCode" inp)
    maybeNextInp = runIntOp currOp inp

toInt :: BigNumber -> Int
toInt b = round $ toNumber b

getParam :: forall s. Int -> ProgramStepF s -> Int -> Maybe Int
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

readIntOp :: forall s. ProgramStepF s -> IntOp
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

runIntOp :: forall s. IntOp -> ProgramStepF s -> Maybe (ProgramStepF s)
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
    let i = inp.inputFn inp.state
    outArr <- aUpdateAt i1 i inp.arr
    pure inp{arr=outArr, pointer=inp.pointer+2}
  Output i1 -> do
    o <- aIndex inp.arr i1
    let state' = inp.outputFn o inp.state
    pure inp{state=state', pointer=inp.pointer+2}
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
