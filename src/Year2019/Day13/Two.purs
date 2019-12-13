module Year2019.Day13.Two where

import Control.Alternative (pure)
import Control.Bind (bind, discard)
import Control.Category ((<<<))
import Control.Monad (class Monad, when)
import Data.Array as A
import Data.Boolean (otherwise)
import Data.BooleanAlgebra ((&&))
import Data.DivisionRing (sub)
import Data.Either (Either(..))
import Data.Eq (class Eq, (/=), (==))
import Data.EuclideanRing (div, mod, (-))
import Data.Field ((*), (+))
import Data.Foldable (foldl)
import Data.Function (($))
import Data.Functor (map, (<$>))
import Data.Int (pow, round)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord ((<))
import Data.Semigroup ((<>))
import Data.Semiring (one, zero)
import Data.Show (class Show, show)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Foreign.Object as FO
import Unsafe.Coerce (unsafeCoerce)
import Util.BigNum (BigNumber, parseBigInt, parseBigNumber, toNumber)
import Util.Input (readInputSep)

main :: Effect Unit
main = do
  inpStr <- readInputSep (Pattern ",") "inputs/2019/13/1"
  let programArr = map (fromMaybe zero <<< parseBigInt) inpStr
  let program = foldl
                  (\o (Tuple i a) -> FO.insert (show i) a o)
                  FO.empty
                  (A.mapWithIndex Tuple programArr)
  log "RUNNING..."
  launchAff_ do
    p <- runProgram 0 (insertCoins 2 program)
    liftEffect $ log $ "SCORE: " <> show p.state.score

insertCoins :: Int -> Memory -> Memory
insertCoins n = FO.insert "0" (fromInt n)

data Sprite
  = Empty
  | Wall
  | Block
  | Paddle
  | Ball

instance showSprite :: Show Sprite where
  show Empty = "empty"
  show Wall = "wall"
  show Block = "block"
  show Paddle = "paddle"
  show Ball = "ball"

instance eqSprite :: Eq Sprite where
  eq Empty Empty = true
  eq Wall Wall = true
  eq Block Block = true
  eq Paddle Paddle = true
  eq Ball Ball = true
  eq _ _ = false

intToSprite :: Int -> Maybe Sprite
intToSprite 0 = Just Empty
intToSprite 1 = Just Wall
intToSprite 2 = Just Block
intToSprite 3 = Just Paddle
intToSprite 4 = Just Ball
intToSprite _ = Nothing

-- No need to store typ, since as soon as we get typ, we update canvas
type Out = {x::Maybe Int, y::Maybe Int}
type Canvas = M.Map Pos Sprite
type State = {canvas::Canvas, out::Out, score::Int, paddlex::Int, ballx::Int}
type Pos = {x::Int, y::Int}

countSprites :: Sprite -> Canvas -> Int
countSprites s = M.size <<< M.filter (_ == s)

initState :: State
initState = {canvas:M.empty, out:{x:Nothing, y:Nothing}, score:0, paddlex:0, ballx:0}

type M = Aff

runProgram :: Int -> Memory -> M ProgramStep
runProgram pointer program = do
  runIntCode
    { arr: program
    , state: initState
    , inputFn: joystick
    , outputFn:handleOut
    , stateCallback:render
    , pointer:pointer
    , relative:0
    }

showStateVars :: State -> String
showStateVars state =
  "RENDER: " <> show state.out <>
  ", SCORE: " <> show state.score <>
  ", PADDLE: " <> show state.paddlex <>
  ", BALL: " <> show state.ballx

render :: State -> M Unit
render state = pure unit -- log $ showStateVars state

-- Signum, but maps 0 to 0, and works with big numbers
signumz :: BigNumber -> BigNumber
signumz x
  | x == zero = zero
  | x < zero = sub zero one
  | otherwise = one


-- IMPORTANT NOTE:
-- DECIDED NOT TO IMPLEMENT USER INPUT AND RENDERING OF CANVAS FOR NOW
-- The following just tracks the ball position and makes the paddle follow
joystick :: State -> M BigNumber
joystick s = pure $ signumz $ fromInt $ s.ballx - s.paddlex
  -- ke <- getNextKey
  -- case ke of
  --   ControlKey Esc _ -> exitProcess
  --   _ -> log $ show ke
  -- pure zero

handleOut :: BigNumber -> State -> M State
handleOut o s = pure $ case s.out.x of
  Nothing -> s{out=s.out{x=Just (toInt o)}}
  Just x -> case s.out.y of
    Nothing -> s{out=s.out{y=Just (toInt o)}}
    Just y ->
      let s' = s{out={x:Nothing,y:Nothing}}
      in
        if x == (0-1) && y == 0
        then s'{ score = toInt o }
        else
          case intToSprite (toInt o) of
            Nothing ->
              s'{ canvas=s.canvas }
            Just Paddle ->
              s'{ paddlex=x }
            Just Ball ->
              s'{ ballx=x }
            Just sprite ->
              s'{ canvas=M.insert {x:x,y:y} sprite s.canvas }

cIndex :: Pos -> Canvas -> Maybe Sprite
cIndex = M.lookup

type ProgramStep = ProgramStepF State M


-- INTCODE RUNNER BELOW

-- JS objects used as sparse arrays
type Memory = FO.Object BigNumber

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

type ProgramStepF s m =
  { arr :: Memory
  , state :: s
  , inputFn :: s -> m BigNumber
  , outputFn :: BigNumber -> s -> m s
  , stateCallback :: s -> m Unit
  , pointer :: Int
  , relative :: Int
  }

runIntCode :: forall s m. Eq s => Monad m => ProgramStepF s m -> m (ProgramStepF s m)
runIntCode inp = do
  maybeNextInp <- runIntOp currOp inp
  case maybeNextInp of
    Nothing -> pure inp
    Just nextInp -> do
      when (inp.state /= nextInp.state) $ inp.stateCallback inp.state
      runIntCode nextInp
  where
    currOp = readIntOp inp

toInt :: BigNumber -> Int
toInt b = round $ toNumber b

getParam :: forall s m. Int -> ProgramStepF s m -> Int -> Maybe Int
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

readIntOp :: forall s m. ProgramStepF s m -> IntOp
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

runIntOp :: forall s m. Monad m => IntOp -> ProgramStepF s m -> m (Maybe (ProgramStepF s m))
runIntOp op inp = case op of
  Unknown -> pure Nothing
  Halt -> pure Nothing
  Plus i1' i2' o -> pure do
    i1 <- aIndex inp.arr i1'
    i2 <- aIndex inp.arr i2'
    out <- aUpdateAt o (i1+i2) inp.arr
    pure inp{arr=out, pointer=inp.pointer+4}
  Mult i1' i2' o -> pure do
    i1 <- aIndex inp.arr i1'
    i2 <- aIndex inp.arr i2'
    out <- aUpdateAt o (i1*i2) inp.arr
    pure inp{arr=out, pointer=inp.pointer+4}
  Input i1 -> do
    i <- inp.inputFn inp.state
    -- liftEffect $ log $ "INPUT: " <> show i
    pure do
      outArr <- aUpdateAt i1 i inp.arr
      pure inp{arr=outArr, pointer=inp.pointer+2}
  Output i1 -> do
    let mo = aIndex inp.arr i1
    case mo of
      Nothing -> pure Nothing
      Just o -> do
        state' <- inp.outputFn o inp.state
        pure $ Just inp{state=state', pointer=inp.pointer+2}
  JumpIfTrue i1' i2' -> pure do
    i1 <- toInt <$> aIndex inp.arr i1'
    if i1 /= 0
    then do
      i2 <- aIndex inp.arr i2'
      pure inp{pointer=toInt i2}
    else pure inp{pointer=inp.pointer+3}
  JumpIfFalse i1' i2' -> pure do
    i1 <- toInt <$> aIndex inp.arr i1'
    if i1 == 0
    then do
      i2 <- aIndex inp.arr i2'
      pure inp{pointer=toInt i2}
    else pure inp{pointer=inp.pointer+3}
  LessThan i1' i2' o -> pure do
    i1 <- aIndex inp.arr i1'
    i2 <- aIndex inp.arr i2'
    out <- aUpdateAt o (if i1<i2 then one else zero) inp.arr
    pure inp{arr=out,pointer=inp.pointer+4}
  Equals i1' i2' o -> pure do
    i1 <- aIndex inp.arr i1'
    i2 <- aIndex inp.arr i2'
    out <- aUpdateAt o (if i1==i2 then one else zero) inp.arr
    pure inp{arr=out,pointer=inp.pointer+4}
  RelativeOffset i1' -> pure do
    i1 <- aIndex inp.arr i1'
    pure inp{pointer=inp.pointer+2, relative=inp.relative + toInt i1}
