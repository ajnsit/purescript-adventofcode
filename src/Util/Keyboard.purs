module Util.Keyboard where

import Ansi.Codes (prefix)
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.String (Pattern(..), stripPrefix)
import Data.Unit (Unit)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

type RawKeyPress =
  { sequence :: String
  , name :: String
  , ctrl :: Boolean
  , meta :: Boolean
  , shift :: Boolean
  }

data Direction = Up | Down | Left | Right
instance showDirection :: Show Direction
  where
    show Up = "Up"
    show Down = "Down"
    show Left = "Left"
    show Right = "Right"

newtype Character = Character String

type KeyOptions =
  { ctrl :: Boolean
  , meta :: Boolean
  , shift :: Boolean
  }

data ControlKeyType = Return | Enter | Esc | Space
instance showControlKeyType :: Show ControlKeyType
  where
    show Return = "<return>"
    show Enter = "<enter>"
    show Esc = "<esc>"
    show Space = "<space>"

data KeyPressEvent
  = Arrow Direction KeyOptions
  | Key String KeyOptions
  | ControlKey ControlKeyType KeyOptions
  | UnsupportedKey

instance showKeyPress :: Show KeyPressEvent
  where
    show (Arrow dir _) = "Arrow" <> show dir
    show (Key k _) = "Key press: " <> k
    show (ControlKey seq _) = "Control key: " <> show seq
    show _ = "Unknown key"

toKeyEvent :: RawKeyPress -> KeyPressEvent
toKeyEvent raw = case stripPrefix (Pattern prefix) raw.sequence of
  Just c -> case c of
    "A" -> Arrow Up options
    "B" -> Arrow Down options
    "C" -> Arrow Right options
    "D" -> Arrow Left options
    _ -> UnsupportedKey
  Nothing -> case raw.name of
    "return" -> ControlKey Return options
    "enter" -> ControlKey Enter options
    "escape" -> ControlKey Esc options
    "space" -> ControlKey Space options
    _ -> Key raw.name options
  where
    options = ({ ctrl : raw.ctrl, meta : raw.meta, shift: raw.shift })

setNodeRawMode :: Aff Unit
setNodeRawMode = fromEffectFnAff setNodeRawModeImpl

exitProcess :: Aff Unit
exitProcess = fromEffectFnAff exitProcessImpl

getNextKey :: Aff KeyPressEvent
getNextKey = toKeyEvent <$> fromEffectFnAff getNextKeyImpl

foreign import getNextKeyImpl :: EffectFnAff RawKeyPress
foreign import exitProcessImpl :: EffectFnAff Unit
foreign import setNodeRawModeImpl :: EffectFnAff Unit
