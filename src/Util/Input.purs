module Util.Input where

import Control.Alternative (pure)
import Control.Bind (bind)
import Data.Array (mapMaybe)
import Data.Function (($))
import Data.Maybe (Maybe)
import Data.String (Pattern, split)
import Data.String.Utils (lines)
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

newtype Sep a = Sep a

readInputSep :: Pattern -> String -> Effect (Array String)
readInputSep pattern filename = do
  contents <- readTextFile UTF8 filename
  pure (split pattern contents)

readInputLines :: String -> Effect (Array String)
readInputLines filename = do
  contents <- readTextFile UTF8 filename
  pure (lines contents)

readInputLinesReader :: forall a. (String -> Maybe a) -> String -> Effect (Array a)
readInputLinesReader reader filename = do
  contents <- readInputLines filename
  pure $ mapMaybe reader contents
