module Util.Input where

import Control.Alternative (pure)
import Control.Bind (bind)
import Data.Array (mapMaybe)
import Data.Function (($))
import Data.Maybe (Maybe)
import Data.String.Utils (lines)
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

readInput :: String -> Effect (Array String)
readInput filename = do
  contents <- readTextFile UTF8 filename
  pure (lines contents)

readInputReader :: forall a. (String -> Maybe a) -> String -> Effect (Array a)
readInputReader reader filename = do
  contents <- readInput filename
  pure $ mapMaybe reader contents
