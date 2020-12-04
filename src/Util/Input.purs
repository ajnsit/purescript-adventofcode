module Util.Input where

import Control.Alternative (pure)
import Control.Bind (bind)
import Data.Array (mapMaybe)
import Data.Array as A
import Data.Boolean (otherwise)
import Data.Function (($))
import Data.Maybe (Maybe)
import Data.Ord ((<=))
import Data.String (split, splitAt)
import Data.String as S
import Data.String.Pattern (Pattern(..))
import Data.String.Utils (lines)
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

newtype Sep a = Sep a

readInput :: String -> Effect String
readInput filename = readTextFile UTF8 filename

readInputChunked :: Int -> String -> Effect (Array String)
readInputChunked len filename = do
  contents <- readTextFile UTF8 filename
  pure (chunk len contents)

chunk :: Int -> String -> Array String
chunk len contents
  | S.length contents <= 0 = []
  | otherwise =
      let res = splitAt len contents
      in A.cons res.before (chunk len res.after)

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

readInputParagraphs :: String -> Effect (Array String)
readInputParagraphs = readInputSep (Pattern "\n\n")
