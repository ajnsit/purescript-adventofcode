module Year2019.Day8.One where

import Control.Bind (bind)
import Data.Array as A
import Data.CommutativeRing ((*))
import Data.Eq ((==))
import Data.Foldable (minimumBy)
import Data.Function (($))
import Data.Functor (map)
import Data.Ord (comparing)
import Data.Show (show)
import Data.String as S
import Data.String.CodeUnits as SC
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)
import Util.Input (readInputChunked)

main :: Effect Unit
main = do
  lines <- readInputChunked layerSize "inputs/2019/8/1"
  let layers = A.filter (\l -> S.length l == layerSize) lines
  let targetLayer = minimumBy (comparing $ charCount '0') layers
  log $ show $ map (\l -> charCount '1' l * charCount '2' l) targetLayer

charCount :: Char -> String -> Int
charCount c s = A.length $ A.filter (_==c) $ SC.toCharArray s

layerSize :: Int
layerSize = 25 * 6
