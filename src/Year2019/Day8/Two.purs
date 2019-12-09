module Year2019.Day8.Two where

import Control.Bind (bind)
import Data.Array as A
import Data.CommutativeRing ((*))
import Data.Eq ((==))
import Data.Foldable (foldl)
import Data.Function (($))
import Data.Functor (map)
import Data.String as S
import Data.String.CodeUnits as SC
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)
import Util.Input (chunk, readInputChunked)

main :: Effect Unit
main = do
  lines <- readInputChunked layerSize "inputs/2019/8/1"
  let layers = map SC.toCharArray $ A.filter (\l -> S.length l == layerSize) lines
  let finalImage = mergeLayers layerSize layers
  log $ showLayer finalImage

mergePix :: Char -> Char -> Char
mergePix '2' b = b
mergePix a _ = a
emptyPix :: Char
emptyPix = '2'
dispPix :: Char -> Char
dispPix '0' = '░'
dispPix '1' = '█'
dispPix _   = ' '

appendLayer :: Array Char -> Array Char -> Array Char
appendLayer a b = A.zipWith mergePix a b

emptyLayer :: Int -> Array Char
emptyLayer len = A.replicate len emptyPix

showLayer :: Array Char -> String
showLayer l = S.joinWith "\n" $ chunk cols $ SC.fromCharArray $ map dispPix l

mergeLayers :: Int -> Array (Array Char) -> Array Char
mergeLayers len = foldl appendLayer (emptyLayer len)

layerSize :: Int
layerSize = rows * cols
rows :: Int
rows = 6
cols :: Int
cols = 25
