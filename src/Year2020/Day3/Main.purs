module Year2020.Day3.Main where

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Data.Array as A
import Data.Boolean (otherwise)
import Data.CommutativeRing ((*), (+))
import Data.Eq ((==))
import Data.EuclideanRing (mod)
import Data.Function (($))
import Data.Maybe (Maybe(..))
import Data.Ord ((>=))
import Data.Show (show)
import Data.String as S
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)
import Util.Input (readInputLines)

main :: Effect Unit
main = do
  hill <- readInputLines "src/Year2020/Day3/input"
  log $ show $ ntrees {x:3, y:1} hill
  log $ show $
    ntrees {x:1, y:1} hill
    * ntrees {x:3, y:1} hill
    * ntrees {x:5, y:1} hill
    * ntrees {x:7, y:1} hill
    * ntrees {x:1, y:2} hill

-- Y: going down
type Pos = {x::Int, y::Int}

-- Repeats infinitely to the right
type Hill = Array String

type Slope = Pos

maxY :: Hill -> Int
maxY = A.length

next :: Slope -> Pos -> Pos
next slope {x,y} = {x: x+slope.x, y: y+slope.y}

treeAt :: Pos -> Hill -> Maybe Boolean
treeAt {x,y} h = do
  srow <- A.index h y
  let row = S.toCodePointArray srow
  let len = A.length row
  ch <- A.index row (x `mod` len)
  pure (ch == (S.codePointFromChar '#'))

ntrees :: Slope -> Hill -> Int
ntrees slope hill = go 0 {x:0, y:0}
  where
  go count pos
    | pos.y >= maxY hill = count
    | otherwise = case treeAt pos hill of
      Just true -> go (count+1) (next slope pos)
      Just false -> go count (next slope pos)
      Nothing -> count
