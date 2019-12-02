module Year2019.Day1.Two where

import Control.Bind (bind)
import Data.Array (foldl)
import Data.Bounded ((<=))
import Data.Field ((+), (/))
import Data.Function (($))
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..))
import Data.Ring ((-))
import Data.Show (show)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)
import Util.Input (readInputLines)
import Util.Parse (parseInt10)

main :: Effect Unit
main = do
  inp <- readInputLines "inputs/2019/1/1"
  log $ show $ foldl fuelReader 0 inp

fuelReader :: Int -> String -> Int
fuelReader prev str = case parseInt10 str of
  Nothing -> prev
  Just curr -> fuel curr + prev

fuel :: Int -> Int
fuel mass =
  let fuelmass = floor (toNumber mass / 3.0) - 2
  in if fuelmass <= 0
     then 0
     else fuelmass + fuel fuelmass
