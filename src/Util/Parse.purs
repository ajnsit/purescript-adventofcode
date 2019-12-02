module Util.Parse where

import Data.Int (round)
import Data.Maybe (Maybe(..))
import Global (isNaN)

foreign import unsafeParseIntBase :: String -> Int -> Number
foreign import unsafeParseInt10 :: String -> Number
foreign import unsafeParseFloat :: String -> Number

parseInt10 :: String -> Maybe Int
parseInt10 s =
  let x = unsafeParseInt10 s
  in if isNaN x
     then Nothing
     else Just (round x)

parseFloat :: String -> Maybe Number
parseFloat s =
  let x = unsafeParseFloat s
  in if isNaN x
     then Nothing
     else Just x
