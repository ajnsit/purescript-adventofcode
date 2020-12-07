module Year2020.Day7.Main where

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Data.Array as A
import Data.CommutativeRing ((*), (+))
import Data.Eq ((==))
import Data.Foldable (sum)
import Data.Function (const, ($))
import Data.Functor (map, (<$>))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String as S
import Data.String.Pattern (Pattern(..))
import Data.String.Utils (words)
import Data.Traversable (traverse)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)
import Util.Input (readInputLines)
import Util.Parse (parseInt10)
import Util.Util (splitFirst)

main :: Effect Unit
main = do
  srules <- readInputLines "src/Year2020/Day7/input"
  let rules = A.mapMaybe parseRule srules
  log $ "Part 1: " <> (show $ A.length $ parentColors rules "shiny gold")
  log $ "Part 2: " <> (show $ countBags rules "shiny gold")

type Rule =
  { color :: String
  , bags :: Array Bags
  }
type Bags = {bag::String, number::Int}

parseRule :: String -> Maybe Rule
parseRule s = do
  x <- splitFirst (Pattern " bags contain ") s
  let color = x.left
  let rest = x.right
  let inner = S.split (Pattern ", ") x.right
  bags <- traverse parseBags inner
  pure {color, bags}

parseBags :: String -> Maybe Bags
parseBags "no other bags" = Nothing
parseBags s = do
  x <- A.uncons (words s)
  number <- parseInt10 x.head
  y <- A.uncons x.tail
  z <- A.uncons y.tail
  pure {number, bag:y.head <> " " <> z.head}

countBags :: Array Rule -> String -> Int
countBags rules color =
  let matchingRule = A.find (ruleWithThisColor color) rules
  in case matchingRule of
    Nothing -> 0
    Just r -> countBagsInThisRule r + sum (map (\b -> countBags rules b.bag * b.number) r.bags)
  where
  ruleWithThisColor :: String -> Rule -> Boolean
  ruleWithThisColor color' rule = rule.color == color'
  countBagsInThisRule :: Rule -> Int
  countBagsInThisRule rule = sum $ map _.number rule.bags

parentColors :: Array Rule -> String -> Array String
parentColors rules color' = go [] color'
  where
    colorWithThisChild :: String -> Rule -> Maybe String
    colorWithThisChild color rule = const rule.color <$> A.find (\b -> b.bag == color) rule.bags

    go :: Array String -> String -> Array String
    go prev color =
      let matching = A.difference (A.mapMaybe (colorWithThisChild color) rules) prev
      in A.foldl go (A.union prev matching) matching
