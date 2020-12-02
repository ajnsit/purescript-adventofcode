module Year2019.Day14.One where

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Category ((<<<))
import Control.Monad.State (State)
import Control.Monad.State as S
import Control.Monad.State.Class as S
import Data.Array as A
import Data.Boolean (otherwise)
import Data.CommutativeRing ((*))
import Data.Eq ((==))
import Data.EuclideanRing (div)
import Data.Field ((+), (/))
import Data.Foldable (foldl, sum)
import Data.Function (($))
import Data.Functor (map, (<$>))
import Data.Int (ceil, floor, rem, toNumber)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord ((<=), (>))
import Data.Ring ((-))
import Data.Semigroup (append, (<>))
import Data.Show (show)
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Class.Console (log)
import Util.Input (readInputLines)
import Util.Parse (parseInt10)

main :: Effect Unit
main = do
  recipeBook <- (mkRecipeBook <<< A.catMaybes <<< map parseEquation) <$> readInputLines "inputs/2019/14/2"
  log $ show $ recipeBook
  log $ show $ oresNeededForIngredient recipeBook {name:"FUEL", qty:1}

type Ingredients = M.Map String Int
type Leftovers = Ingredients

type Ingredient =
  { qty :: Int
  , name :: String
  }

type Equation =
  { inputs :: Ingredients
  , output :: Ingredient
  }

-- Find the recipe for any ingredient quickly
type Recipe =
  { inputs :: Ingredients
  , outputQty :: Int
  }
type RecipeBook = M.Map String Recipe

timesToMake :: Int -> Int -> Int
timesToMake needed inMultipleOf =
  ceil $ toNumber needed / toNumber inMultipleOf

-- change the ingredients directly producible from ORE to ORE
-- ORE => {in:1, out:1}
-- So if 3A <= 10X, 5B <= 13X
-- {in:10*, out:3}


expando :: RecipeBook -> Ingredient -> State Leftovers Int
expando recipeBook ingredient
  | ingredient.name == "ORE" = pure ingredient.qty
  | otherwise = do
    let recipeM = M.lookup ingredient.name recipeBook
    case recipeM of
      Nothing -> pure 0
      Just recipe -> do
        leftovers <- S.get
        let multiplier = timesToMake ingredient.qty recipe.outputQty
        let remaining = multiplier * recipe.outputQty - ingredient.qty
        let remainingIngredients = remaining (map (_ * multiplier) recipe.inputs) leftovers
        let toProduce = traverse (expando recipeBook) remainingIngredients
        pure $ foldl addOreIngredients {oreQty:0, ingredients:M.empty} rest

remaining :: Ingredients -> Ingredients -> Ingredients
remaining needed available = M.mapMaybeWithKey handle needed
  where
    handle k a = case M.lookup k available of
      Nothing -> Just a
      Just x -> let y = a - x in if y > 0 then Just y else Nothing

oresNeededForIngredient :: RecipeBook -> Ingredient -> Maybe Int
oresNeededForIngredient recipeBook ingredient
  | ingredient.name == "ORE" = Just ingredient.qty
  | otherwise = do
    recipe <- M.lookup ingredient.name recipeBook
    oreQty <- traverse (oresNeededForIngredient recipeBook) recipe.inputs
    pure $ (sum oreQty) * timesToMake ingredient.qty recipe.outputQty

parseEquation :: String -> Maybe Equation
parseEquation s = do
  let sides = split (Pattern " => ") s
  lhs <- A.head sides
  rhs <- A.index sides 1
  let inputStrs = split (Pattern ", ") lhs
  let inputs = A.catMaybes $ map parseIngredient inputStrs
  output <- parseIngredient rhs
  pure { inputs:inputs
       , output:output
       }

parseIngredient :: String -> Maybe Ingredient
parseIngredient s = do
  let words = split (Pattern " ") s
  qtyStr <- A.head words
  qty <- parseInt10 qtyStr
  name <- A.index words 1
  pure { qty:qty
       , name:name
       }

mkRecipeBook :: Array Equation -> RecipeBook
mkRecipeBook = M.fromFoldable <<< map \e ->
  Tuple
    e.output.name
    { outputQty: e.output.qty
    , inputs: e.inputs
    }
