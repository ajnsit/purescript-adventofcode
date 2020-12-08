module Year2020.Day04 where

import AOC.Lib (parseInt10)
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.MonadPlus (guard)
import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.BooleanAlgebra ((&&))
import Data.Either (Either, either)
import Data.Function (const, ($))
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Ord ((<=), (>=))
import Data.Semigroup ((<>))
import Data.Set as Set
import Data.Show (show)
import Data.String as S
import Data.String.Pattern (Pattern(..))
import Data.String.Regex as SR
import Data.String.Regex.Flags as RF
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)

--------------------------------------------------------------------------------
-- Write your solutions here

part1 :: String -> Effect Unit
part1 input = do
  let passports = S.split (Pattern "\n\n") input
  log $ "Part 1 ==> Passports with all fields present: " <> (show $ A.length $ A.mapMaybe parsePassport1 passports)

part2 :: String -> Effect Unit
part2 input = do
  let passports = S.split (Pattern "\n\n") input
  log $ "Part 2 ==> Passports with all valid fields: " <> (show $ A.length $ A.mapMaybe parsePassport passports)

--------------------------------------------------------------------------------

type Passport =
  { byr :: String
  , iyr :: String
  , eyr :: String
  , hgt :: String
  , hcl :: String
  , ecl :: String
  , pid :: String
  , cid :: Maybe String
  }

keys :: Array String
keys =
  [ "byr"
  , "iyr"
  , "eyr"
  , "hgt"
  , "hcl"
  , "ecl"
  , "pid"
  , "cid"
  ]

eToMaybe :: forall a b. Either a b -> Maybe b
eToMaybe = either (const Nothing) Just

regexFlags :: RF.RegexFlags
regexFlags = RF.noFlags

parsePassport1 :: String -> Maybe Passport
parsePassport1 s = do
  whitespace <- eToMaybe $ SR.regex "\\s+" regexFlags
  let fields = SR.split whitespace s
  let m = toMap M.empty fields
  byr <- M.lookup "byr" m
  iyr <- M.lookup "iyr" m
  eyr <- M.lookup "eyr" m
  hgt <- M.lookup "hgt" m
  hcl <- M.lookup "hcl" m
  ecl <- M.lookup "ecl" m
  pid <- M.lookup "pid" m
  let cid = M.lookup "cid" m
  -- Check no other keys
  guard $ Set.isEmpty $ Set.difference (M.keys m) (Set.fromFoldable keys)
  pure {byr, iyr, eyr, hgt, hcl, ecl, pid, cid}

parsePassport :: String -> Maybe Passport
parsePassport s = do
  whitespace <- eToMaybe $ SR.regex "\\s+" regexFlags
  let fields = SR.split whitespace s
  let m = toMap M.empty fields

  yearRegex <- eToMaybe $ SR.regex "^\\d{4}$" regexFlags

  byr <- M.lookup "byr" m
  byri <- parseInt10 byr
  guard $ SR.test yearRegex byr
  guard $ byri >= 1920 && byri <= 2002

  iyr <- M.lookup "iyr" m
  iyri <- parseInt10 iyr
  guard $ SR.test yearRegex iyr
  guard $ iyri >= 2010 && iyri <= 2020

  eyr <- M.lookup "eyr" m
  eyri <- parseInt10 eyr
  guard $ SR.test yearRegex eyr
  guard $ eyri >= 2020 && eyri <= 2030

  hgt <- M.lookup "hgt" m
  hgtRegex <- eToMaybe $ SR.regex "^\\d+([a-zA-Z]{2})$" regexFlags
  matches <- SR.match hgtRegex hgt
  mhgtUnit <- NEA.index matches 1
  hgtUnit <- mhgtUnit
  hgti <- parseInt10 hgt
  guard $ case hgtUnit of
    "in" -> hgti >= 59 && hgti <= 76
    "cm" -> hgti >= 150 && hgti <= 193
    _ -> false

  hcl <- M.lookup "hcl" m
  hclRegex <- eToMaybe $ SR.regex "^#[0-9a-f]{6}$" regexFlags
  guard $ SR.test hclRegex hcl

  ecl <- M.lookup "ecl" m
  eclRegex <- eToMaybe $ SR.regex "^(amb|blu|brn|gry|grn|hzl|oth)$" regexFlags
  guard $ SR.test eclRegex ecl

  pid <- M.lookup "pid" m
  pidRegex <- eToMaybe $ SR.regex "^[0-9]{9}$" regexFlags
  guard $ SR.test pidRegex pid

  let cid = M.lookup "cid" m

  -- Check no other keys
  guard $ Set.isEmpty $ Set.difference (M.keys m) (Set.fromFoldable keys)

  pure {byr, iyr, eyr, hgt, hcl, ecl, pid, cid}

toMap :: M.Map String String -> Array String -> M.Map String String
toMap = A.foldl \m f ->
    let args = S.split (Pattern ":") f
        res = do
          x <- A.uncons args
          y <- A.uncons x.tail
          pure {x,y}
    in maybe m (\ {x,y} -> M.insert x.head y.head m) res
