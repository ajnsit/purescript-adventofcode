{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "advent-of-code"
, dependencies =
  [ "aff"
  , "ansi"
  , "arrays"
  , "bignumber"
  , "console"
  , "contravariant"
  , "control"
  , "debug"
  , "effect"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "foreign-object"
  , "free"
  , "functors"
  , "integers"
  , "lazy"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "node-path"
  , "node-readline"
  , "numbers"
  , "optparse"
  , "ordered-collections"
  , "parsing"
  , "prelude"
  , "psci-support"
  , "safe-coerce"
  , "st"
  , "strings"
  , "stringutils"
  , "tuples"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
