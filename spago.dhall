{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "ansi"
  , "console"
  , "debug"
  , "effect"
  , "foreign-object"
  , "functors"
  , "node-fs"
  , "node-readline"
  , "psci-support"
  , "stringutils"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
