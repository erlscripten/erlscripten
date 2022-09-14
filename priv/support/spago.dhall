{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "erlscripten-playground"
, dependencies =
  [ "aff"
  , "arraybuffer"
  , "arrays"
  , "avar"
  , "base58"
  , "bigints"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "integers"
  , "lazy"
  , "lists"
  , "maybe"
  , "node-buffer"
  , "numbers"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "psci-support"
  , "purescript-erlps-core"
  , "rationals"
  , "refs"
  , "spec"
  , "strings"
  , "tuples"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
