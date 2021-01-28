{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "erlscripten-playground"
, dependencies =
  [ "arraybuffer"
  , "base58"
  , "bigints"
  , "console"
  , "effect"
  , "integers"
  , "numbers"
  , "lists"
  , "node-buffer"
  , "psci-support"
  , "rationals"
  , "spec"
  , "numbers"
  , "purescript-erlps-core"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
