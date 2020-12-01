{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "arraybuffer"
  , "base58"
  , "bigints"
  , "console"
  , "effect"
  , "integers"
  , "lists"
  , "node-buffer"
  , "psci-support"
  , "rationals"
  , "spec"
  , "purescript-erlps-core"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
