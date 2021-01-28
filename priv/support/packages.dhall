let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20200922/packages.dhall sha256:5edc9af74593eab8834d7e324e5868a3d258bbab75c5531d2eb770d4324a2900

in  upstream
    with purescript-erlps-core =
        { dependencies =
          [ "prelude" ]
        , repo =
          "https://github.com/erlscripten/erlps-core.git"
        , version =
          "main"
        }
    with base58 =
        { dependencies =
          [ "prelude" ]
        , repo =
          "https://github.com/throughnothing/purescript-base58.git"
        , version =
          "v0.0.3"
        }
    with math.repo = "https://github.com/minoki/purescript-math/"
    with math.version = "es6-functions"
