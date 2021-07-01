{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purefunctor-me"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "effect"
  , "either"
  , "halogen"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "routing"
  , "routing-duplex"
  , "simple-json"
  , "transformers"
  , "type-equality"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
