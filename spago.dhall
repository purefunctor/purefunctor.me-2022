{ name = "purefunctor-me"
, dependencies =
  [ "aff"
  , "arrays"
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
, sources = [ "src/**/*.purs" ]
}
