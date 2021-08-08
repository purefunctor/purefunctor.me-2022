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
  , "strings"
  , "transformers"
  , "type-equality"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
