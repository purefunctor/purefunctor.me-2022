{ name = "purefunctor-me"
, dependencies =
  [ "aff"
  , "arrays"
  , "effect"
  , "either"
  , "halogen"
  , "halogen-store"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "routing"
  , "routing-duplex"
  , "safe-coerce"
  , "simple-json"
  , "strings"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
