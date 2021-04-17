{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purefunctor-me"
, dependencies =
  [ "aff"
  , "affjax"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "biscotti-cookie"
  , "codec-argonaut"
  , "console"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign-object"
  , "halogen"
  , "halogen-formless"
  , "http-methods"
  , "lists"
  , "maybe"
  , "newtype"
  , "precise-datetime"
  , "prelude"
  , "psci-support"
  , "routing"
  , "routing-duplex"
  , "simple-json"
  , "slug"
  , "string-parsers"
  , "transformers"
  , "tuples"
  , "type-equality"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
