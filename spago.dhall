{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purefunctor-me"
, dependencies =
  [ "affjax"
  , "argonaut-codecs"
  , "biscotti-cookie"
  , "codec-argonaut"
  , "console"
  , "effect"
  , "halogen"
  , "halogen-formless"
  , "precise-datetime"
  , "psci-support"
  , "routing"
  , "routing-duplex"
  , "slug"
  ]
, packages = ./packages.dhall
, sources = [ "pf_app/src/**/*.purs", "pf_app/test/**/*.purs" ]
}
