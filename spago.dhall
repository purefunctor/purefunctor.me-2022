{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purefunctor-me"
, dependencies =
  [ "console"
  , "effect"
  , "halogen"
  , "psci-support"
  , "routing"
  , "routing-duplex"
  ]
, packages = ./packages.dhall
, sources = [ "pf_app/src/**/*.purs", "pf_app/test/**/*.purs" ]
}