{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purefunctor-me"
, dependencies = [ "console", "effect", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "pf_app/src/**/*.purs", "pf_app/test/**/*.purs" ]
}
