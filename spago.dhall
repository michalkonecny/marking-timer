{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "marking-timer"
, dependencies = [ "console", "effect", "psci-support", "halogen" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
