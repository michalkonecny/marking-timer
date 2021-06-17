{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "marking-timer"
, dependencies =
  [ "argonaut", "console", "effect", "halogen", "psci-support", "web-storage" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
