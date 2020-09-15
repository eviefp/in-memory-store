{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
    [ "console"
    , "effect"
    , "psci-support"
    , "foldable-traversable"
    , "tuples"
    , "node-process"
    , "strings"
    , "node-readline-aff"
    , "aff"
    , "ordered-collections"
    , "node-net"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
