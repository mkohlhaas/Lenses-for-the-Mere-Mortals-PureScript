{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "lists"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "profunctor-lenses"
  , "strings"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
