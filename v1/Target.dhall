{-

A "compilation target".

Every target can have its own dependencies, source globs, etc.
By convention a package needs to have at least one target called `lib`.

Other common ones are `app`, `test`, `dev`, `bench`, etc.

-}

let Map = (./Prelude.dhall).Map.Type

let Target =
      -- A mapping between package names (as published on the Registry or
      -- included in the Package Set) and SemVer ranges for them.
      { dependencies : Map Text Text
      -- Local sources to include in the compilation for the target. Sources
      -- must be directories, ie. "src" or "test/package"
      , sources : List Text
      }

in  Target
