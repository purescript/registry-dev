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
      -- A mapping between package names and SemVer ranges for them.
      -- Said dependencies are not PureScript code, and they are used
      -- in FFI to interact with the underlying backend.
      , nativeDependencies : Map Text Text
      -- Local source globs to include in the compilation for the target
      , sources : List Text
      }

in  Target