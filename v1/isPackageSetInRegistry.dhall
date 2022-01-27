let Prelude = ./Prelude.dhall

in \(externalPackage : Type) ->
      let Address = (./Address.dhall) externalPackage

      let Repo = ./Repo.dhall

      -- we define a function that will map an element of the map to `True`
      -- only if it's a `Registry` constructor
      let Pkg = Prelude.Map.Entry Text Address
      let Pkgs = Prelude.Map.Type Text Address
      let isRegistryPkg = \(pkg : Pkg) -> merge
            { Registry = \(x : Text) -> True
            , External = \(x : externalPackage) -> False
            } pkg.mapValue

      -- here we map all the packages in the set with the function defined above,
      -- then we say "all the elements of this list are `True`"
      let isPackageSetInRegistry = \(pkgs : Pkgs) ->
            Prelude.List.all Bool (\(x : Bool) -> x == True)
            (Prelude.List.map Pkg Bool isRegistryPkg pkgs)

      in isPackageSetInRegistry
