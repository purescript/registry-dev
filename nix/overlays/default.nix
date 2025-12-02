# Registry overlays
{
  purescript-overlay,
  mkSpagoDerivation,
  registryLib,
  spagoSrc,
  npmSrc,
}:
[
  purescript-overlay.overlays.default
  mkSpagoDerivation.overlays.default
  (import ./registry.nix { inherit registryLib spagoSrc npmSrc; })
]
