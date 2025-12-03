# Registry overlays
{
  purescript-overlay,
  mkSpagoDerivation,
  spagoSrc,
  npmSrc,
}:
[
  purescript-overlay.overlays.default
  mkSpagoDerivation.overlays.default
  (import ./registry.nix { inherit spagoSrc npmSrc; })
]
