#!/usr/bin/env nix-shell
#!nix-shell -i bash ../../shell.nix

set -euo pipefail

# This script verifies that
# - all the dhall we have in the repo actually compiles
# - all the example manifests actually typecheck as Manifests

for FILE in $(find v1 -iname "*.dhall")
do
  echo "Typechecking ${FILE}";
  dhall <<< "./${FILE}" > /dev/null
done

for FILE in $(find examples -iname "*.json")
do
  echo "Conforming ${FILE} to the Manifest type"
  cat "${FILE}" | json-to-dhall --records-loose --unions-strict "./v1/Manifest.dhall" > /dev/null
done
