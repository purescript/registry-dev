#!/usr/bin/env nix-shell
#!nix-shell -i bash

set -euxo pipefail

# This script checks that there are no duplicate entries in the two json files listing packages

bower=$(cat bower-packages.json new-packages.json | jq -s "add | length")
unique=$(cat bower-packages.json new-packages.json | jq -s "unique | add | length")

if [ "$total" == "$unique" ]; then
  exit 0
else
  echo "New packages already exist in the registry!"
  exit 1
fi
