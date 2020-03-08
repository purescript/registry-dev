#!/usr/bin/env bash

set -euxo pipefail

# This script checks that there are no duplicate entries in the two json files listing packages

total=$(cat bower-packages.json new-packages.json | jq -s "map(length) | add")
unique=$(cat bower-packages.json new-packages.json | jq -s "map(keys) | add | unique | length"
)

if [ "$total" == "$unique" ]; then
  exit 0
else
  echo "New packages alredy exist in the registry!"
  exit 1
fi
