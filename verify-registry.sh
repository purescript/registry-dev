#!/usr/bin/env nix-shell
#!nix-shell -i bash

set -euo pipefail

# This script runs some checks on the actual structure of the registry,
# to verify that new packages are compliant with our guidelines


# Check that there are no duplicate entries in the two json files listing packages

total=$(cat bower-packages.json new-packages.json | jq -s "map(length) | add")
unique=$(cat bower-packages.json new-packages.json | jq -s "map(keys) | add | unique | length"
)

if [ "$total" != "$unique" ]; then
  echo "New packages already exist in the registry!"
  exit 1
fi


# Check that all packages resolve to a repo
tmpfile=$(mktemp /tmp/registry-script.XXXXXX)
cat bower-packages.json new-packages.json | jq -s -r '.[0] | map(values) | @sh' | tr -d \' > $tmpfile
cat $tmpfile
cat $tmpfile | parallel --pipe -j 20 'echo "Verifying if repo exists: {}" && curl -L --silent --show-error {} > /dev/null'
rm "$tmpfile"
