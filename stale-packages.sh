#! /usr/bin/env bash

# This script finds packages whose repositories have been
# transferred to a new owner but whose corresponding
# URL in the registry hasn't been updated to reflect this change.

set -euo pipefail

function repository_status {
  local repository_url; repository_url="$1"
  curl --head --output /dev/null --write-out '%{http_code}' --silent "${repository_url%.git}"
}

function main {
  local registry="$1"
  local org="$2"
  local repository_url
  echo "Listing stale packages in $registry"
  for package_name in $(jq --raw-output --arg org "$org" 'with_entries(select(.value | test("/" + $org + "/"))) | keys[]' "$registry"); do
    repository_url="$(jq --raw-output --arg key "$package_name" '.[$key]' "$registry")"
    if [ "$(repository_status "$repository_url")" = 301 ]; then
      echo "$package_name"
    fi
  done
}

case "${1-unexpected}" in
  purescript|purescript-contrib|purescript-node|purescript-web )
    main bower-packages.json "$1"; main new-packages.json "$1";;
  "core-contrib-node-web" )
    echo "Checking 'purescript' libraries for stale packages..."
    main bower-packages.json "purescript"
    main new-packages.json "purescript"
    echo "Checking 'purescript-contrib' libraries for stale packages..."
    main bower-packages.json "purescript-contrib"
    main new-packages.json "purescript-contrib"
    echo "Checking 'purescript-node' libraries for stale packages..."
    main bower-packages.json "purescript-node"
    main new-packages.json "purescript-node"
    echo "Checking 'purescript-web' libraries for stale packages..."
    main bower-packages.json "purescript-web"
    main new-packages.json "purescript-web"
    ;;
  * ) echo "Expecting one of purescript, purescript-contrib, purescript-node, purescript-web, or core-contrib-node-web.";;
esac
