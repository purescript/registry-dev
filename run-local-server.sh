#!/usr/bin/env bash

# Script to run the PureScript Registry server locally without VM.
#
# The script starts a bunch of external service mocks using WireMock, creates a
# Sqlite DB (if doesn't exist yet) and sets up overriding .env file pointing to
# those mock services and the DB. All of that is kept under `.temp/local-server/`.
# To reset the environment, nuke that directory.

set -euo pipefail

# Configuration
MOCK_GITHUB_PORT=9001
MOCK_BUCKET_PORT=9002
MOCK_S3_PORT=9003
MOCK_PURSUIT_PORT=9004

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PIDS=()
CACHE_DIR="$SCRIPT_DIR/.temp/local-server"
mkdir -p "$CACHE_DIR"

# Cleanup function
cleanup() {
  for pid in "${PIDS[@]}"; do
    if kill -0 "$pid" 2>/dev/null; then
      kill "$pid" 2>/dev/null || true
    fi
  done

  echo "Killed all mock services."
}

# Set up cleanup trap
trap cleanup EXIT INT TERM

# Function to start a wiremock service
start_wiremock() {
  local service_name=$1
  local port=$2
  local mappings=$3
  local service_dir="$CACHE_DIR/wiremock-$service_name"
  mkdir -p "$service_dir/mappings" "$service_dir/__files"
  echo > "$service_dir/mappings/mappings.json" "$mappings"
  cp "$SCRIPT_DIR/app/fixtures/registry-storage"/*.tar.gz "$service_dir/__files/" 2>/dev/null || true

  # Start wiremock
  nix run nixpkgs#wiremock -- \
    --port "$port" \
    --root-dir "$service_dir" \
    --disable-banner \
    --verbose &

  local pid=$!
  PIDS+=("$pid")
}

mkdir -p "$CACHE_DIR/repo-fixtures/purescript"
cp -r "$SCRIPT_DIR/app/fixtures/github-packages"/* "$CACHE_DIR/repo-fixtures/purescript/" 2>/dev/null || true
cp -r "$SCRIPT_DIR/app/fixtures/registry-index" "$CACHE_DIR/repo-fixtures/purescript/" 2>/dev/null || true
cp -r "$SCRIPT_DIR/app/fixtures/package-sets" "$CACHE_DIR/repo-fixtures/purescript/" 2>/dev/null || true

start_wiremock "github-api" $MOCK_GITHUB_PORT '
{
  "mappings": [
    {
      "request": {
        "method": "GET",
        "url": "/repos/purescript/purescript-effect/contents/bower.json?ref=v4.0.0"
      },
      "response": {
        "status": 200,
        "headers": {
          "Content-Type": "application/json"
        },
        "jsonBody": {
          "type": "file",
          "encoding": "base64",
          "content": "ewogICJuYW1lIjogInB1cmVzY3JpcHQtZWZmZWN0IiwKICAiaG9tZXBhZ2UiOiAiaHR0cHM6Ly9naXRodWIuY29tL3B1cmVzY3JpcHQvcHVyZXNjcmlwdC1lZmZlY3QiLAogICJsaWNlbnNlIjogIkJTRC0zLUNsYXVzZSIsCiAgInJlcG9zaXRvcnkiOiB7CiAgICAidHlwZSI6ICJnaXQiLAogICAgInVybCI6ICJodHRwczovL2dpdGh1Yi5jb20vcHVyZXNjcmlwdC9wdXJlc2NyaXB0LWVmZmVjdC5naXQiCiAgfSwKICAiaWdub3JlIjogWwogICAgIioqLy4qIiwKICAgICJib3dlcl9jb21wb25lbnRzIiwKICAgICJub2RlX21vZHVsZXMiLAogICAgIm91dHB1dCIsCiAgICAidGVzdCIsCiAgICAiYm93ZXIuanNvbiIsCiAgICAicGFja2FnZS5qc29uIgogIF0sCiAgImRlcGVuZGVuY2llcyI6IHsKICAgICJwdXJlc2NyaXB0LXByZWx1ZGUiOiAiXjYuMC4wIgogIH0KfQo="
        }
      }
    },
    {
      "request": {
        "method": "GET",
        "url": "/repos/purescript/package-sets/tags"
      },
      "response": {
        "status": 200,
        "headers": {
          "Content-Type": "application/json"
        },
        "jsonBody": {
          "name": "psc-0.15.10-20230105",
          "commit": {
            "sha": "090897c992b2b310b1456506308db789672adac1",
            "url": "https://api.github.com/repos/purescript/package-sets/commits/090897c992b2b310b1456506308db789672adac1"
          }
        }
      }
    }
  ]
}'

start_wiremock "s3-api" $MOCK_S3_PORT '
{
  "mappings": [
    {
      "request": {
        "method": "GET",
        "url": "/prelude/6.0.1.tar.gz"
      },
      "response": {
        "status": 200,
        "headers": {
          "Content-Type": "application/octet-stream"
        },
        "bodyFileName": "prelude-6.0.1.tar.gz"
      }
    }
  ]
}'

start_wiremock "bucket-api" $MOCK_BUCKET_PORT '
{
  "mappings": [
    {
      "request": {
        "method": "GET",
        "urlPattern": "/.*"
      },
      "response": {
        "status": 200,
        "headers": {
          "Content-Type": "application/xml"
        },
        "body": "<?xml version=\"1.0\" encoding=\"UTF-8\"?><ListBucketResult></ListBucketResult>"
      }
    }
  ]
}'

start_wiremock "pursuit-api" $MOCK_PURSUIT_PORT '
{
  "mappings": [
    {
      "request": {
        "method": "POST",
        "urlPattern": "/packages.*"
      },
      "response": {
        "status": 200,
        "headers": {
          "Content-Type": "application/json"
        },
        "jsonBody": {
          "success": true
        }
      }
    }
  ]
}'

if [ ! -f "$SCRIPT_DIR/.env" ]; then
  cp "$SCRIPT_DIR/.env.example" "$SCRIPT_DIR/.env"
fi

if [ ! -f "$CACHE_DIR/registry.sqlite3" ]; then
  sqlite3 "$CACHE_DIR/registry.sqlite3" < "$SCRIPT_DIR/db/schema.sql"
fi

  cat > "$CACHE_DIR/.env.local" <<-END
DATABASE_URL="sqlite:$CACHE_DIR/registry.sqlite3"
DHALL_TYPES="$SCRIPT_DIR/types"
GITHUB_API_URL=http://localhost:$MOCK_GITHUB_PORT
S3_API_URL=http://localhost:$MOCK_S3_PORT
S3_BUCKET_URL=http://localhost:$MOCK_BUCKET_PORT
PURSUIT_API_URL=http://localhost:$MOCK_PURSUIT_PORT
END

# Using a specific version of Spago until the new lockfile structure is
# supported by the PureScript Nix overlay.
npx --yes spago@0.93.19 run -p registry-app
