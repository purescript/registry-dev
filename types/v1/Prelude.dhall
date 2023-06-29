-- We read DHALL_PRELUDE from the environment because we cannot make requests to
-- remote hosts in an offline environment (such as Nix in CI). However, when
-- running locally, we can just use the URL.
env:DHALL_PRELUDE ? https://prelude.dhall-lang.org/v19.0.0/package.dhall
