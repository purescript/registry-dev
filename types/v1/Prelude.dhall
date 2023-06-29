-- We read DHALL_PRELUDE from the environment because we cannot make requests to
-- remote hosts in an offline environment (such as Nix in CI). DHALL_PRELUDE is
-- automatically set in your Nix shell, but if you are not using a Nix shell and
-- want to run this locally then the URL will be used instead.
env:DHALL_PRELUDE ? https://prelude.dhall-lang.org/v19.0.0/package.dhall
