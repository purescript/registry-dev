# PureScript Registry CI

## Setup

Create a `.env` file based off [`.env.example`](./.env.example) and fill in the values for the environment variables:

```sh
cp .env.example .env
```

If you are running scripts in the repository, such as the legacy registry import script, then you may wish to use the provided Nix shell to make sure you have all necessary dependencies available.

```console
$ nix-shell
```

## Legacy Import

You can execute the legacy registry import script with the following command:

```console
$ spago run -m Registry.Scripts.BowerImport
```
