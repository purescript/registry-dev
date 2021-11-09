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
$ spago run -m Registry.Scripts.LegacyImport
```

## Find Dropped Packages

This script reports packages that will be dropped from the package set

### Setup

Before running this script you will need to have a `bower-exclusions.json` file (from running the [Legacy Import](#legacy-import)) and a `package-set-packages.json` file:

```console
$ wget https://raw.githubusercontent.com/purescript/package-sets/master/packages.json -O package-set-packages.json
```

With those files present, run the script with the following command:

```console
$ spago run -m Registry.Scripts.FindDroppedPackages
```
