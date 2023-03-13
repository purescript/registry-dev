module Test.Registry.Internal where

import Prelude

import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Formatter.DateTime as Formatter.DateTime
import Data.Formatter.DateTime as Formatters.DateTime
import Data.String as String
import Data.Tuple (Tuple(..))
import Parsing as Parsing
import Registry.Internal.Format as Internal.Format
import Registry.Internal.Parsing as Internal.Parsing
import Registry.Test.Assert as Assert
import Registry.Test.Utils as Utils
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = do
  Spec.it "Parses valid RFC3339 formats" do
    let valid = [ "2000-01-01T01:01:01Z", "2000-01-01T01:01:01.1Z", "2018-01-02T03:04:05.123Z" ]
    let parse input = lmap (append (input <> ": ") <<< Parsing.parseErrorMessage) (Parsing.runParser input Internal.Parsing.rfc3339)
    let { fail } = Utils.partitionEithers $ map parse valid
    unless (Array.null fail) do
      Assert.fail $ String.joinWith "\n"
        [ "Some well-formed RFC3339-formatted strings names were not parsed correctly:"
        , Array.foldMap (append "\n  - ") fail
        ]

  Spec.it "Fails to parse invalid RFC3339 formats" do
    let invalid = [ "2018-02-03", "2018-01-01T00:00:00" ]
    let { success } = Utils.partitionEithers $ map (flip Parsing.runParser Internal.Parsing.rfc3339) invalid
    unless (Array.null success) do
      Assert.fail $ String.joinWith "\n"
        [ "Some malformed RFC3339-formatted strings names were parsed:"
        , Array.foldMap (append "\n  - " <<< show) success
        ]

  Spec.it "Migrates RFC3339 strings to ISO8601 strings" do
    let
      rfc3339Inputs =
        [ Tuple "2001-01-01T01:01:01Z" "2001-01-01T01:01:01.000Z"
        , Tuple "2001-01-01T01:01:01.001Z" "2001-01-01T01:01:01.001Z"
        , Tuple "2001-01-01T01:01:01.01Z" "2001-01-01T01:01:01.010Z"
        , Tuple "2001-01-01T01:01:01.1Z" "2001-01-01T01:01:01.100Z"
        ]
      parseRFC (Tuple input expected) = case Internal.Format.rfc3339ToISO8601 input of
        Left error -> Left { input, error }
        Right result | result /= expected -> Left { input, error: "Expected " <> expected <> " but received " <> result }
        Right result -> Right result
      converted =
        Utils.partitionEithers $ map parseRFC rfc3339Inputs

    unless (Array.null converted.fail) do
      Assert.fail $ String.joinWith "\n"
        [ "Some well-formed RFC3339-formatted strings names were not converted to ISO8601 strings:"
        , Array.foldMap (\{ input, error } -> "\n  - " <> input <> ": " <> error) converted.fail
        ]

    let
      parseISO input = case Formatters.DateTime.unformat Internal.Format.iso8601DateTime input of
        Left error -> Left { input, error }
        Right value -> Right value
      iso8601 =
        Utils.partitionEithers $ map parseISO converted.success

    unless (Array.null iso8601.fail) do
      Assert.fail $ String.joinWith "\n"
        [ "Some converted ISO8601 strings names were not parsed to DateTime values:"
        , Array.foldMap (\{ input, error } -> "\n  - " <> input <> ": " <> error) iso8601.fail
        ]

  Spec.it "Formats ISO8601 dates" do
    let format = Formatter.DateTime.format Internal.Format.iso8601Date
    let unformat = Formatter.DateTime.unformat Internal.Format.iso8601Date
    let parse input = lmap (append (input <> ": ")) (map format (unformat input))

    -- Note that the formatters library will wrap dates such April 31 to May 1
    -- rather than fail, while explicitly failing on never-allowed dates such
    -- as April 32. Printing will therefore produce a different date.
    let valid = [ "2022-01-01", "1900-12-31", "2022-02-29" ]
    let { fail } = Utils.partitionEithers $ map parse valid
    unless (Array.null fail) do
      Assert.fail $ String.joinWith "\n"
        [ "Some well-formed ISO8601 dates were not parsed correctly:"
        , Array.foldMap (append "\n  - ") fail
        ]

    let invalid = [ "20222-01-01", "2022-00-00", "2022-12-32" ]
    let { success } = Utils.partitionEithers $ map parse invalid
    unless (Array.null success) do
      Assert.fail $ String.joinWith "\n"
        [ "Some malformed ISO8601 dates were parsed:"
        , Array.foldMap (append "\n  - ") success
        ]

  Spec.it "Formats ISO8601 date times" do
    let format = Formatter.DateTime.format Internal.Format.iso8601DateTime
    let unformat = Formatter.DateTime.unformat Internal.Format.iso8601DateTime
    let parse input = lmap (append (input <> ": ")) (map format (unformat input))

    -- Note that the formatters library will wrap dates such April 31 to May 1
    -- rather than fail, while explicitly failing on never-allowed dates such
    -- as April 32. Printing will therefore produce a different date.
    let valid = [ "2022-01-01T02:01:59.000Z" ]
    let { fail } = Utils.partitionEithers $ map parse valid
    unless (Array.null fail) do
      Assert.fail $ String.joinWith "\n"
        [ "Some well-formed ISO8601 date times were not parsed correctly:"
        , Array.foldMap (append "\n  - ") fail
        ]

    let invalid = [ "2022-01-01T00:61:00.000Z", "2022-01-01T00:00:00Z", "2022-12-32", "2022-12-12T12:12:12.00Z" ]
    let { success } = Utils.partitionEithers $ map parse invalid
    unless (Array.null success) do
      Assert.fail $ String.joinWith "\n"
        [ "Some malformed ISO8601 date times were parsed:"
        , Array.foldMap (append "\n  - ") success
        ]

  Spec.it "Parses registry-compliant Git urls" do
    let
      parse str = case Parsing.runParser str Internal.Parsing.gitUrl of
        Left err -> Left (Tuple str (Parsing.parseErrorMessage err))
        Right _ -> Right unit
    let { fail } = Utils.partitionEithers $ map parse validGitUrl
    unless (Array.null fail) do
      Assert.fail $ String.joinWith "\n"
        [ "Some well-formed git urls were not parsed correctly:"
        , Array.foldMap (\(Tuple str err) -> "\n  - " <> str <> ": " <> err) fail
        ]

    let { success } = Utils.partitionEithers $ map (flip Parsing.runParser Internal.Parsing.gitUrl) invalidGitUrl
    unless (Array.null success) do
      Assert.fail $ String.joinWith "\n"
        [ "Some malformed git urls were parsed:"
        , Array.foldMap (append "\n  - ") success
        ]

validGitUrl :: Array String
validGitUrl =
  [ "https://github.com/user/project"
  , "https://github.com/user/project.git"
  , "http://github.com/user/project.git"
  , "https://192.168.101.127/user/project.git"
  , "http://192.168.101.127/user/project.git"
  , "git://host.xz/path/to/repo.git/"
  , "git://host.xz/~user/path/to/repo.git/"
  , "http://host.xz/path/to/repo.git/"
  , "https://host.xz/path/to/repo.git/"
  ]

invalidGitUrl :: Array String
invalidGitUrl =
  [ "git@github.com:user/project.git"
  , "git@192.168.101.127:user/project.git"
  , "ssh://user@host.xz:port/path/to/repo.git/"
  , "ssh://user@host.xz/path/to/repo.git/"
  , "ssh://host.xz:port/path/to/repo.git/"
  , "ssh://host.xz/path/to/repo.git/"
  , "ssh://user@host.xz/path/to/repo.git/"
  , "ssh://host.xz/path/to/repo.git/"
  , "ssh://user@host.xz/~user/path/to/repo.git/"
  , "ssh://host.xz/~user/path/to/repo.git/"
  , "ssh://user@host.xz/~/path/to/repo.git"
  , "ssh://host.xz/~/path/to/repo.git"
  , "/path/to/repo.git/"
  , "path/to/repo.git/"
  , "~/path/to/repo.git"
  , "file:///path/to/repo.git/"
  , "file://~/path/to/repo.git/"
  , "user@host.xz:/path/to/repo.git/"
  , "host.xz:/path/to/repo.git/"
  , "user@host.xz:~user/path/to/repo.git/"
  , "host.xz:~user/path/to/repo.git/"
  , "user@host.xz:path/to/repo.git"
  , "host.xz:path/to/repo.git"
  , "rsync://host.xz/path/to/repo.git/"
  ]
