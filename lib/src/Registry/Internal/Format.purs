module Registry.Internal.Format
  ( iso8601Date
  , iso8601DateTime
  , rfc3339ToISO8601
  ) where

import Prelude

import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Formatter.DateTime (FormatterCommand(..))
import Data.List (List)
import Data.List as List
import Parsing as Parsing
import Registry.Internal.Parsing as Internal.Parsing

-- | INTERNAL
-- |
-- | A formatter for an ISO8601 date (ie. a YYYY-MM-DD string).
iso8601Date :: List FormatterCommand
iso8601Date = List.fromFoldable
  [ YearFull
  , Placeholder "-"
  , MonthTwoDigits
  , Placeholder "-"
  , DayOfMonthTwoDigits
  ]

-- | INTERNAL
-- |
-- | A formatter for the simplified ISO8601 date-time format as described in the
-- | ECMAScript specification:
-- | https://tc39.es/ecma262/#sec-date-time-string-format
iso8601DateTime :: List FormatterCommand
iso8601DateTime = List.fold
  [ iso8601Date
  , List.singleton (Placeholder "T")
  , timeFormat
  , List.singleton (Placeholder ".")
  , utcFormat
  ]
  where
  timeFormat :: List FormatterCommand
  timeFormat = List.fromFoldable
    [ Hours24
    , Placeholder ":"
    , MinutesTwoDigits
    , Placeholder ":"
    , SecondsTwoDigits
    ]

  utcFormat :: List FormatterCommand
  utcFormat = List.fromFoldable
    [ Milliseconds
    , Placeholder "Z"
    ]

-- | Early versions of the registry used RFC3339 strings from the precise-datetime
-- | library, which are almost exactly equivalent with simplified ISO8601 strings,
-- | except that they use 1-3 millisecond places instead of the 3 places mandated
-- | by the ECMAScript spec for ISO8601.
rfc3339ToISO8601 :: String -> Either String String
rfc3339ToISO8601 input = do
  { date, time, milliseconds } <- lmap Parsing.parseErrorMessage $ Parsing.runParser input Internal.Parsing.rfc3339
  pure $ Array.fold [ date, "T", time, ".", milliseconds, "Z" ]
