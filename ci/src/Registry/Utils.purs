module Registry.Utils where

import Registry.Prelude

import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Newtype as Newtype
import Data.Time.Duration as Duration
import Effect.Now as Now

-- | Get the current time, standardizing on the UTC timezone to avoid ambiguity
-- | when running on different machines.
nowUTC :: Effect DateTime
nowUTC = do
  offset <- Newtype.over Duration.Minutes negate <$> Now.getTimezoneOffset
  now <- Now.nowDateTime
  pure $ fromMaybe now $ DateTime.adjust (offset :: Duration.Minutes) now
