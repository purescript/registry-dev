module Slug where

import Prelude

-- | A URL-safe slug.
newtype Slug = Slug String

derive instance eqSlug :: Eq Slug
derive instance ordSlug :: Ord Slug

-- | Convert a Slug to a String.
toString :: Slug -> String
toString (Slug s) = s
