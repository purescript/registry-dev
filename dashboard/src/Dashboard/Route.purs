module Dashboard.Route
  ( Route(..)
  , JobsListParams
  , defaultParams
  , routes
  ) where

import Prelude hiding ((/))

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..))
import Routing.Duplex (RouteDuplex(..), RouteDuplex')
import Routing.Duplex as RD
import Routing.Duplex.Parser (RouteParser)
import Routing.Duplex.Printer (RoutePrinter)

-- | Optional filter parameters carried on the JobsList route.
-- | Only non-default values are encoded in the URL hash.
type JobsListParams =
  { range :: Maybe String
  , status :: Maybe String
  , jobType :: Maybe String
  , package :: Maybe String
  , version :: Maybe String
  , compiler :: Maybe String
  , autoRefresh :: Maybe Boolean
  , since :: Maybe String
  , until :: Maybe String
  , order :: Maybe String
  , page :: Maybe Int
  }

-- | All-Nothing params representing defaults (no filters active).
defaultParams :: JobsListParams
defaultParams =
  { range: Nothing
  , status: Nothing
  , jobType: Nothing
  , package: Nothing
  , version: Nothing
  , compiler: Nothing
  , autoRefresh: Nothing
  , since: Nothing
  , until: Nothing
  , order: Nothing
  , page: Nothing
  }

data Route
  = JobsList JobsListParams
  | JobDetail String

derive instance Eq Route

-- | The routing codec. Handles:
-- |   /              -> JobsList with optional query params
-- |   /?range=1h&... -> JobsList with filter params
-- |   /jobs/:id      -> JobDetail
routes :: RouteDuplex' Route
routes = RD.root routeChoice
  where
  routeChoice :: RouteDuplex' Route
  routeChoice = RouteDuplex enc dec

  enc :: Route -> RoutePrinter
  enc = case _ of
    JobsList params -> do
      let RD.RouteDuplex encParams _ = jobsListParams
      encParams params
    JobDetail jobId -> do
      let RD.RouteDuplex encDetail _ = RD.path "jobs" RD.segment
      encDetail jobId

  dec :: RouteParser Route
  dec = do
    let RD.RouteDuplex _ decDetail = RD.end (RD.path "jobs" RD.segment)
    let RD.RouteDuplex _ decParams = RD.end jobsListParams
    (JobDetail <$> decDetail) <|> (JobsList <$> decParams)

  -- | Parse/print optional query parameters for the jobs list view.
  jobsListParams :: RouteDuplex' JobsListParams
  jobsListParams = RD.params
    { range: RD.optional <<< RD.string
    , status: RD.optional <<< RD.string
    , jobType: RD.optional <<< RD.string
    , package: RD.optional <<< RD.string
    , version: RD.optional <<< RD.string
    , compiler: RD.optional <<< RD.string
    , autoRefresh: RD.optional <<< RD.boolean
    , since: RD.optional <<< RD.string
    , until: RD.optional <<< RD.string
    , order: RD.optional <<< RD.string
    , page: RD.optional <<< RD.int <<< RD.string
    }
