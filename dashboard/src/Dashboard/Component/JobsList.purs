-- | The Jobs List component displays a filterable list of registry jobs with
-- | page-based pagination.
module Dashboard.Component.JobsList
  ( Input
  , Output(..)
  , Filters
  , TimeRange
  , StatusFilter
  , component
  ) where

import Prelude

import Control.Alt ((<|>))
import Dashboard.API (ApiConfig, ApiError)
import Dashboard.API as API
import Dashboard.Job (JobStatus(..), JobSummary)
import Dashboard.Job as Job
import Dashboard.Route (JobsListParams)
import Data.Array as Array
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Newtype (unwrap)
import Data.String as String
import Data.Time.Duration (Seconds(..))
import Effect.Aff (Milliseconds(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Now as Now
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Registry.API.V1 (Job, JobType(..), SortOrder(..))
import Registry.API.V1 as V1
import Registry.PackageName as PackageName
import Registry.Version as Version

-- | The number of jobs to display per page.
pageSize :: Int
pageSize = 100

-- | The component input is the API configuration plus optional filter
-- | parameters parsed from the URL hash.
type Input =
  { apiConfig :: ApiConfig
  , params :: JobsListParams
  }

-- | The component output signals navigation to a job detail view or notifies
-- | the parent that filter state has changed (so the URL hash can be updated).
data Output
  = NavigateToJob String
  | FiltersChanged JobsListParams

-- | Time range options for the server-side `since`/`until` query parameters.
-- | `UntilNow` sends only `until = now` with no `since` bound, returning all
-- | jobs up to the current time.
data TimeRange
  = UntilNow
  | LastHour
  | Last24Hours
  | LastWeek
  | Custom

derive instance Eq TimeRange

-- | Return the window size in hours for preset time ranges.
timeRangeHours :: TimeRange -> Number
timeRangeHours = case _ of
  UntilNow -> 24.0
  LastHour -> 1.0
  Last24Hours -> 24.0
  LastWeek -> 168.0
  Custom -> 24.0

-- | Which column the jobs list is sorted by. Currently only Created is
-- | supported server-side; Started is display-only.
data SortField = SortByCreated

derive instance Eq SortField

-- | The default sort order is newest-first.
defaultSortOrder :: SortOrder
defaultSortOrder = DESC

-- | The status filter merges the old "include completed" checkbox with the
-- | status dropdown into a single control.
data StatusFilter
  = ActiveOnly
  | AllStatuses
  | OnlyPending
  | OnlyRunning
  | OnlySucceeded
  | OnlyFailed

derive instance Eq StatusFilter

-- | Client-side filter state.
type Filters =
  { jobType :: Maybe JobType
  , packageName :: String
  , packageVersion :: String
  , compilerVersion :: String
  , statusFilter :: StatusFilter
  }

emptyFilters :: Filters
emptyFilters =
  { jobType: Nothing
  , packageName: ""
  , packageVersion: ""
  , compilerVersion: ""
  , statusFilter: AllStatuses
  }

type State =
  { apiConfig :: ApiConfig
  , jobs :: Array JobSummary
  , loading :: Boolean
  , error :: Maybe String
  , timeRange :: TimeRange
  , autoRefresh :: Boolean
  , refreshSubId :: Maybe H.SubscriptionId
  , filters :: Filters
  , since :: Maybe DateTime
  , until :: Maybe DateTime
  , sortField :: SortField
  , sortOrder :: SortOrder
  -- | Raw input values for the Custom time range datetime-local inputs.
  , sinceStr :: String
  , untilStr :: String
  -- | The current page number (1-indexed).
  , currentPage :: Int
  -- | Whether there are more results beyond the current page.
  , hasNextPage :: Boolean
  -- | Boundary timestamps for pages we have visited. `pageCursors !! 0` is
  -- | the cursor that takes us from page 1 to page 2, etc.
  , pageCursors :: Array DateTime
  }

data Action
  = Initialize
  | FetchJobs
  | FetchJobsSilent
  | HandleFetchResult (Either ApiError (Array Job))
  | SetTimeRange TimeRange
  | SetCustomSinceDate String
  | SetCustomSinceTime String
  | SetCustomUntilDate String
  | SetCustomUntilTime String
  | ToggleAutoRefresh Boolean
  | SetFilterJobType String
  | SetFilterPackageName String
  | SetFilterPackageVersion String
  | SetFilterCompilerVersion String
  | SetFilterStatus String
  | ClearFilters
  | SetSort SortField
  | NavigateToJobDetail String
  | NextPage
  | PrevPage
  | Tick
  | Receive Input

component :: forall query m. MonadAff m => H.Component query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

initialState :: Input -> State
initialState input = do
  let p = input.params
  { apiConfig: input.apiConfig
  , jobs: []
  , loading: true
  , error: Nothing
  , timeRange: parseTimeRange (fromMaybe "" p.range)
  , autoRefresh: fromMaybe false p.autoRefresh
  , refreshSubId: Nothing
  , filters:
      { jobType: p.jobType >>= parseJobType
      , packageName: fromMaybe "" p.package
      , packageVersion: fromMaybe "" p.version
      , compilerVersion: fromMaybe "" p.compiler
      , statusFilter: parseStatusFilter (fromMaybe "" p.status)
      }
  , sortField: SortByCreated
  , sortOrder: case p.order of
      Just "asc" -> ASC
      _ -> defaultSortOrder
  , since: Nothing
  , until: Nothing
  , sinceStr: fromMaybe "" p.since
  , untilStr: fromMaybe "" p.until
  , currentPage: fromMaybe 1 p.page
  , hasNextPage: true
  , pageCursors: []
  }

-- --------------------------------------------------------------------------
-- Converting between URL params and component state
-- --------------------------------------------------------------------------

-- | Build URL parameters from the current component state, omitting defaults.
stateToParams :: State -> JobsListParams
stateToParams s =
  { range: case s.timeRange of
      Last24Hours -> Nothing
      _ -> Just (printTimeRange s.timeRange)
  , status: case s.filters.statusFilter of
      AllStatuses -> Nothing
      sf -> Just (printStatusFilter sf)
  , jobType: map printJobType s.filters.jobType
  , package: case String.trim s.filters.packageName of
      "" -> Nothing
      v -> Just v
  , version: case String.trim s.filters.packageVersion of
      "" -> Nothing
      v -> Just v
  , compiler: case String.trim s.filters.compilerVersion of
      "" -> Nothing
      v -> Just v
  , autoRefresh: case s.autoRefresh of
      false -> Nothing
      true -> Just true
  , since: case s.timeRange of
      Custom | s.sinceStr /= "" -> Just s.sinceStr
      _ -> Nothing
  , until: case s.timeRange of
      Custom | s.untilStr /= "" -> Just s.untilStr
      _ -> Nothing
  , order:
      if s.sortOrder == defaultSortOrder then Nothing
      else Just (if s.sortOrder == ASC then "asc" else "desc")
  , page:
      if s.currentPage <= 1 then Nothing
      else Just s.currentPage
  }

-- --------------------------------------------------------------------------
-- TimeRange serialization
-- --------------------------------------------------------------------------

printTimeRange :: TimeRange -> String
printTimeRange = case _ of
  UntilNow -> "all"
  LastHour -> "1h"
  Last24Hours -> "24h"
  LastWeek -> "1w"
  Custom -> "custom"

parseTimeRange :: String -> TimeRange
parseTimeRange = case _ of
  "all" -> UntilNow
  "1h" -> LastHour
  "24h" -> Last24Hours
  "1w" -> LastWeek
  "custom" -> Custom
  _ -> Last24Hours

-- --------------------------------------------------------------------------
-- StatusFilter serialization
-- --------------------------------------------------------------------------

printStatusFilter :: StatusFilter -> String
printStatusFilter = case _ of
  ActiveOnly -> "active"
  AllStatuses -> "all"
  OnlyPending -> "pending"
  OnlyRunning -> "running"
  OnlySucceeded -> "succeeded"
  OnlyFailed -> "failed"

-- --------------------------------------------------------------------------
-- JobType serialization
-- --------------------------------------------------------------------------

printJobType :: JobType -> String
printJobType = case _ of
  PublishJobType -> "publish"
  UnpublishJobType -> "unpublish"
  TransferJobType -> "transfer"
  MatrixJobType -> "matrix"
  PackageSetJobType -> "packageset"

-- --------------------------------------------------------------------------
-- Rendering
-- --------------------------------------------------------------------------

render :: forall m. State -> H.ComponentHTML Action () m
render state = do
  let filteredJobs = applyFilters state.filters state.jobs
  HH.div_
    [ renderPageTitle
    , renderToolbar state
    , renderContent state filteredJobs
    , renderPagination state
    ]

renderPageTitle :: forall w i. HH.HTML w i
renderPageTitle =
  HH.div [ HP.class_ (HH.ClassName "page-title") ]
    [ HH.h1 [ HP.class_ (HH.ClassName "page-title__title") ] [ HH.text "Jobs" ]
    ]

-- | The toolbar is a single bar with three logical zones separated by
-- | vertical dividers: Query (time range) | Filters (client-side) | Actions
-- | (refresh controls).
renderToolbar :: forall m. State -> H.ComponentHTML Action () m
renderToolbar state =
  HH.div [ HP.class_ (HH.ClassName "jobs-toolbar") ]
    ( [ HH.div [ HP.class_ (HH.ClassName "jobs-toolbar__zone jobs-toolbar__zone--query") ]
          [ renderField "TIME RANGE" $ renderTimeRangeSelect state ]
      , HH.div [ HP.class_ (HH.ClassName "jobs-toolbar__divider") ] []
      , HH.div [ HP.class_ (HH.ClassName "jobs-toolbar__zone jobs-toolbar__zone--filters") ]
          ( [ renderField "STATUS" $ renderStatusSelect state.filters.statusFilter
            , renderField "TYPE" $ renderTypeSelect state.filters.jobType
            , renderField "PACKAGE" $ renderTextFilter "Package name" state.filters.packageName SetFilterPackageName
            , renderField "VERSION" $ renderTextFilter "Version" state.filters.packageVersion SetFilterPackageVersion
            , renderField "COMPILER" $ renderTextFilter "Compiler" state.filters.compilerVersion SetFilterCompilerVersion
            ]
              <> clearLink
          )
      , HH.div [ HP.class_ (HH.ClassName "jobs-toolbar__divider") ] []
      , HH.div [ HP.class_ (HH.ClassName "jobs-toolbar__zone jobs-toolbar__zone--actions") ]
          [ renderField "REFRESH" $
              HH.label [ HP.class_ (HH.ClassName "toolbar-toggle") ]
                [ HH.input
                    [ HP.type_ HP.InputCheckbox
                    , HP.checked state.autoRefresh
                    , HE.onChecked ToggleAutoRefresh
                    ]
                , HH.text " Auto "
                , HH.span
                    [ HP.class_
                        ( HH.ClassName
                            ( "refresh-indicator"
                                <> if state.autoRefresh then "" else " refresh-indicator--inactive"
                            )
                        )
                    ]
                    []
                ]
          ]
      ]
        <> customRangeRow
    )
  where
  clearLink
    | hasActiveFilters state.filters =
        [ HH.button
            [ HP.class_ (HH.ClassName "jobs-toolbar__clear")
            , HE.onClick \_ -> ClearFilters
            ]
            [ HH.text "Clear" ]
        ]
    | otherwise = []

  customRangeRow
    | state.timeRange == Custom =
        let
          sinceDatePart = String.take 10 state.sinceStr
          sinceTimePart = String.drop 11 state.sinceStr
          untilDatePart = String.take 10 state.untilStr
          untilTimePart = String.drop 11 state.untilStr
        in
          [ HH.div [ HP.class_ (HH.ClassName "jobs-toolbar__custom-range") ]
              [ HH.label [ HP.class_ (HH.ClassName "toolbar-field__label") ] [ HH.text "FROM" ]
              , HH.input
                  [ HP.type_ HP.InputDate
                  , HP.class_ (HH.ClassName "toolbar-input")
                  , HP.value sinceDatePart
                  , HE.onValueChange SetCustomSinceDate
                  ]
              , HH.input
                  [ HP.type_ HP.InputTime
                  , HP.class_ (HH.ClassName "toolbar-input")
                  , HP.value sinceTimePart
                  , HE.onValueChange SetCustomSinceTime
                  ]
              , HH.label [ HP.class_ (HH.ClassName "toolbar-field__label") ] [ HH.text "TO" ]
              , HH.input
                  [ HP.type_ HP.InputDate
                  , HP.class_ (HH.ClassName "toolbar-input")
                  , HP.value untilDatePart
                  , HE.onValueChange SetCustomUntilDate
                  ]
              , HH.input
                  [ HP.type_ HP.InputTime
                  , HP.class_ (HH.ClassName "toolbar-input")
                  , HP.value untilTimePart
                  , HE.onValueChange SetCustomUntilTime
                  ]
              ]
          ]
    | otherwise = []

-- | A labeled field: small uppercase label above the control.
renderField :: forall m. String -> H.ComponentHTML Action () m -> H.ComponentHTML Action () m
renderField label control =
  HH.div [ HP.class_ (HH.ClassName "toolbar-field") ]
    [ HH.span [ HP.class_ (HH.ClassName "toolbar-field__label") ] [ HH.text label ]
    , control
    ]

-- | A sortable column header. Shows the sort indicator only when this column
-- | is the active sort field.
sortableHeader :: forall m. State -> SortField -> String -> H.ComponentHTML Action () m
sortableHeader state field label =
  HH.th
    [ HP.class_ (HH.ClassName "jobs-table__th jobs-table__th--sortable")
    , HE.onClick \_ -> SetSort field
    ]
    ( [ HH.text (label <> " ") ]
        <>
          if state.sortField == field then
            [ HH.span [ HP.class_ (HH.ClassName "sort-indicator") ]
                [ HH.text (if state.sortOrder == DESC then "\x25BC" else "\x25B2") ]
            ]
          else []
    )

renderTimeRangeSelect :: forall m. State -> H.ComponentHTML Action () m
renderTimeRangeSelect state =
  HH.select
    [ HP.class_ (HH.ClassName "toolbar-select")
    , HE.onValueChange handleChange
    ]
    [ HH.option [ HP.value "all", HP.selected (state.timeRange == UntilNow) ] [ HH.text "Until now" ]
    , HH.option [ HP.value "1", HP.selected (state.timeRange == LastHour) ] [ HH.text "Last hour" ]
    , HH.option [ HP.value "24", HP.selected (state.timeRange == Last24Hours) ] [ HH.text "Last 24 hours" ]
    , HH.option [ HP.value "168", HP.selected (state.timeRange == LastWeek) ] [ HH.text "Last week" ]
    , HH.option [ HP.value "custom", HP.selected (state.timeRange == Custom) ] [ HH.text "Custom" ]
    ]
  where
  handleChange val = SetTimeRange case val of
    "1" -> LastHour
    "24" -> Last24Hours
    "168" -> LastWeek
    "custom" -> Custom
    _ -> UntilNow

renderStatusSelect :: forall m. StatusFilter -> H.ComponentHTML Action () m
renderStatusSelect current =
  HH.select
    [ HP.class_ (HH.ClassName "toolbar-select")
    , HE.onValueChange SetFilterStatus
    ]
    [ HH.option [ HP.value "active", HP.selected (current == ActiveOnly) ] [ HH.text "Active" ]
    , HH.option [ HP.value "all", HP.selected (current == AllStatuses) ] [ HH.text "All" ]
    , HH.option [ HP.value "pending", HP.selected (current == OnlyPending) ] [ HH.text "Pending" ]
    , HH.option [ HP.value "running", HP.selected (current == OnlyRunning) ] [ HH.text "Running" ]
    , HH.option [ HP.value "succeeded", HP.selected (current == OnlySucceeded) ] [ HH.text "Succeeded" ]
    , HH.option [ HP.value "failed", HP.selected (current == OnlyFailed) ] [ HH.text "Failed" ]
    ]

renderTypeSelect :: forall m. Maybe JobType -> H.ComponentHTML Action () m
renderTypeSelect current =
  HH.select
    [ HP.class_ (HH.ClassName "toolbar-select")
    , HE.onValueChange SetFilterJobType
    ]
    [ HH.option [ HP.value "", HP.selected (isNothing current) ] [ HH.text "All" ]
    , HH.option [ HP.value "publish", HP.selected (current == Just PublishJobType) ] [ HH.text "Publish" ]
    , HH.option [ HP.value "unpublish", HP.selected (current == Just UnpublishJobType) ] [ HH.text "Unpublish" ]
    , HH.option [ HP.value "transfer", HP.selected (current == Just TransferJobType) ] [ HH.text "Transfer" ]
    , HH.option [ HP.value "matrix", HP.selected (current == Just MatrixJobType) ] [ HH.text "Matrix" ]
    , HH.option [ HP.value "packageset", HP.selected (current == Just PackageSetJobType) ] [ HH.text "Package Set" ]
    ]

renderTextFilter :: forall m. String -> String -> (String -> Action) -> H.ComponentHTML Action () m
renderTextFilter placeholder current action =
  HH.input
    [ HP.class_ (HH.ClassName "toolbar-input")
    , HP.placeholder placeholder
    , HP.value current
    , HE.onValueInput action
    ]

renderContent :: forall m. State -> Array JobSummary -> H.ComponentHTML Action () m
renderContent state filteredJobs
  | state.loading =
      HH.div [ HP.class_ (HH.ClassName "loading-state") ]
        [ HH.div [ HP.class_ (HH.ClassName "spinner") ] []
        , HH.p_ [ HH.text "Loading jobs..." ]
        ]
  | Just err <- state.error =
      HH.div [ HP.class_ (HH.ClassName "error-state") ]
        [ HH.p [ HP.class_ (HH.ClassName "error-message") ] [ HH.text err ]
        , HH.button
            [ HP.class_ (HH.ClassName "toolbar-btn")
            , HE.onClick \_ -> FetchJobs
            ]
            [ HH.text "Retry" ]
        ]
  | Array.null state.jobs =
      HH.div [ HP.class_ (HH.ClassName "empty-state") ]
        [ HH.p_ [ HH.text "No jobs found for the selected time range." ] ]
  | Array.null filteredJobs =
      HH.div [ HP.class_ (HH.ClassName "empty-state") ]
        [ HH.p_
            [ HH.text "No jobs match the current filters. "
            , HH.button
                [ HP.class_ (HH.ClassName "jobs-toolbar__clear")
                , HE.onClick \_ -> ClearFilters
                ]
                [ HH.text "Clear filters" ]
            ]
        ]
  | otherwise =
      HH.div_
        [ renderPagination state
        , HH.table [ HP.class_ (HH.ClassName "jobs-table") ]
            [ HH.thead_
                [ HH.tr_
                    [ HH.th [ HP.class_ (HH.ClassName "jobs-table__th jobs-table__th--rownum") ] [ HH.text "#" ]
                    , HH.th [ HP.class_ (HH.ClassName "jobs-table__th") ] [ HH.text "Type" ]
                    , HH.th [ HP.class_ (HH.ClassName "jobs-table__th") ] [ HH.text "Package" ]
                    , HH.th [ HP.class_ (HH.ClassName "jobs-table__th") ] [ HH.text "Status" ]
                    , sortableHeader state SortByCreated "Created"
                    , HH.th [ HP.class_ (HH.ClassName "jobs-table__th") ] [ HH.text "Started" ]
                    , HH.th [ HP.class_ (HH.ClassName "jobs-table__th") ] [ HH.text "Compiler" ]
                    , HH.th [ HP.class_ (HH.ClassName "jobs-table__th") ] [ HH.text "Duration" ]
                    ]
                ]
            , HH.tbody_ (Array.mapWithIndex renderJobRow filteredJobs)
            ]
        ]

renderJobRow :: forall m. Int -> JobSummary -> H.ComponentHTML Action () m
renderJobRow index summary = do
  let jobIdStr = unwrap summary.jobId
  let jobTypeName = V1.printJobType summary.jobType
  let statusName = Job.printStatus (Job.deriveStatus summary)
  HH.tr
    [ HP.class_ (HH.ClassName "jobs-table__row")
    , HE.onClick \_ -> NavigateToJobDetail jobIdStr
    ]
    [ HH.td [ HP.class_ (HH.ClassName "jobs-table__td jobs-table__td--rownum") ]
        [ HH.text (show (index + 1)) ]
    , HH.td [ HP.class_ (HH.ClassName "jobs-table__td") ]
        [ HH.span [ HP.class_ (HH.ClassName ("job-type-badge job-type-badge--" <> jobTypeName)) ]
            [ HH.text jobTypeName ]
        ]
    , HH.td [ HP.class_ (HH.ClassName "jobs-table__td") ]
        [ HH.span [ HP.class_ (HH.ClassName "job-package") ]
            [ HH.text (fromMaybe "\x2014" (map PackageName.print summary.packageName)) ]
        , case summary.packageVersion of
            Just v -> HH.span [ HP.class_ (HH.ClassName "job-version") ] [ HH.text ("@" <> Version.print v) ]
            Nothing -> HH.text ""
        ]
    , HH.td [ HP.class_ (HH.ClassName "jobs-table__td") ]
        [ HH.span [ HP.class_ (HH.ClassName ("job-status job-status--" <> statusName)) ]
            [ HH.text statusName ]
        ]
    , HH.td [ HP.class_ (HH.ClassName "jobs-table__td") ]
        [ HH.text (Job.formatTimestamp summary.createdAt) ]
    , HH.td [ HP.class_ (HH.ClassName "jobs-table__td") ]
        [ HH.text (fromMaybe "\x2014" (map Job.formatTimestamp summary.startedAt)) ]
    , HH.td [ HP.class_ (HH.ClassName "jobs-table__td") ]
        [ HH.text (fromMaybe "\x2014" (map Version.print summary.compilerVersion)) ]
    , HH.td [ HP.class_ (HH.ClassName "jobs-table__td") ]
        [ HH.text (computeDuration summary) ]
    ]

-- | Render pagination controls showing the current page and prev/next buttons.
renderPagination :: forall m. State -> H.ComponentHTML Action () m
renderPagination state
  | state.loading || Array.null state.jobs = HH.text ""
  | otherwise =
      HH.div [ HP.class_ (HH.ClassName "jobs-nav") ]
        [ HH.button
            [ HP.class_ (HH.ClassName "jobs-nav__btn")
            , HP.disabled (state.currentPage <= 1)
            , HE.onClick \_ -> PrevPage
            ]
            [ HH.text "\x25C0" ]
        , HH.span [ HP.class_ (HH.ClassName "jobs-nav__info") ]
            [ HH.text ("Page " <> show state.currentPage) ]
        , HH.button
            [ HP.class_ (HH.ClassName "jobs-nav__btn")
            , HP.disabled (not state.hasNextPage)
            , HE.onClick \_ -> NextPage
            ]
            [ HH.text "\x25B6" ]
        ]

-- --------------------------------------------------------------------------
-- Action handling
-- --------------------------------------------------------------------------

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize ->
    handleAction FetchJobs

  -- When the parent passes new params (e.g. the user clicked the title to
  -- go home), re-apply them if they differ from the current component state.
  Receive input -> do
    state <- H.get
    let currentParams = stateToParams state
    H.modify_ _ { apiConfig = input.apiConfig }
    when (input.params /= currentParams) do
      let p = input.params
      H.modify_ _
        { timeRange = parseTimeRange (fromMaybe "" p.range)
        , filters =
            { jobType: p.jobType >>= parseJobType
            , packageName: fromMaybe "" p.package
            , packageVersion: fromMaybe "" p.version
            , compilerVersion: fromMaybe "" p.compiler
            , statusFilter: parseStatusFilter (fromMaybe "" p.status)
            }
        , sortOrder = case p.order of
            Just "asc" -> ASC
            _ -> defaultSortOrder
        , sinceStr = fromMaybe "" p.since
        , untilStr = fromMaybe "" p.until
        , currentPage = fromMaybe 1 p.page
        , since = Nothing
        , until = Nothing
        , pageCursors = []
        , hasNextPage = true
        }
      handleAction FetchJobs

  FetchJobs -> do
    H.modify_ _ { loading = true, error = Nothing }
    result <- doFetchJobs
    handleAction (HandleFetchResult result)

  FetchJobsSilent -> do
    result <- doFetchJobs
    handleAction (HandleFetchResult result)

  HandleFetchResult result -> case result of
    Left err -> do
      let msg = API.printApiError err
      let
        displayMsg =
          if String.contains (String.Pattern "Failed to fetch") msg then
            "Unable to reach the registry API. This may be a CORS configuration issue."
          else
            msg
      H.modify_ _ { loading = false, error = Just displayMsg, jobs = [] }
    Right jobs -> do
      state <- H.get
      let summaries = map Job.toJobSummary jobs
      let newFingerprints = map jobFingerprint summaries
      let oldFingerprints = map jobFingerprint state.jobs
      -- Skip the state update when the set of jobs hasn't changed. This
      -- avoids VDOM diffing on every auto-refresh tick when nothing new
      -- has arrived.
      unless (not state.loading && newFingerprints == oldFingerprints) do
        let hasNext = Array.length jobs >= pageSize
        H.modify_ _ { loading = false, error = Nothing, jobs = summaries, hasNextPage = hasNext }

  SetTimeRange range -> do
    when (range == Custom) do
      now <- liftEffect Now.nowDateTime
      let sinceDefault = subtractHours 24.0 now
      H.modify_ _ { sinceStr = Job.formatDateTimeLocal sinceDefault, untilStr = Job.formatDateTimeLocal now }
    H.modify_ _ { timeRange = range, since = Nothing, until = Nothing, currentPage = 1, pageCursors = [], hasNextPage = true }
    handleAction FetchJobs
    notifyFiltersChanged

  SetCustomSinceDate dateVal -> do
    state <- H.get
    let timePart = String.drop 11 state.sinceStr
    let newSince = dateVal <> "T" <> (if timePart == "" then "00:00" else timePart)
    updateCustomSince newSince

  SetCustomSinceTime timeVal -> do
    state <- H.get
    let datePart = String.take 10 state.sinceStr
    let newSince = (if datePart == "" then "1970-01-01" else datePart) <> "T" <> timeVal
    updateCustomSince newSince

  SetCustomUntilDate dateVal -> do
    state <- H.get
    let timePart = String.drop 11 state.untilStr
    let newUntil = dateVal <> "T" <> (if timePart == "" then "00:00" else timePart)
    updateCustomUntil newUntil

  SetCustomUntilTime timeVal -> do
    state <- H.get
    let datePart = String.take 10 state.untilStr
    let newUntil = (if datePart == "" then "1970-01-01" else datePart) <> "T" <> timeVal
    updateCustomUntil newUntil

  ToggleAutoRefresh enabled -> do
    state <- H.get
    for_ state.refreshSubId H.unsubscribe
    if enabled then do
      subId <- H.subscribe =<< Job.timerEmitter refreshInterval Tick
      H.modify_ _ { autoRefresh = true, refreshSubId = Just subId }
    else
      H.modify_ _ { autoRefresh = false, refreshSubId = Nothing }
    notifyFiltersChanged

  SetFilterJobType val -> updateFilter _ { jobType = parseJobType val }
  SetFilterPackageName val -> updateFilter _ { packageName = val }
  SetFilterPackageVersion val -> updateFilter _ { packageVersion = val }
  SetFilterCompilerVersion val -> updateFilter _ { compilerVersion = val }

  SetFilterStatus val -> do
    let sf = parseStatusFilter val
    let needsRefetch s = statusFilterNeedsRefetch s.filters.statusFilter sf
    state <- H.get
    updateFilter _ { statusFilter = sf }
    -- Re-fetch when switching between Active and other modes, because
    -- Active excludes completed jobs server-side.
    when (needsRefetch state) do
      H.modify_ _ { currentPage = 1, pageCursors = [], hasNextPage = true }
      handleAction FetchJobs

  ClearFilters -> do
    H.modify_ _ { filters = emptyFilters, sortOrder = defaultSortOrder, currentPage = 1, pageCursors = [], hasNextPage = true }
    notifyFiltersChanged

  SetSort field -> do
    H.modify_ \s -> do
      let newOrder = if s.sortField == field then (if s.sortOrder == DESC then ASC else DESC) else DESC
      s { sortField = field, sortOrder = newOrder, currentPage = 1, pageCursors = [], hasNextPage = true }
    handleAction FetchJobs
    notifyFiltersChanged

  NavigateToJobDetail jobId ->
    H.raise (NavigateToJob jobId)

  NextPage -> do
    state <- H.get
    when (state.hasNextPage && not state.loading) do
      let
        cursor = case state.sortOrder of
          DESC -> extremeCreatedAt min state.jobs
          ASC -> extremeCreatedAt max state.jobs
      case cursor of
        Nothing -> pure unit
        Just ts -> do
          let newCursors = state.pageCursors <> [ ts ]
          H.modify_ _ { currentPage = state.currentPage + 1, pageCursors = newCursors }
          handleAction FetchJobs
          notifyFiltersChanged

  PrevPage -> do
    state <- H.get
    when (state.currentPage > 1) do
      let newCursors = fromMaybe [] (Array.init state.pageCursors)
      H.modify_ _ { currentPage = state.currentPage - 1, pageCursors = newCursors, hasNextPage = true }
      handleAction FetchJobs
      notifyFiltersChanged

  Tick ->
    handleAction FetchJobsSilent

-- --------------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------------

-- | Update one filter field, and notify the parent.
updateFilter :: forall m. MonadAff m => (Filters -> Filters) -> H.HalogenM State Action () Output m Unit
updateFilter f = do
  H.modify_ \s -> s { filters = f s.filters }
  notifyFiltersChanged

-- | Lightweight fingerprint of a job for equality checking.
jobFingerprint :: JobSummary -> { jobId :: String, finishedAt :: Maybe DateTime, success :: Boolean }
jobFingerprint job =
  { jobId: unwrap job.jobId
  , finishedAt: job.finishedAt
  , success: job.success
  }

-- | Build query parameters from the current state and fetch jobs from the API.
doFetchJobs :: forall m. MonadAff m => H.HalogenM State Action () Output m (Either ApiError (Array Job))
doFetchJobs = do
  state <- H.get
  now <- liftEffect Now.nowDateTime
  let
    customSince = Job.parseDateTimeLocal state.sinceStr
    customUntil = Job.parseDateTimeLocal state.untilStr
    baseSince = state.since <|> case state.timeRange of
      Custom -> customSince
      UntilNow -> Nothing
      _ -> Just (subtractHours (timeRangeHours state.timeRange) now)
    baseUntil = state.until <|> case state.timeRange of
      Custom -> customUntil
      UntilNow -> Just now
      _ -> Nothing
    pageCursor = Array.index state.pageCursors (state.currentPage - 2)
    since = case state.sortOrder of
      DESC -> baseSince
      ASC ->
        if state.currentPage > 1 then pageCursor
        else baseSince
    until = case state.sortOrder of
      DESC ->
        if state.currentPage > 1 then pageCursor
        else baseUntil
      ASC -> baseUntil
    includeCompleted = Just (state.filters.statusFilter /= ActiveOnly)
  H.liftAff $ API.fetchJobs state.apiConfig { since, until, order: Just state.sortOrder, includeCompleted }

-- | Update the combined sinceStr from a date or time part change, fetch if
-- | both endpoints parse, and sync the URL.
updateCustomSince :: forall m. MonadAff m => String -> H.HalogenM State Action () Output m Unit
updateCustomSince newSince = do
  H.modify_ _ { sinceStr = newSince, since = Nothing, until = Nothing, currentPage = 1, pageCursors = [], hasNextPage = true }
  state <- H.get
  case Job.parseDateTimeLocal newSince, Job.parseDateTimeLocal state.untilStr of
    Just _, Just _ -> handleAction FetchJobs
    _, _ -> pure unit
  notifyFiltersChanged

-- | Update the combined untilStr from a date or time part change, fetch if
-- | both endpoints parse, and sync the URL.
updateCustomUntil :: forall m. MonadAff m => String -> H.HalogenM State Action () Output m Unit
updateCustomUntil newUntil = do
  H.modify_ _ { untilStr = newUntil, since = Nothing, until = Nothing, currentPage = 1, pageCursors = [], hasNextPage = true }
  state <- H.get
  case Job.parseDateTimeLocal state.sinceStr, Job.parseDateTimeLocal newUntil of
    Just _, Just _ -> handleAction FetchJobs
    _, _ -> pure unit
  notifyFiltersChanged

-- | Notify the parent component that filter state has changed so the URL can
-- | be updated.
notifyFiltersChanged :: forall m. MonadAff m => H.HalogenM State Action () Output m Unit
notifyFiltersChanged = do
  state <- H.get
  H.raise (FiltersChanged (stateToParams state))

refreshInterval :: Milliseconds
refreshInterval = Milliseconds 5000.0

-- | Find the extreme (min or max) `createdAt` timestamp among job summaries.
extremeCreatedAt :: (DateTime -> DateTime -> DateTime) -> Array JobSummary -> Maybe DateTime
extremeCreatedAt pick = Array.foldl (\acc s -> Just (maybe s.createdAt (pick s.createdAt) acc)) Nothing
  where
  maybe fallback f = case _ of
    Nothing -> fallback
    Just x -> f x

-- | Apply client-side filters to an array of job summaries.
applyFilters :: Filters -> Array JobSummary -> Array JobSummary
applyFilters filters = Array.filter matchesAll
  where
  matchesAll summary =
    matchesType summary
      && matchesPackageName summary
      && matchesPackageVersion summary
      && matchesCompilerVersion summary
      && matchesStatusFilter summary

  matchesType summary = case filters.jobType of
    Nothing -> true
    Just jt -> summary.jobType == jt

  matchesPackageName summary = case String.trim filters.packageName of
    "" -> true
    needle -> case summary.packageName of
      Nothing -> false
      Just name -> String.contains (String.Pattern (String.toLower needle)) (String.toLower (PackageName.print name))

  matchesPackageVersion summary = case String.trim filters.packageVersion of
    "" -> true
    needle -> case summary.packageVersion of
      Nothing -> false
      Just ver -> String.contains (String.Pattern (String.toLower needle)) (String.toLower (Version.print ver))

  matchesCompilerVersion summary = case String.trim filters.compilerVersion of
    "" -> true
    needle -> case summary.compilerVersion of
      Nothing -> false
      Just ver -> String.contains (String.Pattern (String.toLower needle)) (String.toLower (Version.print ver))

  matchesStatusFilter summary = do
    let s = Job.deriveStatus summary
    case filters.statusFilter of
      ActiveOnly -> s == Pending || s == Running
      AllStatuses -> true
      OnlyPending -> s == Pending
      OnlyRunning -> s == Running
      OnlySucceeded -> s == Succeeded
      OnlyFailed -> s == Failed

-- | Returns true when at least one client-side filter deviates from defaults.
hasActiveFilters :: Filters -> Boolean
hasActiveFilters f =
  f.statusFilter /= AllStatuses
    || isJust f.jobType
    || String.trim f.packageName /= ""
    || String.trim f.packageVersion /= ""
    || String.trim f.compilerVersion /= ""

-- | Parse a job type filter value from a select element.
parseJobType :: String -> Maybe JobType
parseJobType = case _ of
  "publish" -> Just PublishJobType
  "unpublish" -> Just UnpublishJobType
  "transfer" -> Just TransferJobType
  "matrix" -> Just MatrixJobType
  "packageset" -> Just PackageSetJobType
  _ -> Nothing

-- | Parse a status filter value from a select element.
parseStatusFilter :: String -> StatusFilter
parseStatusFilter = case _ of
  "all" -> AllStatuses
  "pending" -> OnlyPending
  "running" -> OnlyRunning
  "succeeded" -> OnlySucceeded
  "failed" -> OnlyFailed
  "active" -> ActiveOnly
  _ -> AllStatuses

-- | Determine whether changing the status filter requires a server re-fetch.
statusFilterNeedsRefetch :: StatusFilter -> StatusFilter -> Boolean
statusFilterNeedsRefetch old new = (old == ActiveOnly) /= (new == ActiveOnly)

-- | Subtract a number of hours from a DateTime.
subtractHours :: Number -> DateTime -> DateTime
subtractHours hours dt = do
  let totalSeconds = hours * 3600.0
  let duration = Seconds (negate totalSeconds)
  fromMaybe dt (DateTime.adjust duration dt)

-- | Compute the duration string for a job.
computeDuration :: forall r. { startedAt :: Maybe DateTime, finishedAt :: Maybe DateTime | r } -> String
computeDuration job = case job.startedAt of
  Nothing -> "\x2014"
  Just started -> case job.finishedAt of
    Nothing -> "running..."
    Just finished -> Job.formatDurationBetween started finished
