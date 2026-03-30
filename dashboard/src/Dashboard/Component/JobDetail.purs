-- | The Job Detail component displays detailed information about a single
-- | registry job, including its metadata, payload, and a log viewer with
-- | filtering, pagination, and auto-refresh.
module Dashboard.Component.JobDetail
  ( Input
  , Output(..)
  , component
  ) where

import Prelude

import Dashboard.API (ApiConfig, ApiError)
import Dashboard.API as API
import Dashboard.Job as Job
import Data.Array as Array
import Data.Codec.JSON as CJ
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (unwrap)
import Effect.Aff (Milliseconds(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Now as Now
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import JSON as JSON
import Registry.API.V1 (Job(..), JobId(..), LogLevel(..), LogLine, SortOrder(..))
import Registry.API.V1 as V1
import Registry.Internal.Codec as Internal.Codec
import Registry.Operation as Operation
import Registry.PackageName as PackageName
import Registry.Version (Version)
import Registry.Version as Version
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

-- --------------------------------------------------------------------------
-- Public types
-- --------------------------------------------------------------------------

-- | The component input: the job ID to display and the API configuration.
type Input = { jobId :: String, apiConfig :: ApiConfig }

-- | The component output signals navigation back to the jobs list.
data Output = NavigateBack

-- --------------------------------------------------------------------------
-- Internal types
-- --------------------------------------------------------------------------

type State =
  { apiConfig :: ApiConfig
  , jobId :: String
  , job :: Maybe Job
  , loading :: Boolean
  , error :: Maybe String
  , logLevel :: LogLevel
  , allLogs :: Array LogLine
  , lastLogTimestamp :: Maybe DateTime
  , logSortOrder :: SortOrder
  , logAutoRefresh :: Boolean
  , logRefreshSubId :: Maybe H.SubscriptionId
  , payloadCollapsed :: Boolean
  , logUntil :: Maybe DateTime
  , logPage :: Int
  , currentTime :: Maybe DateTime
  }

data Action
  = Initialize
  | FetchJob
  | HandleFetchResult (Either ApiError Job)
  | SetLogLevel String
  | HandleLogLevelResult (Either ApiError Job)
  | ToggleLogAutoRefresh Boolean
  | ToggleLogSortOrder
  | LogRefreshTick
  | HandleLogRefreshResult (Either ApiError Job)
  | TogglePayload
  | LogPrevPage
  | LogNextPage
  | GoBack MouseEvent
  | Receive Input

-- --------------------------------------------------------------------------
-- Component
-- --------------------------------------------------------------------------

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
initialState input =
  { apiConfig: input.apiConfig
  , jobId: input.jobId
  , job: Nothing
  , loading: true
  , error: Nothing
  , logLevel: Info
  , allLogs: []
  , lastLogTimestamp: Nothing
  , logSortOrder: DESC
  , logAutoRefresh: true
  , logRefreshSubId: Nothing
  , payloadCollapsed: false
  , logUntil: Nothing
  , logPage: 0
  , currentTime: Nothing
  }

-- --------------------------------------------------------------------------
-- Rendering
-- --------------------------------------------------------------------------

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ renderBreadcrumb state
    , renderPageTitle state
    , renderContent state
    ]

renderBreadcrumb :: forall m. State -> H.ComponentHTML Action () m
renderBreadcrumb state =
  HH.nav [ HP.class_ (HH.ClassName "breadcrumb-bar") ]
    [ HH.div [ HP.class_ (HH.ClassName "breadcrumb-bar__inner") ]
        [ HH.a
            [ HP.class_ (HH.ClassName "breadcrumb-bar__link")
            , HP.href "#/"
            , HE.onClick GoBack
            ]
            [ HH.text "Jobs" ]
        , HH.span [ HP.class_ (HH.ClassName "breadcrumb-bar__sep") ] [ HH.text "/" ]
        , HH.span [ HP.class_ (HH.ClassName "breadcrumb-bar__current") ]
            [ HH.text state.jobId ]
        ]
    ]

renderPageTitle :: forall m. State -> H.ComponentHTML Action () m
renderPageTitle state = case state.job of
  Nothing ->
    HH.div [ HP.class_ (HH.ClassName "page-title clearfix") ]
      [ HH.h1 [ HP.class_ (HH.ClassName "page-title__title") ]
          [ HH.text (if state.loading then "Loading..." else "Job") ]
      ]
  Just job -> do
    let jobTypeName = V1.printJobType (Job.getJobType job)
    let
      titleText = case Job.getPackageName job of
        Just name -> PackageName.print name <> fromMaybe "" (map (\v -> "@" <> Version.print v) (Job.getPackageVersion job))
        Nothing -> "Package Set Update"
    HH.div [ HP.class_ (HH.ClassName "page-title clearfix") ]
      [ HH.h1 [ HP.class_ (HH.ClassName "page-title__title") ]
          [ HH.span [ HP.class_ (HH.ClassName ("job-type-badge job-type-badge--" <> jobTypeName)) ]
              [ HH.text jobTypeName ]
          , HH.text (" " <> titleText)
          ]
      ]

renderContent :: forall m. State -> H.ComponentHTML Action () m
renderContent state
  | state.loading =
      HH.div [ HP.class_ (HH.ClassName "loading-state") ]
        [ HH.div [ HP.class_ (HH.ClassName "spinner") ] []
        , HH.p_ [ HH.text "Loading job..." ]
        ]
  | Just err <- state.error =
      HH.div [ HP.class_ (HH.ClassName "error-state") ]
        [ HH.p [ HP.class_ (HH.ClassName "error-message") ] [ HH.text err ] ]
  | Just job <- state.job =
      renderJobDetail state job
  | otherwise =
      HH.div [ HP.class_ (HH.ClassName "error-state") ]
        [ HH.p [ HP.class_ (HH.ClassName "error-message") ] [ HH.text "Job not found." ] ]

renderJobDetail :: forall m. State -> Job -> H.ComponentHTML Action () m
renderJobDetail state job = do
  let info = V1.jobInfo job
  let statusName = Job.printStatus (Job.deriveStatus info)
  HH.div [ HP.class_ (HH.ClassName "job-detail") ]
    [ renderInfoBlock state.currentTime info statusName (Job.getCompilerVersion job)
    , renderPayloadSection state job
    , renderLogsSection state
    ]

renderInfoBlock :: forall m. Maybe DateTime -> V1.JobInfo () -> String -> Maybe Version -> H.ComponentHTML Action () m
renderInfoBlock mNow info statusName compiler = do
  let waitDuration = computeDurationBetween mNow info.createdAt info.startedAt
  let runDuration = map (\s -> computeDurationBetween mNow s info.finishedAt) info.startedAt
  HH.div [ HP.class_ (HH.ClassName "job-detail__timestamps") ]
    ( Array.catMaybes
        [ Just $ renderInfoRow "Job ID"
            (HH.code [ HP.class_ (HH.ClassName "job-detail__ts-value") ] [ HH.text (unwrap info.jobId) ])
        , Just $ renderInfoRow "Status"
            ( HH.span [ HP.class_ (HH.ClassName ("job-status job-status--" <> statusName)) ]
                [ HH.text statusName ]
            )
        , map (\c -> renderInfoRow "Compiler" (tsValue (Version.print c))) compiler
        , Just $ renderInfoRow "Created" (tsValue (Job.formatTimestamp info.createdAt))
        , Just $ renderInfoRow "Started" (tsValue (fromMaybe "\x2014" (map Job.formatTimestamp info.startedAt)))
        , Just $ renderInfoRow "Finished" (tsValue (fromMaybe "\x2014" (map Job.formatTimestamp info.finishedAt)))
        , if isJust info.startedAt then Just $ renderInfoRow "Wait time" (tsValue waitDuration)
          else Nothing
        , map (\dur -> renderInfoRow "Duration" (tsValue dur)) runDuration
        ]
    )

renderInfoRow :: forall m. String -> H.ComponentHTML Action () m -> H.ComponentHTML Action () m
renderInfoRow label value =
  HH.div [ HP.class_ (HH.ClassName "job-detail__ts-row") ]
    [ HH.span [ HP.class_ (HH.ClassName "job-detail__ts-label") ] [ HH.text label ]
    , value
    ]

tsValue :: forall m. String -> H.ComponentHTML Action () m
tsValue text = HH.span [ HP.class_ (HH.ClassName "job-detail__ts-value") ] [ HH.text text ]

renderPayloadSection :: forall m. State -> Job -> H.ComponentHTML Action () m
renderPayloadSection state job =
  HH.div [ HP.class_ (HH.ClassName "job-detail__section") ]
    [ HH.div [ HP.class_ (HH.ClassName "job-detail__section-header") ]
        [ HH.h2 [ HP.class_ (HH.ClassName "job-detail__section-title") ]
            [ HH.text "Payload" ]
        , HH.button
            [ HP.class_ (HH.ClassName "toolbar-btn toolbar-btn--small")
            , HE.onClick \_ -> TogglePayload
            ]
            [ HH.text (if state.payloadCollapsed then "Show" else "Hide") ]
        ]
    , if state.payloadCollapsed then
        HH.text ""
      else
        HH.pre [ HP.class_ (HH.ClassName "json-viewer") ]
          [ HH.text (getPayloadJson job) ]
    ]

renderLogsSection :: forall m. State -> H.ComponentHTML Action () m
renderLogsSection state =
  HH.div [ HP.class_ (HH.ClassName "job-detail__section") ]
    [ HH.div [ HP.class_ (HH.ClassName "job-detail__section-header") ]
        [ HH.h2 [ HP.class_ (HH.ClassName "job-detail__section-title") ]
            [ HH.text "Logs" ]
        , HH.div [ HP.class_ (HH.ClassName "log-controls") ]
            [ renderLogLevelSelect state.logLevel
            , HH.button
                [ HP.class_ (HH.ClassName "toolbar-btn toolbar-btn--small")
                , HE.onClick \_ -> ToggleLogSortOrder
                ]
                [ HH.text (if state.logSortOrder == ASC then "Oldest first \x25B2" else "Newest first \x25BC") ]
            , HH.label [ HP.class_ (HH.ClassName "toolbar-label") ]
                [ HH.input
                    [ HP.type_ HP.InputCheckbox
                    , HP.checked state.logAutoRefresh
                    , HE.onChecked ToggleLogAutoRefresh
                    ]
                , HH.text " Auto-refresh "
                , HH.span
                    [ HP.class_
                        ( HH.ClassName
                            ( "refresh-indicator"
                                <> if state.logAutoRefresh then "" else " refresh-indicator--inactive"
                            )
                        )
                    ]
                    []
                ]
            ]
        ]
    , renderLogEntries state
    ]

renderLogLevelSelect :: forall m. LogLevel -> H.ComponentHTML Action () m
renderLogLevelSelect current =
  HH.select
    [ HP.class_ (HH.ClassName "toolbar-select toolbar-select--small")
    , HE.onValueChange SetLogLevel
    ]
    [ HH.option [ HP.value "DEBUG", HP.selected (current == Debug) ] [ HH.text "Debug" ]
    , HH.option [ HP.value "INFO", HP.selected (current == Info) ] [ HH.text "Info" ]
    , HH.option [ HP.value "WARN", HP.selected (current == Warn) ] [ HH.text "Warn" ]
    , HH.option [ HP.value "NOTICE", HP.selected (current == Notice) ] [ HH.text "Notice" ]
    , HH.option [ HP.value "ERROR", HP.selected (current == Error) ] [ HH.text "Error" ]
    ]

renderLogEntries :: forall m. State -> H.ComponentHTML Action () m
renderLogEntries state
  | Array.null state.allLogs =
      HH.div [ HP.class_ (HH.ClassName "empty-state") ]
        [ HH.p_ [ HH.text "No logs at this level." ] ]
  | otherwise = do
      -- Logs are stored in ASC order; reverse at render time for DESC.
      let
        displayLogs = case state.logSortOrder of
          ASC -> state.allLogs
          DESC -> Array.reverse state.allLogs
      let totalLogs = Array.length displayLogs
      let totalPages = logTotalPages totalLogs
      let page = min state.logPage (totalPages - 1)
      let pageStart = page * logPageSize
      let pageLogs = Array.slice pageStart (pageStart + logPageSize) displayLogs
      HH.div_
        [ renderLogLegend
        , HH.table [ HP.class_ (HH.ClassName "log-table") ]
            [ HH.thead_
                [ HH.tr_
                    [ HH.th [ HP.class_ (HH.ClassName "log-table__th log-table__th--rownum") ] [ HH.text "#" ]
                    , HH.th [ HP.class_ (HH.ClassName "log-table__th log-table__th--time") ] [ HH.text "Time" ]
                    , HH.th [ HP.class_ (HH.ClassName "log-table__th") ] [ HH.text "Message" ]
                    ]
                ]
            , HH.tbody_ (Array.mapWithIndex (renderLogEntry state.logSortOrder totalLogs pageStart) pageLogs)
            ]
        , renderLogPagination page totalPages totalLogs
        ]

renderLogEntry :: forall m. SortOrder -> Int -> Int -> Int -> LogLine -> H.ComponentHTML Action () m
renderLogEntry sortOrder totalLogs offset index logLine = do
  let level = V1.printLogLevel logLine.level
  let
    rowNum = case sortOrder of
      ASC -> offset + index + 1
      DESC -> totalLogs - offset - index
  HH.tr [ HP.class_ (HH.ClassName ("log-entry log-entry--" <> level)) ]
    [ HH.td [ HP.class_ (HH.ClassName "log-entry__rownum") ]
        [ HH.text (show rowNum) ]
    , HH.td [ HP.class_ (HH.ClassName "log-entry__time") ]
        [ HH.text (Job.formatTimestamp logLine.timestamp) ]
    , HH.td [ HP.class_ (HH.ClassName "log-entry__message") ]
        [ HH.pre [ HP.class_ (HH.ClassName "log-entry__text") ]
            [ HH.text logLine.message ]
        ]
    ]

renderLogLegend :: forall m. H.ComponentHTML Action () m
renderLogLegend =
  HH.div [ HP.class_ (HH.ClassName "log-legend") ]
    (map renderItem [ Debug, Info, Warn, Notice, Error ])
  where
  renderItem level = do
    let name = V1.printLogLevel level
    HH.span [ HP.class_ (HH.ClassName "log-legend__item") ]
      [ HH.span [ HP.class_ (HH.ClassName ("log-legend__swatch log-legend__swatch--" <> name)) ] []
      , HH.text name
      ]

-- | Render pagination controls for the log table, reusing the jobs-nav CSS.
renderLogPagination :: forall m. Int -> Int -> Int -> H.ComponentHTML Action () m
renderLogPagination page totalPages totalLogs
  | totalPages <= 1 = HH.text ""
  | otherwise =
      HH.div [ HP.class_ (HH.ClassName "jobs-nav") ]
        [ HH.button
            [ HP.class_ (HH.ClassName "jobs-nav__btn")
            , HP.disabled (page <= 0)
            , HE.onClick \_ -> LogPrevPage
            ]
            [ HH.text "\x25C0" ]
        , HH.span [ HP.class_ (HH.ClassName "jobs-nav__info") ]
            [ HH.text ("Page " <> show (page + 1) <> " of " <> show totalPages <> " (" <> show totalLogs <> " entries)") ]
        , HH.button
            [ HP.class_ (HH.ClassName "jobs-nav__btn")
            , HP.disabled (page >= totalPages - 1)
            , HE.onClick \_ -> LogNextPage
            ]
            [ HH.text "\x25B6" ]
        ]

-- --------------------------------------------------------------------------
-- Action handling
-- --------------------------------------------------------------------------

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    now <- liftEffect Now.nowDateTime
    H.modify_ _ { currentTime = Just now }
    handleAction FetchJob
    state <- H.get
    let
      finished = case state.job of
        Just job -> isJust (V1.jobInfo job).finishedAt
        Nothing -> false
    unless finished do
      subId <- H.subscribe =<< Job.timerEmitter logRefreshInterval LogRefreshTick
      H.modify_ _ { logRefreshSubId = Just subId }

  Receive input -> do
    state <- H.get
    when (state.jobId /= input.jobId || state.apiConfig /= input.apiConfig) do
      H.modify_ _ { jobId = input.jobId, apiConfig = input.apiConfig }
      handleAction FetchJob

  FetchJob -> do
    H.modify_ _ { loading = true, error = Nothing }
    state <- H.get
    result <- H.liftAff $ API.fetchJob state.apiConfig (JobId state.jobId)
      { level: Just state.logLevel, since: Nothing, until: Nothing, order: Just ASC }
    handleAction (HandleFetchResult result)

  HandleFetchResult result -> case result of
    Left err -> do
      let msg = API.printApiError err
      H.modify_ _ { loading = false, error = Just msg, job = Nothing }
    Right job -> do
      now <- liftEffect Now.nowDateTime
      let info = V1.jobInfo job
      let logs = info.logs
      let lastTs = map _.timestamp (Array.last logs)
      H.modify_ _
        { loading = false
        , error = Nothing
        , job = Just job
        , allLogs = logs
        , lastLogTimestamp = lastTs
        , logUntil = info.finishedAt
        , logPage = 0
        , currentTime = Just now
        }
      fetchAllRemainingLogs
      stopAutoRefreshIfFinished info

  SetLogLevel val -> do
    let level = parseLogLevel val
    H.modify_ _ { logLevel = level, allLogs = [], lastLogTimestamp = Nothing, logPage = 0 }
    state <- H.get
    result <- H.liftAff $ API.fetchJob state.apiConfig (JobId state.jobId)
      { level: Just level, since: Nothing, until: state.logUntil, order: Just ASC }
    handleAction (HandleLogLevelResult result)

  HandleLogLevelResult result -> case result of
    Left _ ->
      H.modify_ _ { allLogs = [], lastLogTimestamp = Nothing }
    Right job -> do
      let logs = (V1.jobInfo job).logs
      let lastTs = map _.timestamp (Array.last logs)
      H.modify_ _ { allLogs = logs, lastLogTimestamp = lastTs }
      fetchAllRemainingLogs

  ToggleLogAutoRefresh enabled -> do
    state <- H.get
    for_ state.logRefreshSubId H.unsubscribe
    if enabled then do
      subId <- H.subscribe =<< Job.timerEmitter logRefreshInterval LogRefreshTick
      H.modify_ _ { logAutoRefresh = true, logRefreshSubId = Just subId }
    else
      H.modify_ _ { logAutoRefresh = false, logRefreshSubId = Nothing }

  ToggleLogSortOrder -> do
    H.modify_ \s -> s
      { logSortOrder = if s.logSortOrder == ASC then DESC else ASC
      , logPage = 0
      }

  LogRefreshTick -> do
    now <- liftEffect Now.nowDateTime
    H.modify_ _ { currentTime = Just now }
    state <- H.get
    -- No-op if the job is already finished: all logs have been fetched.
    let
      finished = case state.job of
        Just job -> isJust (V1.jobInfo job).finishedAt
        Nothing -> false
    unless finished do
      result <- H.liftAff $ API.fetchJob state.apiConfig (JobId state.jobId)
        { level: Just state.logLevel, since: state.lastLogTimestamp, until: Nothing, order: Just ASC }
      handleAction (HandleLogRefreshResult result)

  HandleLogRefreshResult result -> case result of
    -- Intentional silent retry: transient API errors are ignored and the
    -- next tick will attempt another fetch automatically.
    Left _ -> pure unit
    Right job -> do
      now <- liftEffect Now.nowDateTime
      state <- H.get
      let info = V1.jobInfo job
      let newLogs = Array.filter (isNewerThan state.lastLogTimestamp) info.logs
      when (not (Array.null newLogs)) do
        let lastTs = map _.timestamp (Array.last newLogs)
        -- Logs are always stored in ASC order; new logs are appended at the end.
        let combined = capLogs state.logSortOrder (state.allLogs <> newLogs)
        H.modify_ _ { allLogs = combined, lastLogTimestamp = lastTs }
      -- Update job status and logUntil from the refreshed data
      H.modify_ _ { job = Just job, logUntil = info.finishedAt, currentTime = Just now }
      stopAutoRefreshIfFinished info

  TogglePayload ->
    H.modify_ \s -> s { payloadCollapsed = not s.payloadCollapsed }

  LogPrevPage ->
    H.modify_ \s -> s { logPage = max 0 (s.logPage - 1) }

  LogNextPage -> do
    state <- H.get
    let totalPages = logTotalPages (Array.length state.allLogs)
    H.modify_ \s -> s { logPage = min (totalPages - 1) (s.logPage + 1) }

  GoBack ev -> do
    liftEffect $ Event.preventDefault (MouseEvent.toEvent ev)
    H.raise NavigateBack

-- --------------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------------

-- | Stop the auto-refresh timer if the job has finished.
stopAutoRefreshIfFinished :: forall m. V1.JobInfo () -> H.HalogenM State Action () Output m Unit
stopAutoRefreshIfFinished info =
  when (isJust info.finishedAt) do
    state <- H.get
    for_ state.logRefreshSubId H.unsubscribe
    H.modify_ _ { logAutoRefresh = false, logRefreshSubId = Nothing }

-- | Maximum number of log entries to keep in memory.
maxLogEntries :: Int
maxLogEntries = 2000

-- | Number of log entries to display per page in the log table.
logPageSize :: Int
logPageSize = 200

-- | Compute total pages for log pagination, always returning at least 1.
logTotalPages :: Int -> Int
logTotalPages total = max 1 (((total - 1) / logPageSize) + 1)

-- | Cap an array of logs (stored in ASC order) to `maxLogEntries`, trimming
-- | from the end furthest from the user's current view.
capLogs :: SortOrder -> Array LogLine -> Array LogLine
capLogs sortOrder logs =
  if Array.length logs <= maxLogEntries then logs
  else case sortOrder of
    ASC -> Array.take maxLogEntries logs
    DESC -> Array.drop (Array.length logs - maxLogEntries) logs

-- | Maximum number of pagination requests when fetching all remaining logs.
maxPaginationIterations :: Int
maxPaginationIterations = 20

logRefreshInterval :: Milliseconds
logRefreshInterval = Milliseconds 3000.0

-- | Parse a log level string from the select element.
parseLogLevel :: String -> LogLevel
parseLogLevel = case _ of
  "DEBUG" -> Debug
  "WARN" -> Warn
  "NOTICE" -> Notice
  "ERROR" -> Error
  _ -> Info

-- | Check if a log line is newer than the given timestamp.
isNewerThan :: Maybe DateTime -> LogLine -> Boolean
isNewerThan mTs logLine = case mTs of
  Nothing -> true
  Just ts -> logLine.timestamp > ts

-- | Encode the job's payload to a pretty-printed JSON string.
getPayloadJson :: Job -> String
getPayloadJson = case _ of
  PublishJob j -> JSON.printIndented (CJ.encode Operation.publishCodec j.payload)
  UnpublishJob j -> JSON.printIndented (CJ.encode Operation.authenticatedCodec j.payload)
  TransferJob j -> JSON.printIndented (CJ.encode Operation.authenticatedCodec j.payload)
  MatrixJob j -> JSON.printIndented (CJ.encode (Internal.Codec.packageMap Version.codec) j.payload)
  PackageSetJob j -> JSON.printIndented (CJ.encode Operation.packageSetOperationCodec j.payload)

-- | Compute a human-readable duration between a start time and an optional
-- | end time. If the end time is absent, uses the current time as a fallback
-- | and appends "(ongoing)".
computeDurationBetween :: Maybe DateTime -> DateTime -> Maybe DateTime -> String
computeDurationBetween mNow start = case _ of
  Just end -> Job.formatDurationBetween start end
  Nothing -> case mNow of
    Just now -> Job.formatDurationBetween start now <> " (ongoing)"
    Nothing -> "ongoing"

-- | Fetch all remaining log pages after the initial fetch by looping until
-- | either an empty batch is returned or the maximum number of iterations
-- | is reached.
fetchAllRemainingLogs :: forall m. MonadAff m => H.HalogenM State Action () Output m Unit
fetchAllRemainingLogs = go 0
  where
  go :: Int -> H.HalogenM State Action () Output m Unit
  go iteration =
    when (iteration < maxPaginationIterations) do
      state <- H.get
      -- Stop fetching if we've already reached the log cap
      when (Array.length state.allLogs < maxLogEntries) do
        case state.lastLogTimestamp of
          Nothing -> pure unit
          Just since -> do
            result <- H.liftAff $ API.fetchJob state.apiConfig (JobId state.jobId)
              { level: Just state.logLevel, since: Just since, until: Nothing, order: Just ASC }
            case result of
              Left _ -> pure unit
              Right job -> do
                let newLogs = Array.filter (isNewerThan (Just since)) (V1.jobInfo job).logs
                if Array.null newLogs then
                  pure unit
                else do
                  currentState <- H.get
                  let combined = capLogs currentState.logSortOrder (currentState.allLogs <> newLogs)
                  let lastTs = map _.timestamp (Array.last combined)
                  H.modify_ _ { allLogs = combined, lastLogTimestamp = lastTs }
                  go (iteration + 1)
