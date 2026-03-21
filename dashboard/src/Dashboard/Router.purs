module Dashboard.Router
  ( component
  ) where

import Prelude

import Dashboard.API as API
import Dashboard.Component.JobDetail as JobDetail
import Dashboard.Component.JobsList as JobsList
import Dashboard.Route (Route(..))
import Dashboard.Route as Route
import Data.Const (Const)
import Data.Either (hush)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Routing.Duplex as RD
import Type.Proxy (Proxy(..))
import Web.Event.Event as Event
import Web.Event.EventTarget as EventTarget
import Web.HTML as HTML
import Web.HTML.Event.HashChangeEvent.EventTypes as HashChangeEvent
import Web.HTML.Location as Location
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

type Slots =
  ( jobsList :: H.Slot (Const Void) JobsList.Output Unit
  , jobDetail :: H.Slot (Const Void) JobDetail.Output String
  )

_jobsList :: Proxy "jobsList"
_jobsList = Proxy

_jobDetail :: Proxy "jobDetail"
_jobDetail = Proxy

type State =
  { route :: Route
  , lastJobsListParams :: Route.JobsListParams
  }

data Action
  = Initialize
  | HandleRouteChange Route
  | HandleJobsListOutput JobsList.Output
  | HandleJobDetailOutput JobDetail.Output
  | GoHome MouseEvent
  | GoTab Route MouseEvent

component :: forall query output m. MonadAff m => H.Component query Unit output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

initialState :: Unit -> State
initialState _ =
  { route: JobsList Route.defaultParams
  , lastJobsListParams: Route.defaultParams
  }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state =
  HH.div [ HP.style "height: 100%" ]
    [ HH.div [ HP.class_ (HH.ClassName "everything-except-footer") ]
        [ HH.div [ HP.class_ (HH.ClassName "top-banner") ]
            [ HH.div [ HP.class_ (HH.ClassName "container clearfix") ]
                [ HH.a
                    [ HP.class_ (HH.ClassName "top-banner__logo")
                    , HP.href "#/"
                    , HE.onClick GoHome
                    ]
                    [ HH.text "PureScript Registry" ]
                ]
            ]
        , if Array.length tabs > 1 then
            HH.div [ HP.class_ (HH.ClassName "tab-bar") ]
              [ HH.div [ HP.class_ (HH.ClassName "container") ]
                  [ HH.nav [ HP.class_ (HH.ClassName "tab-bar__nav") ]
                      tabs
                  ]
              ]
          else
            HH.text ""
        , HH.div [ HP.class_ (HH.ClassName "container") ]
            [ content ]
        ]
    , HH.div [ HP.class_ (HH.ClassName "footer") ]
        [ HH.div [ HP.classes [ HH.ClassName "footer__inner", HH.ClassName "container" ] ]
            [ HH.span [ HP.class_ (HH.ClassName "footer__label") ]
                [ HH.text "PureScript Registry Dashboard" ]
            , HH.ul [ HP.class_ (HH.ClassName "footer__links") ]
                [ HH.li_ [ HH.a [ HP.href "https://github.com/purescript/registry" ] [ HH.text "Registry" ] ]
                , HH.li_ [ HH.a [ HP.href "https://github.com/purescript/registry-dev" ] [ HH.text "GitHub" ] ]
                , HH.li_ [ HH.a [ HP.href "https://registry.purescript.org/api/v1/status" ] [ HH.text "API" ] ]
                , HH.li_ [ HH.a [ HP.href "https://www.purescript.org" ] [ HH.text "purescript.org" ] ]
                ]
            ]
        ]
    ]
  where
  tabs =
    [ HH.a
        [ HP.classes [ HH.ClassName "tab-bar__tab", HH.ClassName "tab-bar__tab--active" ]
        , HP.href "#/"
        , HE.onClick (GoTab (JobsList state.lastJobsListParams))
        ]
        [ HH.text "Jobs" ]
    ]

  content = case state.route of
    JobsList params ->
      HH.slot _jobsList unit JobsList.component { apiConfig: API.defaultConfig, params } HandleJobsListOutput
    JobDetail jobId ->
      HH.slot _jobDetail jobId JobDetail.component { jobId, apiConfig: API.defaultConfig } HandleJobDetailOutput

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action Slots output m Unit
handleAction = case _ of
  Initialize -> do
    route <- liftEffect getRouteFromHash
    applyRoute route
    emitter <- liftEffect hashChangeEmitter
    void $ H.subscribe (map HandleRouteChange emitter)

  HandleRouteChange route ->
    applyRoute route

  HandleJobsListOutput (JobsList.NavigateToJob jobId) ->
    liftEffect $ setHash ("#/jobs/" <> jobId)

  HandleJobsListOutput (JobsList.FiltersChanged params) -> do
    H.modify_ _ { lastJobsListParams = params }
    liftEffect $ replaceHash (routeToHash (JobsList params))

  HandleJobDetailOutput JobDetail.NavigateBack -> do
    state <- H.get
    liftEffect $ setHash (routeToHash (JobsList state.lastJobsListParams))

  GoHome ev -> do
    liftEffect $ Event.preventDefault (MouseEvent.toEvent ev)
    state <- H.get
    liftEffect $ setHash (routeToHash (JobsList state.lastJobsListParams))

  GoTab route ev -> do
    liftEffect $ Event.preventDefault (MouseEvent.toEvent ev)
    liftEffect $ setHash (routeToHash route)

applyRoute :: forall output m. Applicative m => Route -> H.HalogenM State Action Slots output m Unit
applyRoute route = case route of
  JobsList params -> H.modify_ _ { route = route, lastJobsListParams = params }
  _ -> H.modify_ _ { route = route }

-- | Read the current URL hash and parse it into a Route, falling back to
-- | JobsList with default params if parsing fails.
getRouteFromHash :: Effect Route
getRouteFromHash = do
  hash <- getHash
  pure $ fromMaybe (JobsList Route.defaultParams) (parseHash hash)

-- | Get the current hash from window.location.
getHash :: Effect String
getHash = do
  window <- HTML.window
  location <- Window.location window
  Location.hash location

-- | Set the hash on window.location. This creates a new history entry and
-- | fires the hashchange event.
setHash :: String -> Effect Unit
setHash hash = do
  window <- HTML.window
  location <- Window.location window
  Location.setHash hash location

-- | Replace the current hash without creating a new history entry and without
-- | firing the hashchange event. This is used for filter updates so that each
-- | keystroke doesn't pollute the browser history.
replaceHash :: String -> Effect Unit
replaceHash hash = do
  window <- HTML.window
  location <- Window.location window
  Location.replace hash location

-- | Convert a Route to a hash string (e.g. "#/?range=1h&status=running").
routeToHash :: Route -> String
routeToHash route = "#" <> RD.print Route.routes route

-- | Parse a hash string (which may include a leading '#') into a Route.
parseHash :: String -> Maybe Route
parseHash h = do
  let path = hashToPath h
  hush (RD.parse Route.routes path)

-- | Convert a hash string like "#/jobs/abc" to a path like "/jobs/abc".
-- | An empty or absent hash maps to "/".
hashToPath :: String -> String
hashToPath h = case String.stripPrefix (Pattern "#") h of
  Just rest
    | rest == "" -> "/"
    | otherwise -> rest
  Nothing
    | h == "" -> "/"
    | otherwise -> h

-- | Create a Halogen Emitter that fires the parsed Route whenever the URL
-- | hash changes.
hashChangeEmitter :: Effect (HS.Emitter Route)
hashChangeEmitter = do
  window <- HTML.window
  pure $ HS.makeEmitter \push -> do
    let target = Window.toEventTarget window
    let eventType = HashChangeEvent.hashchange
    listener <- EventTarget.eventListener \_ -> do
      route <- getRouteFromHash
      push route
    EventTarget.addEventListener eventType listener false target
    pure do
      EventTarget.removeEventListener eventType listener false target
