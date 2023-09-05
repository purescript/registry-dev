-- | An effect for the Pursuit API.
module Registry.App.Effect.Pursuit where

import Registry.App.Prelude

import Data.Argonaut.Core as Argonaut
import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.HTTP.Method as Method
import Data.Map as Map
import Data.Profunctor as Profunctor
import Effect.Exception as Exception
import Fetch.Retry as Fetch
import Foreign (unsafeFromForeign)
import Registry.App.Effect.Env (RESOURCE_ENV)
import Registry.App.Effect.Env as Env
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Legacy.LenientVersion (LenientVersion(..))
import Registry.App.Legacy.LenientVersion as LenientVersion
import Registry.Foreign.Gzip as Gzip
import Registry.Foreign.Octokit (GitHubToken(..))
import Registry.PackageName as PackageName
import Registry.Version as Version
import Run (AFF, Run)
import Run as Run

-- | An effect for interacting with Pursuit
data Pursuit a
  = Publish Json (Either String Unit -> a)
  | GetPublishedVersions PackageName (Either String (Map Version URL) -> a)

derive instance Functor Pursuit

type PURSUIT r = (pursuit :: Pursuit | r)

_pursuit :: Proxy "pursuit"
_pursuit = Proxy

-- | Publish a package to Pursuit using the JSON output of the compiler.
publish :: forall r. Json -> Run (PURSUIT + r) (Either String Unit)
publish json = Run.lift _pursuit (Publish json identity)

-- | List published versions from Pursuit
getPublishedVersions :: forall r. PackageName -> Run (PURSUIT + r) (Either String (Map Version URL))
getPublishedVersions name = Run.lift _pursuit (GetPublishedVersions name identity)

-- | Run the PURSUIT effect given a handler.
interpret :: forall r a. (Pursuit ~> Run r) -> Run (PURSUIT + r) a -> Run r a
interpret handler = Run.interpret (Run.on _pursuit handler Run.send)

-- | Handle Pursuit by skipping all calls.
handlePure :: forall r a. Pursuit a -> Run r a
handlePure = case _ of
  Publish _ reply -> pure $ reply $ Right unit
  GetPublishedVersions _ reply -> pure $ reply $ Right Map.empty

-- | Handle Pursuit by executing HTTP requests using the provided auth token.
handleAff :: forall r a. GitHubToken -> Pursuit a -> Run (RESOURCE_ENV + LOG + AFF + r) a
handleAff (GitHubToken token) = case _ of
  Publish payload reply -> do
    { pursuitApiUrl } <- Env.askResourceEnv
    Log.debug "Pushing to Pursuit..."

    result <- Run.liftAff do
      gzipped <- Gzip.compress (Argonaut.stringify payload)
      Fetch.withRetryRequest (Array.fold [ pursuitApiUrl, "/packages" ])
        { method: Method.POST
        , body: gzipped
        , headers:
            { "Accept": "application/json"
            , "Content-Encoding": "gzip"
            , "Authorization": "token " <> token
            }
        }

    result' <- case result of
      Cancelled -> do
        Log.error $ "Pursuit failed to connect after several retries."
        pure $ Left $ "Expected to receive a 201 status from Pursuit, but failed to connect after several retries."
      Failed reqError -> case reqError of
        Fetch.FetchError err -> do
          pure $ Left $ "Pursuit publishing failed with an HTTP error: " <> Exception.message err
        Fetch.StatusError { text: textAff, status } -> do
          text <- Run.liftAff textAff
          Log.error $ "Pursuit publishing failed with status " <> show status <> " and body\n" <> text
          pure $ Left $ "Expected to receive a 201 status from Pursuit, but received " <> show status <> " instead."
      Succeeded { text: textAff, status }
        | status == 201 -> do
            Log.debug "Received 201 status, which indicates the upload was successful."
            pure $ Right unit
        | otherwise -> do
            text <- Run.liftAff textAff
            Log.error $ "Pursuit publishing failed with status " <> show status <> " and body\n" <> text
            pure $ Left $ "Expected to receive a 201 status from Pursuit, but received " <> show status <> " instead."

    pure $ reply result'

  GetPublishedVersions pname reply -> do
    { pursuitApiUrl } <- Env.askResourceEnv
    let name = PackageName.print pname
    let url = Array.fold [ pursuitApiUrl, "/packages/purescript-" <> name <> "/available-versions" ]
    Log.debug $ "Checking if package docs for " <> name <> " are published on Pursuit using endpoint " <> url
    result <- Run.liftAff $ Fetch.withRetryRequest url
      { headers: { accept: "application/json" }
      }

    case result of
      Cancelled -> do
        Log.error $ "Could not reach Pursuit after multiple retries at URL " <> url
        pure $ reply $ Left $ "Could not reach Pursuit to determine published versions for " <> name
      Failed (Fetch.FetchError httpError) -> do
        let printedError = Exception.message httpError
        Log.error $ "Pursuit publishing failed because of an HTTP error: " <> printedError
        pure $ reply $ Left "Could not reach Pursuit due to an HTTP error."
      Failed (Fetch.StatusError { text: textAff, status }) -> do
        text <- Run.liftAff textAff
        Log.error $ "Could not fetch published versions from Pursuit (received non-200 response) " <> show status <> " and body\n" <> text
        pure $ reply $ Left $ "Received non-200 response from Pursuit: " <> show status
      Succeeded { text: textAff, json: jsonAff } -> do
        json <- Run.liftAff jsonAff
        case CA.decode availableVersionsCodec (unsafeFromForeign json) of
          Left error -> do
            let printed = CA.printJsonDecodeError error
            text <- Run.liftAff textAff
            Log.error $ "Failed to decode body " <> text <> "\n with error: " <> printed
            pure $ reply $ Left $ "Received a response from Pursuit, but it could not be decoded:\n\n" <> printed <> "\n\ncc: @purescript/packaging"
          Right versions -> do
            Log.debug "Found versions from Pursuit!"
            pure $ reply $ Right versions

-- The Pursuit /available-versions endpoint returns versions as a tuple of the
-- version number and documentation URL, represented as a two-element array.
-- [["2.0.0","https://pursuit.purescript.org/packages/purescript-halogen/2.0.0"]]
availableVersionsCodec :: JsonCodec (Map Version URL)
availableVersionsCodec = Profunctor.dimap toRep fromRep (CA.array (CA.array CA.string))
  where
  toRep = map (\(Tuple version url) -> [ Version.print version, url ]) <<< Map.toUnfoldable
  fromRep = Map.fromFoldable <<< Array.mapMaybe \array -> do
    rawVersion <- Array.index array 0
    LenientVersion { version } <- hush $ LenientVersion.parse rawVersion
    url <- Array.index array 1
    pure $ Tuple version url
