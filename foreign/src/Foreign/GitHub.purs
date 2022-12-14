-- | Low-level bindings to Octokit and its request functions.
-- |
-- | There is a GitHub module in the registry application as well, containing
-- | app-specific requests, but the modules are kept separate because this low-
-- | level code is required to implement both the app-specific requests and the
-- | logging effect that those requests rely on.
module Registry.Foreign.GitHub
  ( Base64Content(..)
  , GitHubAPIError
  , GitHubError(..)
  , GitHubRoute(..)
  , GitHubToken(..)
  , JSArgs(..)
  , Octokit
  , RateLimit
  , Request
  , decodeBase64Content
  , githubApiErrorCodec
  , githubErrorCodec
  , newOctokit
  , noArgs
  , printGitHubError
  , printGitHubRoute
  , rateLimitRequest
  , request
  , rfc1123Format
  , unsafeToJSArgs
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CA.Record
import Data.Codec.Argonaut.Variant as CA.Variant
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as Instant
import Data.Either (Either(..))
import Data.Formatter.DateTime (Formatter, FormatterCommand(..))
import Data.HTTP.Method (Method(..))
import Data.Int as Int
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor as Profunctor
import Data.String as String
import Data.String.Base64 as Base64
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Variant as Variant
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Uncurried (EffectFn1, EffectFn6, runEffectFn1, runEffectFn6)
import Foreign.Object (Object)
import Foreign.Object as Object
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | A private GitHub API token
newtype GitHubToken = GitHubToken String

derive instance Newtype GitHubToken _
derive newtype instance Eq GitHubToken
derive newtype instance Ord GitHubToken

-- | An instance of GitHub's Octokit client
foreign import data Octokit :: Type

foreign import newOctokitImpl :: EffectFn1 GitHubToken Octokit

newOctokit :: GitHubToken -> Effect Octokit
newOctokit = runEffectFn1 newOctokitImpl

-- | A newline-delimited base64-encoded file retrieved from the GitHub API
newtype Base64Content = Base64Content String

derive instance Newtype Base64Content _

decodeBase64Content :: Base64Content -> Either String String
decodeBase64Content (Base64Content string) =
  case traverse Base64.decode $ String.split (String.Pattern "\n") string of
    Left error -> Left $ Aff.message error
    Right values -> Right $ Array.fold values

type RateLimit =
  { limit :: Int
  , remaining :: Int
  , resetTime :: Maybe Instant
  }

rateLimitRequest :: Request RateLimit
rateLimitRequest =
  { route: GitHubRoute GET [ "rate_limit" ] Map.empty
  , headers: Object.empty
  , args: noArgs
  , paginate: false
  , codec: Profunctor.dimap toJsonRep fromJsonRep $ CA.Record.object "RateLimit"
      { data: CA.Record.object "RateLimit.data"
          { resources: CA.Record.object "RateLimit.data.resources"
              { core: CA.Record.object "RateLimit.data.resources.core"
                  { limit: CA.int
                  , remaining: CA.int
                  , reset: CA.number
                  }
              }
          }
      }
  }
  where
  toJsonRep { limit, remaining, resetTime } = do
    let reset = Maybe.fromMaybe (-9999.0) ((unwrap <<< Instant.unInstant) <$> resetTime)
    { data: { resources: { core: { limit, remaining, reset } } } }

  fromJsonRep { data: { resources: { core: { limit, remaining, reset } } } } =
    { limit, remaining, resetTime: Instant.instant $ Aff.Milliseconds $ reset * 1000.0 }

-- | A route for the GitHub API, ie. "GET /repos/purescript/registry/tags".
-- | Meant for internal use.
data GitHubRoute = GitHubRoute Method (Array String) (Map String String)

derive instance Eq GitHubRoute
derive instance Ord GitHubRoute

-- | Format a route as a usable GitHub route for Octokit
printGitHubRoute :: GitHubRoute -> String
printGitHubRoute (GitHubRoute method segments params) = show method <> " " <> printPath <> printParams
  where
  printPath = Array.foldMap (append "/") segments
  printParams = case Map.size params of
    0 -> ""
    _ -> append "?" $ String.joinWith "&" $ map (\(Tuple key val) -> key <> "=" <> val) $ Map.toUnfoldable params

-- | An opaque type for PureScript types we want to pass directly to JavaScript
-- | through the FFI. Should only be used with JavaScript-compatible types for
-- | the sake of setting headers.
data JSArgs

-- | Coerce a record to a JSArgs opaque type.
unsafeToJSArgs :: forall a. Record a -> JSArgs
unsafeToJSArgs = unsafeCoerce

noArgs :: JSArgs
noArgs = unsafeToJSArgs {}

type Request a =
  { route :: GitHubRoute
  , headers :: Object String
  , args :: JSArgs
  , paginate :: Boolean
  , codec :: JsonCodec a
  }

foreign import requestImpl :: forall r. EffectFn6 Octokit String (Object String) JSArgs (Object Json -> r) (Json -> r) (Promise r)
foreign import paginateImpl :: forall r. EffectFn6 Octokit String (Object String) JSArgs (Object Json -> r) (Json -> r) (Promise r)

-- | Make a request to the GitHub API.
request :: forall a. Octokit -> Request a -> Aff (Either GitHubError a)
request octokit { route, headers, args, paginate, codec } = do
  result <- Promise.toAffE $ runEffectFn6 (if paginate then paginateImpl else requestImpl) octokit (printGitHubRoute route) headers args Left Right
  pure $ case result of
    Left githubError -> case decodeGitHubAPIError githubError of
      Left decodeError -> Left $ UnexpectedError decodeError
      Right decoded -> Left $ APIError decoded
    Right json -> case CA.decode codec json of
      Left decodeError -> Left $ DecodeError $ CA.printJsonDecodeError decodeError
      Right parsed -> Right parsed
  where
  decodeGitHubAPIError :: Object Json -> Either String GitHubAPIError
  decodeGitHubAPIError object = lmap CA.printJsonDecodeError do
    statusCode <- atKey "status" CA.int object
    message <- case statusCode of
      304 -> pure ""
      _ -> atKey "response" CA.jobject object >>= atKey "data" CA.jobject >>= atKey "message" CA.string
    pure { statusCode, message }

type GitHubAPIError =
  { statusCode :: Int
  , message :: String
  }

githubApiErrorCodec :: JsonCodec GitHubAPIError
githubApiErrorCodec = CA.Record.object "GitHubAPIError"
  { statusCode: CA.int
  , message: CA.string
  }

data GitHubError
  = UnexpectedError String
  | APIError GitHubAPIError
  | DecodeError String

derive instance Eq GitHubError
derive instance Ord GitHubError

githubErrorCodec :: JsonCodec GitHubError
githubErrorCodec = Profunctor.dimap toVariant fromVariant $ CA.Variant.variantMatch
  { unexpectedError: Right CA.string
  , apiError: Right githubApiErrorCodec
  , decodeError: Right CA.string
  }
  where
  toVariant = case _ of
    UnexpectedError error -> Variant.inj (Proxy :: _ "unexpectedError") error
    APIError error -> Variant.inj (Proxy :: _ "apiError") error
    DecodeError error -> Variant.inj (Proxy :: _ "decodeError") error

  fromVariant = Variant.match
    { unexpectedError: UnexpectedError
    , apiError: APIError
    , decodeError: DecodeError
    }

printGitHubError :: GitHubError -> String
printGitHubError = case _ of
  UnexpectedError message -> Array.fold
    [ "Unexpected error: "
    , message
    ]
  APIError fields -> Array.fold
    [ "GitHub API error ("
    , Int.toStringAs Int.decimal fields.statusCode
    , "): "
    , fields.message
    ]
  DecodeError error -> Array.fold
    [ "Decoding error: "
    , error
    ]

-- GitHub uses the RFC1123 time format: "Thu, 05 Jul 2022"
-- http://www.csgnetwork.com/timerfc1123calc.html
--
-- It expects the time to be in UTC.
rfc1123Format :: Formatter
rfc1123Format = List.fromFoldable
  [ DayOfWeekNameShort
  , Placeholder ", "
  , DayOfMonthTwoDigits
  , Placeholder " "
  , MonthShort
  , Placeholder " "
  , YearFull
  , Placeholder " "
  , Hours24
  , Placeholder ":"
  , MinutesTwoDigits
  , Placeholder ":"
  , SecondsTwoDigits
  , Placeholder " "
  , Placeholder "UTC"
  ]

atKey :: forall a. String -> JsonCodec a -> Object Json -> Either JsonDecodeError a
atKey key codec object =
  Maybe.maybe
    (Left (CA.AtKey key CA.MissingValue))
    (lmap (CA.AtKey key) <<< CA.decode codec)
    (Object.lookup key object)
