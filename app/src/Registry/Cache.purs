module Registry.Cache
  ( Cache
  , CacheEntry
  , readJsonEntry
  , useCache
  , writeJsonEntry
  ) where

import Registry.Prelude

import Data.Array as Array
import Data.DateTime (DateTime)
import Data.Map as Map
import Data.Maybe (maybe')
import Data.RFC3339String (RFC3339String(..))
import Data.RFC3339String as RFC3339String
import Data.String as String
import Effect.Exception as Aff
import Effect.Ref as Ref
import Foreign.Node.FS as FSE
import JSURI (encodeURIComponent)
import Node.FS.Aff as FSA
import Node.FS.Sync as FS
import Node.Path as Path
import Registry.Json as Json

entryPath :: FilePath -> CacheKey -> FilePath
entryPath cacheDir (CacheKey filename) = Path.concat [ cacheDir, filename ]

type CacheEntry =
  { modified :: DateTime
  , value :: String
  }

type Cache =
  { read :: String -> Effect (Either String CacheEntry)
  , write :: String -> String -> Effect Unit
  , remove :: String -> Effect Unit
  }

newtype CacheKey = CacheKey String

derive newtype instance Eq CacheKey
derive newtype instance Ord CacheKey

toCacheKey :: String -> CacheKey
toCacheKey key =
  maybe' (\_ -> unsafeCrashWith ("Unable to encode " <> key <> " as CacheKey")) CacheKey
    $ encodeURIComponent
    $ String.replaceAll (String.Pattern "@") (String.Replacement "$")
    $ String.replaceAll (String.Pattern "/") (String.Replacement "_")
    $ String.replaceAll (String.Pattern " ") (String.Replacement "__") key

readJsonEntry :: forall a. Json.RegistryJson a => String -> Cache -> Effect (Either String { modified :: DateTime, value :: a })
readJsonEntry key { read } =
  read key >>= case _ of
    Left err -> pure (Left err)
    Right { modified, value } -> pure do
      parsedValue <- Json.parseJson value
      Right { modified, value: parsedValue }

writeJsonEntry :: forall a. Json.RegistryJson a => String -> a -> Cache -> Effect Unit
writeJsonEntry key value { write } = write key (Json.stringifyJson value)

useCache :: FilePath -> Aff Cache
useCache cacheDir = do
  FSE.ensureDirectory cacheDir

  entries <- do
    files <- FSA.readdir cacheDir
    for files \file -> do
      contents <- Json.readJsonFile (entryPath cacheDir (CacheKey file))
      case contents of
        Left err -> do
          log $ "Failed to decode entry (" <> file <> "): " <> err
          pure Nothing
        Right result ->
          case RFC3339String.toDateTime result.modified of
            Nothing -> do
              log $ "Failed to decode entry (" <> file <> ") because of a malformed RFC3339String: " <> un RFC3339String result.modified
              pure Nothing
            Just dateTime ->
              pure $ Just $ Tuple (CacheKey file) (result { modified = dateTime })

  cacheRef <- liftEffect $ Ref.new $ Map.fromFoldable $ Array.catMaybes entries

  let
    write :: CacheKey -> String -> Effect Unit
    write key value = do
      utcTime <- nowUTC
      let entry = { modified: utcTime, value }
      let jsonEntry = entry { modified = RFC3339String.fromDateTime entry.modified }
      FS.writeTextFile UTF8 (entryPath cacheDir key) (Json.stringifyJson jsonEntry)
      Ref.modify_ (Map.insert key entry) cacheRef

    read :: CacheKey -> Effect (Either String CacheEntry)
    read key = do
      cache <- Ref.read cacheRef
      case Map.lookup key cache of
        Nothing -> do
          try (FS.readTextFile UTF8 (entryPath cacheDir key)) >>= case _ of
            Left err -> pure $ Left $ Aff.message err
            Right contents -> case Json.parseJson contents of
              Left err ->
                remove key $> Left err
              Right entry -> case RFC3339String.toDateTime entry.modified of
                Nothing -> remove key $> Left ("Malformed RFC3339String: " <> un RFC3339String entry.modified)
                Just dateTime -> pure $ Right $ entry { modified = dateTime }
        Just entry -> do
          pure $ Right entry

    remove :: CacheKey -> Effect Unit
    remove key = do
      Ref.modify_ (Map.delete key) cacheRef
      FS.unlink (entryPath cacheDir key)

  pure
    { read: \key -> read (toCacheKey key)
    , write: \key value -> write (toCacheKey key) value
    , remove: \key -> remove (toCacheKey key)
    }
