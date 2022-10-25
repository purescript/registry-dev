module Registry.Hash
  ( Sha256
  , parseSha256
  , sha256File
  , sha256String
  , sha256Buffer
  , unsafeSha256
  ) where

import Registry.Prelude

import Data.Array as Array
import Data.List.Lazy as List.Lazy
import Data.String.CodeUnits as String.CodeUnits
import Foreign.Node.Crypto as Crypto
import Node.Buffer as Buffer
import Node.FS.Aff as FS
import Parsing (ParseError)
import Parsing as Parsing
import Parsing.String as Parsing.String
import Registry.Json as Json

-- | A base64-encoded subresource integrity hash using the SHA256 algorithm.
newtype Sha256 = Sha256 String

derive instance Eq Sha256

instance Show Sha256 where
  show (Sha256 hash) = hash

instance RegistryJson Sha256 where
  encode (Sha256 hash) = Json.encode hash
  decode input = do
    string <- Json.decode input
    let jsonError = append "Expected Sha256 hash: " <<< Parsing.parseErrorMessage
    lmap jsonError $ parseSha256 string

-- | Hash a file using SHA256
sha256File :: FilePath -> Aff Sha256
sha256File filepath = do
  fileBuffer <- FS.readFile filepath
  liftEffect $ sha256Buffer fileBuffer

-- | Hash a string using SHA256
sha256String :: String -> Effect Sha256
sha256String string = do
  buffer <- Buffer.fromString string UTF8
  sha256Buffer buffer

-- | Hash a buffer using SHA256
sha256Buffer :: Buffer -> Effect Sha256
sha256Buffer buffer = do
  newHash <- Crypto.createHash "sha256"
  hash <- Crypto.updateHash buffer newHash
  digest <- Crypto.digestHash hash
  string <- Buffer.toString Base64 digest
  let sriString = "sha256-" <> string
  pure $ Sha256 sriString

-- | A valid hash has a "sha256-" prefix, 43 chars, and a "=" padding character.
parseSha256 :: String -> Either ParseError Sha256
parseSha256 input = Parsing.runParser input do
  prefix <- Parsing.String.string "sha256-"
  hash <- List.Lazy.replicateM 43 Parsing.String.anyChar
  suffix <- Parsing.String.char '='
  let fromCharList = String.CodeUnits.fromCharArray <<< Array.fromFoldable
  pure $ Sha256 $ prefix <> fromCharList hash <> String.CodeUnits.singleton suffix

unsafeSha256 :: String -> Sha256
unsafeSha256 input = case parseSha256 input of
  Left error ->
    unsafeCrashWith $ Parsing.parseErrorMessage error
  Right hash ->
    hash
