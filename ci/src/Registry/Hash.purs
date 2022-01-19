module Registry.Hash
  ( Sha256
  , sha256File
  , parseSha256
  , unsafeSha256
  ) where

import Registry.Prelude

import Data.Array as Array
import Data.List.Lazy as List.Lazy
import Data.String.CodeUnits as CodeUnits
import Node.Buffer as Buffer
import Node.Crypto.Hash as Hash
import Node.FS.Aff as FS
import Registry.Json as Json
import Text.Parsing.StringParser (ParseError)
import Text.Parsing.StringParser as StringParser
import Text.Parsing.StringParser.CodeUnits as StringParser.CodeUnits

-- | A base64-encoded subresource integrity hash using the SHA256 algorithm.
newtype Sha256 = Sha256 String

derive instance Eq Sha256

instance Show Sha256 where
  show (Sha256 hash) = hash

instance RegistryJson Sha256 where
  encode (Sha256 hash) = Json.encode hash
  decode input = do
    string <- Json.decode input
    let jsonError = append "Expected Sha256 hash: " <<< StringParser.printParserError
    lmap jsonError $ parseSha256 string

-- | Hash a file using SHA256
sha256File :: FilePath -> Aff Sha256
sha256File filepath = do
  fileBuffer <- FS.readFile filepath
  liftEffect do
    newHash <- Hash.createHash "sha256"
    fileHash <- Hash.update fileBuffer newHash
    digest <- Hash.digest fileHash
    string <- Buffer.toString Base64 digest
    let sriString = "sha256-" <> string
    pure $ Sha256 sriString

-- | A valid hash has a "sha256-" prefix, 43 chars, and a "=" padding character.
parseSha256 :: String -> Either ParseError Sha256
parseSha256 = StringParser.runParser do
  prefix <- StringParser.CodeUnits.string "sha256-"
  hash <- List.Lazy.replicateM 43 StringParser.CodeUnits.anyChar
  suffix <- StringParser.CodeUnits.char '='
  let fromCharList = CodeUnits.fromCharArray <<< Array.fromFoldable
  pure $ Sha256 $ prefix <> fromCharList hash <> CodeUnits.singleton suffix

unsafeSha256 :: String -> Sha256
unsafeSha256 input = case parseSha256 input of
  Left error ->
    unsafeCrashWith $ StringParser.printParserError error
  Right hash ->
    hash
