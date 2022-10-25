-- | Implementation of the `SRIHash` data type from the registry spec. The
-- | registry records the hash of package tarballs in the package metadata so
-- | that package managers can ensure the source code they download is correct.
-- | https://github.com/purescript/registry-dev/blob/master/SPEC.md#srihash
module Registry.SRIHash
  ( SRIHash
  , hashBuffer
  , hashFile
  , hashString
  , parse
  , parser
  , print
  ) where

import Prelude

import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.List.Lazy as List.Lazy
import Data.String.CodeUnits as String.CodeUnits
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Parsing (Parser)
import Parsing as Parsing
import Parsing.String as Parsing.String

-- | A subresource integrity hash using the SHA256 algorithm.
-- | https://developer.mozilla.org/en-US/docs/Web/Security/Subresource_Integrity
newtype SRIHash = SRIHash { sri :: String, hash :: String }

derive instance Eq SRIHash

print :: SRIHash -> String
print (SRIHash { sri, hash }) = sri <> "-" <> hash

-- | Parse a string containing a valid subresource integrity hash using sha256
parse :: String -> Either String SRIHash
parse = lmap Parsing.parseErrorMessage <<< flip Parsing.runParser parser

-- | A parser for strings that are valid SRIHash values. A valid SRIHash has a
-- | 'sha256-' prefix, 43 chars, and a '=' terminating character.
parser :: Parser String SRIHash
parser = do
  prefix <- Parsing.String.string "sha256"
  _ <- Parsing.String.char '-'
  hash <- List.Lazy.replicateM 43 Parsing.String.anyChar
  suffix <- Parsing.String.char '='
  Parsing.String.eof
  let fromCharList = String.CodeUnits.fromCharArray <<< Array.fromFoldable
  pure $ SRIHash { sri: prefix, hash: fromCharList hash <> String.CodeUnits.singleton suffix }

-- | Create the sha256 SRI hash for a file
hashFile :: FilePath -> Aff SRIHash
hashFile path = do
  fileBuffer <- FS.readFile path
  liftEffect $ hashBuffer fileBuffer

-- | Create the sha256 SRI hash for a string
hashString :: String -> Effect SRIHash
hashString string = do
  buffer <- Buffer.fromString string UTF8
  hashBuffer buffer

-- | Create the sha256 SRI hash for a buffer
hashBuffer :: Buffer -> Effect SRIHash
hashBuffer buffer = do
  newHash <- createHash "sha256"
  hash <- updateHash buffer newHash
  digest <- digestHash hash
  string <- Buffer.toString Base64 digest
  pure $ SRIHash { sri: "sha256", hash: string }

-- The functions below rely on Node's 'crypto' module, but do not incur any new
-- JS dependencies.

foreign import data HashObject :: Type

foreign import createHash :: String -> Effect HashObject

foreign import updateHash :: Buffer -> HashObject -> Effect HashObject

foreign import digestHash :: HashObject -> Effect Buffer
