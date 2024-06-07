-- | Implementation of the `Sha256` data type from the registry spec. The
-- | registry records the hash of package tarballs in the package metadata so
-- | that package managers can ensure the source code they download is correct.
-- | https://github.com/purescript/registry-dev/blob/master/SPEC.md#Sha256
module Registry.Sha256
  ( Sha256
  , codec
  , hashBuffer
  , hashFile
  , hashString
  , parse
  , parser
  , print
  ) where

import Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Control.Monad.Except (Except, except)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Codec as Codec
import Data.Codec.JSON as CJ
import Data.Either (Either)
import Data.List.Lazy as List.Lazy
import Data.String.CodeUnits as String.CodeUnits
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import JSON (JSON)
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS.Aff
import Node.Path (FilePath)
import Parsing (Parser)
import Parsing as Parsing
import Parsing.String as Parsing.String

-- | A subresource integrity hash using the SHA256 algorithm.
-- | https://developer.mozilla.org/en-US/docs/Web/Security/Subresource_Integrity
newtype Sha256 = Sha256 { sri :: String, hash :: String }

derive instance Eq Sha256

-- | A codec for encoding and decoding a `Sha256` as a JSON string
codec :: CJ.Codec Sha256
codec = CJ.named "Sha256" $ Codec.codec' decode encode
  where
  decode :: JSON -> Except CJ.DecodeError Sha256
  decode = except <<< lmap CJ.DecodeError.basic <<< parse <=< Codec.decode CJ.string

  encode :: Sha256 -> JSON
  encode = print >>> CJ.encode CJ.string

-- | Print a Sha256 as a subresource integrity hash using sha256
print :: Sha256 -> String
print (Sha256 { sri, hash }) = sri <> "-" <> hash

-- | Parse a string containing a valid subresource integrity hash using sha256
parse :: String -> Either String Sha256
parse = lmap Parsing.parseErrorMessage <<< flip Parsing.runParser parser

-- | A parser for strings that are valid Sha256 values. A valid Sha256 has a
-- | 'sha256-' prefix, 43 chars, and a '=' terminating character.
parser :: Parser String Sha256
parser = do
  prefix <- Parsing.String.string "sha256"
  _ <- Parsing.String.char '-'
  hash <- List.Lazy.replicateM 43 Parsing.String.anyChar
  suffix <- Parsing.String.char '='
  Parsing.String.eof
  let fromCharList = String.CodeUnits.fromCharArray <<< Array.fromFoldable
  pure $ Sha256 { sri: prefix, hash: fromCharList hash <> String.CodeUnits.singleton suffix }

-- | Create the sha256 SRI hash for a file
hashFile :: forall m. MonadAff m => FilePath -> m Sha256
hashFile path = liftAff do
  fileBuffer <- FS.Aff.readFile path
  liftEffect $ hashBuffer fileBuffer

-- | Create the sha256 SRI hash for a string
hashString :: forall m. MonadEffect m => String -> m Sha256
hashString string = liftEffect do
  buffer <- Buffer.fromString string UTF8
  hashBuffer buffer

-- | Create the sha256 SRI hash for a buffer
hashBuffer :: forall m. MonadEffect m => Buffer -> m Sha256
hashBuffer buffer = liftEffect do
  newHash <- createHash "sha256"
  hash <- updateHash buffer newHash
  digest <- digestHash hash
  string <- Buffer.toString Base64 digest
  pure $ Sha256 { sri: "sha256", hash: string }

-- The functions below rely on Node's 'crypto' module, but do not incur any new
-- JS dependencies.

foreign import data HashObject :: Type

foreign import createHash :: String -> Effect HashObject

foreign import updateHash :: Buffer -> HashObject -> Effect HashObject

foreign import digestHash :: HashObject -> Effect Buffer
