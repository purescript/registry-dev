module Registry.SSH
  ( ParsedKey
  , parse
  , parsePrivate
  , sign
  , verify
  , SignedData
  , keyType
  , isPrivateKey
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Function.Uncurried (Fn1, Fn2, Fn3, Fn4, runFn1, runFn2, runFn3, runFn4)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Effect.Exception (Error)
import Effect.Exception as Exception
import Node.Buffer (Buffer)

-- | A parsed SSH key which can be used to sign or verify payloads.
data ParsedKey

instance Eq ParsedKey where
  eq a b = runFn2 equalsImpl a b

foreign import parseKeyImpl :: forall r. Fn4 (Error -> r) (ParsedKey -> r) Buffer (Nullable Buffer) r

-- | Parse a public or non-password-protected SSH key
parse :: Buffer -> Either String ParsedKey
parse buf = parsePrivate { key: buf, passphrase: Nothing }

-- | Parse a password-protected SSH key
parsePrivate :: { key :: Buffer, passphrase :: Maybe Buffer } -> Either String ParsedKey
parsePrivate { key, passphrase } = runFn4 parseKeyImpl (Left <<< Exception.message) Right key (toNullable passphrase)

foreign import signImpl :: Fn2 ParsedKey Buffer Buffer

-- | Sign a payload using a parsed SSH key. Returns the signature.
sign :: ParsedKey -> Buffer -> Buffer
sign key = runFn2 signImpl key

foreign import verifyImpl :: Fn3 ParsedKey Buffer Buffer Boolean

type SignedData = { data :: Buffer, signature :: Buffer }

-- | Verify that a payload was signed using the given key by matching the data,
-- | signature, and public key against one another.
verify :: ParsedKey -> SignedData -> Boolean
verify key payload = runFn3 verifyImpl key payload.data payload.signature

foreign import keyTypeImpl :: Fn1 ParsedKey String

-- | Retrieve the type of a key, such as ssh-rsa
keyType :: ParsedKey -> String
keyType = runFn1 keyTypeImpl

foreign import isPrivateKeyImpl :: Fn1 ParsedKey Boolean

-- | Determine whether a parsed key is a private or public key.
isPrivateKey :: ParsedKey -> Boolean
isPrivateKey = runFn1 isPrivateKeyImpl

foreign import equalsImpl :: Fn2 ParsedKey ParsedKey Boolean
