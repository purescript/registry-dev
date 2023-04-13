module Registry.SSH
  ( PublicKey
  , PrivateKey
  , SignedData
  , parsePublicKey
  , parsePrivateKey
  , parsePrivateKeyWithPassword
  , sign
  , verify
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Function.Uncurried (Fn1, Fn2, Fn3, Fn4, runFn1, runFn2, runFn3, runFn4)
import Data.Nullable (Nullable, notNull, null)
import Effect.Exception (Error)
import Effect.Exception as Exception
import Node.Buffer (Buffer)

-- | A parsed SSH public key which can be used to verify payloads.
newtype PublicKey = PublicKey ParsedKey

-- | A parsed SSH private key which can be used to sign payloads.
newtype PrivateKey = PrivateKey ParsedKey

data ParsedKey

instance Eq ParsedKey where
  eq a b = runFn2 equalsImpl a b

foreign import parseKeyImpl :: forall r. Fn4 (Error -> r) (ParsedKey -> r) Buffer (Nullable Buffer) r

parse :: Buffer -> Either String ParsedKey
parse buf = runFn4 parseKeyImpl (Left <<< Exception.message) Right buf null

-- | Parse a non-password-protected private SSH key
parsePrivateKey :: Buffer -> Either String PrivateKey
parsePrivateKey buffer = case parse buffer of
  Right parsed | not (isPrivateKey parsed) -> Left $ "Expected private key, but this is a public key of type " <> keyType parsed
  result -> map PrivateKey result

-- | Parse a password-protected private SSH key
parsePrivateKeyWithPassword :: { key :: Buffer, passphrase :: Buffer } -> Either String PrivateKey
parsePrivateKeyWithPassword { key, passphrase } =
  case runFn4 parseKeyImpl (Left <<< Exception.message) Right key (notNull passphrase) of
    Right parsed | not (isPrivateKey parsed) -> Left $ "Expected private key, but this is a public key of type " <> keyType parsed
    result -> map PrivateKey result

-- | Parse a public SSH key
parsePublicKey :: Buffer -> Either String PublicKey
parsePublicKey buffer = case parse buffer of
  Right parsed | isPrivateKey parsed -> Left $ "Expected public key, but this is a private key of type " <> keyType parsed
  result -> map PublicKey result

-- | A pair of data and a signature from the data being signed by a SSH key.
type SignedData = { data :: Buffer, signature :: Buffer }

foreign import signImpl :: Fn2 ParsedKey Buffer Buffer

-- | Sign a payload using a parsed SSH key. Returns the signature.
sign :: PrivateKey -> Buffer -> SignedData
sign (PrivateKey key) buffer = do
  let signature = runFn2 signImpl key buffer
  { data: buffer, signature }

foreign import verifyImpl :: Fn3 ParsedKey Buffer Buffer Boolean

-- | Verify that a payload was signed using the given key by matching the data,
-- | signature, and public key against one another.
verify :: PublicKey -> SignedData -> Boolean
verify (PublicKey key) payload = runFn3 verifyImpl key payload.data payload.signature

foreign import keyTypeImpl :: Fn1 ParsedKey String

-- | Retrieve the type of a key, such as ssh-rsa
keyType :: ParsedKey -> String
keyType = runFn1 keyTypeImpl

foreign import isPrivateKeyImpl :: Fn1 ParsedKey Boolean

-- | Determine whether a parsed key is a private or public key.
isPrivateKey :: ParsedKey -> Boolean
isPrivateKey = runFn1 isPrivateKeyImpl

foreign import equalsImpl :: Fn2 ParsedKey ParsedKey Boolean
