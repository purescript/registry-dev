module Registry.SSH
  ( PublicKey
  , PrivateKey
  , Signature(..)
  , parsePublicKey
  , parsePrivateKey
  , parsePrivateKeyWithPassword
  , sign
  , verify
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Function.Uncurried (Fn1, Fn2, Fn3, Fn4, runFn1, runFn2, runFn3, runFn4)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable, notNull, null)
import Effect.Exception as Exception

-- | A parsed SSH public key which can be used to verify payloads.
newtype PublicKey = PublicKey ParsedKey

derive instance Eq PublicKey

-- | A parsed SSH private key which can be used to sign payloads.
newtype PrivateKey = PrivateKey ParsedKey

derive instance Eq PrivateKey

data ParsedKey

instance Eq ParsedKey where
  eq a b = runFn2 equalsImpl a b

foreign import parseKeyImpl :: forall r. Fn4 (Exception.Error -> r) (ParsedKey -> r) String (Nullable String) r

parse :: String -> Either String ParsedKey
parse buf = runFn4 parseKeyImpl (Left <<< Exception.message) Right buf null

-- | Parse a non-password-protected private SSH key
parsePrivateKey :: String -> Either String PrivateKey
parsePrivateKey key = case parse key of
  Right parsed | not (isPrivateKey parsed) -> Left $ "Expected private key, but this is a public key of type " <> keyType parsed
  result -> map PrivateKey result

-- | Parse a password-protected private SSH key
parsePrivateKeyWithPassword :: { key :: String, passphrase :: String } -> Either String PrivateKey
parsePrivateKeyWithPassword { key, passphrase } =
  case runFn4 parseKeyImpl (Left <<< Exception.message) Right key (notNull passphrase) of
    Right parsed | not (isPrivateKey parsed) -> Left $ "Expected private key, but this is a public key of type " <> keyType parsed
    result -> map PrivateKey result

-- | Parse a public SSH key
parsePublicKey :: String -> Either String PublicKey
parsePublicKey key = case parse key of
  Right parsed | isPrivateKey parsed -> Left $ "Expected public key, but this is a private key of type " <> keyType parsed
  result -> map PublicKey result

-- | A hex-encoded SSH signature
newtype Signature = Signature String

derive instance Newtype Signature _
derive newtype instance Eq Signature

foreign import signImpl :: Fn2 ParsedKey String Signature

-- | Sign a payload using a parsed SSH key. Returns the signature.
sign :: PrivateKey -> String -> Signature
sign (PrivateKey key) = runFn2 signImpl key

foreign import verifyImpl :: Fn3 ParsedKey String Signature Boolean

-- | Verify that a payload was signed using the given key by matching the data,
-- | signature, and public key against one another.
verify :: PublicKey -> String -> Signature -> Boolean
verify (PublicKey key) payload signature = runFn3 verifyImpl key payload signature

foreign import keyTypeImpl :: Fn1 ParsedKey String

-- | Retrieve the type of a key, such as ssh-rsa
keyType :: ParsedKey -> String
keyType = runFn1 keyTypeImpl

foreign import isPrivateKeyImpl :: Fn1 ParsedKey Boolean

-- | Determine whether a parsed key is a private or public key.
isPrivateKey :: ParsedKey -> Boolean
isPrivateKey = runFn1 isPrivateKeyImpl

foreign import equalsImpl :: Fn2 ParsedKey ParsedKey Boolean
