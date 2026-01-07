module Registry.App.Auth
  ( SignAuthenticated
  , signPayload
  , verifyPackageSetPayload
  , verifyPayload
  ) where

import Registry.App.Prelude

import Data.Array as Array
import Data.String as String
import Registry.Operation (AuthenticatedData, PackageSetUpdateRequest)
import Registry.SSH as SSH

-- We take pacchettibotti as an extra owner because pacchettibotti can always
-- sign authenticated transfers. This is not sufficient to blanket authorize
-- things, as someone still needs to use the pacchettibotti credentials to
-- actually sign the payload.
verifyPayload :: Owner -> Array Owner -> AuthenticatedData -> Aff (Either String Unit)
verifyPayload pacchettiBotti owners auth = do
  let eitherKeys = traverse (SSH.parsePublicKey <<< formatOwner) (Array.cons pacchettiBotti owners)
  pure do
    keys <- eitherKeys
    unless (Array.any (\key -> SSH.verify key auth.rawPayload auth.signature) keys) do
      Left "None of the owner keys are suitable to verify the payload."
  where
  formatOwner (Owner owner) =
    String.joinWith " " [ owner.keytype, owner.public, fromMaybe "id" owner.id ]

type SignAuthenticated =
  { privateKey :: String
  , rawPayload :: String
  }

signPayload :: SignAuthenticated -> Either String SSH.Signature
signPayload { privateKey, rawPayload } = do
  private <- lmap SSH.printPrivateKeyParseError $ SSH.parsePrivateKey { key: privateKey, passphrase: Nothing }
  pure $ SSH.sign private rawPayload

-- | Verify a package set update request using pacchettibotti's key.
-- | Returns an error if the signature is invalid or missing.
verifyPackageSetPayload :: Owner -> PackageSetUpdateRequest -> Aff (Either String Unit)
verifyPackageSetPayload pacchettiBotti request = do
  case request.signature of
    Nothing ->
      pure $ Left "Package set update requires a signature for restricted operations."
    Just signature -> do
      let eitherKey = SSH.parsePublicKey (formatOwner pacchettiBotti)
      pure do
        key <- eitherKey
        unless (SSH.verify key request.rawPayload signature) do
          Left "The pacchettibotti signature is not valid for this payload."
  where
  formatOwner (Owner owner) =
    String.joinWith " " [ owner.keytype, owner.public, fromMaybe "id" owner.id ]
