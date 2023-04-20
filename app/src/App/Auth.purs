module Registry.App.Auth
  ( SignAuthenticated
  , signPayload
  , verifyPayload
  ) where

import Registry.App.Prelude

import Data.Array as Array
import Data.String as String
import Registry.Operation (AuthenticatedData)
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
    String.joinWith " " [ owner.keytype, owner.public, fromMaybe "comment" owner.comment ]

type SignAuthenticated =
  { privateKey :: String
  , rawPayload :: String
  }

signPayload :: SignAuthenticated -> Either String SSH.Signature
signPayload { privateKey, rawPayload } = do
  private <- SSH.parsePrivateKey privateKey
  pure $ SSH.sign private rawPayload
