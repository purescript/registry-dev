module Registry.App.Auth
  ( SignAuthenticated
  , signPayload
  , verifyPayload
  ) where

import Registry.App.Prelude

import Data.Array as Array
import Data.String as String
import Node.Buffer as Buffer
import Registry.Operation (AuthenticatedData)
import Registry.SSH as SSH

-- We take pacchettibotti as an extra owner because pacchettibotti can always
-- sign authenticated transfers. This is not sufficient to blanket authorize
-- things, as someone still needs to use the pacchettibotti credentials to
-- actually sign the payload.
verifyPayload :: Owner -> Array Owner -> AuthenticatedData -> Aff (Either String Unit)
verifyPayload pacchettiBotti owners auth = do
  let eitherKeys = traverse (SSH.parsePublicKey <<< formatOwner) (Array.cons pacchettiBotti owners)
  case eitherKeys of
    Left error -> pure (Left error)
    Right keys -> do
      signature <- liftEffect $ Buffer.fromString (String.joinWith "\n" auth.signature) UTF8
      case Array.any (\key -> SSH.verify key { data: auth.rawPayload, signature }) keys of
        true -> pure (Right unit)
        _ -> pure (Left "None of the owner keys are suitable to verify the payload.")
  where
  formatOwner (Owner owner) =
    String.joinWith " " [ owner.email, owner.keytype, owner.public ]

type SignAuthenticated =
  { privateKey :: String
  , rawPayload :: String
  }

signPayload :: SignAuthenticated -> Either String Buffer
signPayload { privateKey, rawPayload } = do
  private <- SSH.parsePrivateKey privateKey
  pure (SSH.sign private rawPayload).signature
