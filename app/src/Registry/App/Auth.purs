module Registry.App.Auth
  ( SignAuthenticated
  , signPayload
  , verifyPayload
  ) where

import Registry.App.Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Data.String as String
import Foreign.Tmp as Tmp
import Node.ChildProcess as Node.ChildProcess
import Node.FS.Aff as FS.Aff
import Node.FS.Perms as Perms
import Node.Path as Path
import Registry.Operation (AuthenticatedData)
import Sunde as Process

allowedSignersPath :: FilePath
allowedSignersPath = "allowed_signers"

payloadSignaturePath :: FilePath
payloadSignaturePath = "payload_signature.sig"

sshKeyPath :: FilePath
sshKeyPath = "id_ed25519"

-- Payload verification is based on this description:
-- https://www.agwa.name/blog/post/ssh_signatures
verifyPayload :: NonEmptyArray Owner -> AuthenticatedData -> Aff (Either String String)
verifyPayload owners { email, signature, rawPayload } = do
  tmp <- liftEffect Tmp.mkTmpDir
  let joinWithNewlines = String.joinWith "\n"
  let signers = joinWithNewlines $ NonEmptyArray.toArray $ map formatOwner owners
  FS.Aff.writeTextFile UTF8 (Path.concat [ tmp, allowedSignersPath ]) signers
  FS.Aff.writeTextFile UTF8 (Path.concat [ tmp, payloadSignaturePath ]) (joinWithNewlines signature)
  -- The 'ssh-keygen' command will only exit normally if the signature verifies,
  -- and otherwise will report an error status code.
  sshKeygenVerify tmp
  where
  formatOwner (Owner owner) =
    String.joinWith " " [ owner.email, owner.keytype, owner.public ]

  sshKeygenVerify tmp = do
    let cmd = "ssh-keygen"
    let stdin = Just rawPayload
    let args = [ "-Y", "verify", "-f", allowedSignersPath, "-I", email, "-n", "file", "-s", payloadSignaturePath ]
    result <- Process.spawn { cmd, stdin, args } (Node.ChildProcess.defaultSpawnOptions { cwd = Just tmp })
    pure $ bimap String.trim String.trim case result.exit of
      Node.ChildProcess.Normally 0 -> Right result.stdout
      _ -> Left result.stderr

type SignAuthenticated =
  { publicKey :: String
  , privateKey :: String
  , rawPayload :: String
  }

signPayload :: SignAuthenticated -> Aff (Either String (Array String))
signPayload { publicKey, privateKey, rawPayload } = do
  tmp <- liftEffect Tmp.mkTmpDir
  let publicKeyPath = Path.concat [ tmp, sshKeyPath <> ".pub" ]
  let privateKeyPath = Path.concat [ tmp, sshKeyPath ]
  -- Key files must have a single trailing newline.
  FS.Aff.writeTextFile UTF8 publicKeyPath (String.trim publicKey <> "\n")
  FS.Aff.writeTextFile UTF8 privateKeyPath (String.trim privateKey <> "\n")
  for_ (Perms.permsFromString "0600") (FS.Aff.chmod privateKeyPath)
  sshKeygenSign tmp
  where
  sshKeygenSign tmp = do
    let cmd = "ssh-keygen"
    let stdin = Just rawPayload
    -- If you specify - for the filename, the file to sign is read from standard
    -- in and the signature is written to standard out.
    let args = [ "-Y", "sign", "-f", sshKeyPath, "-n", "file", "-" ]
    result <- Process.spawn { cmd, stdin, args } (Node.ChildProcess.defaultSpawnOptions { cwd = Just tmp })
    pure case result.exit of
      Node.ChildProcess.Normally 0 ->
        Right $ String.split (String.Pattern "\n") $ String.trim result.stdout
      _ ->
        Left $ String.trim result.stderr
