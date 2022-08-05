module Registry.SSH
  ( SignAuthenticated
  , signPacchettiBotti
  , verifyPayload
  ) where

import Registry.Prelude

import Data.Array.NonEmpty as NEA
import Data.String as String
import Foreign.Tmp as Tmp
import Node.ChildProcess as Node.ChildProcess
import Node.FS.Aff as FS
import Node.FS.Perms as Perms
import Node.Path as Path
import Registry.Schema (AuthenticatedData(..), Owner(..))
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
verifyPayload owners (AuthenticatedData { email, signature, rawPayload }) = do
  tmp <- liftEffect Tmp.mkTmpDir
  let joinWithNewlines = String.joinWith "\n"
  let signers = joinWithNewlines $ NEA.toArray $ map formatOwner owners
  FS.writeTextFile UTF8 (Path.concat [ tmp, allowedSignersPath ]) signers
  FS.writeTextFile UTF8 (Path.concat [ tmp, payloadSignaturePath ]) (joinWithNewlines signature)
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

signPacchettiBotti :: SignAuthenticated -> Aff (Either String (Array String))
signPacchettiBotti { publicKey, privateKey, rawPayload } = do
  tmp <- liftEffect Tmp.mkTmpDir
  let privateKeyPath = Path.concat [ tmp, sshKeyPath ]
  let publicKeyPath = privateKeyPath <> ".pub"
  FS.writeTextFile UTF8 publicKeyPath publicKey
  FS.writeTextFile UTF8 privateKeyPath privateKey
  let maybePerms = Perms.permsFromString "0600"
  for_ maybePerms \perms -> do
    FS.chmod publicKeyPath perms
    FS.chmod privateKeyPath perms
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
