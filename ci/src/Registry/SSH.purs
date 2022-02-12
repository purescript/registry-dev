module Foreign.SSH where

import Registry.Prelude

import Data.Array.NonEmpty as NEA
import Data.String as String
import Foreign.Tmp as Tmp
import Node.ChildProcess as NodeProcess
import Node.FS.Aff as FS
import Node.Path as Path
import Registry.Schema (AuthenticatedData(..), Owner(..))
import Sunde as Process

allowedSigners :: FilePath
allowedSigners = "allowed_signers"

payloadSignature :: FilePath
payloadSignature = "payload_signature.sig"

-- Payload verification is based on this description:
-- https://www.agwa.name/blog/post/ssh_signatures
verifyPayload :: NonEmptyArray Owner -> AuthenticatedData -> Aff (Either String String)
verifyPayload owners (AuthenticatedData { email, signature, rawPayload }) = do
  tmp <- liftEffect Tmp.mkTmpDir
  let joinWithNewlines = String.joinWith "\n"
  let signers = joinWithNewlines $ NEA.toArray $ map formatOwner owners
  FS.writeTextFile UTF8 (Path.concat [ tmp, allowedSigners ]) signers
  FS.writeTextFile UTF8 (Path.concat [ tmp, payloadSignature ]) (joinWithNewlines signature)
  -- The 'ssh-keygen' command will only exit normally if the signature verifies,
  -- and otherwise will report an error status code.
  sshKeygenVerify tmp
  where
  formatOwner (Owner owner) =
    String.joinWith " " [ owner.email, owner.keytype, owner.public ]

  sshKeygenVerify tmp = do
    let cmd = "ssh-keygen"
    let stdin = Just rawPayload
    let args = [ "-Y", "verify", "-f", allowedSigners, "-I", email, "-n", "file", "-s", payloadSignature ]
    result <- Process.spawn { cmd, stdin, args } (NodeProcess.defaultSpawnOptions { cwd = Just tmp })
    pure $ bimap String.trim String.trim case result.exit of
      NodeProcess.Normally 0 -> Right result.stdout
      _ -> Left result.stderr
