module Test.Registry.SSH (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Registry.SSH (Signature(..))
import Registry.SSH as SSH
import Registry.Test.Assert as Assert
import Registry.Test.Utils as Utils
import Test.Spec as Spec

spec :: Spec.Spec Unit
spec = do
  Spec.it "Parses an ED25519 private key" do
    case SSH.parsePrivateKey { key: id_ed25519, passphrase: Nothing } of
      Left err -> Assert.fail $ "Failed to parse ed_25519 private key: " <> SSH.printParsePrivateKeyError err
      Right _ -> pure unit

  Spec.it "Parses an ED25519 public key" do
    case SSH.parsePublicKey id_ed25519_pub of
      Left err -> Assert.fail $ "Failed to parse ed_25519 public key: " <> err
      Right _ -> pure unit

  Spec.it "Parses a password-protected RSA private key" do
    case SSH.parsePrivateKey { key: id_rsa, passphrase: Nothing } of
      Left err1@SSH.RequiresPassphrase -> do
        SSH.printParsePrivateKeyError err1 `Assert.shouldEqual` "Encrypted private key requires a passphrase"
        case SSH.parsePrivateKey { key: id_rsa, passphrase: Just id_rsa_password } of
          Left err2 -> Assert.fail $ "Failed to parse id_rsa private key with password: " <> SSH.printParsePrivateKeyError err2
          Right _ -> pure unit
      Left otherError -> Assert.fail $ "Should have required a passphrase, but got: " <> SSH.printParsePrivateKeyError otherError
      Right _ -> Assert.fail $ "Expected parse failure, but got key."

  Spec.it "Parses an RSA public key" do
    case SSH.parsePublicKey id_rsa_pub of
      Left err -> Assert.fail $ "Failed to parse rsa public key: " <> err
      Right _ -> pure unit

  Spec.it "Does not parse public key as private key and vice versa" do
    case SSH.parsePublicKey id_ed25519 of
      Left _ -> pure unit
      Right _ -> Assert.fail "Parsed private key as a public key."

    case SSH.parsePrivateKey { key: id_ed25519_pub, passphrase: Nothing } of
      Left _ -> pure unit
      Right _ -> Assert.fail "Parsed public key as a private key."

  Spec.it "Signs payload" do
    let signature = SSH.sign (Utils.unsafeSSHPrivateKey id_ed25519) payload
    Assert.shouldEqual signature payloadHexSignature

  Spec.it "Verifies signed payload" do
    unless (SSH.verify (Utils.unsafeSSHPublicKey id_ed25519_pub) payload payloadHexSignature) do
      Assert.fail "Failed to verify signed payload."

  Spec.it "Does not falsely verify payloads" do
    let privateKey = Utils.unsafeSSHPrivateKey id_ed25519
    let signature = SSH.sign privateKey payload
    let publicKey = Utils.unsafeSSHPublicKey id_ed25519_pub

    let badPayload = String.drop 1 payload
    let badSignature = SSH.sign privateKey badPayload
    let badPublicKey = Utils.unsafeSSHPublicKey id_rsa_pub

    when (SSH.verify badPublicKey payload signature) do
      Assert.fail "Verified a signed payload with an incorrect key."

    when (SSH.verify publicKey payload badSignature) do
      Assert.fail "Verified a signed payload with an incorrect signature."

    when (SSH.verify publicKey badPayload signature) do
      Assert.fail "Verified a signed payload with an incorrect input data payload."

-- ssh-keygen -t ed25519 -C "your_email@example.com"
id_ed25519 :: String
id_ed25519 = String.joinWith "\n"
  [ "-----BEGIN OPENSSH PRIVATE KEY-----"
  , "b3BlbnNzaC1rZXktdjEAAAAABG5vbmUAAAAEbm9uZQAAAAAAAAABAAAAMwAAAAtzc2gtZW"
  , "QyNTUxOQAAACDdKoRtqQVRN+v7q4GpWn2oeJ3c6uKVhuMWenLEX3YFcAAAAKAMzocpDM6H"
  , "KQAAAAtzc2gtZWQyNTUxOQAAACDdKoRtqQVRN+v7q4GpWn2oeJ3c6uKVhuMWenLEX3YFcA"
  , "AAAEBx28Ox8iudqyeko/fOF47/79/HnNKyHsEIk3jPtr0CzN0qhG2pBVE36/urgalafah4"
  , "ndzq4pWG4xZ6csRfdgVwAAAAFnlvdXJfZW1haWxAZXhhbXBsZS5jb20BAgMEBQYH"
  , "-----END OPENSSH PRIVATE KEY-----"
  ]

id_ed25519_pub :: String
id_ed25519_pub = String.joinWith " "
  [ "ssh-ed25519"
  , "AAAAC3NzaC1lZDI1NTE5AAAAIN0qhG2pBVE36/urgalafah4ndzq4pWG4xZ6csRfdgVw"
  , "your_email@example.com"
  ]

-- ssh-keygen -t rsa -b 4096 -C "your_email@example.com"
id_rsa :: String
id_rsa = String.joinWith "\n"
  [ "-----BEGIN OPENSSH PRIVATE KEY-----"
  , "b3BlbnNzaC1rZXktdjEAAAAACmFlczI1Ni1jdHIAAAAGYmNyeXB0AAAAGAAAABBmy/ohkF"
  , "9UYcgUOG4pSBPdAAAAEAAAAAEAAAIXAAAAB3NzaC1yc2EAAAADAQABAAACAQDOHWfcD2vl"
  , "rcaEngneZ2TlHjnjLKoQCuy9R95F1qrfRIE0N6xH7eHMuJGFIvqeuKivSXWLUKQslf2XIA"
  , "7n0PEX0vmzzM7JZNvOkIFoOinBfCKqAx1dIle7yYPAUZPrzBidyLv+4aCJ+zu819yHA5tf"
  , "oZB87+N0QAZcEptYw3taWHZGTZdNpgIgcpDGEnUihuQ9eYbdePokWbDsSgBT7AMjpAPTN5"
  , "Yvg27jNNm6/WdooY7O9tP4Xdheb7GUIabKeNDX4sK0hBWVcS4SVTMVV8ifflKWXboJqIhX"
  , "vUHcn1Te4o193aC+VgDFyzIAhPiZjfI/Fnha9XVPOXZMotIkJ0xiH7jzFljYshExCiecED"
  , "bLnV67Z/CEzVmw7kYTYs2+fpJ6cnJGHWIfaU2cz3C+empn3kZ6So+CM/8oHVpt0UjhTuqI"
  , "av29OlouAxcv8eLPHPmTINyiaZ7b+slZBsNcUgW5r54sJvsXyzx9DQN+jkfwtucxN3JQcn"
  , "IhZcYvD9KSZtHRtz9iLOYLL6Pimbb4K9l98+Br4G40Mjby3ElsAwtPGOLdimyZAD2t2eyD"
  , "u64kOm2zS9jS6JJ9/8uKxlSyenUBxQ+ITwn1enEd4qq1qpnUT/F7PKqv9SSn6UBoMXq+uT"
  , "Fa5rXevOMhl7whAZjttyFokYQsacy6kbvlLMOWcsLqMwAAB1BlCh6hghDjksoDC+To4PA2"
  , "jQmCdyODiejDlpeBFjaKrYCfi1ZV8Q7FeIARo7zX094r5FPMkVR0Kvl5uVDVJPp1fC/wZ4"
  , "MOUaOFQDCetK+IWmB2nzb71/xgQbdNESXeBBnVvI6EYiM9seU+Cq8S+qZix69ifYyqKZCo"
  , "jmB/3WwVvdPa3moX3d4zQz3eWu6qLkcZHZuhMtM6AKFRMYky8lj8J0F0EgNq6rRmysBALT"
  , "2U0IjKrx43yvaU3rzYcwoDHwe1gVKQM+rP7hZZ7OvQXk9QLrfFXWPFdrXDA/A2w0eZtS2q"
  , "CRbwV3xJS4+AwYtmeH6Z4Q0Q9OoAnG9oIH643rmUFeeNBtJSuNIxOTibRDfRXj+E5AO3Ru"
  , "62hcfzOl6LbXYDQYn9nraqNkYD9VMGu2uGVrYuz7jFMFZenFfRqw5/3UgMISC0a6AQw2Es"
  , "lXgOJRsEggGlc7/8tTP3GHcDx4UF4GfTk8xzrXI7hgkFRT6nG+giVO1xNGnJxmAuPtBVRU"
  , "GQ+WBH61oK7vw39iKewb6TkrTWza3/WuUa4C1tVO/cg4PfyDFQE8vV58z43p2PJ9ezqsBo"
  , "EjuAcstNes73cMY1nt+jn/9XB+J5ZsukoG4Alw0mqapd/wlAzH0riWvVueO4uhZVWTft/O"
  , "FiFS2fxjVy2ceNQq0930GenejwFM9c7o80pOajFixl8dbfJ3sIViy0CjgfeCc5Z9py2zPT"
  , "durwMKnGFwvC+CctZCPvdWded3WOQgmz1GrWXyXvQS6bwVLQ+hJ5MtKdO+PfGicU6uEIbl"
  , "nthZLF2DxjVE52GfkHM5btLfK+jmetLu3FcYCsx5E1ND1MWz2K7K9Yd9GJBoQ+d6NJl7Lm"
  , "1laiLiCrgGheCgNsgldrDSYSpilg+BYil4z8RZGJ4CjpjsIEg1W1uIlKQxB+3MV1qDiCkX"
  , "eSWgdpsxDjQD6Ep35e3j4G92AG3DCmsLahz/q9rUBRUHiHPLCAqWiqP1+NZJLMLFKYL2QE"
  , "z5rEnm+xb0MytbUJsANkFActyyyPAS+Ts2u/3gNK4pPJ/gz1z3S0s7MdsV8qZ6kCR394fu"
  , "XjZEoOh2bILMtsevZO78+BS8/dOlobpPh6KllDYsYVbkUxJC5aHtwXhPPS1FKyslyTnLAN"
  , "ffKVkg5Tshxydi4d1hLBjXawfKAly9Nzo8jSlSydWz8HAqGoBypP3rJ69zw3Z+kb19QQyU"
  , "M8YmCcA1PQdwoFaEyHIrsm2DfL/xnJVe3D+CRvWRoS9xZk9yggMp3SxF47doC6L6v56Fnk"
  , "SKLzTe/wm1QrRvIYTQy9aMhmbMUVZ3IUNrEOY9wjj86qPJRniNAHq0LdZJIp1Y4SMPHEiD"
  , "F6a7d9+it3sqp7lANJfMWhhz0YEVADAPTorSfBxuO6cDmqpfPkGBrBv9qEe5oFeU08HpOO"
  , "O2u2ywj7YJIThPgHfCjrpVBz2gWtKw5qSWtdH0UiH/VJmoeN511n9yN5aGgbICK9oiLFM3"
  , "CL0bHx4svafQxdEhvlNoBjzu7AMIVo7n+xfKPZI+q0/WpLY6Yx1G8Y8ivpk0L/DeDhTgSW"
  , "YuKp+8KkRETgb/fD5GlvPtHTHqgHjpoKKvjcM6DF45En85/3iGz0jEzzVbzWkJzy9iYVul"
  , "O246AAJla6ga8SRHbwTxw/d8AaFJnBYVw/zpG51yfIQxVqmKdthdChIoY4az8/ywByq1QR"
  , "q0ujP46dk7CfBqCUMBFG3vh1ndTM7WkutDO17UXLYROQnrTsJQvErWSGzcFBydtfxuH0e3"
  , "DnIcZKbbgs7RhySx1FFTDnyNBLyTFASVG4pvGIk6hVas5VEdySu6JPWm3lX37G2D7H/P8M"
  , "bQ8AvuRbDGTYcbzmvwtPTb1cea6ZwZ8r1/OPr7fN+tBKgERG9U8vd9wq2h77FZ5nRvRu9k"
  , "FGEPaQXUL95D2dMme4iYjBwBSHsY1bR2l0w1wKN5wdaDYNK09NPJGI4jg/z9rV1lzZ3XVH"
  , "IqKBF0R1toe7fvps7kok6zDUUZtJyuTBVIDr1JC6HJU/Wv7ZvuJguUK/GM/9ZxeKGhefVQ"
  , "EjWnI3M8kryz6wPBPcJz73Yl3EXiTcejiz5AWXo88QvJ3nWvK4A0SfWhVr0e7n7NaboMhb"
  , "Co6+CLsN3t5w9nZE4dWfOchPQWdTmv48BrO6fraMdvnyNuiR7vjQv0AdtL4fRJWruIOCM3"
  , "RR8dhUT2ci5EW2iIrwJ9OwXO2Zr+ERWHIkDWB27IKPuFycS9R/L9jowdSJ/INRdHS8jCnf"
  , "xLdcnMsfbIFdKkpV9ROVPxbVVOVskKLZtg6nI8F5/lkUnaVN7X5zNCmcAWL85oLW19kG64"
  , "Q6J6qwPHQ8JRpCelonvlySSGHjIy1ONfJnLU3EgtOm7FWU5mF7TTDieH8zmJVOTWmUovPa"
  , "laO5cnDuuOvz0KPWyYrekCoIYtwc5G2l1KB3bddUh8Xq78A1/GNKbD6o8kADqYHHGnOG9e"
  , "EwKrIJasPflNRLac079nrRaPw="
  , "-----END OPENSSH PRIVATE KEY-----"
  ]

id_rsa_pub :: String
id_rsa_pub = String.joinWith " "
  [ "ssh-rsa"
  , "AAAAB3NzaC1yc2EAAAADAQABAAACAQDOHWfcD2vlrcaEngneZ2TlHjnjLKoQCuy9R95F1qrfRIE0N6xH7eHMuJGFIvqeuKivSXWLUKQslf2XIA7n0PEX0vmzzM7JZNvOkIFoOinBfCKqAx1dIle7yYPAUZPrzBidyLv+4aCJ+zu819yHA5tfoZB87+N0QAZcEptYw3taWHZGTZdNpgIgcpDGEnUihuQ9eYbdePokWbDsSgBT7AMjpAPTN5Yvg27jNNm6/WdooY7O9tP4Xdheb7GUIabKeNDX4sK0hBWVcS4SVTMVV8ifflKWXboJqIhXvUHcn1Te4o193aC+VgDFyzIAhPiZjfI/Fnha9XVPOXZMotIkJ0xiH7jzFljYshExCiecEDbLnV67Z/CEzVmw7kYTYs2+fpJ6cnJGHWIfaU2cz3C+empn3kZ6So+CM/8oHVpt0UjhTuqIav29OlouAxcv8eLPHPmTINyiaZ7b+slZBsNcUgW5r54sJvsXyzx9DQN+jkfwtucxN3JQcnIhZcYvD9KSZtHRtz9iLOYLL6Pimbb4K9l98+Br4G40Mjby3ElsAwtPGOLdimyZAD2t2eyDu64kOm2zS9jS6JJ9/8uKxlSyenUBxQ+ITwn1enEd4qq1qpnUT/F7PKqv9SSn6UBoMXq+uTFa5rXevOMhl7whAZjttyFokYQsacy6kbvlLMOWcsLqMw=="
  , "your_email@example.com"
  ]

id_rsa_password :: String
id_rsa_password = "abc123"

payload :: String
payload = "{ \"name\": \"node-os\", \"newLocation\": { \"githubOwner\": \"purescript-node\", \"githubRepo\": \"purescript-node-os\" } }"

payloadHexSignature :: Signature
payloadHexSignature = Signature "1f4967eaa5de1076bb2185b818ea4fb7c18cfe83af951ab32c3bcb4a300dfe9b3795daaae1e7a6d5fb9f72c4cec8003f79a452f2dc9da9ec8cfa63b243c80503"
