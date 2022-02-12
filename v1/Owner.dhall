{-

A package owner, described using their SSH key and associated email address. It
is not necessary to provide a valid email address, but the email address
provided must match the one used to sign payloads.

https://man.openbsd.org/ssh-keygen#ALLOWED_SIGNERS

-}

let Owner =
      -- An email address associated with a public SSH key
      { email : Text
      -- The type of the SSH key, e.g. ssh-rsa or ssh-ed25519
      , keytype : Text
      -- The public key
      , public : Text
      }

in  Owner
