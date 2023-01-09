module Test.Registry.App.CLI.Tar where

import Registry.App.Prelude

import Effect.Aff as Aff
import Node.FS.Aff as FS.Aff
import Node.FS.Stats as FS.Stats
import Node.Path as Path
import Registry.App.CLI.Tar as Tar
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.Tmp as Tmp
import Registry.Sha256 as Sha256
import Registry.Test.Assert as Assert
import Test.Spec as Spec

spec :: Spec.Spec Unit
spec = do
  Spec.it "Successfully produces tarball from directory" do
    { bytes } <- createTarball
    bytes `Assert.shouldSatisfy` (_ > 30.0)

  Spec.it "Tarballs are identical if contents are identical" do
    tarball1 <- createTarball
    Aff.delay (Aff.Milliseconds 1010.0)
    tarball2 <- createTarball
    tarball1 `Assert.shouldEqual` tarball2
  where
  createTarball = do
    packageTmp <- Tmp.mkTmpDir
    let packagePath = Path.concat [ packageTmp, "package" ]
    FS.Extra.ensureDirectory packagePath
    writeTmp packagePath "README.md" "# README\nThis is my package."
    writeTmp packagePath "purs.json" "{ \"name\": \"project\" }"
    Tar.create { cwd: packageTmp, folderName: "package" }
    -- Before we complete the process, a quick check to verify that our tarball
    -- does indeed include the contents we expect.
    FS.Extra.remove packagePath
    Tar.extract { cwd: packageTmp, archive: "package.tar.gz" }
    files <- FS.Aff.readdir packagePath
    if files == [ "README.md", "purs.json" ] then
      hashAndBytes (packagePath <> ".tar.gz")
    else
      unsafeCrashWith "Tar extraction failed."

  writeTmp tmp name contents =
    FS.Aff.writeTextFile UTF8 (Path.concat [ tmp, name ]) contents

  hashAndBytes path = do
    FS.Stats.Stats { size: bytes } <- FS.Aff.stat path
    hash <- Sha256.hashFile path
    pure { hash, bytes }
