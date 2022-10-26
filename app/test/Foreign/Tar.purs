module Test.Foreign.Tar where

import Registry.Prelude

import Effect.Aff as Aff
import Foreign.Node.FS as FSE
import Foreign.Tar as Tar
import Foreign.Tmp as Tmp
import Node.FS.Aff as FS
import Node.FS.Aff as FSA
import Node.FS.Stats as FS.Stats
import Node.Path as Path
import Registry.Sha256 as Sha256
import Test.Assert as Assert
import Test.Spec as Spec

tar :: Spec.Spec Unit
tar = do
  Spec.describe "Tar" do
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
    packageTmp <- liftEffect Tmp.mkTmpDir
    let packagePath = Path.concat [ packageTmp, "package" ]
    FSE.ensureDirectory packagePath
    writeTmp packagePath "README.md" "# README\nThis is my package."
    writeTmp packagePath "purs.json" "{ \"name\": \"project\" }"
    liftEffect $ Tar.create { cwd: packageTmp, folderName: "package" }
    -- Before we complete the process, a quick check to verify that our tarball
    -- does indeed include the contents we expect.
    FSE.remove packagePath
    liftEffect $ Tar.extract { cwd: packageTmp, archive: "package.tar.gz" }
    files <- FS.readdir packagePath
    if files == [ "README.md", "purs.json" ] then
      hashAndBytes (packagePath <> ".tar.gz")
    else
      unsafeCrashWith "Tar extraction failed."

  writeTmp tmp name contents =
    FSA.writeTextFile UTF8 (Path.concat [ tmp, name ]) contents

  hashAndBytes path = do
    FS.Stats.Stats { size: bytes } <- FS.stat path
    hash <- Sha256.hashFile path
    pure { hash, bytes }
