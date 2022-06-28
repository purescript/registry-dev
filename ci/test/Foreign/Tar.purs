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
import Registry.Hash as Hash
import Test.Spec as Spec
import Test.Spec.Assertions as Assert

tar :: Spec.Spec Unit
tar = do
  Spec.describe "Tar" do
    Spec.it "Successfully produces tarball from directory" do
      { bytes } <- createTarball
      bytes `Assert.shouldSatisfy` (_ > 0.0)

    Spec.it "Tarballs are identical if contents are identical" do
      tarball1 <- createTarball
      Aff.delay (Aff.Milliseconds 100.0)
      tarball2 <- createTarball
      tarball1 `Assert.shouldEqual` tarball2
  where
  createTarball = do
    packageTmp <- liftEffect Tmp.mkTmpDir
    let packagePath = Path.concat [ packageTmp, "package" ]
    FSE.ensureDirectory packagePath
    writeTmp packagePath "README.md" "# README\nThis is my package."
    writeTmp packagePath "purs.json" "{ \"name\": \"project\" }"
    let archiveName = packagePath <> ".tar.gz"
    liftEffect $ Tar.create { cwd: packageTmp, folderName: packagePath, archiveName }
    hashAndBytes archiveName

  writeTmp tmp name contents =
    FSA.writeTextFile ASCII (Path.concat [ tmp, name ]) contents

  hashAndBytes path = do
    FS.Stats.Stats { size: bytes } <- FS.stat path
    hash <- Hash.sha256File path
    pure { hash, bytes }
