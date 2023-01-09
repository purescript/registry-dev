module Test.Registry.Foreign.FastGlob (spec) where

import Prelude

import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS as FS
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Node.Process as Process
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.FastGlob as FastGlob
import Registry.Foreign.Tmp as Tmp
import Registry.Test.Assert as Assert
import Test.Spec as Spec

spec :: Spec.Spec Unit
spec = do
  Spec.it "Prevents directory traversal" do
    tmp <- Tmp.mkTmpDir
    let outerFile = Path.concat [ tmp, "flake.nix" ]
    FS.Aff.writeTextFile UTF8 outerFile "<contents>"
    let innerDirectory = Path.concat [ tmp, "inner" ]
    FS.Extra.ensureDirectory innerDirectory
    { succeeded, failed } <- FastGlob.match innerDirectory [ "../flake.nix" ]
    succeeded `Assert.shouldEqual` []
    failed `Assert.shouldEqual` [ "../flake.nix" ]

  Spec.it "Prevents symlink directory traversal" do
    tmp <- Tmp.mkTmpDir
    let outerFile = Path.concat [ tmp, "shell.nix" ]
    FS.Aff.writeTextFile UTF8 outerFile "<contents>"
    let innerDirectory = Path.concat [ tmp, "inner" ]
    FS.Extra.ensureDirectory innerDirectory
    FS.Aff.symlink outerFile (Path.concat [ innerDirectory, "shell.nix" ]) FS.FileLink
    { succeeded, failed } <- FastGlob.match innerDirectory [ "./shell.nix" ]
    succeeded `Assert.shouldEqual` []
    failed `Assert.shouldEqual` [ "./shell.nix" ]

  -- A glob that is technically a directory traversal but which doesn't
  -- actually match any files won't throw an error since there are no results.
  Spec.it "Prevents traversal to a non-existing file" do
    cwd <- liftEffect Process.cwd
    result <- FastGlob.match cwd [ "/var/www/root/shell.nix" ]
    unless (result == mempty) do
      Assert.fail $ "Expected no results, but received " <> show result
