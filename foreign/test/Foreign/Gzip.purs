module Test.Registry.Foreign.Gzip (spec) where

import Prelude

import Data.Either (Either(..))
import Node.FS.Aff as FS.Aff
import Node.Library.Execa as Execa
import Node.Path as Path
import Registry.Foreign.Gzip as Gzip
import Registry.Foreign.Tmp as Tmp
import Registry.Test.Assert as Assert
import Test.Spec as Spec

spec :: Spec.Spec Unit
spec = do
  Spec.describe "compress" do
    Spec.it "Compresses a string with gzip" do
      tmp <- Tmp.mkTmpDir
      let
        file = Path.concat [ tmp, "out.gz" ]
        contents = "<contents>"
      buffer <- Gzip.compress contents
      FS.Aff.writeFile file buffer
      result <- _.result =<< Execa.execa "zcat" [ file ] identity
      (_.stdout <$> result) `Assert.shouldEqual` Right contents
