module CommentTest where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import GitHub (IssueNumber(..))
import Registry.API (comment, mkEnv, runApiM)

main :: Effect Unit
main = launchAff_ testComment

testComment :: Aff Unit
testComment = runApiM (mkEnv (IssueNumber 149)) do
  comment "This is issue #149"
