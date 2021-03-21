module Registry.RegistryM where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Reader (class MonadAsk, ReaderT, asks, runReaderT)
import Data.Newtype (class Newtype)
import Effect.Aff (Aff, Error)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import GitHub (IssueNumber)
import GitHub as GitHub

type Env =
  { comment :: String -> Aff Unit
  , commitToTrunk :: Aff Unit
  , uploadPackage :: Aff Unit
  }

mkEnv :: IssueNumber -> Env
mkEnv issue =
  { comment:
      void <<< GitHub.createComment issue
  , commitToTrunk:
      pure unit -- TODO
  , uploadPackage:
      pure unit -- TODO
  }

newtype RegistryM a = RegistryM (ReaderT Env Aff a)

derive instance newtypeRegistryM :: Newtype (RegistryM a) _

derive newtype instance functorRegistryM :: Functor RegistryM
derive newtype instance applyRegistryM :: Apply RegistryM
derive newtype instance applicativeRegistryM :: Applicative RegistryM
derive newtype instance bindRegistryM :: Bind RegistryM
derive newtype instance monadRegistryM :: Monad RegistryM
derive newtype instance monadEffectRegistryM :: MonadEffect RegistryM
derive newtype instance monadAffRegistryM :: MonadAff RegistryM
derive newtype instance monadErrorRegistryM :: MonadThrow Error RegistryM
derive newtype instance monadAskRegistryM :: MonadAsk Env RegistryM

runRegistryM :: forall a. Env -> RegistryM a -> Aff a
runRegistryM env (RegistryM m) = runReaderT m env

-- | Post a comment to the user's issue
comment :: String -> RegistryM Unit
comment body = do
  comment' <- asks _.comment
  liftAff $ comment' body

-- | Post an error to the user's issue and then throw an exception
throwWithComment :: forall a. String -> RegistryM a
throwWithComment body = comment body *> Aff.throwError (Aff.error body)

-- | Commit a change to the default branch of the registry repository
commitToTrunk :: RegistryM Unit
commitToTrunk = liftAff =<< asks _.commitToTrunk

-- | Upload a package to the backend storage provider
uploadPackage :: RegistryM Unit
uploadPackage = liftAff =<< asks _.uploadPackage
