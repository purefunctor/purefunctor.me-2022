module PF.Capability.Resources where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Decode (decodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen (HalogenM, lift)
import PF.Data.Resources (BlogPost, Repository)


class MonadAff m <= ManageBlogPost m where
  getBlogPosts :: m (Maybe (Array BlogPost))


instance affManageBlogPost :: ManageBlogPost Aff where
  getBlogPosts = do
    response <- AX.get ResponseFormat.json "/blog"
    case response of
      Left err -> do
        log $ AX.printError err
        pure Nothing
      Right res -> do
        case decodeJson res.body of
          Left err -> do
            log $ show err
            pure Nothing
          Right pst -> pure pst


instance manageBlogPostHalogenM
  :: ( MonadAff m
     , ManageBlogPost m
     )
  => ManageBlogPost (HalogenM state action slots output m) where
  getBlogPosts = lift $ getBlogPosts


class MonadAff m <= ManageRepository m where
  getRepositories :: m (Maybe (Array Repository))


instance affManageRepository :: ManageRepository Aff where
  getRepositories = do
    response <- AX.get ResponseFormat.json "/repo"
    case response of
      Left err -> do
        log $ AX.printError err
        pure Nothing
      Right res -> do
        case decodeJson res.body of
          Left err -> do
            log $ show err
            pure Nothing
          Right pst -> pure pst


instance manageRepositoryHalogenM
  :: ( MonadAff m
     , ManageRepository m
     )
  => ManageRepository (HalogenM state action slots output m) where
  getRepositories = lift $ getRepositories


class ManageLogin m where
  login :: forall r. m { | r }
