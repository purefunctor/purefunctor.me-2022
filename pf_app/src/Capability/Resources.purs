module Website.Capability.Resources where

import Prelude

import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (encodeJson)
import Data.Codec.Argonaut (JsonCodec, printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen (HalogenM, lift)
import Website.API.Endpoint (Endpoint(..))
import Website.API.Request (RequestMethod(..), mkRequest, mkRequest_)
import Website.Data.Resources (BlogPost, Repository, LoginCreds, blogPostCodec, repositoryCodec)


class MonadAff m <= ManageBlogPost m where
  getBlogPosts :: m (Maybe (Array BlogPost))


instance affManageBlogPost :: ManageBlogPost Aff where
  getBlogPosts =
    mkRequest { endpoint: BlogPosts, method: Get } >>=
      decode (CA.array blogPostCodec)


instance manageBlogPostHalogenM
  :: ( MonadAff m
     , ManageBlogPost m
     )
  => ManageBlogPost (HalogenM state action slots output m) where
  getBlogPosts = lift $ getBlogPosts


class MonadAff m <= ManageRepository m where
  getRepositories :: m (Maybe (Array Repository))


instance affManageRepository :: ManageRepository Aff where
  getRepositories =
    mkRequest { endpoint: Repositories, method: Get } >>=
      decode (CA.array repositoryCodec)


instance manageRepositoryHalogenM
  :: ( MonadAff m
     , ManageRepository m
     )
  => ManageRepository (HalogenM state action slots output m) where
  getRepositories = lift $ getRepositories


class ManageLogin m where
  login :: LoginCreds -> m Boolean


instance affManageLogin :: ManageLogin Aff where
  login creds = do
    mResponse <- mkRequest_ Nothing
      { endpoint: Login
      , method: Post $ Just $ encodeJson creds
      }
    pure $ case mResponse of
      Just { status } -> status == StatusCode 204
      Nothing -> false


instance manageLoginHalogenM
  :: ( MonadAff m
     , ManageLogin m
     )
  => ManageLogin (HalogenM state action slots output m) where
  login = lift <<< login



decode :: forall m r. MonadAff m => JsonCodec r -> Maybe Json -> m (Maybe r)
decode _ Nothing = log "Error in obtaining response" *> pure Nothing
decode codec (Just json) =
  case CA.decode codec json of
    Left err -> log (printJsonDecodeError err) *> pure Nothing
    Right result -> pure (Just result)
