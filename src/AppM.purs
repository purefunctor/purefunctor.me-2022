module Website.AppM where

import Prelude

import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Reader (class MonadAsk)
import Control.Monad.Reader.Trans (ReaderT, asks, runReaderT)
import Data.Argonaut.Encode (encodeJson)
import Data.Codec.Argonaut as CA
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Routing.Duplex as RD
import Simple.JSON (write)
import Type.Equality (class TypeEquals, from)
import Web.HTML (window)
import Web.HTML.Window as Window
import Website.API.Endpoint (Endpoint(..))
import Website.API.Request (RequestMethod(..), mkRequest, mkRequest_)
import Website.Capability.Navigation (class Navigate)
import Website.Capability.OpenUrl (class OpenUrl)
import Website.Capability.Resources (class ManageBlogPost, class ManageLogin, class ManageRepository, decode)
import Website.Data.Resources (blogPostCodec, repositoryCodec)
import Website.Data.Routes (routeCodec)
import Website.Env (Env)


newtype AppM a = AppM (ReaderT Env Aff a)


runAppM ∷ Env → AppM ~> Aff
runAppM env (AppM m) = runReaderT m env


derive newtype instance functorAppM ∷ Functor AppM
derive newtype instance applyAppM ∷ Apply AppM
derive newtype instance applicativeAppM ∷ Applicative AppM
derive newtype instance bindAppM ∷ Bind AppM
derive newtype instance monadAppM ∷ Monad AppM
derive newtype instance monadEffectAppM ∷ MonadEffect AppM
derive newtype instance monadAffAppM ∷ MonadAff AppM


instance monadAskAppM ∷ TypeEquals e Env ⇒ MonadAsk e AppM where
  ask = AppM $ asks from


instance navigateAppM ∷ Navigate AppM where
  navigate route = do
    pushInterface <- asks _.pushInterface
    liftEffect $ pushInterface.pushState (write { }) (RD.print routeCodec route)


instance openUrlAppM :: OpenUrl AppM where
  openUrl url = liftEffect do
    void $ window >>= Window.open url "" ""


instance manageBlogPostAppM ∷ ManageBlogPost AppM where
  getBlogPosts = AppM $
    mkRequest { endpoint: BlogPosts, method: Get } >>=
      decode (CA.array blogPostCodec)


instance manageRepositoryAppM ∷ ManageRepository AppM where
  getRepositories = AppM $
    mkRequest { endpoint: Repositories, method: Get } >>=
      decode (CA.array repositoryCodec)


instance manageLoginAppM ∷ ManageLogin AppM where
  login creds = AppM $ do
    mResponse <- mkRequest_ Nothing
      { endpoint: Login
      , method: Post $ Just $ encodeJson creds
      }
    pure $ case mResponse of
      Just { status } -> status == StatusCode 204
      Nothing -> false
