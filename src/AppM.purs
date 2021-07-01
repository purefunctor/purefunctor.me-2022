module Website.AppM where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Control.Monad.Reader.Trans (ReaderT, asks, runReaderT)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Routing.Duplex as RD
import Simple.JSON (write)
import Type.Equality (class TypeEquals, from)
import Website.Capability.Navigation (class Navigate)
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
