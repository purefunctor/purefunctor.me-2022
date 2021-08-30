module Website.AppM where

import Prelude

import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, getStore, runStoreT)
import Routing.Duplex as RD
import Safe.Coerce (coerce)
import Simple.JSON (write)
import Website.Capability.Navigation (class Navigate)
import Website.Data.Routes (routeCodec)
import Website.Store as Store

newtype AppM a = AppM (StoreT Store.Action Store.Store Aff a)

runAppM ∷ ∀ q i o. Store.Store → H.Component q i o AppM → Aff (H.Component q i o Aff)
runAppM store = runStoreT store Store.reduce <<< coerce

derive newtype instance functorAppM ∷ Functor AppM
derive newtype instance applyAppM ∷ Apply AppM
derive newtype instance applicativeAppM ∷ Applicative AppM
derive newtype instance bindAppM ∷ Bind AppM
derive newtype instance monadAppM ∷ Monad AppM
derive newtype instance monadEffectAppM ∷ MonadEffect AppM
derive newtype instance monadAffAppM ∷ MonadAff AppM
derive newtype instance monadStoreAppM ∷ MonadStore Store.Action Store.Store AppM

instance navigateAppM ∷ Navigate AppM where
  navigate route = do
    { pushInterface } ← getStore
    liftEffect $ pushInterface.pushState (write {}) (RD.print routeCodec route)
