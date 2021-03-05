module Website.Capability.Navigation where

import Prelude

import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen (HalogenM, lift)
import Routing.Duplex as RD
import Routing.Hash as RH
import Website.Data.Routes (Route, routeCodec)


class MonadAff m <= Navigate m where
  navigate :: Route -> m Unit


instance navigateHalogenM
  :: Navigate m
  => Navigate (HalogenM state action slots output m) where
  navigate = lift <<< navigate
