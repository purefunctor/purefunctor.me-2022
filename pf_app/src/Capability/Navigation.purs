module Website.Capability.Navigation where

import Prelude

import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen (HalogenM, lift)
import Routing.Duplex as RD
import Routing.Hash as RH
import Website.Data.Routes (Routes, routeCodec)


class MonadAff m <= Navigate m where
  navigate :: Routes -> m Unit


instance navigateAff :: Navigate Aff where
  navigate = liftEffect <<< RH.setHash <<< RD.print routeCodec


instance navigateHalogenM
  :: Navigate m
  => Navigate (HalogenM state action slots output m) where
  navigate = lift <<< navigate
