module Website.Capability.OpenUrl where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Halogen (HalogenM, lift)


class MonadAff m <= OpenUrl m where
  openUrl :: String -> m Unit


instance openURLHalogenM
  :: OpenUrl m
  => OpenUrl (HalogenM state action slots output m) where
  openUrl = lift <<< openUrl
