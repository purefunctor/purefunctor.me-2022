module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import PF.Component.Router as Router
import PF.Data.Routes as Routes
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  halogenIO <- runUI Router.component unit body
  void $ liftEffect $ matchesWith (parse Routes.routeCodec) \old new ->
    when (old /= Just new) do
      launchAff_ $ halogenIO.query $ H.tell $ Router.Navigate new
