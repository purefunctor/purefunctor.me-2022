module Main where

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (parse)
import Routing.PushState (makeInterface)
import Web.HTML (window)
import Web.HTML.Location as Location
import Web.HTML.Window as Window
import Website.AppM (runAppM)
import Website.Component.Router as Router
import Website.Data.Routes (Route(..), routeCodec)


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody

  pushInterface <- liftEffect makeInterface

  let
    rootComponent ∷ H.Component HH.HTML Router.Query Router.Input Void Aff
    rootComponent = H.hoist (runAppM { pushInterface }) Router.component

  route_ <- liftEffect $
    window >>= Window.location >>= Location.pathname

  let
    route = fromMaybe NotFoundR (hush $ parse routeCodec route_)

  halogenIO <- runUI rootComponent route body

  void $ liftEffect $ pushInterface.listen \location -> do
    let
      mNew = hush $ parse routeCodec $ location.pathname
    case mNew of
      Just new -> do
        launchAff_ $ halogenIO.query $ H.tell $ Router.Navigate new
      Nothing ->
        pure unit
