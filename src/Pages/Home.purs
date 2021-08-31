module Website.Pages.Home where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Website.Capability.Navigation (class Navigate, navigate)
import Website.Data.Routes (Route(..))

component ∷ ∀ q i o m. Navigate m ⇒ H.Component q i o m
component = Hooks.component \_ _ → do
  Hooks.pure $
    HH.div [ HP.id "home-page" ]
      [ HH.div [ HP.id "home-page-info" ]
          [ HH.img
              [ HP.id "home-page-info-pp"
              , HP.src "logo.svg"
              , HP.height 128
              , HP.width 128
              , HP.alt "Profile Picture"
              , HP.tabIndex 0
              ]
          , HH.div [ HP.id "home-page-info-bt" ]
              [ routeLink AboutR "home-page-info-bt-green" "About"
              , routeLink NotFoundR "home-page-info-bt-blue" "Works"
              , routeLink NotFoundR "home-page-info-bt-yellow" "Posts"
              , routeLink NotFoundR "home-page-info-bt-orange" "Contact"
              ]
          ]
      ]
  where
  routeLink route id text =
    HH.a
      [ HP.id id
      , HP.tabIndex 0
      , HE.onClick \_ → navigate route
      , HE.onKeyUp \e →
          if KeyboardEvent.key e == "Enter" then navigate route
          else pure unit
      ]
      [ HH.div [ HP.id $ id <> "__deco-top" ] []
      , HH.div [ HP.id $ id <> "__deco-bot" ] []
      , HH.p_
          [ HH.text text
          ]
      ]
