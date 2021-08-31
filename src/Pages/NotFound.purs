module Website.Pages.NotFound where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks

component
  ∷ ∀ q i o m
  . H.Component q i o m
component = Hooks.component \_ _ → do
  Hooks.pure $
    HH.div [ HP.id "null-page" ]
      [ HH.p [ HP.id "null-page__notice" ]
          [ HH.text "🚧 Under Construction 🚧"
          ]
      ]
