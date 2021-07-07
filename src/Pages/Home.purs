module Website.Pages.Home where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP


component ∷
  ∀ query input output m
  . H.Component query input output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
  }
  where
    initialState = \_ → unit

    render ∷ ∀ state w a. state → HH.HTML w a
    render _ =
      HH.div [ HP.id "home-page" ]
        [ HH.p [ HP.id "home-page__notice" ]
            [ HH.text "🚧 Under Construction 🚧"
            ]
        ]
