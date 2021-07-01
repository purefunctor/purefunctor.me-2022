module Website.Pages.NotFound where

import Prelude

import Halogen as H
import Halogen.HTML as HH


component ∷
  ∀ query input output m
  . H.Component query input output m
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval $ H.defaultEval
  }


render ∷ ∀ state w a. state → HH.HTML w a
render _ = HH.div [ ] [ HH.text "🚧 Under Construction 🚧" ]
