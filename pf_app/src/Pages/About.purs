module Website.Pages.About where

import Prelude

import Halogen as H
import Halogen.HTML as HH


type State = Unit


component :: forall query input output m. H.Component HH.HTML query input output m
component =
  H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
  }


initialState :: forall input. input -> Unit
initialState _ = unit


render :: forall action m. State -> H.ComponentHTML action () m
render _ =
  HH.div_
  [ HH.h1_
    [ HH.text "About Page"
    ]
  ]
