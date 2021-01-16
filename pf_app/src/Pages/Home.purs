module PF.Pages.Home where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import PF.Component.Utils (css)


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
  HH.div [ css "flex h-screen w-screen bg-gray-100" ]
  [ HH.div [ css "bg-gray-200 h-full w-full lg:w-11/12 mx-auto shadow-xl" ]
    [ HH.div [ css "flex flex-col h-full justify-center items-center space-y-5" ]
      [ HH.div [ css "h-56 w-56 bg-green-200 rounded-full shadow-xl" ]
        [
        ]
      , HH.div [ css "text-4xl text-center" ]
        [ HH.text "PureFunctor"
        ]
      , HH.div [ css "text-4xl font-thin text-center" ]
        [ HH.text "Student, Python, FP"
        ]
      ]
    ]
  ]
