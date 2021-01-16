module PF.Pages.Home where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
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
  HH.div [ css "bg-gray-100 h-screen overflow-auto" , HP.style "scroll-snap-type: y mandatory;" ]
  [ HH.div [ css "h-auto w-full lg:w-11/12 mx-auto" ]
    [ HH.div [ css "h-screen flex flex-col justify-center items-center space-y-5" , HP.style "scroll-snap-align: start;" ]
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
    , HH.div [ css "h-screen p-5 text-4xl" , HP.style "scroll-snap-align: start;" ]
      [ HH.text "About"
      ]
    , HH.div [ css "h-screen p-5 text-4xl" , HP.style "scroll-snap-align: start;" ]
      [ HH.text "Contact"
      ]
    ]
  ]
