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
  HH.div [ css "bg-gray-100 h-screen overflow-auto scroll-snap-y-mandatory"  ]
  [ HH.div [ css "h-auto w-full lg:w-11/12 mx-auto" ]
    [ HH.div [ css "h-screen flex flex-col justify-center items-center space-y-5 scroll-snap-align-start" ]
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
    , subsection "About" [ css "text-lg p-5" ]
      [ HH.text "Text"
      ]
    , subsection "Contact" [ css "text-lg p-5" ]
      [ HH.text "Text"
      ]
    ]
  ]
  where
    subsection title props content =
      HH.div [ css "h-screen p-5 scroll-snap-align-start divide-y-2" ]
      [ HH.div [ css "text-4xl p-5" ]
        [ HH.text title
        ]
      , HH.div props content
      ]
