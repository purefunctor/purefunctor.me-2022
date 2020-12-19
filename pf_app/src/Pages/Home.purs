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
  HH.div [ css "flex flex-col h-screen p-5 bg-gradient-to-br from-green-500 to-blue-900" ]
  [ HH.div [ css "grid grid-cols-2 grid-rows-2 flex-1 gap-5" ]
    [ HH.div [ css "box-content row-span-2 p-5 text-left text-6xl bg-gray-200 border-4 border-gray-900" ]
      [ HH.text "PureFunctor"
      ]
    , HH.div [ css "box-content text-left p-5 text-6xl bg-gray-200 border-4 border-gray-900" ]
      [ HH.text "Projects"
      ]
    , HH.div [ css "box-content text-left p-5 text-6xl bg-gray-200 border-4 border-gray-900" ]
      [ HH.text "Socials"
      ]
    ]
  ]
