module Website.Pages.Home where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Website.Capability.OpenUrl (class OpenUrl)
import Website.Capability.Resources (class ManageRepository)
import Website.Component.Utils (css, css')


type State = Unit


component
  :: forall query input output m.
     MonadAff m
  => ManageRepository m
  => OpenUrl m
  => H.Component query input output m
component =
  H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
  }


initialState :: forall input. input -> State
initialState _ = unit


render
  :: forall action slots m.
     MonadAff m
  => ManageRepository m
  => OpenUrl m
  => State
  -> H.ComponentHTML action slots m
render _ =
  HH.div
  [ css'
    [ "bg-faint h-screen overflow-auto"
    , "lg:scroll-snap-y-proximity no-scroll-snap-type"
    ]
  ]
  [ HH.div [ css "h-auto w-full lg:w-11/12 mx-auto" ]
    [ HH.section
      [ css'
        [ "h-screen flex flex-col"
        , "justify-center items-center space-y-5"
        , "lg:scroll-snap-align-start no-scroll-snap-align"
        ]
      ]
      [ HH.img
        [ css "h-56 w-56 rounded-full shadow-xl ring-2 ring-black"
        , HP.src "https://avatars.githubusercontent.com/u/66708316?v=4"
        , HP.width 256
        , HP.height 256
        , HP.alt "GitHub Profile Picture"
        ]
      , HH.div [ css "text-4xl font-extralight text-center" ]
        [ HH.text "PureFunctor"
        ]
      , HH.div [ css "text-4xl font-thin text-center" ]
        [ HH.text "Student, Python, FP"
        ]
      ]
    ]
  ]
