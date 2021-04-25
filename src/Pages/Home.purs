module Website.Pages.Home where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Website.Capability.OpenUrl (class OpenUrl)
import Website.Capability.Resources (class ManageRepository)
import Website.Component.Utils (css)


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
  [ css "bg-faint h-screen overflow-auto lg:scroll-snap-y-proximity no-scroll-snap-type"
  ]
  [ HH.section [ css "flex flex-col h-full w-full lg:w-11/12 mx-auto" ]
    [ HH.div [ css "lg:scroll-snap-align-start no-scroll-snap-align" ]
      [ HH.nav [ css "flex justify-between items-center px-8 h-32" ]
        [ HH.span [ css "space-x-6" ]
          [ picture "inline self-center rounded-full" 32 32
          , HH.p [ css "inline" ]
            [ HH.text "Pure's Website"
            ]
          ]
        , HH.ul [ css "space-x-6" ]
          [ navLink "About"
          , navLink "Projects"
          , navLink "Contact"
          , navLink "Blog"
          ]
        ]
      ]

    , HH.section [ css "flex-grow flex mx-auto items-center" ]
      [ picture "h-56 w-56 rounded-full shadow-xl ring-2 ring-black" 256 256
      ]

    , HH.div [ css "h-32" ]
      [ -- Flexbox centering hack.
      ]
    ]
  ]
  where
    picture c w h =
      HH.img
      [ css c
      , HP.src "https://avatars.githubusercontent.com/u/66708316?v=4"
      , HP.width w
      , HP.height h
      , HP.alt "GitHub Profile Picture"
      ]

    navLink title =
      HH.li [ css "inline text-underline cursor-pointer" ]
      [ HH.text title
      ]
