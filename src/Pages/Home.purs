module Website.Pages.Home where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Website.Capability.OpenUrl (class OpenUrl)
import Website.Capability.Resources (class ManageRepository)
import Website.Component.Utils (css, css')
import Website.Pages.Home.AboutCard as AboutCard
import Website.Pages.Home.ContactCards as ContactCards
import Website.Pages.Home.ProjectCards as ProjectCards


type State = Unit
type ChildSlots =
  ( projects :: ProjectCards.Slot
  , contacts :: ContactCards.Slot
  )


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
  :: forall action m.
     MonadAff m
  => ManageRepository m
  => OpenUrl m
  => State
  -> H.ComponentHTML action ChildSlots m
render _ =
  HH.div
  [ css "bg-faint h-screen overflow-auto lg:scroll-snap-y-proximity no-scroll-snap-type"
  ]
  [ HH.div [ css "h-auto w-full lg:w-11/12 mx-auto" ]
    [ HH.section [ css "h-screen flex flex-col" ]
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

    , subsection "min-h-screen" "About"
      [ AboutCard.element ]

    , subsection "min-h-screen" "Projects"
      [ ProjectCards.make ( Proxy :: Proxy "projects" )
      ]

    , subsection "min-h-screen" "Contact"
      [ ContactCards.make ( Proxy :: Proxy "contacts" )
      ]
    ]
  ]
  where
    subsection extra title child =
      HH.section
      [ css'
        [ extra
        , "flex flex-col"
        , "lg:scroll-snap-align-start no-scroll-snap-align"
        , "divide-y divide-faint-200"
        ]
      ] $
      [ HH.header [ css "font-extralight text-4xl p-5 mx-auto" ]
        [ HH.h1_ [ HH.text title ]
        ]
      ] <> child

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
