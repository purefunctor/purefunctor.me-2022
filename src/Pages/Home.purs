module Website.Pages.Home where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Website.Capability.Navigation (class Navigate)
import Website.Capability.OpenUrl (class OpenUrl)
import Website.Capability.Resources (class ManageRepository)
import Website.Component.Navbar (WithNavbar, _navbar)
import Website.Component.Navbar as Navbar


type State = Unit

type Slots = WithNavbar ()


component
  :: forall query input output m.
     MonadAff m
  => ManageRepository m
  => Navigate m
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
  => Navigate m
  => OpenUrl m
  => State
  -> H.ComponentHTML action Slots m
render _ =
  HH.div [ HP.id "home-page" ]
  [ HH.section [ HP.id "home-base" ]
    [ HH.slot _navbar unit Navbar.component unit absurd
    , HH.section [ HP.id "home-center" ]
      [ HH.img
        [ HP.src "https://avatars.githubusercontent.com/u/66708316?v=4"
        , HP.width 256
        , HP.height 256
        , HP.alt "GitHub Profile Picture"
        ]
      ]
    , HH.div [ HP.id "home-bottom" ]
      [
      ]
    ]
  ]
