module Website.Pages.Contact where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Website.Capability.Navigation (class Navigate)
import Website.Capability.OpenUrl (class OpenUrl, openUrl)
import Website.Component.Navbar (WithNavbar, _navbar)
import Website.Component.Navbar as Navbar
import Website.Component.Utils (css)


type State = Unit

type Slots = WithNavbar ()

data Action = OpenLink String


component
 :: forall query input output m
  . MonadAff m
 => Navigate m
 => OpenUrl m
 => H.Component query input output m
component =
  H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    }
  }


initialState :: forall input. input -> State
initialState _ = unit


render
  :: forall m
   . MonadAff m
  => Navigate m
  => OpenUrl m
  => State
  -> H.ComponentHTML Action Slots m
render _ =
  HH.div [ HP.id "contact-page" ]
  [ HH.section [ HP.id "contact-base" ]
    [ HH.slot _navbar unit Navbar.component unit absurd
    , HH.div [ HP.id "contact-center" ]
      [ HH.ul [ HP.id "contact-list" ]
        [ contactItem
            "fas fa-at fa-4x" "email-card" "mailto:justin@purefunctor.me"
        , contactItem
            "fab fa-twitter fa-4x" "twitter-card" "https://twitter.com/PureFunctor"
        , contactItem
            "fab fa-discord fa-4x" "discord-card" "https://pythondiscord.org"
        ]
      ]
    , HH.div [ HP.id "contact-bottom" ]
      [
      ]
    ]
  ]
  where
    contactItem fa id to =
      HH.a [ HP.id id, HP.href to ]
      [ HH.i [ css fa ] [ ]
      ]


handleAction
  :: forall output m
   . MonadEffect m
  => OpenUrl m
  => Action
  -> H.HalogenM State Action Slots output m Unit
handleAction = case _ of
  OpenLink url -> openUrl url
