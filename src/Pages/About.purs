module Website.Pages.About where

import Prelude

import Data.String (Pattern(..), split)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.KeyboardEvent as Keyboardevent
import Website.Capability.Navigation (class Navigate, navigate)
import Website.Data.Routes (Route(..))


aboutText :: forall w a. Array (HH.HTML w a)
aboutText = splitParagraphs """
I'm a Filipino open-source developer and self-taught functional
programmer who's passionate about learning new ideas and applying
theory into practice.

At the moment, I'm working on multiple open-source projects in
different programming languages such as PureScript, Haskell, Erlang/OTP,
Elixir, and Python; these projects are also listed on the Works page
of this website.
"""
  where
    splitParagraphs = split (Pattern "\n\n") >>> map \text ->
      HH.p [ HP.id "about-page-body-paragraph" ] [ HH.text text ]


data Action
  = NavigateTo Route
  | Null


component ∷
  ∀ query input output m
  .  Navigate m
  => H.Component query input output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    }
  }
  where
    initialState = \_ → unit

    render _ =
      HH.div [ HP.id "about-page" ]
        [ HH.article [ HP.id "about-page-body" ] $
            [ HH.span [ HP.id "about-page-body-title" ]
                [ HH.h1
                    [ HP.id "about-page-body-title__page_name"
                    ]
                    [ HH.text "About"
                    ]
                , HH.a
                    [ HP.id "about-page-body-title__go_back"
                    , HP.tabIndex 0
                    , HE.onClick \_ -> NavigateTo HomeR
                    , HE.onKeyUp \e ->
                        if Keyboardevent.key e == "Enter"
                        then NavigateTo HomeR
                        else Null
                    ]
                    [ HH.text "Go Back"
                    ]
                ]
            , HH.hr_
            ] <> aboutText
        ]

    handleAction = case _ of
      NavigateTo route -> navigate route
      Null -> pure unit
