module Website.Pages.Home where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Website.Capability.Navigation (class Navigate, navigate)
import Website.Data.Routes (Route(..))


data Action
  = NavigateTo Route
  | Null


pp :: String
pp = "https://avatars.githubusercontent.com/u/66708316?v=4"


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
      HH.div [ HP.id "home-page" ]
        [ HH.div [ HP.id "home-page-info" ]
            [ HH.img
                [ HP.id "home-page-info-pp"
                , HP.src pp
                , HP.height 128
                , HP.width 128
                , HP.alt "Profile Picture"
                , HP.tabIndex 0
                ]
            , HH.div [ HP.id "home-page-info-bt" ]
                [ routeLink NotFoundR "home-page-info-bt-green"  "About"
                , routeLink NotFoundR "home-page-info-bt-blue"   "Works"
                , routeLink NotFoundR "home-page-info-bt-yellow" "Posts"
                , routeLink NotFoundR "home-page-info-bt-orange" "Contact"
                ]
            ]
        ]

    handleAction = case _ of
      NavigateTo route -> navigate route
      Null -> pure unit

    routeLink route id text =
      HH.a
        [ HP.id id
        , HP.tabIndex 0
        , HE.onClick \_ -> NavigateTo route
        , HE.onKeyUp \e ->
            if KeyboardEvent.key e == "Enter"
              then NavigateTo route
              else Null
        ]
        [ HH.div [ HP.id $ id <> "__deco-top" ] [ ]
        , HH.div [ HP.id $ id <> "__deco-bot" ] [ ]
        , HH.p_
            [ HH.text text
            ]
        ]
