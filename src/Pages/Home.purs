module Website.Pages.Home where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Website.Capability.Navigation (class Navigate, navigate)
import Website.Capability.Resources (class ManageRepository)
import Website.Data.Routes (Route(..))


data Action
  = Navigate Route


type State = Unit


component
  :: forall query input output m.
     MonadAff m
  => ManageRepository m
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
  :: forall slots m.
     MonadAff m
  => ManageRepository m
  => State
  -> H.ComponentHTML Action slots m
render _ =
  HH.div [ HP.id "home-page" ]
  [ HH.section [ HP.id "home-base" ]
    [ HH.nav [ HP.id "home-top" ]
      [ HH.span_
        [ HH.img
          [ HP.src "https://avatars.githubusercontent.com/u/66708316?v=4"
          , HP.width 32
          , HP.height 32
          , HP.alt "GitHub Profile Picture"
          ]
        , navItem "Pure's Website" HomeR
        ]
      , HH.ul_
        [ navItem "About" AboutR
        , navItem "Projects" ContactR
        , navItem "Contact" ProjectsR
        , HH.a [ HP.href "https://blog.purefunctor.me" ]
          [ HH.text "Blog"
          ]
        ]
      ]
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
  where
    navItem title route =
      HH.button
      [ HE.onClick \_ -> Navigate route
      , HP.tabIndex 0
      , HPA.role "link"
      , HPA.label $ "Navigate to " <> title <> " page"
      ]
      [ HH.text title
      ]


handleAction
  :: forall state slots output m
   . MonadAff m
  => Navigate m
  => Action
  -> H.HalogenM state Action slots output m Unit
handleAction = case _ of
  Navigate route -> do
    navigate route
