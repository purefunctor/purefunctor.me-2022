module Website.Pages.Home where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent as MouseEvent
import Website.Capability.Navigation (class Navigate, navigate)
import Website.Capability.OpenUrl (class OpenUrl, openUrl)
import Website.Capability.Resources (class ManageRepository)
import Website.Data.Routes (Route(..))


type State = Unit

data Action
  = OpenUrl String
  | Navigate Event Route


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
    { handleAction = handleAction
    }
  }


initialState :: forall input. input -> State
initialState _ = unit


render
  :: forall slots m.
     MonadAff m
  => ManageRepository m
  => Navigate m
  => OpenUrl m
  => State
  -> H.ComponentHTML Action slots m
render _ =
  HH.div [ HP.id "home-page" ]
  [ HH.div [ HP.id "home-pair" ]
    [ HH.img
      [ HP.id "home-pair-image"
      , HP.src "https://avatars.githubusercontent.com/u/66708316?v=4"
      , HP.alt "GitHub Profile Picture"
      ]
    , HH.div [ HP.id "home-pair-boxes" ]
      [ item "home-pair-boxes__item-tl" AboutR "About"
      , item "home-pair-boxes__item-tr" ProjectsR "Projects"
      , item "home-pair-boxes__item-bl" ContactR "Links"
      , HH.a
        [ HP.id "home-pair-boxes__item-br"
        , HP.href "https://blog.purefunctor.me"
        ]
        [ HH.text "Blog"
        ]
      ]
    ]
  ]
  where
    item id route title =
      HH.a
      [ HE.onClick $ flip Navigate route <<< MouseEvent.toEvent
      , HP.id id
      , HP.tabIndex 0
      , HPA.role "link"
      , HPA.label $ "Navigate to " <> title <> " page."
      ]
      [ HH.text title
      ]

handleAction
  :: forall slots output m
   . MonadAff m
  => Navigate m
  => OpenUrl m
  => Action
  -> H.HalogenM State Action slots output m Unit
handleAction =
  case _ of
    Navigate event route -> do
      H.liftEffect $ Event.preventDefault event
      navigate route
    OpenUrl url -> openUrl url
