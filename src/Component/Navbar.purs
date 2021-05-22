module Website.Component.Navbar where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent as MouseEvent
import Website.Capability.Navigation (class Navigate, navigate)
import Website.Capability.OpenUrl (class OpenUrl, openUrl)
import Website.Component.Utils (css)
import Website.Data.Routes (Route(..))


type State = { burgerToggle :: Boolean }

data Action
  = OpenUrl String
  | Navigate Event Route
  | ToggleBurger


type WithNavbar r =
  ( navbar :: forall query. H.Slot query Void Unit
  | r
  )

_navbar :: Proxy "navbar"
_navbar = Proxy


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
  where
    initialState :: input -> State
    initialState _ = { burgerToggle : false }

    render :: forall w. State -> HH.HTML w Action
    render { burgerToggle } =
      HH.div [ HP.id "container" ] $
      [ HH.nav [ HP.id "navbar" ]
        [ HH.span_
          [ HH.img
            [ HP.src "https://avatars.githubusercontent.com/u/66708316?v=4"
            , HP.width 32
            , HP.height 32
            , HP.alt "GitHub Profile Picture"
            ]
          , navItem "Pure's Website" HomeR
          ]
        , HH.div [ HP.id "desktop-links" ]
          [ navItem "About" AboutR
          , navItem "Projects" ProjectsR
          , navItem "Contact" ContactR
          , HH.a [ HP.href "https://blog.purefunctor.me" ]
            [ HH.text "Blog"
            ]
          ]
        , HH.button
          [ HP.id "mobile-burger"
          , HE.onClick \_ -> ToggleBurger
          ]
          [ HH.i [ css "fas fa-bars" ] [ ]
          ]
        ]
      ] <> case burgerToggle of
        true ->
          [ HH.div [ HP.id "mobile-items" ]
            [ navItem "About" AboutR
            , navItem "Projects" ProjectsR
            , navItem "Contact" ContactR
            ]
          ]
        false -> [ ]
      where
        navItem title route =
          HH.a
          [ HE.onClick $ flip Navigate route <<< MouseEvent.toEvent
          , HP.tabIndex 0
          , HPA.role "link"
          , HPA.label $ "Navigate to " <> title <> " page"
          ]
          [ HH.text title
          ]

    -- render :: forall w. State -> HH.HTML w Action
    -- render _ =
    --   HH.nav [ HP.id "navbar" ]
    --   [ HH.span_
    --     [ HH.img
    --       [ HP.src "https://avatars.githubusercontent.com/u/66708316?v=4"
    --       , HP.width 32
    --       , HP.height 32
    --       , HP.alt "GitHub Profile Picture"
    --       ]
    --     , navItem "Pure's Website" HomeR
    --     ]
    --   , HH.ul_
    --     [ navItem "About" AboutR
    --     , navItem "Projects" ProjectsR
    --     , navItem "Contact" ContactR
    --     , HH.a [ HP.href "https://blog.purefunctor.me" ]
    --       [ HH.text "Blog"
    --       ]
    --     ]
    --   ]
    --   where
    --     navItem title route =
    --       HH.a
    --       [ HE.onClick $ flip Navigate route <<< MouseEvent.toEvent
    --       , HP.tabIndex 0
    --       , HPA.role "link"
    --       , HPA.label $ "Navigate to " <> title <> " page"
    --       ]
    --       [ HH.text title
    --       ]

    handleAction
      :: forall slots
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
        ToggleBurger ->
          H.modify_ \s -> s { burgerToggle = not s.burgerToggle }
