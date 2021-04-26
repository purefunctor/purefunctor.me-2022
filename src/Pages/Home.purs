module Website.Pages.Home where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Website.Capability.Navigation (class Navigate, navigate)
import Website.Capability.Resources (class ManageRepository)
import Website.Component.HTML.Navbar (navbar)
import Website.Component.HTML.ProfilePicture (profilePicture)
import Website.Component.Utils (css)
import Website.Data.Routes (Route)


data Action
  = Navigate Route


type State = Unit


component
  :: forall query input output m.
     MonadAff m
  => ManageRepository m
  => Navigate m
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
  => State
  -> H.ComponentHTML Action slots m
render _ =
  HH.div
  [ css "bg-faint h-screen overflow-auto lg:scroll-snap-y-proximity no-scroll-snap-type"
  ]
  [ HH.section [ css "flex flex-col h-full w-full lg:w-11/12 mx-auto" ]
    [ navbar Navigate

    , HH.section [ css "flex-grow flex mx-auto items-center" ]
      [ profilePicture "h-56 w-56 rounded-full shadow-xl ring-2 ring-black" 256 256
      ]

    , HH.div [ css "h-32" ]
      [ -- Flexbox centering hack.
      ]
    ]
  ]


handleAction
  :: forall state slots output m
   . MonadEffect m
  => Navigate m
  => Action
  -> H.HalogenM state Action slots output m Unit
handleAction = case _ of
  Navigate route -> do
    navigate route
