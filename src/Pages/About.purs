module Website.Pages.About where

import Prelude

import Data.Array (intercalate)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Website.Capability.Navigation (class Navigate, navigate)
import Website.Capability.OpenUrl (class OpenUrl, openUrl)
import Website.Component.HTML.Navbar (navbar)
import Website.Component.Utils (css)
import Website.Data.Routes (Route)


type State = Unit

data Action
  = OpenLink String
  | Navigate Route


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


render :: forall w. State -> HH.HTML w Action
render _ =
  HH.div [ css "bg-faint h-screen overflow-auto" ]
  [ HH.section [ css "flex flex-col h-full w-full lg:w-11/12 mx-auto" ]
    [ navbar Navigate
    , HH.article [ css "p-5" ]
      [ HH.p [ css "lg:text-lg text-md font-light text-left lg:w-5/6 lg:mx-auto" ]
        [ HH.text "Greetings, I'm Justin [or Pure]"
        , HH.br_
        , HH.br_
        , HH.text $ intercalate " "
          [ "I'm a 17-year-old student from the Philippines who's very"
          , "passionate about computer science especially functional"
          , "programming. I'm actively learning languages such as Haskell"
          , "and Python, as well as various technologies like Nix."
          ]
        , HH.br_
        , HH.br_
        , HH.text $ intercalate " "
          [ "I'm also a staff member on Python Discord, a community with"
          , "over 150,000 members focused on the Python programming language"
          , "helping new and experienced developers alike."
          ]
        , HH.br_
        , HH.br_
        , HH.text $ intercalate " "
          [ "I use Emacs as my primary development environment; I also like"
          , "learning about category theory, although most of my intuition"
          , "of it comes from practical applications e.g. in Haskell."
          ]
        , HH.br_
        , HH.br_
        , HH.text $ intercalate " "
          [ "Free and open-source software is great! Most of my development"
          , "and general-purpose work is done on a GNU/Linux machine."
          ]
        ]
      ]
    ]
  ]


handleAction
  :: forall state slots output m
   . MonadAff m
  => Navigate m
  => OpenUrl m
  => Action
  -> H.HalogenM state Action slots output m Unit
handleAction = case _ of
  OpenLink url -> openUrl url
  Navigate route -> navigate route
