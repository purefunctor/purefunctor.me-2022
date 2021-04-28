module Website.Pages.About where

import Prelude

import Data.Array (intercalate)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Website.Capability.Navigation (class Navigate)
import Website.Capability.OpenUrl (class OpenUrl)
import Website.Component.Navbar (WithNavbar, _navbar)
import Website.Component.Navbar as Navbar


type State = Unit

type Slots = WithNavbar ()


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
  }


initialState :: forall input. input -> State
initialState _ = unit


render
  :: forall action m.
     MonadAff m
  => Navigate m
  => OpenUrl m
  => State
  -> H.ComponentHTML action Slots m
render _ =
  HH.div [ HP.id "about-page" ]
  [ HH.section [ HP.id "about-base" ]
    [ HH.slot _navbar unit Navbar.component unit absurd
    , HH.article_
      [ HH.p_
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
