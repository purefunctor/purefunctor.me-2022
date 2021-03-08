module Website.Pages.Home.AboutCard where

import Prelude

import Data.Array (intercalate)
import Halogen.HTML as HH
import Website.Component.Utils (css)


element :: forall w i. HH.HTML w i
element =
  HH.div [ css "p-5" ]
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
