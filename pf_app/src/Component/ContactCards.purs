module Website.Component.ContactCards where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Website.Component.Utils (css, css')


element :: forall w i. HH.HTML w i
element =
  HH.div [ css "flex flex-1 md:flex-row flex-col" ]
  [ makeCard "bg-pink-100 text-pink-500" "mailto:justin@purefunctor.me"
    [ HH.i [ css "fas fa-envelope fa-4x" ] [ ]
    ]
  , makeCard "bg-blue-100 text-blue-500" "https://twitter.com/PureFunctor"
    [ HH.i [ css "fab fa-twitter fa-4x" ] [ ]
    ]
  , makeCard "bg-purple-100 text-gray-900" "https://pythondiscord.org"
    [ HH.i [ css "fab fa-discord fa-4x" ] [ ]
    ]
  ]
  where
    makeCard :: String -> String -> Array (HH.HTML w i) -> HH.HTML w i
    makeCard extra link child =
      HH.a
      [ css'
        [ extra
        , "flex-grow flex"
        , "p-5 m-5 rounded-lg shadow-md"
        , "items-center justify-center"
        ]
      , HP.href link
      ]
      child
