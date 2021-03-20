module Website.Pages.Home.ContactCards where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Website.Component.Utils (css, css')


element :: forall w i. HH.HTML w i
element =
  HH.div
  [ css'
    [ "flex flex-1 md:flex-row flex-col"
    , "md:space-x-5 md:space-y-0 space-y-5 p-5 justify-evenly"
    ]
  ]
  [ makeCard "bg-pink-100 text-pink-500" "mailto:justin@purefunctor.me"
    [ HH.i [ css "fas fa-mail-bulk fa-4x" ] [ ]
    ]
  , makeCard "bg-blue-100 text-blue-500" "https://twitter.com/PureFunctor"
    [ HH.i [ css "fab fa-twitter fa-4x" ] [ ]
    ]
  , makeCard "bg-green-100 text-gray-900" "https://pythondiscord.org"
    [ HH.i [ css "fab fa-discord fa-4x" ] [ ]
    ]
  ]
  where
    makeCard :: String -> String -> Array (HH.HTML w i) -> HH.HTML w i
    makeCard extra link child =
      HH.a
      [ css'
        [ extra
        , "flex flex-col h-64 w-full overflow-hidden"
        , "ring-2 ring-black shadow-xl rounded-xl"
        ]
      , HP.href link
      ]
      [ HH.div [ css "ring-2 ring-black h-20 bg-pixel-pattern" ] [ ]
      , HH.div [ css "flex flex-grow items-center justify-center" ] child
      ]
