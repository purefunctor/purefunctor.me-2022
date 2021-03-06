module Website.Pages.NotFound where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Website.Component.Utils (css, css')


component ∷
  ∀ query input output m
  . H.Component HH.HTML query input output m
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval $ H.defaultEval
  }


render ∷ ∀ state w a. state → HH.HTML w a
render _ =
  HH.div
  [ css'
    [ "bg-faint h-screen"
    , "flex flex-col"
    , "justify-center items-center"
    ]
  ]
  [ HH.div
    [ css'
      [ "h-auto md:w-1/3 sm:w-2/3 w-5/6"
      , "bg-white flex flex-col overflow-hidden"
      , "ring-2 ring-black shadow-xl rounded-xl"
      ]
    ]
    [ HH.div [ css "h-20 bg-pixel-pattern" ] [ ]
    , HH.div
      [ css "m-5 space-y-5" ]
      [ HH.p
        [ css "text-xl font-bold"
        ]
        [ HH.text "404 - Not Found" ]
      , HH.p_
        [ HH.text "Looks like we can't find that page. Here's a few things to try:" ]
      , HH.ul
        [ css "mx-5 list-disc space-y-5" ]
        [ HH.li_
          [ HH.text "Check if you've typed the URL correctly; note that content may have also been moved or removed."
          ]
        , HH.li_
          [ HH.text "If you think this is a bug, feel free to open an issue in the site's "
          , HH.a
            [ css "underline text-blue-500 hover:text-blue-600 visited:text-purple-600"
            , HP.href "https://github.com/PureFunctor/purefunctor.me"
            ]
            [ HH.text "repository"
            ]
          , HH.text "."
          ]
        ]
      ]
    ]
  ]
