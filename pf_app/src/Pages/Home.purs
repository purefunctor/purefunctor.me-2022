module PF.Pages.Home where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import PF.Component.Utils (css, classes)


type State = Unit


component :: forall query input output m. H.Component HH.HTML query input output m
component =
  H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
  }


initialState :: forall input. input -> Unit
initialState _ = unit


render :: forall action m. State -> H.ComponentHTML action () m
render _ =
  pageRoot
  [ tileGrid
    [ tileBase [ "row-span-2" ] [  ]
      [ tileCover [ "bg-green-500" ] [ ]
        [ HH.div [ css "p-5" ]
          [ HH.text "PureFunctor Cover"
          ]
        ]
      , tileContent [ "bg-blue-500" ] [ ]
        [ HH.div [ css "p-5" ]
          [ HH.text "PureFunctor"
          ]
        ]
      ]
    , tileBase [ ] [ ]
      [ tileCover [ "bg-green-500" ] [ ]
        [ HH.div [ css "p-5" ]
          [ HH.text "Projects Cover"
          ]
        ]
      , tileContent [ "bg-blue-500" ] [ ]
        [ HH.div [ css "p-5" ]
          [ HH.text "Projects"
          ]
        ]
      ]
    , tileBase [ ] [ ]
      [ tileCover [ "bg-green-500" ] [ ]
        [ HH.div [ css "p-5" ]
          [ HH.text "Socials Cover"
          ]
        ]
      , tileContent [ "bg-blue-500" ] [ ]
        [ HH.div [ css "p-5" ]
          [ HH.text "Socials"
          ]
        ]
      ]
    ]
  ]
  where
  -- | Root container for the page
  pageRoot = HH.div classList
    where
    classList = classes $
      [ "flex"
      , "flex-col"
      , "h-screen"
      , "p-5"
      , "bg-gradient-to-br"
      , "from-green-500"
      , "to-blue-900"
      ]

  -- | Parent container for the tiles
  tileGrid = HH.div classList
    where
    classList = classes $
      [ "grid"
      , "grid-cols-2"
      , "grid-rows-2"
      , "flex-1"
      , "gap-5"
      ]

  -- | Base container for each tile
  tileBase classList properties =
    HH.div $ classList' <> properties
    where
    classList' = classes $
      [ "relative"
      , "border-4"
      , "border-gray-900"
      ] <> classList

  -- | Base container for the tile contents
  tileContent classList properties =
    HH.div $ classList' <> properties
    where
    classList' = classes $
      [ "absolute"
      , "box-content"
      , "h-full"
      , "w-full"
      , "z-0"
      ] <> classList

  -- | Base container for the tile cover
  tileCover classList properties =
    HH.div $ classList' <> properties
    where
    classList' = classes $
      [ "absolute"
      , "box-content"
      , "h-full"
      , "w-full"
      , "z-10"
      ] <> classList
