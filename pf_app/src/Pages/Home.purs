module PF.Pages.Home where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import PF.Component.Utils (classes)


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
    [ sTile [ "row-span-2" ]
      [ HH.text "PureFunctor"
      ]
    , dTile
      [ HH.text "Projects"
      ]
    , dTile
      [ HH.text "Socials"
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

  -- | Tile factory function
  tile_ classList properties =
    HH.div $ classList' <> properties
    where
    classList' = classes $
      [ "box-content"
      , "text-left"
      , "p-5"
      , "text-6xl"
      , "bg-gray-200"
      , "border-4"
      , "border-gray-900"
      ] <> classList

  -- | Tile with extra styles
  sTile = flip tile_ []

  -- | Tile with extra properties
  pTile = tile_ []

  -- | Tile with no extras
  dTile = tile_ [] []
