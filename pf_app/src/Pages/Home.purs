module PF.Pages.Home where

import Prelude

import Data.Array (singleton)
import Halogen as H
import Halogen.HTML as HH
import PF.Component.Utils (css, classes)


data Tile = Info | Projects | Socials

type State =
  { infoOpened :: Boolean
  , projectsOpened :: Boolean
  , socialsOpened :: Boolean
  }

data Action = Toggle Tile


component :: forall query input output m. H.Component HH.HTML query input output m
component =
  H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    }
  }


initialState :: forall input. input -> State
initialState _ =
  { infoOpened: false
  , projectsOpened: false
  , socialsOpened: false
  }


render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  pageRoot
  [ tileGrid
    [ tallTileBase
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
    , shortTileBase
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
    , shortTileBase
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
  tileBase extras =
    HH.div baseStyle <<< singleton <<< HH.div containerStyle
    where
      baseStyle = classes $
        [ "border-4"
        , "border-gray-900"
        ] <> extras
      containerStyle = classes
        [ "relative"
        , "h-full"
        , "w-full"
        ]

  tallTileBase = tileBase [ "row-span-2" ]
  shortTileBase = tileBase [ ]

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


handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
handleAction (Toggle tile) = do
  state <- H.get
  case tile of
    Info -> H.put $ state { infoOpened = not state.infoOpened }
    Projects -> H.put $ state { projectsOpened = not state.projectsOpened }
    Socials -> H.put $ state { socialsOpened = not state.socialsOpened }
