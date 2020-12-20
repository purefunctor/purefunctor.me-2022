module PF.Pages.Home where

import Prelude

import Data.Array (singleton)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import PF.Component.Utils (css, classes)
import Web.Event.Event (EventType(..))


data Tile
  = Info
  | Projects
  | Socials

data CoverState
  = Open
  | Shut

data TileState
  = Initial
  | HaltedOn CoverState
  | MovingTo CoverState

type State =
  { infoTile :: TileState
  , projectsTile :: TileState
  , socialsTile :: TileState
  }

data Action = SetTo Tile TileState


fromTileState :: Tile -> State -> TileState
fromTileState t s = case t of
  Info -> s.infoTile
  Projects -> s.projectsTile
  Socials -> s.socialsTile


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
  { infoTile: Initial
  , projectsTile: Initial
  , socialsTile: Initial
  }


render :: forall m. State -> H.ComponentHTML Action () m
render state =
  pageRoot
  [ tileGrid
    [ tallTileBase
      [ tileCover Info
        [ HH.div [ css "p-5" ]
          [ HH.text "PureFunctor Cover"
          ]
        ]
      , tileContent Info
        [ HH.div [ css "p-5" ]
          [ HH.text "PureFunctor"
          ]
        ]
      ]
    , shortTileBase
      [ tileCover Projects
        [ HH.div [ css "p-5" ]
          [ HH.text "Projects Cover"
          ]
        ]
      , tileContent Projects
        [ HH.div [ css "p-5" ]
          [ HH.text "Projects"
          ]
        ]
      ]
    , shortTileBase
      [ tileCover Socials
        [ HH.div [ css "p-5" ]
          [ HH.text "Socials Cover"
          ]
        ]
      , tileContent Socials
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
        , "garage-clip"
        ]

  tallTileBase = tileBase [ "row-span-2" ]
  shortTileBase = tileBase [ ]

  -- | Base container for the tile contents
  tileContent_ classList properties =
    HH.div $ classList' <> properties
    where
    classList' = classes $
      [ "absolute"
      , "box-content"
      , "h-full"
      , "w-full"
      , "z-0"
      ] <> classList

  tileContent _ =
    tileContent_ [ "bg-blue-500" ] [  ]

  -- | Base container for the tile cover
  tileCover_ classList properties =
    HH.div $ classList' <> properties
    where
    classList' = classes $
      [ "absolute"
      , "box-content"
      , "h-full"
      , "w-full"
      , "z-10"
      ] <> classList

  tileCover tile =
    tileCover_
      [ "bg-green-500", tileAnimation ]
      [ HE.onClick onClickEvent
      , HE.handler (EventType "animationend") onAnimationEndEvent
      ]
    where
      tState = fromTileState tile state
      tileAnimation = case tState of
        (MovingTo Open) -> "close-to-open"
        (MovingTo Shut) -> "open-to-close"
        (HaltedOn Open) -> "close-to-open"
        (HaltedOn Shut) -> "open-to-close"
        _ -> ""
      onClickEvent _ = case tState of
        Initial -> Just $ SetTo tile (MovingTo Open)
        (HaltedOn Open) -> Just $ SetTo tile (MovingTo Shut)
        (HaltedOn Shut) -> Just $ SetTo tile (MovingTo Open)
        _ -> Nothing
      onAnimationEndEvent _ = case tState of
        (MovingTo cover) -> Just $ SetTo tile (HaltedOn cover)
        _ -> Nothing


handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
handleAction (SetTo tile tState) = do
  state <- H.get
  case tile of
    Info -> H.put $ state { infoTile = tState }
    Projects -> H.put $ state { projectsTile = tState }
    Socials -> H.put $ state { socialsTile = tState }
