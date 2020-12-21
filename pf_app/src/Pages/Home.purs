module PF.Pages.Home where

import Prelude

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
    [ tileContainer Info
      [ HH.div [ css "p-5" ]
        [ HH.text "PureFunctor"
        ]
      ]
      [ HH.div [ css "p-5" ]
        [ HH.text "PureFunctor"
        ]
      ]
    , tileContainer Projects
      [ HH.div [ css "text-6xl p-5" ]
        [ HH.text "Projects"
        ]
      ]
      [ HH.div [ css "p-5" ]
        [ HH.text "Projects"
        ]
      ]
    , tileContainer Socials
      [ HH.div [ css "text-6xl p-5" ]
        [ HH.text "Socials"
        ]
      ]
      [ HH.div [ css "p-5" ]
        [ HH.text "Socials"
        ]
      ]
    ]
  ]
  where
  -- | Root container for the page
  pageRoot = HH.div $ classes
    [ "flex"
    , "flex-col"
    , "h-screen"
    , "p-5"
    , "bg-gradient-to-br"
    , "from-green-500"
    , "to-blue-900"
    ]

  -- | Parent container for the tiles
  tileGrid = HH.div $ classes
    [ "grid"
    , "grid-cols-2"
    , "grid-rows-2"
    , "flex-1"
    , "gap-5"
    ]

  -- | Container for each tile
  tileContainer tile cover content =
    borderContainer
    [ clipperContainer
      [ coverContainer cover
      , contentContainer content
      ]
    ]
    where
    borderContainer = HH.div styles
      where
      styles = classes $
        [ "border-4"
        , "border-gray-900"
        ] <> tileStyles

      tileStyles = case tile of
        Info -> [ "row-span-2" ]
        _    -> [ ]

    clipperContainer = HH.div $ classes
      [ "relative"
      , "h-full"
      , "w-full"
      , "garage-clip"
      ]

    contentContainer = HH.div $ classes
      [ "absolute"
      , "box-content"
      , "h-full"
      , "w-full"
      , "z-0"
      , "bg-white"
      ]

    coverContainer = HH.div $ styles <> events
      where
      styles = classes
        [ "absolute"
        , "box-content"
        , "h-full"
        , "w-full"
        , "z-10"
        , "bg-gray-100"
        , tileAnimation
        ]

      events =
        [ HE.onClick onClickEvent
        , HE.handler (EventType "animationend") onAnimationEndEvent
        ]

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
        (MovingTo cState) -> Just $ SetTo tile (HaltedOn cState)
        _ -> Nothing


handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
handleAction (SetTo tile tState) = do
  state <- H.get
  case tile of
    Info -> H.put $ state { infoTile = tState }
    Projects -> H.put $ state { projectsTile = tState }
    Socials -> H.put $ state { socialsTile = tState }
