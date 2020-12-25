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
  | Expected CoverState

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
  HH.div [ css "page-root" ]
  [ HH.div [ css "tile-grid" ]
    [ tileContainer Info
    , tileContainer Projects
    , tileContainer Socials
    ]
  ]
  where
  -- | Container for each tile
  tileContainer tile =
    HH.div borderContainer
    [ HH.div clipperContainer
      [ HH.div coverContainer tileCover
      , HH.div contentContainer tileContent
      ]
    ]
    where
    tState = fromTileState tile state

    borderContainer = case tile of
      Info -> [ css "border-container-info" ]
      _    -> [ css "border-container" ]

    clipperContainer = [ css "clipper-container garage-clip" ]
    contentContainer = [ css "content-container" ]

    coverContainer = styles <> events
      where
      styles = classes $ [ "cover-container" ] <> animation

      events =
        [ HE.onClick onClickEvent
        , HE.handler (EventType "animationend") onAnimationEndEvent
        ]

      animation = case tState of
        (MovingTo Open) -> [ "close-to-open" ]
        (MovingTo Shut) -> [ "open-to-close" ]
        (HaltedOn Open) -> [ "open" ]
        (HaltedOn Shut) -> [ "shut" ]
        _ -> [ ]

      onClickEvent _ = case tState of
        Initial -> Just $ SetTo tile (MovingTo Open)
        (Expected Shut) -> Just $ SetTo tile (MovingTo Shut)
        (Expected Open) -> Just $ SetTo tile (MovingTo Open)
        _ -> Nothing

      onAnimationEndEvent _ = case tState of
        (MovingTo cState) -> Just $ SetTo tile (HaltedOn cState)
        _ -> Nothing

    tileCover =
      [ HH.div fullFlex
        [ HH.div coverFlex coverItems
        , HH.i chevron [ ]
        ]
      ]
      where
      fullFlex = [ css "full-flex" ] <> events
        where
        events =
          [ HE.onMouseOver onMouseOverEvent
          , HE.onMouseOut onMouseOutEvent
          ]

        onMouseOverEvent _ = case tState of
          Initial -> Just $ SetTo tile (Expected Open)
          (HaltedOn Open) -> Just $ SetTo tile (Expected Shut)
          (HaltedOn Shut) -> Just $ SetTo tile (Expected Open)
          _ -> Nothing

        onMouseOutEvent _ = case tState of
          (Expected Open) -> Just $ SetTo tile (HaltedOn Shut)
          (Expected Shut) -> Just $ SetTo tile (HaltedOn Open)
          _ -> Nothing

      coverFlex = case tile of
        Info -> [ css "cover-flex-info" ]
        _    -> [ css "cover-flex" ]

      coverItems = case tile of
        Info ->
          [ HH.div [ css "cover-items-info-image" ] [  ]
          , HH.div [ css "cover-items-info-name" ] [ HH.text "PureFunctor" ]
          , HH.div [ css "cover-items-info-sub" ] [ HH.text "Student, Python, FP" ]
          ]
        Projects ->
          [ HH.div [ css "cover-items-projects-socials" ] [ HH.text "Projects" ]
          ]
        Socials ->
          [ HH.div [ css "cover-items-projects-socials" ] [ HH.text "Socials" ]
          ]

      chevron = chevronStyles
        where
        chevronStyles = classes $
          [ "fas"
          , "fa-chevron-down"
          , "animate-bounce"
          , "mx-auto"
          , "mb-5"
          ] <> animation

        animation = case tState of
          (Expected _) -> [ "rotate-180" ]
          (MovingTo _) -> [ "rotate-180" ]
          (HaltedOn _) -> [ "rotate-0" ]
          _ -> [ ]

    tileContent = case tile of
      Info ->
        [ HH.div [ css "p-5" ]
          [ HH.text "PureFunctor"
          ]
        ]
      Projects ->
        [ HH.div [ css "p-5" ]
          [ HH.text "Projects"
          ]
        ]
      Socials ->
        [ HH.div [ css "p-5" ]
          [ HH.text "Socials"
          ]
        ]


handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
handleAction (SetTo tile tState) = do
  state <- H.get
  case tile of
    Info -> H.put $ state { infoTile = tState }
    Projects -> H.put $ state { projectsTile = tState }
    Socials -> H.put $ state { socialsTile = tState }
