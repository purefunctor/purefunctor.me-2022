module PF.Pages.Home where

import Prelude

import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.Animated as HN
import Halogen.HTML as HH
import PF.Component.Utils (css)


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

type ChildSlots = (_tile_cover :: HN.Slot Void Int)

_tile_cover :: SProxy "_tile_cover"
_tile_cover = SProxy


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

render :: forall m. State -> H.ComponentHTML Action ChildSlots m
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
      [ coverContainer
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

    coverContainer =
      HH.slot _tile_cover 0 HN.component
      { start: "close"
      , toFinal: "close-to-open"
      , final: "open"
      , toStart: "open-to-close"
      , render: HH.div [ css "cover-container" ] [ tileCover ]
      } absurd
      where
      tileCover =
        HH.div [ css "full-flex" ]
        [ HH.div coverFlex coverItems
        , HH.div chevron [ ]
        ]
        where
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

        chevron = [ css "fas fa-chevron-down animate-bounce mx-auto mb-5" ]

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


handleAction :: forall output m. Action -> H.HalogenM State Action ChildSlots output m Unit
handleAction (SetTo tile tState) = do
  state <- H.get
  case tile of
    Info -> H.put $ state { infoTile = tState }
    Projects -> H.put $ state { projectsTile = tState }
    Socials -> H.put $ state { socialsTile = tState }
