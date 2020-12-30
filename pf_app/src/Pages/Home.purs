module PF.Pages.Home where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.Animated as HN
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import PF.Component.Utils (css)


data Tile
  = Info
  | Projects
  | Socials


derive instance eqTile :: Eq Tile
derive instance ordTile :: Ord Tile


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


data Action = SetTo Tile TileState | TileClicked Tile


type ChildSlots = (_tile_cover :: HN.Slot Action Tile)


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
    [ mkTile Info
    , mkTile Projects
    , mkTile Socials
    ]
  ]
  where
    -- | Container for each tile
    mkTile tile =
      HH.div border
      [ HH.div clipper
        [ cover
        , content
        ]
      ]
      where
        border = case tile of
          Info -> [ css "border-container-info" ]
          _    -> [ css "border-container" ]

        clipper = [ css "clipper-container garage-clip" ]

        content = HH.div [ css "content-container" ] tileContent
          where
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

        -- Temporary workaround as I've styled this in a very odd
        -- manner; I could fix this upstream but I can also add a
        -- special class just for this use-case; alternatively, I
        -- could also just simplify my current hierarchy.
        cover =
          HH.slot _tile_cover tile HN.component
          { start: "shut cover-container"
          , toFinal: "close-to-open cover-container"
          , final: "open cover-container"
          , toStart: "open-to-close cover-container"
          , render: tileCover
          } Just
          where
            tileCover =
              HH.div [ css "full-flex" , HE.onClick \_ -> Just $ HN.Raise $ TileClicked tile ]
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


handleAction :: forall output m. Action -> H.HalogenM State Action ChildSlots output m Unit
handleAction = case _ of
  (TileClicked tile) -> void $ H.query _tile_cover tile $ H.tell HN.ToggleAnimation
  _ -> pure unit
