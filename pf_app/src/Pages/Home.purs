module PF.Pages.Home where

import Prelude

import Data.Array (concat)
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
  pageRoot
  [ tileGrid
    [ tileContainer Info (tileCover Info) infoContent
    , tileContainer Projects (tileCover Projects) projectsContent
    , tileContainer Socials (tileCover Socials) socialsContent
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
      styles = classes $
        [ "absolute"
        , "box-content"
        , "h-full"
        , "w-full"
        , "z-10"
        , "bg-gray-100"
        ] <> optionalStyles

      optionalStyles = concat [ tileAnimation ]

      events =
        [ HE.onClick onClickEvent
        , HE.handler (EventType "animationend") onAnimationEndEvent
        ]

      tState = fromTileState tile state

      tileAnimation = case tState of
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

  tileCover tile =
    [ fullFlex [ coverFlex coverItems , chevron ] ]
    where
    fullFlex = HH.div $ styles <> events
      where
      styles = classes
        [ "flex"
        , "flex-col"
        , "h-full"
        , "w-full"
        , "cursor-pointer"
        ]
      events =
        [ HE.onMouseOver onMouseOverEvent
        , HE.onMouseOut onMouseOutEvent
        ]

      tState = fromTileState tile state

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
      Info -> HH.div $ classes
        [ "flex"
        , "flex-col"
        , "flex-grow"
        , "items-center"
        , "place-content-center"
        , "space-y-5"
        ]
      _ -> HH.div $ classes
        [ "flex"
        , "flex-col"
        , "flex-grow"
        , "m-5"
        ]

    coverItems = case tile of
      Info ->
        [ image [  ]
        , name [ HH.text "PureFunctor" ]
        , sub [ HH.text "Student, Python, FP" ]
        ]
        where
        image = HH.div $ classes
          [ "bg-green-500"
          , "h-56"
          , "w-56"
          , "rounded-full"
          , "shadow-2xl"
          ]
        name = HH.div $ classes
          [ "text-4xl"
          , "font-extralight"
          , "select-none"
          ]
        sub = HH.div $ classes
          [ "text-2xl"
          , "font-thin"
          , "select-none"
          ]
      Projects ->
        [ HH.div projectsSocialsStyles [ HH.text "Projects" ]
        ]
      Socials ->
        [ HH.div projectsSocialsStyles [ HH.text "Socials" ]
        ]
      where
      projectsSocialsStyles = classes
        [ "font-sans"
        , "font-thin"
        , "text-6xl"
        , "select-none"
        ]

    chevron = HH.i chevronStyles [ ]
      where
      chevronStyles = classes $
        [ "fas"
        , "fa-chevron-down"
        , "animate-bounce"
        , "mx-auto"
        , "mb-5"
        ] <> optionalStyles

      tState = fromTileState tile state

      optionalStyles = concat [ chevronAnimation ]

      chevronAnimation = case tState of
        (Expected _) -> [ "rotate-180" ]
        (MovingTo _) -> [ "rotate-180" ]
        (HaltedOn _) -> [ "rotate-0" ]
        _ -> [ ]

  infoContent =
    [ HH.div [ css "p-5" ]
      [ HH.text "PureFunctor"
      ]
    ]

  projectsContent =
    [ HH.div [ css "p-5" ]
      [ HH.text "Projects"
      ]
    ]

  socialsContent =
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
