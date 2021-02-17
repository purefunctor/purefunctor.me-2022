module PF.Pages.Home where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class.Console (log)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import PF.Capability.Resources (class ManageRepository, getRepositories)
import PF.Component.Utils (css)


type State = Unit
data Action = Initialize


component
  :: forall query input output m.
     MonadAff m
  => ManageRepository m
  => H.Component HH.HTML query input output m
component =
  H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , initialize = Just Initialize
    }
  }


initialState :: forall input. input -> Unit
initialState _ = unit


render
  :: forall m.
     MonadAff m
  => ManageRepository m
  => State
  -> H.ComponentHTML Action () m
render _ =
  HH.div [ css "bg-gray-100 h-screen overflow-auto scroll-snap-y-mandatory"  ]
  [ HH.div [ css "h-auto w-full lg:w-11/12 mx-auto" ]
    [ HH.div [ css "h-screen flex flex-col justify-center items-center space-y-5 scroll-snap-align-start" ]
      [ HH.div [ css "h-56 w-56 bg-green-200 rounded-full shadow-xl" ]
        [
        ]
      , HH.div [ css "text-4xl text-center" ]
        [ HH.text "PureFunctor"
        ]
      , HH.div [ css "text-4xl font-thin text-center" ]
        [ HH.text "Student, Python, FP"
        ]
      ]
    , subsection "About" [ css "text-lg p-5" ]
      [ HH.text "Text"
      ]
    , subsection "Projects" [ css "text-lg p-5" ]
      [ HH.text "Text"
      ]
    , subsection "Contact" [ css "text-lg p-5" ]
      [ HH.text "Text"
      ]
    ]
  ]
  where
    subsection title props content =
      HH.div [ css "h-screen p-5 scroll-snap-align-start divide-y-2" ]
      [ HH.div [ css "text-4xl p-5" ]
        [ HH.text title
        ]
      , HH.div props content
      ]


handleAction
  :: forall output m.
     MonadAff m
  => ManageRepository m
  => Action
  -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    mRepositories <- getRepositories
    case mRepositories of
      Just repositories -> log $ show repositories
      Nothing -> log "parsing exception"
