module Website.Pages.Home where

import Prelude

import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Website.Capability.Resources (class ManageRepository)
import Website.Component.Utils (css, css')
import Website.Pages.Home.AboutCard as AboutCard
import Website.Pages.Home.ContactCards as ContactCards
import Website.Pages.Home.ProjectCards as ProjectCards


type State = Unit
type ChildSlots =
  ( projects :: ProjectCards.Slot )


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
  }


initialState :: forall input. input -> State
initialState _ = unit


render
  :: forall action m.
     MonadAff m
  => ManageRepository m
  => State
  -> H.ComponentHTML action ChildSlots m
render _ =
  HH.div [ css "bg-gray-100 h-screen overflow-auto scroll-snap-y-proximity"  ]
  [ HH.div [ css "h-auto w-full lg:w-11/12 mx-auto" ]
    [ HH.div [ css "h-screen flex flex-col justify-center items-center space-y-5 scroll-snap-align-start" ]
      [ HH.div [ css "h-56 w-56 bg-green-200 rounded-full shadow-xl" ]
        [
        ]
      , HH.div [ css "text-4xl font-extralight text-center" ]
        [ HH.text "PureFunctor"
        ]
      , HH.div [ css "text-4xl font-thin text-center" ]
        [ HH.text "Student, Python, FP"
        ]
      ]
    , subsection "min-h-screen" "About"
      [ AboutCard.element
      ]
    , subsection "min-h-screen" "Projects"
      [ ProjectCards.make ( SProxy :: SProxy "projects" )
      ]
    , subsection "min-h-screen" "Contact"
      [ ContactCards.element
      ]
    ]
  ]
  where
    subsection extra title child =
      HH.div [ css' [ extra, "flex flex-col scroll-snap-align-start divide-y-2" ] ] $
      [ HH.div [ css "font-extralight text-4xl p-5" ]
        [ HH.text title
        ]
      ] <> child


handleAction
  :: forall action output m.
     MonadAff m
  => ManageRepository m
  => action
  -> H.HalogenM State action ChildSlots output m Unit
handleAction _ = pure unit
