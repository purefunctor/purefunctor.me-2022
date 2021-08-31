module Website.Component.Router where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Monad (class MonadStore)
import Type.Proxy (Proxy(..))
import Website.Capability.Navigation (class Navigate)
import Website.Data.Routes (Route(..))
import Website.Pages.About as About
import Website.Pages.Home as Home
import Website.Pages.NotFound as NotFound
import Website.Store as Store

type State = { currentRoute ∷ Route }
data Query a = Navigate Route a
type Input = Route
type ChildSlots =
  ( home ∷ H.Slot Query Void Unit
  , about ∷ H.Slot Query Void Unit
  , notFound ∷ H.Slot Query Void Unit
  )

component
  ∷ ∀ output m
  . MonadAff m
  ⇒ MonadStore Store.Action Store.Store m
  ⇒ Navigate m
  ⇒ H.Component Query Input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleQuery = handleQuery
        }
    }

initialState ∷ Route → State
initialState currentRoute = { currentRoute }

render
  ∷ ∀ action m
  . MonadAff m
  ⇒ Navigate m
  ⇒ MonadStore Store.Action Store.Store m
  ⇒ State
  → H.ComponentHTML action ChildSlots m
render { currentRoute } =
  case currentRoute of
    HomeR → HH.slot (Proxy ∷ _ "home") unit Home.component unit absurd
    AboutR → HH.slot (Proxy ∷ _ "about") unit About.component unit absurd
    NotFoundR → HH.slot (Proxy ∷ _ "notFound") unit NotFound.component unit absurd

handleQuery
  ∷ ∀ action output m a
  . MonadAff m
  ⇒ Navigate m
  ⇒ Query a
  → H.HalogenM State action ChildSlots output m (Maybe a)
handleQuery = case _ of
  Navigate route a → do
    H.put { currentRoute: route }
    pure (Just a)
