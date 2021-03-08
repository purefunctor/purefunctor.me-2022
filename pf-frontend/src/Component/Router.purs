module Website.Component.Router where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Website.Capability.Navigation (class Navigate)
import Website.Capability.Resources (class ManageBlogPost, class ManageRepository, class ManageLogin)
import Website.Data.Routes (Route(..))
import Website.Pages.Admin as Admin
import Website.Pages.Home as Home
import Website.Pages.NotFound as NotFound


type State = { currentRoute :: Route }
data Query a = Navigate Route a
type Input = Route
type ChildSlots =
  ( home :: H.Slot Query Void Unit
  , admin :: H.Slot Query Void Unit
  , notFound :: H.Slot Query Void Unit
  )


component
  :: forall output m.
     MonadAff m
  => ManageBlogPost m
  => ManageRepository m
  => ManageLogin m
  => Navigate m
  => H.Component HH.HTML Query Input output m
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
  :: forall action m.
     MonadAff m
  => ManageBlogPost m
  => ManageRepository m
  => ManageLogin m
  => Navigate m
  => State
  -> H.ComponentHTML action ChildSlots m
render { currentRoute } =
  case currentRoute of
    AdminR -> HH.slot (SProxy :: _ "admin") unit Admin.component unit absurd
    HomeR -> HH.slot (SProxy :: _ "home") unit Home.component unit absurd
    NotFoundR -> HH.slot (SProxy :: _ "notFound") unit NotFound.component unit absurd


handleQuery
  :: forall action output m a.
     MonadAff m
  => ManageBlogPost m
  => ManageRepository m
  => ManageLogin m
  => Navigate m
  => Query a
  -> H.HalogenM State action ChildSlots output m (Maybe a)
handleQuery = case _ of
  Navigate route a -> do
    { currentRoute } <- H.get
    H.put { currentRoute: route }
    pure (Just a)
