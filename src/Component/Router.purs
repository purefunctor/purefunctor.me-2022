module Website.Component.Router where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Website.Capability.Navigation (class Navigate)
import Website.Capability.OpenUrl (class OpenUrl)
import Website.Capability.Resources (class ManageBlogPost, class ManageRepository, class ManageLogin)
import Website.Data.Routes (Route(..))
import Website.Pages.About as About
import Website.Pages.Admin as Admin
import Website.Pages.Contact as Contact
import Website.Pages.Home as Home
import Website.Pages.NotFound as NotFound
import Website.Pages.Projects as Projects


type State = { currentRoute :: Route }
data Query a = Navigate Route a
type Input = Route
type ChildSlots =
  ( home :: H.Slot Query Void Unit
  , admin :: H.Slot Query Void Unit
  , notFound :: H.Slot Query Void Unit
  , about :: H.Slot Query Void Unit
  , contact :: H.Slot Query Void Unit
  , projects :: H.Slot Query Void Unit
  )


component
  :: forall output m.
     MonadAff m
  => ManageBlogPost m
  => ManageRepository m
  => ManageLogin m
  => Navigate m
  => OpenUrl m
  => H.Component Query Input output m
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
  => OpenUrl m
  => State
  -> H.ComponentHTML action ChildSlots m
render { currentRoute } =
  case currentRoute of
    AdminR -> HH.slot (Proxy :: _ "admin") unit Admin.component unit absurd
    HomeR -> HH.slot (Proxy :: _ "home") unit Home.component unit absurd
    NotFoundR -> HH.slot (Proxy :: _ "notFound") unit NotFound.component unit absurd
    AboutR -> HH.slot (Proxy :: _ "about") unit About.component unit absurd
    ContactR -> HH.slot (Proxy :: _ "contact") unit Contact.component unit absurd
    ProjectsR -> HH.slot (Proxy :: _ "projects") unit Projects.component unit absurd


handleQuery
  :: forall action output m a.
     MonadAff m
  => ManageBlogPost m
  => ManageRepository m
  => ManageLogin m
  => Navigate m
  => OpenUrl m
  => Query a
  -> H.HalogenM State action ChildSlots output m (Maybe a)
handleQuery = case _ of
  Navigate route a -> do
    H.put { currentRoute: route }
    pure (Just a)