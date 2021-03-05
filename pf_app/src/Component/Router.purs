module Website.Component.Router where

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Routing.Duplex as RD
import Web.HTML (window)
import Web.HTML.Location as Location
import Web.HTML.Window as Window
import Website.Capability.Navigation (class Navigate)
import Website.Capability.Resources (class ManageBlogPost, class ManageRepository, class ManageLogin)
import Website.Data.Routes (Route(..), routeCodec)
import Website.Pages.Admin as Admin
import Website.Pages.Home as Home


type State = { currentRoute :: Route }
data Action = Initialize
data Query a = Navigate Route a
type ChildSlots =
  ( home :: H.Slot Query Void Unit
  , admin :: H.Slot Query Void Unit
  )


component
  :: forall input output m.
     MonadAff m
  => ManageBlogPost m
  => ManageRepository m
  => ManageLogin m
  => Navigate m
  => H.Component HH.HTML Query input output m
component =
  H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , handleQuery = handleQuery
    , initialize = Just Initialize
    }
  }


initialState :: forall input. input -> State
initialState _ = { currentRoute: HomeR }


render
  :: forall m.
     MonadAff m
  => ManageBlogPost m
  => ManageRepository m
  => ManageLogin m
  => Navigate m
  => State
  -> H.ComponentHTML Action ChildSlots m
render { currentRoute } =
  case currentRoute of
    AdminR -> HH.slot (SProxy :: _ "admin") unit Admin.component unit absurd
    HomeR -> HH.slot (SProxy :: _ "home") unit Home.component unit absurd


handleAction
  :: forall output m.
     MonadAff m
  => ManageBlogPost m
  => ManageRepository m
  => ManageLogin m
  => Navigate m
  => Action
  -> H.HalogenM State Action ChildSlots output m Unit
handleAction = case _ of
  Initialize -> do
    route_ <- H.liftEffect $
      window >>= Window.location >>= Location.pathname

    let mRoute = hush $ RD.parse routeCodec route_

    H.modify_ _ { currentRoute = fromMaybe HomeR mRoute }


handleQuery
  :: forall output m a.
     MonadAff m
  => ManageBlogPost m
  => ManageRepository m
  => ManageLogin m
  => Navigate m
  => Query a
  -> H.HalogenM State Action ChildSlots output m (Maybe a)
handleQuery = case _ of
  Navigate route a -> do
    { currentRoute } <- H.get
    H.put { currentRoute: route }
    pure (Just a)
