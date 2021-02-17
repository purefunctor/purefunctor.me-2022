module PF.Component.Router where

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import PF.Capability.Resources (class ManageBlogPost, class ManageRepository)
import PF.Data.Routes (Routes(..), routeCodec)
import PF.Pages.About as About
import PF.Pages.Home as Home
import Routing.Duplex as RD
import Routing.Hash as RH


type State = { currentRoute :: Routes }
data Action = Initialize
data Query a = Navigate Routes a
type ChildSlots =
  ( home :: H.Slot Query Void Unit
  , about :: H.Slot Query Void Unit
  )


component
  :: forall input output m.
     MonadAff m
  => ManageBlogPost m
  => ManageRepository m
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
  => State
  -> H.ComponentHTML Action ChildSlots m
render { currentRoute } =
  case currentRoute of
    AboutR -> HH.slot (SProxy :: _ "about") unit About.component unit absurd
    HomeR -> HH.slot (SProxy :: _ "home") unit Home.component unit absurd


handleAction
  :: forall output m.
     MonadAff m
  => ManageBlogPost m
  => ManageRepository m
  => Action
  -> H.HalogenM State Action ChildSlots output m Unit
handleAction = case _ of
  Initialize -> do
    initialRoute <- hush <<< (RD.parse routeCodec) <$> liftEffect RH.getHash
    liftEffect <<< RH.setHash <<< RD.print routeCodec <<< fromMaybe HomeR $ initialRoute


handleQuery
  :: forall output m a.
     MonadAff m
  => ManageBlogPost m
  => ManageRepository m
  => Query a
  -> H.HalogenM State Action ChildSlots output m (Maybe a)
handleQuery = case _ of
  Navigate route a -> do
    { currentRoute } <- H.get
    H.put { currentRoute: route }
    pure (Just a)
