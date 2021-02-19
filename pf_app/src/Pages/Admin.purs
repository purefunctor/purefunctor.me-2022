module Website.Pages.Admin where

import Prelude

import Data.Const (Const)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event as Event
import Website.Capability.Resources (class ManageLogin, login)
import Website.Data.Resources (LoginCreds)
import Website.Utils.Cookies (getXsrfToken)


newtype LoginForm r f = LoginForm (r
  ( username :: f Void String String
  , password :: f Void String String
  ))

derive instance newtypeLoginForm :: Newtype (LoginForm r f) _


data FormAction
  = Submit Event.Event


type FormQuery
  = Const Void


formComponent
  :: forall i m.
     MonadAff m
  => F.Component LoginForm FormQuery () i LoginCreds m
formComponent = F.component formInput $ F.defaultSpec
  { render = formRender
  , handleEvent = formHandleEvent
  , handleAction = formHandleAction
  }
  where
    formInput _ =
      { initialInputs: Nothing
      , validators: LoginForm
          { username: F.noValidation
          , password: F.noValidation
          }
      }

    formRender { form } =
      HH.form
        [ HE.onSubmit \ev -> Just $ F.injAction $ Submit ev
        ]
        [ HH.input
          [ HP.value $ F.getInput _username form
          , HE.onValueInput $ Just <<< F.set _username
          ]
        , HH.input
          [ HP.value $ F.getInput _password form
          , HE.onValueInput $ Just <<< F.set _password
          ]
        , HH.button
          [ HP.type_ HP.ButtonSubmit ]
          [ HH.text "Submit" ]
        ]
      where
        _username :: SProxy "username"
        _username = SProxy

        _password :: SProxy "password"
        _password = SProxy

    formHandleEvent = F.raiseResult

    formHandleAction = case _ of
      Submit event -> do
        H.liftEffect $ Event.preventDefault event
        eval F.submit
      where
        eval act = F.handleAction formHandleAction formHandleEvent act


type State = { isLoggedIn :: Boolean }

data Action
  = Initialize
  | HandleLoginForm LoginCreds

type ChildSlots =
  ( formless :: F.Slot LoginForm FormQuery () LoginCreds Unit )


component
  :: forall query input output m.
     MonadAff m
  => ManageLogin m
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


initialState :: forall input. input -> State
initialState _ = { isLoggedIn: false }


render
  :: forall m.
     MonadAff m
  => ManageLogin m
  => State
  -> H.ComponentHTML Action ChildSlots m
render { isLoggedIn } =
  HH.div_
  [ if isLoggedIn
      then HH.div_ [ HH.text "Logged In" ]
      else HH.slot F._formless unit formComponent unit (Just <<< HandleLoginForm)
  ]


handleAction
  :: forall output m.
     MonadAff m
  => ManageLogin m
  => Action
  -> H.HalogenM State Action ChildSlots output m Unit
handleAction = case _ of
  Initialize -> do
    case hush getXsrfToken of
      Just _ -> H.put { isLoggedIn: true }
      Nothing -> pure unit
  HandleLoginForm creds ->
    login creds *> H.put { isLoggedIn: true }
