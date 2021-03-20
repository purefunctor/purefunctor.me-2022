module Website.Pages.Admin where

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event as Event
import Website.Capability.Resources (class ManageLogin, login)
import Website.Component.Utils (css, css')
import Website.Data.Resources (LoginCreds)
import Website.Utils.Cookies (getXsrfToken)


newtype LoginForm r f = LoginForm (r
  ( username :: f Void String String
  , password :: f Void String String
  ))

derive instance newtypeLoginForm :: Newtype (LoginForm r f) _


data FormAction
  = Submit Event.Event


data LoginState
  = Waiting
  | Failed
  | Granted


data FormQuery a
  = SetLoginState LoginState a

derive instance functorFormQuery :: Functor FormQuery


formComponent
  :: forall i m.
     MonadAff m
  => F.Component LoginForm FormQuery () i LoginCreds m
formComponent = F.component formInput $ F.defaultSpec
  { render = formRender
  , handleEvent = formHandleEvent
  , handleAction = formHandleAction
  , handleQuery = formHandleQuery
  }
  where
    formInput _ =
      { initialInputs: Nothing
      , validators: LoginForm
          { username: F.noValidation
          , password: F.noValidation
          }
      , loginState: Waiting
      }

    formRender { form, loginState } =
      HH.form
      [ css'
        [ "flex flex-col bg-faint overflow-hidden"
        , "h-64 md:w-1/3 sm:w-2/3 w-5/6 m-auto space-y-5"
        , "shadow-xl rounded-xl ring-2 ring-black"
        ]
      , HE.onSubmit \ev -> Just $ F.injAction $ Submit ev
      ]
      [ HH.div
        [ css'
          [ "p-3 text-center ring-2 ring-black"
          , case loginState of
               Waiting -> "bg-purple-200"
               Failed -> "bg-pink-200"
               Granted -> "bg-green-200"
          ]
        ]
        [ HH.text
          case loginState of
             Waiting -> "Admin Page"
             Failed -> "Login Failed!"
             Granted -> "Login Success"
        ]
      , HH.input
        [ css $ "flex-grow rounded-md p-2 bg-faint-100 shadow-inner focus:ring-2 mx-5"
        , HP.value $ F.getInput _username form
        , HE.onValueInput $ Just <<< F.set _username
        , HP.placeholder "Username"
        ]
      , HH.input
        [ css $ "flex-grow rounded-md p-2 bg-faint-100 shadow-inner focus:ring-2 mx-5"
        , HP.value $ F.getInput _password form
        , HE.onValueInput $ Just <<< F.set _password
        , HP.type_ $ HP.InputPassword
        , HP.placeholder "Password"
        ]
      , HH.button
        [ css'
          [ "flex-grow bg-green-200 hover:bg-green-300 focus:bg-green-300"
          , "ring-2 ring-black"
          ]
        , HP.type_ HP.ButtonSubmit
        ]
        [ HH.text "Login"
        ]
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

    formHandleQuery :: forall a. FormQuery a -> H.HalogenM _ _ _ _ _ (Maybe a)
    formHandleQuery = case _ of
      SetLoginState e a -> do
        H.modify_ _ { loginState = e }
        pure (Just a)


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
  HH.div [ css "flex flex-1 h-screen bg-faint" ]
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

  HandleLoginForm creds -> do
    success <- login creds

    void $ H.query F._formless unit $ F.injQuery $
      SetLoginState Waiting unit

    let loginState = if success then Granted else Failed
        holdTime = if success then 1500.0 else 3000.0

    void $ H.query F._formless unit $ F.injQuery $
      SetLoginState loginState unit

    H.liftAff $ Aff.delay $ Milliseconds holdTime

    when (not success) $
      void $ H.query F._formless unit $ F.injQuery $
        SetLoginState Waiting unit

    H.put { isLoggedIn: success }
