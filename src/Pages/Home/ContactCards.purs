module Website.Pages.Home.ContactCards where

import Prelude

import Data.Symbol (class IsSymbol)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Prim.Row (class Cons)
import Type.Proxy (Proxy)
import Web.HTML (window)
import Web.HTML.Window as Window
import Website.Component.Utils (css, css')


type State = Unit

data Action = OpenLink String

type Slot = forall query. H.Slot query Void Unit


make
  :: forall l query _1 slots action m
   . MonadAff m
  => Cons l (H.Slot query Void Unit) _1 slots
  => IsSymbol l
  => Proxy l
  -> HH.HTML (H.ComponentSlot slots m action) action
make label = HH.slot label unit component unit absurd


component
  :: forall query input output m
   . MonadAff m
  => H.Component query input output m
component =
  H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    }
  }


initialState :: forall input. input -> State
initialState _ = unit


render :: forall slots m. State -> H.ComponentHTML Action slots m
render _ =
  HH.div
  [ css'
    [ "flex flex-1 md:flex-row flex-col"
    , "md:space-x-5 md:space-y-0 space-y-5 p-5 justify-evenly"
    ]
  ]
  [ makeCard "bg-pink-100 text-pink-500" "mailto:justin@purefunctor.me"
    [ HH.i [ css "fas fa-mail-bulk fa-4x" ] [ ]
    ]
  , makeCard "bg-blue-100 text-blue-500" "https://twitter.com/PureFunctor"
    [ HH.i [ css "fab fa-twitter fa-4x" ] [ ]
    ]
  , makeCard "bg-green-100 text-gray-900" "https://pythondiscord.org"
    [ HH.i [ css "fab fa-discord fa-4x" ] [ ]
    ]
  ]
  where
    makeCard extra link child =
      HH.div
      [ css'
        [ extra
        , "flex flex-col h-64 w-full overflow-hidden"
        , "ring-2 ring-black shadow-xl rounded-xl"
        ]
      , HE.onClick \_ -> OpenLink link
      , HP.tabIndex 0
      , HPA.role "link"
      , HPA.label $ "Navigate to URL: " <> link
      ]
      [ HH.div [ css "ring-2 ring-black h-20 bg-pixel-pattern" ] [ ]
      , HH.div [ css "flex flex-grow items-center justify-center" ] child
      ]


handleAction
  :: forall slots output m
   . MonadEffect m
  => Action
  -> H.HalogenM State Action slots output m Unit
handleAction = case _ of
  OpenLink url -> H.liftEffect do
    void $ window >>= Window.open url "" ""
