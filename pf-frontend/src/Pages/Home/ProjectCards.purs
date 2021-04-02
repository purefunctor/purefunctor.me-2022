module Website.Pages.Home.ProjectCards where

import Prelude

import Data.Array (length)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Prim.Row (class Cons)
import Type.Proxy (Proxy)
import Website.Capability.Resources (class ManageRepository, getRepositories)
import Website.Component.Utils (css, css')
import Website.Data.Resources (Repository)


type State =
  { shown :: Boolean
  , repositories :: Array Repository
  }
data Action = Initialize
type Slot = forall query. H.Slot query Void Unit


make
  :: forall l query _1 slots action m.
     MonadAff m
  => ManageRepository m
  => Cons l (H.Slot query Void Unit) _1 slots
  => IsSymbol l
  => Proxy l
  -> HH.HTML (H.ComponentSlot slots m action) action
make label = HH.slot label unit component unit absurd


component
  :: forall query input output m.
     MonadAff m
  => ManageRepository m
  => H.Component query input output m
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
initialState _ = { shown: false, repositories: [ ] }


render :: forall action slots m. State -> H.ComponentHTML action slots m
render { shown, repositories } =
  HH.div [ css "p-5 flex-grow flex flex-wrap place-content-center" ]
  if shown && length repositories /= 0
     then
       makeCard <$> repositories
     else
       [ HH.div [ css "text-4xl animate-pulse" ] [ HH.text "..." ]
       ]
  where
    makeCard :: forall w i. Repository -> HH.HTML w i
    makeCard repository =
      HH.a
      [ css'
        [ "md:w-max w-full h-40 m-2"
        , "flex flex-col flex-none"
        , "bg-white rounded-xl md:shadow-lg shadow-md divide-solid divide-y-2"
        , "transform transition ease-out hover:-translate-y-2 cursor-pointer hover-box"
        , "ring-2 ring-black"
        ]
      , HP.href repository.url
      ]
      [ HH.div
        [ css "flex p-4 text-2xl" ]
        [ HH.div
          [ css "flex-grow truncate" ]
          [ HH.text $ repository.name
          ]
        , HH.i [ css "fab fa-github" ] [ ]
        ]
      , HH.div
        [ css "flex flex-col h-full p-4" ]
        [ HH.div
          [ css "flex-grow truncate text-md" ]
          [ HH.text $ repository.description
          ]
        , HH.div
          [ css "flex" ]
          [ HH.div
            [ css "flex-grow" ]
            [ HH.text $ show repository.stars <> " "
            , HH.i [ css "far fa-star" ] [ ]
            ]
          , HH.div_
            [ HH.text $ show repository.commits <> " "
            , HH.i [ css "fas fa-history" ] [ ]
            ]
          ]
        ]
       ]


handleAction
  :: forall slots output m.
     MonadAff m
  => ManageRepository m
  => Action
  -> H.HalogenM State Action slots output m Unit
handleAction = case _ of
  Initialize ->
    getRepositories >>=
      case _ of
        Just repositories -> H.put { shown: true, repositories }
        Nothing -> pure unit
