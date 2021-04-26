module Website.Pages.Projects where

import Prelude

import Data.Array (length)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Website.Capability.Navigation (class Navigate, navigate)
import Website.Capability.OpenUrl (class OpenUrl, openUrl)
import Website.Capability.Resources (class ManageRepository, getRepositories)
import Website.Component.HTML.Navbar (navbar)
import Website.Component.Utils (css, css')
import Website.Data.Resources (Repository)
import Website.Data.Routes (Route)


type State =
  { shown :: Boolean
  , repositories :: Array Repository
  }

data Action
  = Initialize
  | OpenLink String
  | Navigate Route


component
  :: forall query input output m.
     MonadAff m
  => ManageRepository m
  => Navigate m
  => OpenUrl m
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


render :: forall slots m. State -> H.ComponentHTML Action slots m
render { shown, repositories } =
  HH.div [ css "bg-faint h-screen" ]
  [ HH.div [ css "h-full w-full lg:w-11/12 mx-auto" ]
    [ navbar Navigate
    , HH.div [ css "p-5 flex flex-wrap place-content-center" ]
      if shown && length repositories /= 0
        then
          ( makeCard <$> repositories )
        else
          [ HH.div [ css "text-2xl" ]
            [ HH.text "..."
            ]
          ]
    ]
  ]
  where
    makeCard :: forall w. Repository -> HH.HTML w Action
    makeCard repository =
      HH.button
      [ css'
        [ "md:w-max w-full h-40 m-2"
        , "flex flex-col flex-none"
        , "bg-white rounded-xl md:shadow-lg shadow-md divide-solid divide-y-2"
        , "transform transition ease-out hover:-translate-y-2 cursor-pointer hover-box"
        , "focus:-translate-y-2 ring-2 ring-black"
        ]
      , HE.onClick \_ -> OpenLink repository.url
      , HP.tabIndex 0
      , HPA.role "link"
      , HPA.label $ "Navigate to repository: " <> repository.name
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
          [ HH.div_
            [ HH.text $ show repository.stars <> " "
            , HH.i [ css "far fa-star" ] [ ]
            ]
          , HH.div [ css "flex-grow" ] [ {- space -} ]
          , HH.div_
            [ HH.text $ show ( sum repository.commits ) <> " "
            , HH.i [ css "fas fa-history" ] [ ]
            ]
          ]
        ]
      ]


handleAction
  :: forall slots output m.
     MonadAff m
  => ManageRepository m
  => OpenUrl m
  => Navigate m
  => Action
  -> H.HalogenM State Action slots output m Unit
handleAction = case _ of
  Initialize ->
    getRepositories >>=
      case _ of
        Just repositories -> H.put { shown: true, repositories }
        Nothing -> pure unit
  OpenLink url -> openUrl url
  Navigate route -> navigate route
