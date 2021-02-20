module Website.Component.RepoCard where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Website.Component.Utils (css)
import Website.Data.Resources (Repository)


type Input = Array Repository
type State = Array Repository


component :: forall query output m. H.Component HH.HTML query Input output m
component =
  H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
  }


initialState :: Input -> State
initialState = identity


render :: forall action slots m. State -> H.ComponentHTML action slots m
render repositories =
  HH.div
  [ css "h-full flex flex-wrap place-content-evenly" ]
  cards
  where
    makeCard :: forall w i. Repository -> HH.HTML w i
    makeCard repository =
      HH.a
      [ css "md:w-2/5 w-full h-40 flex flex-col flex-none bg-white rounded-xl divide-solid divide-y-2 md:shadow-lg shadow-md m-2 transform hover:-translate-y-2 cursor-pointer"
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

    cards :: forall w i. Array (HH.HTML w i)
    cards = makeCard <$> repositories
