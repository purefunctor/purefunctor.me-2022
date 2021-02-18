module Website.Data.Resources where

import Prelude

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR


type BlogPost =
  { fullTitle :: String
  , shortTitle :: String
  , contents :: String
  , published :: String
  , updated :: String
  }


type Repository =
  { name :: String
  , owner :: String
  , url :: String
  , stars :: Int
  , commits :: Int
  }


blogPostCodec :: CA.JsonCodec BlogPost
blogPostCodec =
  CA.object "BlogPost" $ CAR.record
    { fullTitle:  CA.string
    , shortTitle: CA.string
    , contents: CA.string
    , published: CA.string
    , updated: CA.string
    }


repositoryCodec :: CA.JsonCodec Repository
repositoryCodec =
  CA.object "Repository" $ CAR.record
    { name: CA.string
    , owner: CA.string
    , url: CA.string
    , stars: CA.int
    , commits: CA.int
    }
