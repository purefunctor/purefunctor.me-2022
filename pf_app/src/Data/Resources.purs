module Website.Data.Resources where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.PreciseDateTime (PreciseDateTime)
import Slug (Slug)
import Slug as Slug
import Website.Data.PreciseDateTime as PDT


type BlogPost =
  { fullTitle :: String
  , shortTitle :: Slug
  , contents :: String
  , published :: PreciseDateTime
  , updated :: PreciseDateTime
  }


type Repository =
  { name :: String
  , owner :: String
  , url :: String
  , stars :: Int
  , commits :: Int
  }


blogPostCodec :: JsonCodec BlogPost
blogPostCodec =
  CA.object "BlogPost" $ CAR.record
    { fullTitle:  CA.string
    , shortTitle: CA.prismaticCodec Slug.parse Slug.toString CA.string
    , contents: CA.string
    , published: PDT.codec
    , updated: PDT.codec
    }


repositoryCodec :: JsonCodec Repository
repositoryCodec =
  CA.object "Repository" $ CAR.record
    { name: CA.string
    , owner: CA.string
    , url: CA.string
    , stars: CA.int
    , commits: CA.int
    }
