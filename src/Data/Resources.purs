module Website.Data.Resources where

import Prelude

import Data.Argonaut.Decode as AD
import Data.Argonaut.Encode as AE
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (hush)
import Data.PreciseDateTime (PreciseDateTime)
import Foreign.Object (Object)
import Slug (Slug)
import Slug as Slug
import Website.Data.PreciseDateTime as PDT


type BlogPost =
  { title :: String
  , short :: Slug
  , contents :: String
  , published :: PreciseDateTime
  , updated :: PreciseDateTime
  }


type Repository =
  { name :: String
  , owner :: String
  , description :: String
  , url :: String
  , stars :: Int
  , commits :: Array Int
  , languages :: Languages
  }


type Languages = Object Int


type LoginCreds =
  { username :: String
  , password :: String
  }


blogPostCodec :: JsonCodec BlogPost
blogPostCodec =
  CA.object "BlogPost" $ CAR.record
    { title:  CA.string
    , short: CA.prismaticCodec "Post Slug" Slug.parse Slug.toString CA.string
    , contents: CA.string
    , published: PDT.codec
    , updated: PDT.codec
    }


repositoryCodec :: JsonCodec Repository
repositoryCodec =
  CA.object "Repository" $ CAR.record
    { name: CA.string
    , owner: CA.string
    , description: CA.string
    , url: CA.string
    , stars: CA.int
    , commits: CA.array CA.int
    , languages: languagesCodec
    }


languagesCodec :: JsonCodec Languages
languagesCodec = CA.prismaticCodec "Languages" from to CA.json
  where
    from = hush <<< AD.decodeJson

    to = AE.encodeJson


loginCredsCodec :: JsonCodec LoginCreds
loginCredsCodec =
  CA.object "Login" $ CAR.record
    { username: CA.string
    , password: CA.string
    }
