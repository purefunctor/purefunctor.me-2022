{-# LANGUAGE OverloadedStrings #-}
module Website.Models
  ( Repository(..)
  )
  where


import Data.Aeson ( object, KeyValue((.=)), ToJSON(toJSON) )


data BlogPost = BlogPost
  { fullTitle  :: String
  , shortTitle :: String
  , contents   :: String
  , published  :: String
  , updated    :: String
  } deriving ( Eq, Show )


instance ToJSON BlogPost where
  toJSON blogPost = object
    [ "fullTitle"  .= fullTitle blogPost
    , "shortTitle" .= shortTitle blogPost
    , "contents"   .= contents blogPost
    , "published"  .= published blogPost
    , "updated"    .= updated blogPost
    ]


data Repository = Repository
  { name    :: String
  , owner   :: String
  , url     :: String
  , stars   :: Int
  , commits :: Int
  } deriving ( Eq, Show )


instance ToJSON Repository where
  toJSON repository = object
    [ "name"    .= name repository
    , "owner"   .= owner repository
    , "url"     .= url repository
    , "stars"   .= stars repository
    , "commits" .= commits repository
    ]
