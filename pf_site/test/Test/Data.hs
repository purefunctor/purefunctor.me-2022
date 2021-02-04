{-# LANGUAGE OverloadedStrings #-}
module Test.Data where

import Data.Aeson

import Data.Time
import Data.Time.Calendar.Julian

import Website.Config
import Website.Models


repos :: [Repository]
repos =
  [ Repository "amalgam-lisp" "PureFunctor" "https://github.com/PureFunctor/amalgam-lisp" 0 0
  , Repository "purefunctor.me" "PureFunctor" "https://github.com/PureFunctor/purefunctor.me" 0 0
  ]


posts :: [BlogPost]
posts =
  [ BlogPost "Haskell Is Simple" "haskell-is-simple" "SOON™" now next
  , BlogPost "Python Is Awesome" "python-is-awesome" "SOON™" now next
  ]
  where
    now = UTCTime (fromJulian 2020 02 02) (secondsToDiffTime 0)
    next = UTCTime (fromJulian 2020 02 03) (secondsToDiffTime 0)


mkLoginPayload :: Configuration -> Value
mkLoginPayload config = object
    [ "username" .= adminUser config
    , "password" .= adminPass config
    ]
