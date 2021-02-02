{-# LANGUAGE OverloadedStrings #-}
module Test.Auth where

import Test.Hspec
import Test.Hspec.Wai
import Test.Utils

import Data.Aeson.Micro

import Network.Wai

import Website.Config


testAuth :: Configuration -> Application -> Spec
testAuth config app = with (pure app) $ do
  describe "POST /login" $ do
    let value = object
          [ "username" .= adminUser config
          , "password" .= adminUser config
          ]
    let headers =
          [ ("Content-Type", "application/json")
          ]
    it "should return 204 on a successful login" $ do
      postJSON "/login" headers value `shouldRespondWith` 204
