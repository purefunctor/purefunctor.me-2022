{-# LANGUAGE OverloadedStrings #-}
module Test.Auth where

import Control.Lens ( (^.) )

import Data.Aeson

import Network.Wai

import Test.Hspec
import Test.Hspec.Wai as WaiTest
import Test.Utils

import Web.Cookie

import Website.Config


testAuth :: Environment -> Application -> Spec
testAuth env app = with (pure app) $ do
  describe "POST /login" $ do
    let value = object
          [ "username" .= (env^.config.admin.username)
          , "password" .= (env^.config.admin.password)
          ]
    it "should return 204 on a successful login" $ do
      postJSON "/login" [] value `shouldRespondWith` 204

    it "should set the appropriate cookies" $ do
      setCookies <- parseSetCookies <$> postJSON "/login" [] value
      (setCookieName <$> setCookies) `shouldContain'` ["JWT-Cookie", "XSRF-TOKEN"]
