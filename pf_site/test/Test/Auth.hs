{-# LANGUAGE OverloadedStrings #-}
module Test.Auth where

import Test.Hspec
import Test.Hspec.Wai as WaiTest
import Test.Utils

import Data.Aeson

import qualified Data.ByteString as BS

import Network.Wai
import Network.Wai.Test as WaiTest

import Web.Cookie

import Website.Config


testAuth :: Configuration -> Application -> Spec
testAuth config app = with (pure app) $ do
  describe "POST /login" $ do
    let value = object
          [ "username" .= adminUser config
          , "password" .= adminUser config
          ]
    it "should return 204 on a successful login" $ do
      postJSON "/login" [] value `shouldRespondWith` 204

    it "should set the appropriate cookies" $ do
      setCookies <- parseSetCookies <$> postJSON "/login" [] value
      (setCookieName <$> setCookies) `shouldContain'` ["JWT-Cookie", "XSRF-TOKEN"]
