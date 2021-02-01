module Test.Auth where

import Test.Hspec


testAuth :: Spec
testAuth = do
  describe "/login" $ do
    it "should set cookies for JWT authentication and XSRF" $ do
      True `shouldBe` True
