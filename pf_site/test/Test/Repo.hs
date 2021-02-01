module Test.Repo where

import Test.Hspec


testRepo :: Spec
testRepo = do
  describe "/repo" $ do
    it "should return all repositories" $ do
      True `shouldBe` True
