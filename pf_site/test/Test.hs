module Main (main) where

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Hello, World" $ do
    it "has 12 characters" $ do
      length "Hello, World" `shouldBe` 12
