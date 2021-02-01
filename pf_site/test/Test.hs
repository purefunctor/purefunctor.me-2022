module Main (main) where

import Test.Hspec

import Website.App
import Website.Config


main :: IO ()
main = do
  (config, app) <- debug_

  hspec $ do
    describe "Hello, World" $ do
      it "has 12 characters" $ do
        length "Hello, World" `shouldBe` 12
