module Main (main) where

import Test.Auth
import Test.Hspec

import Website.Debug
import Website.Config


main :: IO ()
main = do
  (config, app) <- debug_
  hspec $ do
    testAuth config app
