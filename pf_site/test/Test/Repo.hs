{-# LANGUAGE OverloadedStrings #-}
module Test.Repo where

import Control.Monad

import Data.Text.Encoding

import Test.Data
import Test.Hspec
import Test.Hspec.Wai as WaiTest
import Test.Utils

import Network.Wai

import Website.Config
import Website.Models


testRepo :: Configuration -> Application -> Spec
testRepo config app = with (pure app) $ do

  describe "GET /repo" $ do
    it "should return all repositories" $ do
      get "/repo" `shouldRespondWith` matchCodeJSON 200 repos

    it "should return a specific repository" $ do
      forM_ repos $ \repo' -> do
        let endpoint = "/repo/" <> encodeUtf8 (repositoryName repo')
        get endpoint `shouldRespondWith` matchCodeJSON 200 repo'
