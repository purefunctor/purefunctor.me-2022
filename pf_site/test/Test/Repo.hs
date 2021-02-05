{-# LANGUAGE OverloadedStrings #-}
module Test.Repo where

import Control.Monad

import Data.Aeson

import           Database.Persist.Sqlite ( (==.) )
import qualified Database.Persist.Sqlite as Sqlite

import Data.Text.Encoding

import Test.Data
import Test.Hspec
import Test.Hspec.Wai as WaiTest
import Test.Utils

import Network.Wai

import Website.API.Repo
import Website.Config
import Website.Models


testRepo :: Configuration -> Application -> Spec
testRepo config app = with (pure app) $ do

  let loginPayload = mkLoginPayload config

  describe "GET /repo" $ do
    it "should return all repositories" $ do
      get "/repo" `shouldRespondWith` matchCodeJSON 200 repos

    it "should return a specific repository" $ do
      forM_ repos $ \repo' -> do
        let endpoint = "/repo/" <> encodeUtf8 (repositoryName repo')
        get endpoint `shouldRespondWith` matchCodeJSON 200 repo'

  let rName    = "example-boilerplate"
  let rOwner   = "PureFunctor"
  let rUrl     = "https://github.com/PureFunctor/example-boilerplate"
  let rStars   = 0
  let rCommits = 0

  let newRepo  = MutableRepositoryData
        (Just rName) (Just rOwner) (Just rUrl) (Just rStars) (Just rCommits)

  describe "POST /repo" $ do
    it "should require authentication" $ do
      postJSON "/repo" [] (toJSON newRepo) `shouldRespondWith` 401

    it "should mutate the datababse" $ do
      mAuthHeaders <-
        mkAuthHeaders . parseSetCookies <$> postJSON "/login" [] loginPayload

      case mAuthHeaders of
        Just authHeaders -> do
          postJSON "/repo" authHeaders (toJSON newRepo) `shouldRespondWith` 200

          inDB <- WaiTest.liftIO $ flip Sqlite.runSqlPool (connPool config) $ do
            Sqlite.exists [ RepositoryName ==. rName ]

          inDB `shouldBe'` True

        Nothing -> fail "failed to create authentication headers"

    it "should require name and owner" $ do
      let invalid =
            [ newRepo { _name = Nothing , _owner = Nothing }
            , newRepo { _name = Nothing }
            , newRepo { _owner = Nothing }
            ]

      mAuthHeaders <-
        mkAuthHeaders . parseSetCookies <$> postJSON "/login" [] loginPayload

      case mAuthHeaders of
        Just authHeaders ->
          forM_ invalid $ \payload ->
            postJSON "/repo" authHeaders (toJSON payload) `shouldRespondWith` 400

        Nothing -> fail "failed to create authentication headers"
