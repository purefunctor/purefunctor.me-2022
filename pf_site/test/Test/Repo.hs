{-# LANGUAGE OverloadedStrings #-}
module Test.Repo where

import Control.Monad

import Data.Aeson

import           Database.Persist.Sqlite ( (==.) )
import qualified Database.Persist.Sqlite as Sqlite

import Data.Text.Encoding

import Network.Wai

import Test.Data
import Test.Hspec
import Test.Hspec.Wai as WaiTest
import Test.Utils

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
      withAuth config $ \authHeaders -> do
        postJSON "/repo" authHeaders (toJSON newRepo) `shouldRespondWith` 200

        inDB <- WaiTest.liftIO $ flip Sqlite.runSqlPool (connPool config) $ do
          Sqlite.exists [ RepositoryName ==. rName ]

        inDB `shouldBe'` True

    it "should require name and owner" $ do
      let invalid =
            [ newRepo { _name = Nothing , _owner = Nothing }
            , newRepo { _name = Nothing }
            , newRepo { _owner = Nothing }
            ]

      withAuth config $ \authHeaders ->
        forM_ invalid $ \payload ->
          postJSON "/repo" authHeaders (toJSON payload) `shouldRespondWith` 400

  describe "PUT /repo/<repository-name>" $ do
    let endpoint = "/repo/" <> encodeUtf8 rName
    let commits' = 573 :: Int
    let mutation = object
          [ "commits" .= commits'
          ]

    it "should require authentication" $ do
      putJSON endpoint [] mutation `shouldRespondWith` 401

    it "should mutate the database" $ do
      withAuth config $ \authHeaders -> do
        putJSON endpoint authHeaders mutation `shouldRespondWith` 200

        mMutated <-
          WaiTest.liftIO $ flip Sqlite.runSqlPersistMPool (connPool config) $
            Sqlite.get (RepositoryKey rName)

        case mMutated of
          Just mutated -> repositoryCommits mutated `shouldBe'` commits'
          Nothing      -> fail "failed to obtain blog post"

    it "should require at least one field" $ do
      withAuth config $ \authHeaders ->
        putJSON endpoint authHeaders (object []) `shouldRespondWith` 400

  describe "DELETE /repo/<repository-name>" $ do
    let endpoint = "/repo/" <> encodeUtf8 rName

    it "should require authentication" $ do
      delete' endpoint [] `shouldRespondWith` 401

    it "should mutate the database" $ do
      withAuth config $ \authHeaders -> do
        delete' endpoint authHeaders `shouldRespondWith` 200

        inDB <- WaiTest.liftIO $ flip Sqlite.runSqlPool (connPool config) $ do
          Sqlite.exists [ RepositoryName ==. rName ]

        inDB `shouldBe'` False
