{-# LANGUAGE OverloadedStrings #-}
module Test.Repo where

import Control.Lens ( (^.) )
import Control.Monad

import Data.Aeson
import Data.Text.Encoding

import           Database.Persist.Sqlite ( (==.) )
import qualified Database.Persist.Sqlite as Sqlite

import Network.Wai

import Test.Data
import Test.Hspec
import Test.Hspec.Wai as WaiTest
import Test.Utils

import Website.Config
import Website.Models
import Website.Server.API.Repo


testRepo :: Environment -> Application -> Spec
testRepo env app = with (pure app) $ do

  let loginPayload = mkLoginPayload env

  describe "GET /api/repo" $ do
    it "should return all repositories" $ do
      get "/api/repo" `shouldRespondWith` matchCodeJSON 200 repos

    it "should return a specific repository" $ do
      forM_ repos $ \repo' -> do
        let endpoint = "/api/repo/" <> encodeUtf8 (repositoryName repo')
        get endpoint `shouldRespondWith` matchCodeJSON 200 repo'

  let rName        = "purefunctor.me-legacy"
  let rOwner       = "PureFunctor"
  let rUrl         = "https://github.com/PureFunctor/purefunctor.me-legacy"
  let rDescription = "Old website"
  let rStars       = 0
  let rCommits     = 0

  let newRepo  = MutableRepositoryData
        (Just rName)
        (Just rOwner)
        (Just rUrl)
        (Just rDescription)
        (Just rStars)
        (Just rCommits)

  describe "POST /api/repo" $ do
    it "should require authentication" $ do
      postJSON "/api/repo" [] (toJSON newRepo) `shouldRespondWith` 401

    it "should mutate the datababse" $ do
      withAuth env $ \authHeaders -> do
        postJSON "/api/repo" authHeaders (toJSON newRepo) `shouldRespondWith` 200

        inDB <- WaiTest.liftIO $ flip Sqlite.runSqlPool (env^.pool) $ do
          Sqlite.exists [ RepositoryName ==. rName ]

        inDB `shouldBe'` True

    it "should require name and owner" $ do
      let invalid =
            [ newRepo { _name = Nothing , _owner = Nothing }
            , newRepo { _name = Nothing }
            , newRepo { _owner = Nothing }
            ]

      withAuth env $ \authHeaders ->
        forM_ invalid $ \payload ->
          postJSON "/api/repo" authHeaders (toJSON payload) `shouldRespondWith` 400

  describe "PUT /api/repo/<repository-name>" $ do
    let endpoint = "/api/repo/" <> encodeUtf8 rName
    let commits' = 573 :: Int
    let mutation = object
          [ "commits" .= commits'
          ]

    it "should require authentication" $ do
      putJSON endpoint [] mutation `shouldRespondWith` 401

    it "should mutate the database" $ do
      withAuth env $ \authHeaders -> do
        putJSON endpoint authHeaders mutation `shouldRespondWith` 200

        mMutated <-
          WaiTest.liftIO $ flip Sqlite.runSqlPersistMPool (env^.pool) $
            Sqlite.get (RepositoryKey rName)

        case mMutated of
          Just mutated -> repositoryCommits mutated `shouldBe'` commits'
          Nothing      -> fail "failed to obtain blog post"

    it "should require at least one field" $ do
      withAuth env $ \authHeaders ->
        putJSON endpoint authHeaders (object []) `shouldRespondWith` 400

  describe "DELETE /api/repo/<repository-name>" $ do
    let endpoint = "/api/repo/" <> encodeUtf8 rName

    it "should require authentication" $ do
      delete' endpoint [] `shouldRespondWith` 401

    it "should mutate the database" $ do
      withAuth env $ \authHeaders -> do
        delete' endpoint authHeaders `shouldRespondWith` 200

        inDB <- WaiTest.liftIO $ flip Sqlite.runSqlPool (env^.pool) $ do
          Sqlite.exists [ RepositoryName ==. rName ]

        inDB `shouldBe'` False
