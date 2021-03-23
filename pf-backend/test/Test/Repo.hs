{-# LANGUAGE OverloadedStrings #-}
module Test.Repo where

import Control.Lens ( (^.), _Just )
import Control.Monad

import Data.Aeson
import Data.Text.Encoding

import Database.Beam

import Network.Wai

import qualified Test.Data as TD
import Test.Hspec
import Test.Hspec.Wai as WaiTest
import Test.Utils

import Website.Config
import Website.Database
import Website.Server.API.Repo
import Data.Maybe (isJust)


testRepo :: Environment -> Application -> Spec
testRepo env app = with (pure app) $ do

  let loginPayload = TD.mkLoginPayload env

  describe "GET /api/repo" $ do
    it "should return all repositories" $ do
      get "/api/repo" `shouldRespondWith` matchCodeJSON 200 TD.repos

    it "should return a specific repository" $ do
      forM_ TD.repos $ \repo' -> do
        let endpoint = "/api/repo/" <> encodeUtf8 (repo'^.rName)
        get endpoint `shouldRespondWith` matchCodeJSON 200 repo'

  let
    reName  = "purefunctor.me-legacy"
    reOwner = "PureFunctor"
    reUrl   = "https://github.com/PureFunctor/purefunctor.me"
    reLang  = "Python"
    reDesc  = "Old website"
    reStars = 0
    reCommits = 0

    newRepo = MutableRepositoryData
      (Just reName)
      (Just reOwner)
      (Just reUrl)
      (Just reLang)
      (Just reDesc)
      (Just reStars)
      (Just reCommits)

  describe "POST /api/repo" $ do
    it "should require authentication" $ do
      postJSON "/api/repo" [] (toJSON newRepo) `shouldRespondWith` 401

    it "should mutate the datababse" $ do
      withAuth env $ \authHeaders -> do
        postJSON "/api/repo" authHeaders (toJSON newRepo) `shouldRespondWith` 200

        post' <- runBeamDb env $ runSelectReturningOne $
          lookup_ (websiteDb^.repos) (RepositoryName reName)

        isJust post' `shouldBe'` True

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
    let endpoint = "/api/repo/" <> encodeUtf8 reName
    let commits' = 573 :: Int
    let mutation = object
          [ "commits" .= commits'
          ]

    it "should require authentication" $ do
      putJSON endpoint [] mutation `shouldRespondWith` 401

    it "should mutate the database" $ do
      withAuth env $ \authHeaders -> do
        putJSON endpoint authHeaders mutation `shouldRespondWith` 200

        mMutated <- runBeamDb env $ runSelectReturningOne $
          lookup_ (websiteDb^.repos) (RepositoryName reName)

        ( mMutated^._Just.rName ) `shouldBe'` reName

    it "should require at least one field" $ do
      withAuth env $ \authHeaders ->
        putJSON endpoint authHeaders (object []) `shouldRespondWith` 400

  describe "DELETE /api/repo/<repository-name>" $ do
    let endpoint = "/api/repo/" <> encodeUtf8 reName

    it "should require authentication" $ do
      delete' endpoint [] `shouldRespondWith` 401

    it "should mutate the database" $ do
      withAuth env $ \authHeaders -> do
        delete' endpoint authHeaders `shouldRespondWith` 200

        post' <- runBeamDb env $ runSelectReturningOne $
          lookup_ (websiteDb^.repos) (RepositoryName reName)

        isJust post' `shouldBe'` False
