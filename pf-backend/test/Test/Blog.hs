{-# LANGUAGE OverloadedStrings #-}
module Test.Blog where

import Control.Lens ( (^.), _Just )
import Control.Monad

import Data.Aeson
import Data.Text.Encoding
import Data.Time
import Data.Time.Calendar.Julian
import Data.Maybe (isJust)

import Database.Beam

import Network.Wai

import qualified Test.Data as TD
import Test.Hspec
import Test.Hspec.Wai as WaiTest
import Test.Utils

import Website.Config
import Website.Database
import Website.Server.API.Blog


testBlog :: Environment -> Application -> Spec
testBlog env app = with (pure app) $ do

  let loginPayload = TD.mkLoginPayload env

  describe "GET /api/blog" $ do
    it "should return all blog posts" $ do
      get "/api/blog" `shouldRespondWith` matchCodeJSON 200 TD.posts

    it "should return a specific post" $ do
      forM_ TD.posts $ \post' -> do
        let endpoint = "/api/blog/" <> encodeUtf8 (post'^.pSlug)
        get endpoint `shouldRespondWith` matchCodeJSON 200 post'

  let lTitle  = "Testing With Hspec"
  let sTitle  = "testing-with-hspec"
  let pBody   = "Nothing to see yet"
  let tNow    = UTCTime (fromJulian 2020 02 02) (secondsToDiffTime 0)
  let tNext   = UTCTime (fromJulian 2020 02 03) (secondsToDiffTime 0)

  let newPost = MutableBlogPostData
        (Just lTitle) (Just sTitle) (Just pBody) (Just tNow) (Just tNext)

  describe "POST /api/blog" $ do
    it "should require authentication" $ do
      postJSON "/api/blog" [] (toJSON newPost) `shouldRespondWith` 401

    it "should mutate the database" $ do
      withAuth env $ \authHeaders -> do
        postJSON "/api/blog" authHeaders (toJSON newPost) `shouldRespondWith` 200

        post' <- runBeamDb env $ runSelectReturningOne $
          lookup_ (websiteDb^.posts) (BlogPostSlug sTitle)

        isJust post' `shouldBe'` True

    it "should require title and contents" $ do
      let invalid =
            [ newPost { _title = Nothing }
            , newPost { _contents = Nothing }
            ]

      withAuth env $ \authHeaders ->
        forM_ invalid $ \payload ->
          postJSON "/api/blog" authHeaders (toJSON payload) `shouldRespondWith` 400

  describe "PUT /api/blog/<short-title>" $ do
    let endpoint = "/api/blog/" <> encodeUtf8 sTitle
    let contents' = "BDD in Haskell with Hspec"
    let mutation = object
          [ "contents" .= contents'
          ]

    it "should require authentication" $ do
      putJSON endpoint [] mutation `shouldRespondWith` 401

    it "should mutate the database" $ do
      withAuth env $ \authHeaders -> do
        putJSON endpoint authHeaders mutation `shouldRespondWith` 200

        mMutated <- runBeamDb env $ runSelectReturningOne $
          lookup_ (websiteDb^.posts) (BlogPostSlug sTitle)

        ( mMutated^._Just.pCont ) `shouldBe'` contents'

    it "should require either title, contents, or short" $ do
      withAuth env $ \authHeaders ->
        putJSON endpoint authHeaders (object []) `shouldRespondWith` 400

  describe "DELETE /api/blog/<short-title>" $ do
    let endpoint = "/api/blog/" <> encodeUtf8 sTitle

    it "should require authentication" $ do
      delete' endpoint [] `shouldRespondWith` 401

    it "should mutate the database" $ do
      withAuth env $ \authHeaders -> do
        delete' endpoint authHeaders `shouldRespondWith` 200

        post' <- runBeamDb env $ runSelectReturningOne $
          lookup_ (websiteDb^.posts) (BlogPostSlug sTitle)

        isJust post' `shouldBe'` False
