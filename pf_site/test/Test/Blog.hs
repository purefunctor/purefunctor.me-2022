{-# LANGUAGE OverloadedStrings #-}
module Test.Blog where

import Control.Lens ( (^.) )

import Control.Monad

import Data.Aeson

import Data.Text.Encoding

import Data.Time
import Data.Time.Calendar.Julian

import           Database.Persist.Sqlite ( (==.) )
import qualified Database.Persist.Sqlite as Sqlite

import Network.Wai

import Test.Data
import Test.Hspec
import Test.Hspec.Wai as WaiTest
import Test.Utils

import Website.API.Blog
import Website.Config
import Website.Models


testBlog :: Environment -> Application -> Spec
testBlog env app = with (pure app) $ do

  let loginPayload = mkLoginPayload env

  describe "GET /blog" $ do
    it "should return all blog posts" $ do
      get "/blog" `shouldRespondWith` matchCodeJSON 200 posts

    it "should return a specific post" $ do
      forM_ posts $ \post' -> do
        let endpoint = "/blog/" <> encodeUtf8 (blogPostShort post')
        get endpoint `shouldRespondWith` matchCodeJSON 200 post'

  let lTitle  = "Testing With Hspec"
  let sTitle  = "testing-with-hspec"
  let pBody   = "Nothing to see yet"
  let tNow    = UTCTime (fromJulian 2020 02 02) (secondsToDiffTime 0)
  let tNext   = UTCTime (fromJulian 2020 02 03) (secondsToDiffTime 0)

  let newPost = MutableBlogPostData
        (Just lTitle) (Just sTitle) (Just pBody) (Just tNow) (Just tNext)

  describe "POST /blog" $ do
    it "should require authentication" $ do
      postJSON "/blog" [] (toJSON newPost) `shouldRespondWith` 401

    it "should mutate the database" $ do
      withAuth env $ \authHeaders -> do
        postJSON "/blog" authHeaders (toJSON newPost) `shouldRespondWith` 200

        inDB <-  WaiTest.liftIO $ flip Sqlite.runSqlPool (env^.pool) $ do
          Sqlite.exists [ BlogPostShort ==. sTitle ]

        inDB `shouldBe'` True

    it "should require title and contents" $ do
      let invalid =
            [ newPost { _title = Nothing }
            , newPost { _contents = Nothing }
            ]

      withAuth env $ \authHeaders ->
        forM_ invalid $ \payload ->
          postJSON "/blog" authHeaders (toJSON payload) `shouldRespondWith` 400

  describe "PUT /blog/<short-title>" $ do
    let endpoint = "/blog/" <> encodeUtf8 sTitle
    let contents' = "BDD in Haskell with Hspec"
    let mutation = object
          [ "contents" .= contents'
          ]

    it "should require authentication" $ do
      putJSON endpoint [] mutation `shouldRespondWith` 401

    it "should mutate the database" $ do
      withAuth env $ \authHeaders -> do
        putJSON endpoint authHeaders mutation `shouldRespondWith` 200

        mMutated <-
          WaiTest.liftIO $ flip Sqlite.runSqlPersistMPool (env^.pool) $
            Sqlite.get (BlogPostKey sTitle)

        case mMutated of
          Just mutated -> blogPostContents mutated `shouldBe'` contents'
          Nothing      -> fail "failed to obtain blog post"

    it "should require either title, contents, or short" $ do
      withAuth env $ \authHeaders ->
        putJSON endpoint authHeaders (object []) `shouldRespondWith` 400

  describe "DELETE /blog/<short-title>" $ do
    let endpoint = "/blog/" <> encodeUtf8 sTitle

    it "should require authentication" $ do
      delete' endpoint [] `shouldRespondWith` 401

    it "should mutate the database" $ do
      withAuth env $ \authHeaders -> do
        delete' endpoint authHeaders `shouldRespondWith` 200

        inDB <- WaiTest.liftIO $ flip Sqlite.runSqlPool (env^.pool) $ do
          Sqlite.exists [ BlogPostShort ==. sTitle ]

        inDB `shouldBe'` False
