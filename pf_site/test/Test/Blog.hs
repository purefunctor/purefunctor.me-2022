{-# LANGUAGE OverloadedStrings #-}
module Test.Blog where

import Control.Monad

import Data.Aeson

import Data.Text.Encoding

import Data.Time
import Data.Time.Calendar.Julian

import           Database.Persist.Sqlite ( (==.) )
import qualified Database.Persist.Sqlite as Sqlite

import Test.Data
import Test.Hspec
import Test.Hspec.Wai as WaiTest
import Test.Utils

import Network.Wai

import Website.API.Blog
import Website.Config
import Website.Models


testBlog :: Configuration -> Application -> Spec
testBlog config app = with (pure app) $ do

  let loginPayload = mkLoginPayload config

  describe "GET /blog" $ do
    it "should return all blog posts" $ do
      get "/blog" `shouldRespondWith` matchCodeJSON 200 posts

    it "should return a specific post" $ do
      forM_ posts $ \post' -> do
        let endpoint = "/blog/" <> encodeUtf8 (blogPostShortTitle post')
        get endpoint `shouldRespondWith` matchCodeJSON 200 post'

  let lTitle  = "Testing With Hspec"
  let sTitle  = "testing-with-hspec"
  let pBody   = "Nothing to see yet"
  let tNow    = UTCTime (fromJulian 2020 02 02) (secondsToDiffTime 0)
  let tNext   = UTCTime (fromJulian 2020 02 03) (secondsToDiffTime 0)

  let newPost = MutableBlogPostData
        (Just lTitle) (Just sTitle) (Just pBody) (Just tNow) (Just tNext)

  describe "POST /blog" $ do
    it "should require authentication " $ do
      postJSON "/blog" [] (toJSON newPost) `shouldRespondWith` 401

    it "should mutate the database" $ do
      mAuthHeaders <-
        mkAuthHeaders . parseSetCookies <$> postJSON "/login" [] loginPayload

      case mAuthHeaders of
        Just authHeaders -> do
          postJSON "/blog" authHeaders (toJSON newPost) `shouldRespondWith` 200

          inDB <-  WaiTest.liftIO $ flip Sqlite.runSqlPool (connPool config) $ do
            Sqlite.exists [ BlogPostShortTitle ==. sTitle ]

          inDB `shouldBe'` True

        Nothing -> fail "failed to create authentication headers"

    it "should require title and contents" $ do
      let invalid =
            [ newPost { _title = Nothing }
            , newPost { _contents = Nothing }
            ]

      mAuthHeaders <-
        mkAuthHeaders . parseSetCookies <$> postJSON "/login" [] loginPayload

      case mAuthHeaders of
        Just authHeaders ->
          forM_ invalid $ \payload -> do
            postJSON "/blog" authHeaders (toJSON payload) `shouldRespondWith` 400

        Nothing -> fail "failed to create authentication headers"

  describe "PUT /blog/<short-title>" $ do
    let endpoint = "/blog/" <> encodeUtf8 sTitle
    let contents' = "BDD in Haskell with Hspec"
    let mutation = object
          [ "contents" .= contents'
          ]

    it "should require authentication " $ do
      putJSON endpoint [] mutation `shouldRespondWith` 401

    it "should mutate the database" $ do
      mAuthHeaders <-
        mkAuthHeaders . parseSetCookies <$> postJSON "/login" [] loginPayload

      case mAuthHeaders of
        Just authHeaders -> do
          putJSON endpoint authHeaders mutation `shouldRespondWith` 200

          mMutated <-
            WaiTest.liftIO $ flip Sqlite.runSqlPersistMPool (connPool config) $
              Sqlite.get (BlogPostKey sTitle)

          case mMutated of
            Just mutated -> blogPostContents mutated `shouldBe'` contents'
            Nothing      -> fail "failed to obtain blog post"

        Nothing -> fail "failed to create authentication headers"

    it "should require either title, contents, or short" $ do
      mAuthHeaders <-
        mkAuthHeaders . parseSetCookies <$> postJSON "/login" [] loginPayload

      case mAuthHeaders of
        Just authHeaders ->
          putJSON endpoint authHeaders (object []) `shouldRespondWith` 400
        Nothing -> fail "failed to create authentication headers"

  describe "DELETE /blog/<short-title>" $ do
    let endpoint = "/blog/" <> encodeUtf8 sTitle

    it "should require authentication " $ do
      delete' endpoint [] `shouldRespondWith` 401

    it "should mutate the database" $ do
      mAuthHeaders <-
        mkAuthHeaders . parseSetCookies <$> postJSON "/login" [] loginPayload

      case mAuthHeaders of
        Just authHeaders -> do
          delete' endpoint authHeaders `shouldRespondWith` 200

          inDB <- WaiTest.liftIO $ flip Sqlite.runSqlPool (connPool config) $ do
            Sqlite.exists [ BlogPostShortTitle ==. sTitle ]

          inDB `shouldBe'` False

        Nothing -> fail "failed to create authentication headers"
