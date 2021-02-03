{-# LANGUAGE OverloadedStrings #-}
module Test.Blog where

import Control.Monad

import Data.Text.Encoding

import Test.Data
import Test.Hspec
import Test.Hspec.Wai as WaiTest
import Test.Utils

import Network.Wai

import Website.Config
import Website.Models


testBlog :: Configuration -> Application -> Spec
testBlog config app = with (pure app) $ do
  describe "GET /blog" $ do
    it "should return all blog posts" $ do
      get "/blog" `shouldRespondWith` matchCodeJSON 200 posts
    it "should return a specific post" $ do
      forM_ posts $ \post' -> do
        let endpoint = "/blog/" <> encodeUtf8 (blogPostShortTitle post')
        get endpoint `shouldRespondWith` matchCodeJSON 200 post'

  describe "POST /blog" $ do
    it "should require authentication " $ do
      WaiTest.pendingWith "POST /blog"
    it "should mutate the database" $ do
      WaiTest.pendingWith "POST /blog"
    it "should require title and contents" $ do
      WaiTest.pendingWith "POST /blog"

  describe "PUT /blog" $ do
    it "should require authentication " $ do
      WaiTest.pendingWith "PUT /blog/short-title"
    it "should mutate the database" $ do
      WaiTest.pendingWith "PUT /blog/short-title"
    it "should require either title, contents, or short" $ do
      WaiTest.pendingWith "PUT /blog/short-title"

  describe "DELETE /blog" $ do
    it "should require authentication " $ do
      WaiTest.pendingWith "DELETE /blog/short-title"
    it "should mutate the database" $ do
      WaiTest.pendingWith "DELETE /blog/short-title"
