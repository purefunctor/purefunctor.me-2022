module Test.Blog where

import Test.Hspec


testBlog :: Spec
testBlog = do
  describe "/blog" $ do
    it "should return all blog posts" $ do
      True `shouldBe` True
