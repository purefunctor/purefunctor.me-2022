{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}
module Website.API.Blog where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import Database.Persist.Sqlite
import GHC.Generics (Generic)
import Servant
import Website.Models


type BlogPostAPI =
  "blog" :> Get '[JSON] [BlogPost] :<|>
  "blog" :> Capture "short-title" Text :> Get '[JSON] BlogPost :<|>
  "blog" :> ReqBody '[JSON] CreateBlogPostData :> Post '[JSON] BlogPost


data CreateBlogPostData = CreateBlogPostData
  { title   :: Text
  , content :: Text
  , short   :: Maybe Text
  } deriving (Generic, FromJSON, ToJSON)


blogPostServer :: ConnectionPool -> Server BlogPostAPI
blogPostServer pool = getPosts :<|> getPost :<|> mkPost
  where
    getPosts :: Handler [BlogPost]
    getPosts = do
      posts <- liftIO $ flip runSqlPersistMPool pool $
        selectList [ ] [ ]
      return $ entityVal <$> posts

    getPost :: Text -> Handler BlogPost
    getPost t = do
      post <- liftIO $ flip runSqlPersistMPool pool $
        selectFirst [ BlogPostShortTitle ==. t ] [ ]
      case post of
        (Just post') -> return $ entityVal post'
        Nothing -> throwError err404

    mkPost :: CreateBlogPostData -> Handler BlogPost
    mkPost (CreateBlogPostData title content short) = do
      now <- liftIO getCurrentTime

      let alt = Text.intercalate "_"
              . take 3
              . Text.words
              $ title

      let short' = fromMaybe alt short

      let post = BlogPost title short' content now now

      void $ liftIO $ flip runSqlPersistMPool pool $ insert post

      return post
