{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Website.API.Blog where

import Control.Monad ( void )
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Reader ( asks )

import Data.Aeson ( FromJSON, ToJSON )

import Data.Maybe ( fromMaybe )

import           Data.Text ( Text )
import qualified Data.Text as Text

import Data.Time ( getCurrentTime )

import Database.Persist.Sqlite

import GHC.Generics ( Generic )

import Servant
import Servant.Auth
import Servant.Auth.Server

import Website.API.Auth
import Website.Config
import Website.Models
import Website.WebsiteM


type BlogPostAPI =
  "blog" :>

    ( Get '[JSON] [BlogPost] :<|>

      Capture "short-title" Text :> Get '[JSON] BlogPost :<|>

      ( Auth '[JWT, Cookie] LoginPayload :>
        ( ReqBody '[JSON] CreateBlogPostData :> Post '[JSON] BlogPost
        )
      )

    )


data CreateBlogPostData
  = CreateBlogPostData
      { title   :: Text
      , content :: Text
      , short   :: Maybe Text
      }
  deriving (FromJSON, Generic, ToJSON)


blogPostServer :: ServerT BlogPostAPI WebsiteM
blogPostServer = getPosts :<|> getPost :<|> mkPost
  where
    getPosts :: WebsiteM [BlogPost]
    getPosts = do
      pool <- asks connPool

      posts <- liftIO $ flip runSqlPersistMPool pool $
        selectList [ ] [ ]

      return $ entityVal <$> posts

    getPost :: Text -> WebsiteM BlogPost
    getPost t = do
      pool <- asks connPool

      post <- liftIO $ flip runSqlPersistMPool pool $
        selectFirst [ BlogPostShortTitle ==. t ] [ ]

      case post of
        (Just post') -> return $ entityVal post'
        Nothing      -> throwError err404

    mkPost ::  AuthResult LoginPayload -> CreateBlogPostData -> WebsiteM BlogPost
    mkPost (Authenticated login) (CreateBlogPostData title content short) = do
      pool <- asks connPool

      now <- liftIO getCurrentTime

      let alt = Text.intercalate "_"
              . take 3
              . Text.words
              $ title

      let short' = fromMaybe alt short

      let post = BlogPost title short' content now now

      void $ liftIO $ flip runSqlPersistMPool pool $ insert post

      return post

    mkPost _ _ = throwError err401
