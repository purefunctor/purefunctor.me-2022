{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
module Website.API.Blog where

import Control.Lens

import Control.Monad ( void )
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Reader ( asks )

import Data.Aeson ( FromJSON, ToJSON )
import Data.Aeson.TH ( defaultOptions, deriveJSON, fieldLabelModifier )

import Data.Maybe ( fromJust, fromMaybe )

import           Data.Text ( Text )
import qualified Data.Text as Text

import Data.Time ( UTCTime, getCurrentTime )

import Database.Persist.Sqlite

import GHC.Generics ( Generic )

import Servant
import Servant.Auth
import Servant.Auth.Server

import Control.Applicative
import Website.API.Auth
import Website.API.Common
import Website.Config
import Website.Models
import Website.WebsiteM


type BlogPostAPI =
  "blog" :>

    ( Get '[JSON] [BlogPost] :<|>

      Capture "short-title" Text :> Get '[JSON] BlogPost :<|>

      ( Auth '[JWT, Cookie] LoginPayload :>
        ( ReqBody '[JSON] MutableBlogPostData :> Post '[JSON] MutableEndpointResult
        )
      )

    )


data MutableBlogPostData
  = MutableBlogPostData
      { _title     :: Maybe Text
      , _contents  :: Maybe Text
      , _short     :: Maybe Text
      , _published :: Maybe UTCTime
      , _updated   :: Maybe UTCTime
      }

deriveJSON (defaultOptions { fieldLabelModifier = tail }) ''MutableBlogPostData
makeLenses ''MutableBlogPostData


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

    mkPost :: AuthResult LoginPayload -> MutableBlogPostData -> WebsiteM MutableEndpointResult
    mkPost (Authenticated _) payload = do
      pool <- asks connPool

      now <- liftIO getCurrentTime

      let autoShort
            = Text.intercalate "_"
            . take 3
            . Text.words

      let mPost = BlogPost
            <$> payload ^. title
            <*> ( payload ^. short <|>
                  payload ^. title <&> autoShort
                )
            <*> payload ^. contents
            <*> ( payload ^. published <|> pure now )
            <*> ( payload ^. updated <|> pure now )

      case mPost of

        Just post -> do
          void $ liftIO $ flip runSqlPersistMPool pool $ insert post
          return $ MutableEndpointResult 200 $ "Post created with short name: " <> blogPostShortTitle post

        Nothing -> throwError err400

    mkPost _ _ = throwError err401
