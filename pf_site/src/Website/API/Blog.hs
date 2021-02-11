module Website.API.Blog where

import Control.Applicative

import Control.Lens

import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Reader ( ask )

import Data.Maybe ( isJust )

import           Data.Text ( Text )
import qualified Data.Text as Text

import Data.Time ( UTCTime, getCurrentTime )

import Database.Persist.Sqlite

import Servant
import Servant.Auth.Server

import Website.API.Auth
import Website.API.Common
import Website.Models
import Website.Utils
import Website.WebsiteM


type BlogPostAPI =
  "blog" :>

    ( Get '[JSON] [BlogPost] :<|>

      Capture "short-title" Text :> Get '[JSON] BlogPost :<|>

      RequiresAuth
        :> ReqBody '[JSON] MutableBlogPostData
          :> Post '[JSON] MutableEndpointResult :<|>

      RequiresAuth
        :> Capture "short-title" Text
          :> ReqBody '[JSON] MutableBlogPostData
            :> Put '[JSON] MutableEndpointResult :<|>

      RequiresAuth
        :> Capture "short-title" Text
          :> Delete '[JSON] MutableEndpointResult
    )


data MutableBlogPostData
  = MutableBlogPostData
      { _title     :: Maybe Text
      , _short     :: Maybe Text
      , _contents  :: Maybe Text
      , _published :: Maybe UTCTime
      , _updated   :: Maybe UTCTime
      }

deriveJSON' ''MutableBlogPostData
makeLenses ''MutableBlogPostData


blogPostServer :: ServerT BlogPostAPI WebsiteM
blogPostServer = getPosts :<|> getPost :<|> createPost :<|> updatePost :<|> deletePost
  where
    getPosts :: WebsiteM [BlogPost]
    getPosts = do
      env <- ask

      posts <- runDb env $
        selectList [ ] [ ]

      return $ entityVal <$> posts

    getPost :: Text -> WebsiteM BlogPost
    getPost t = do
      env <- ask

      post <- runDb env $
        get (BlogPostKey t)

      case post of
        (Just post') -> return post'
        Nothing      -> throwError err404

    createPost
      :: AuthResult LoginPayload
      -> MutableBlogPostData
      -> WebsiteM MutableEndpointResult
    createPost (Authenticated _) payload = do
      env <- ask

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
          runDb env $ insert post

          let message = "Post created with short name:" <> blogPostShortTitle post
          let result = MutableEndpointResult 200 message

          return result

        Nothing -> throwError err400

    createPost _ _ = throwError err401

    updatePost
      :: AuthResult LoginPayload
      -> Text
      -> MutableBlogPostData
      -> WebsiteM MutableEndpointResult
    updatePost (Authenticated _) sTitle payload = do
      env <- ask

      now <- liftIO getCurrentTime

      let mUpdates = filter isJust
            [ (BlogPostFullTitle  =.) <$> payload ^. title
            , (BlogPostContents   =.) <$> payload ^. contents
            , (BlogPostShortTitle =.) <$> payload ^. short
            ]

      case mUpdates of

        [] -> throwError err400

        mUpdates' -> do
          let postUpdated = Just $ BlogPostUpdated =. payload ^. updated . non now

          case sequenceA $ postUpdated : mUpdates' of

            Just updates -> do
              runDb env $
                update (BlogPostKey sTitle) updates
              return $ MutableEndpointResult 200 "Post updated."

            Nothing -> throwError err400

    updatePost _ _ _ = throwError err401

    deletePost
      :: AuthResult LoginPayload
      -> Text
      -> WebsiteM MutableEndpointResult
    deletePost (Authenticated _) sTitle = do
      env <- ask

      inDatabase <- runDb env $
        exists [ BlogPostShortTitle ==. sTitle ]

      if inDatabase
        then do
          runDb env $ delete $ BlogPostKey sTitle
          return $ MutableEndpointResult 200 "Post deleted."
        else
          throwError err404

    deletePost _ _ = throwError err401
